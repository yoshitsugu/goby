use std::collections::{HashMap, HashSet};

use goby_core::{Expr, Module, Stmt, ast::InterpolatedPart, types::strip_effect};

use crate::call::flatten_named_call;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LoweringStyle {
    DirectStyle,
    EffectBoundary,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DeclarationLoweringMode {
    pub(crate) declaration_name: String,
    pub(crate) style: LoweringStyle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct EffectId(pub(crate) u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct OpId(pub(crate) u16);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct EffectOperationRef {
    pub(crate) effect_id: EffectId,
    pub(crate) op_id: OpId,
    pub(crate) effect_name: String,
    pub(crate) op_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DeclarationEvidenceRequirement {
    style: LoweringStyle,
    required_effects: Vec<EffectId>,
    referenced_operations: Vec<EffectOperationRef>,
}

impl DeclarationEvidenceRequirement {
    pub(crate) fn style(&self) -> LoweringStyle {
        self.style
    }

    pub(crate) fn passes_evidence(&self) -> bool {
        self.style == LoweringStyle::EffectBoundary
    }

    pub(crate) fn required_effect_count(&self) -> usize {
        self.required_effects.len()
    }

    pub(crate) fn referenced_operation_count(&self) -> usize {
        self.referenced_operations.len()
    }

    pub(crate) fn required_effects(&self) -> &[EffectId] {
        &self.required_effects
    }

    pub(crate) fn referenced_operations(&self) -> &[EffectOperationRef] {
        &self.referenced_operations
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct EvidencePayloadShape {
    operation_table: Vec<EffectOperationRef>,
    declaration_requirements: HashMap<String, DeclarationEvidenceRequirement>,
}

impl EvidencePayloadShape {
    pub(crate) fn operation_table(&self) -> &[EffectOperationRef] {
        &self.operation_table
    }

    pub(crate) fn operation_table_len(&self) -> usize {
        self.operation_table.len()
    }

    pub(crate) fn requirements_len(&self) -> usize {
        self.declaration_requirements.len()
    }

    pub(crate) fn requirement_for(
        &self,
        declaration_name: &str,
    ) -> Option<&DeclarationEvidenceRequirement> {
        self.declaration_requirements.get(declaration_name)
    }

    #[cfg(test)]
    pub(crate) fn operation_for_qualified(
        &self,
        effect_name: &str,
        op_name: &str,
    ) -> Option<EffectOperationRef> {
        self.operation_table
            .iter()
            .find(|op| op.effect_name == effect_name && op.op_name == op_name)
            .cloned()
    }

    /// Lightweight observability fingerprint for internal debugging.
    ///
    /// This value is intentionally not collision-resistant and must not be
    /// used for correctness gating or cache-key integrity.
    pub(crate) fn fingerprint_hint(&self) -> usize {
        let ops_sum = self
            .operation_table
            .iter()
            .map(|op| {
                usize::from(op.effect_id.0)
                    + usize::from(op.op_id.0)
                    + op.effect_name.len()
                    + op.op_name.len()
            })
            .sum::<usize>();
        let mut req_entries = self.declaration_requirements.iter().collect::<Vec<_>>();
        req_entries.sort_by(|(a, _), (b, _)| a.cmp(b));
        let req_sum = req_entries
            .iter()
            .map(|(decl_name, req)| {
                let decl_name_sum = decl_name.len();
                req.required_effects
                    .iter()
                    .map(|id| usize::from(id.0))
                    .sum::<usize>()
                    + req
                        .referenced_operations
                        .iter()
                        .map(|op| usize::from(op.effect_id.0) + usize::from(op.op_id.0))
                        .sum::<usize>()
                    + if req.passes_evidence() { 1 } else { 0 }
                    + decl_name_sum
            })
            .sum::<usize>();
        ops_sum + req_sum
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct LoweringPlan {
    declaration_styles: HashMap<String, LoweringStyle>,
    handler_resume_present: bool,
    evidence_shape: EvidencePayloadShape,
}

impl LoweringPlan {
    pub(crate) fn style_for(&self, declaration_name: &str) -> Option<LoweringStyle> {
        self.declaration_styles.get(declaration_name).copied()
    }

    pub(crate) fn is_direct_style(&self, declaration_name: &str) -> bool {
        self.style_for(declaration_name) == Some(LoweringStyle::DirectStyle)
    }

    pub(crate) fn handler_resume_present(&self) -> bool {
        self.handler_resume_present
    }

    pub(crate) fn evidence_shape(&self) -> &EvidencePayloadShape {
        &self.evidence_shape
    }

    pub(crate) fn evidence_requirement_for(
        &self,
        declaration_name: &str,
    ) -> Option<&DeclarationEvidenceRequirement> {
        self.evidence_shape.requirement_for(declaration_name)
    }

    pub(crate) fn declaration_lowering_modes(&self) -> Vec<DeclarationLoweringMode> {
        let mut out = self
            .declaration_styles
            .iter()
            .map(|(name, style)| DeclarationLoweringMode {
                declaration_name: name.clone(),
                style: *style,
            })
            .collect::<Vec<_>>();
        out.sort_by(|a, b| a.declaration_name.cmp(&b.declaration_name));
        out
    }
}

pub(crate) fn build_lowering_plan(module: &Module) -> LoweringPlan {
    let declaration_names: HashSet<String> =
        module.declarations.iter().map(|d| d.name.clone()).collect();

    let mut declaration_styles: HashMap<String, LoweringStyle> = HashMap::new();
    let mut declaration_calls: HashMap<String, HashSet<String>> = HashMap::new();
    let mut declaration_requirements: HashMap<String, DeclarationEvidenceRequirement> =
        HashMap::new();

    let mut operation_table = Vec::new();
    let mut qualified_operation_index: HashMap<(String, String), EffectOperationRef> =
        HashMap::new();
    let mut op_name_index: HashMap<String, Vec<EffectOperationRef>> = HashMap::new();
    let mut effect_name_to_id: HashMap<String, EffectId> = HashMap::new();

    for (effect_idx, effect_decl) in module.effect_declarations.iter().enumerate() {
        let Ok(effect_id_u16) = u16::try_from(effect_idx) else {
            continue;
        };
        let effect_id = EffectId(effect_id_u16);
        effect_name_to_id.insert(effect_decl.name.clone(), effect_id);
        for (op_idx, member) in effect_decl.members.iter().enumerate() {
            let Ok(op_id_u16) = u16::try_from(op_idx) else {
                continue;
            };
            let op_ref = EffectOperationRef {
                effect_id,
                op_id: OpId(op_id_u16),
                effect_name: effect_decl.name.clone(),
                op_name: member.name.clone(),
            };
            operation_table.push(op_ref.clone());
            qualified_operation_index.insert(
                (effect_decl.name.clone(), member.name.clone()),
                op_ref.clone(),
            );
            op_name_index
                .entry(member.name.clone())
                .or_default()
                .push(op_ref);
        }
    }

    for decl in &module.declarations {
        let mut is_effect_boundary = false;
        let mut required_effects = Vec::new();
        if let Some(annotation) = decl.type_annotation.as_deref()
            && has_effect_clause(annotation)
        {
            is_effect_boundary = true;
            for effect_name in parse_effect_clause_effects(annotation) {
                if let Some(effect_id) = effect_name_to_id.get(&effect_name).copied() {
                    required_effects.push(effect_id);
                }
            }
        }

        let mut calls = HashSet::new();
        let mut referenced_operations = Vec::new();
        if let Some(stmts) = decl.parsed_body.as_deref() {
            let mut inspection = StmtInspection::default();
            inspect_stmts(
                stmts,
                &mut inspection,
                &declaration_names,
                &qualified_operation_index,
                &op_name_index,
            );
            if inspection.contains_using || inspection.contains_resume {
                is_effect_boundary = true;
            }
            calls = inspection.called_declarations;
            referenced_operations = inspection.referenced_operations;
        } else {
            // Missing parsed body forces boundary mode until lowering coverage expands.
            is_effect_boundary = true;
        }

        declaration_styles.insert(
            decl.name.clone(),
            if is_effect_boundary {
                LoweringStyle::EffectBoundary
            } else {
                LoweringStyle::DirectStyle
            },
        );
        declaration_requirements.insert(
            decl.name.clone(),
            DeclarationEvidenceRequirement {
                style: if is_effect_boundary {
                    LoweringStyle::EffectBoundary
                } else {
                    LoweringStyle::DirectStyle
                },
                required_effects,
                referenced_operations,
            },
        );
        declaration_calls.insert(decl.name.clone(), calls);
    }

    // Transitive propagation: callers of effect-boundary declarations become boundaries.
    loop {
        let mut changed = false;
        for (decl_name, callees) in &declaration_calls {
            if declaration_styles.get(decl_name) == Some(&LoweringStyle::EffectBoundary) {
                continue;
            }
            let caller_becomes_boundary = callees.iter().any(|callee| {
                declaration_styles.get(callee) == Some(&LoweringStyle::EffectBoundary)
            });
            if caller_becomes_boundary {
                declaration_styles.insert(decl_name.clone(), LoweringStyle::EffectBoundary);
                if let Some(req) = declaration_requirements.get_mut(decl_name) {
                    req.style = LoweringStyle::EffectBoundary;
                }
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    let handler_resume_present = module
        .handler_declarations
        .iter()
        .flat_map(|h| h.methods.iter())
        .filter_map(|m| m.parsed_body.as_deref())
        .any(stmts_contain_resume);

    LoweringPlan {
        declaration_styles,
        handler_resume_present,
        evidence_shape: EvidencePayloadShape {
            operation_table,
            declaration_requirements,
        },
    }
}

#[derive(Default)]
struct StmtInspection {
    contains_using: bool,
    contains_resume: bool,
    called_declarations: HashSet<String>,
    referenced_operations: Vec<EffectOperationRef>,
    referenced_operation_ids: HashSet<(EffectId, OpId)>,
}

fn inspect_stmts(
    stmts: &[Stmt],
    out: &mut StmtInspection,
    declaration_names: &HashSet<String>,
    qualified_operation_index: &HashMap<(String, String), EffectOperationRef>,
    op_name_index: &HashMap<String, Vec<EffectOperationRef>>,
) {
    for stmt in stmts {
        match stmt {
            Stmt::Binding { value, .. } | Stmt::Expr(value) => {
                inspect_expr(
                    value,
                    out,
                    declaration_names,
                    qualified_operation_index,
                    op_name_index,
                );
            }
            Stmt::Using { body, .. } => {
                out.contains_using = true;
                inspect_stmts(
                    body,
                    out,
                    declaration_names,
                    qualified_operation_index,
                    op_name_index,
                );
            }
        }
    }
}

fn inspect_expr(
    expr: &Expr,
    out: &mut StmtInspection,
    declaration_names: &HashSet<String>,
    qualified_operation_index: &HashMap<(String, String), EffectOperationRef>,
    op_name_index: &HashMap<String, Vec<EffectOperationRef>>,
) {
    if let Expr::Resume { .. } = expr {
        out.contains_resume = true;
    }
    if let Some((callee_name, _)) = flatten_named_call(expr)
        && declaration_names.contains(callee_name)
    {
        out.called_declarations.insert(callee_name.to_string());
    }
    collect_operation_refs(expr, out, qualified_operation_index, op_name_index);

    match expr {
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var(_)
        | Expr::Qualified { .. } => {}
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    inspect_expr(
                        expr,
                        out,
                        declaration_names,
                        qualified_operation_index,
                        op_name_index,
                    );
                }
            }
        }
        Expr::ListLit(items) | Expr::TupleLit(items) => {
            for item in items {
                inspect_expr(
                    item,
                    out,
                    declaration_names,
                    qualified_operation_index,
                    op_name_index,
                );
            }
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                inspect_expr(
                    value,
                    out,
                    declaration_names,
                    qualified_operation_index,
                    op_name_index,
                );
            }
        }
        Expr::BinOp { left, right, .. } => {
            inspect_expr(
                left,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
            inspect_expr(
                right,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
        }
        Expr::Call { callee, arg } => {
            inspect_expr(
                callee,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
            inspect_expr(
                arg,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                inspect_expr(
                    arg,
                    out,
                    declaration_names,
                    qualified_operation_index,
                    op_name_index,
                );
            }
        }
        Expr::Pipeline { value, .. } => inspect_expr(
            value,
            out,
            declaration_names,
            qualified_operation_index,
            op_name_index,
        ),
        Expr::Lambda { body, .. } => inspect_expr(
            body,
            out,
            declaration_names,
            qualified_operation_index,
            op_name_index,
        ),
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = &clause.parsed_body {
                    inspect_stmts(
                        stmts,
                        out,
                        declaration_names,
                        qualified_operation_index,
                        op_name_index,
                    );
                }
            }
        }
        Expr::With { handler, body } => {
            out.contains_using = true;
            inspect_expr(
                handler,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
            inspect_stmts(
                body,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
        }
        Expr::Resume { value } => inspect_expr(
            value,
            out,
            declaration_names,
            qualified_operation_index,
            op_name_index,
        ),
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            inspect_expr(
                condition,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
            inspect_expr(
                then_expr,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
            inspect_expr(
                else_expr,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
        }
        Expr::Case { scrutinee, arms } => {
            inspect_expr(
                scrutinee,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
            for arm in arms {
                inspect_expr(
                    &arm.body,
                    out,
                    declaration_names,
                    qualified_operation_index,
                    op_name_index,
                );
            }
        }
    }
}

fn has_effect_clause(annotation: &str) -> bool {
    let trimmed = annotation.trim();
    strip_effect(trimmed) != trimmed
}

fn stmts_contain_resume(stmts: &[Stmt]) -> bool {
    let mut inspection = StmtInspection::default();
    let declaration_names = HashSet::new();
    inspect_stmts(
        stmts,
        &mut inspection,
        &declaration_names,
        &HashMap::new(),
        &HashMap::new(),
    );
    inspection.contains_resume
}

fn parse_effect_clause_effects(annotation: &str) -> Vec<String> {
    let trimmed = annotation.trim();
    let Some(idx) = find_can_keyword_index(trimmed) else {
        return Vec::new();
    };
    let effect_part = trimmed[idx + 3..].trim();
    if effect_part.is_empty() {
        return Vec::new();
    }
    effect_part
        .split(',')
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(ToString::to_string)
        .collect()
}

fn find_can_keyword_index(annotation: &str) -> Option<usize> {
    for (idx, _) in annotation.char_indices() {
        let rest = &annotation[idx..];
        if !rest.starts_with("can") {
            continue;
        }
        let has_left_whitespace = annotation[..idx]
            .chars()
            .last()
            .is_some_and(char::is_whitespace);
        if !has_left_whitespace {
            continue;
        }
        let has_right_whitespace = annotation[idx + 3..]
            .chars()
            .next()
            .is_none_or(char::is_whitespace);
        if !has_right_whitespace {
            continue;
        }
        return Some(idx);
    }
    None
}

fn collect_operation_refs(
    expr: &Expr,
    out: &mut StmtInspection,
    qualified_operation_index: &HashMap<(String, String), EffectOperationRef>,
    op_name_index: &HashMap<String, Vec<EffectOperationRef>>,
) {
    if let Expr::Qualified { receiver, member } = expr
        && let Some(op_ref) = qualified_operation_index.get(&(receiver.clone(), member.clone()))
    {
        record_operation_ref(out, op_ref);
        return;
    }

    let mut maybe_bare_name: Option<&str> = None;
    if let Some((name, _)) = flatten_named_call(expr) {
        maybe_bare_name = Some(name);
    } else if let Expr::Pipeline { callee, .. } = expr {
        maybe_bare_name = Some(callee);
    }
    if let Some(name) = maybe_bare_name
        && let Some(ops) = op_name_index.get(name)
    {
        for op_ref in ops {
            record_operation_ref(out, op_ref);
        }
    }
}

fn record_operation_ref(out: &mut StmtInspection, op_ref: &EffectOperationRef) {
    if out
        .referenced_operation_ids
        .insert((op_ref.effect_id, op_ref.op_id))
    {
        out.referenced_operations.push(op_ref.clone());
    }
}

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::*;

    #[test]
    fn classifies_pure_declarations_as_direct_style() {
        let source = r#"
add : Int -> Int -> Int
add a b = a + b

main : Unit -> Unit
main =
  print (add 1 2)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert_eq!(plan.style_for("add"), Some(LoweringStyle::DirectStyle));
        assert_eq!(plan.style_for("main"), Some(LoweringStyle::DirectStyle));
        assert!(!plan.handler_resume_present());
    }

    #[test]
    fn marks_can_clause_declaration_and_callers_as_effect_boundary() {
        let source = r#"
effect Log
  log: String -> Unit

fx : Int -> Int can Log
fx x = x

main : Unit -> Unit
main =
  print (fx 1)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert_eq!(plan.style_for("fx"), Some(LoweringStyle::EffectBoundary));
        assert_eq!(plan.style_for("main"), Some(LoweringStyle::EffectBoundary));
    }

    #[test]
    fn marks_with_handler_declaration_as_effect_boundary() {
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with_handler
    log msg ->
      print msg
      resume Unit
  in
    log "x"
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert_eq!(plan.style_for("main"), Some(LoweringStyle::EffectBoundary));
    }

    #[test]
    fn marks_resume_usage_as_effect_boundary() {
        let source = r#"
f : Int -> Int
f x =
  resume x

main : Unit -> Unit
main =
  print (f 1)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert_eq!(plan.style_for("f"), Some(LoweringStyle::EffectBoundary));
        assert_eq!(plan.style_for("main"), Some(LoweringStyle::EffectBoundary));
    }

    #[test]
    fn records_handler_resume_presence() {
        let source = r#"
effect Iter
  next: Int -> Int

handler H for Iter
  next n =
    resume n

main : Unit -> Unit
main =
  print 1
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert!(plan.handler_resume_present());
    }

    #[test]
    fn builds_evidence_shape_with_effect_and_operation_ids() {
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  print 1
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        let shape = plan.evidence_shape();
        assert_eq!(shape.operation_table_len(), 1);
        let op = shape
            .operation_for_qualified("Log", "log")
            .expect("qualified op should be cataloged");
        assert_eq!(op.effect_id, EffectId(0));
        assert_eq!(op.op_id, OpId(0));
        assert!(shape.fingerprint_hint() > 0);
    }

    #[test]
    fn declaration_requirement_tracks_effect_clause_and_operation_refs() {
        let source = r#"
effect Log
  log: String -> Unit

fx : Int -> Int can Log
fx x =
  Log.log "x"
  x

main : Unit -> Unit
main =
  print (fx 1)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        let req = plan
            .evidence_requirement_for("fx")
            .expect("fx requirement should exist");
        assert_eq!(req.style(), LoweringStyle::EffectBoundary);
        assert!(req.passes_evidence());
        assert_eq!(req.required_effect_count(), 1);
        assert_eq!(req.referenced_operation_count(), 1);
    }

    #[test]
    fn propagates_effect_boundary_transitively_across_multiple_hops() {
        let source = r#"
fx : Int -> Int can Log
fx x = x

mid : Int -> Int
mid x = fx x

main : Unit -> Unit
main =
  print (mid 1)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert_eq!(plan.style_for("fx"), Some(LoweringStyle::EffectBoundary));
        assert_eq!(plan.style_for("mid"), Some(LoweringStyle::EffectBoundary));
        assert_eq!(plan.style_for("main"), Some(LoweringStyle::EffectBoundary));
    }

    #[test]
    fn multi_hop_propagation_preserves_caller_evidence_metadata_shape() {
        let source = r#"
effect Log
  log: String -> Unit

fx : Int -> Int can Log
fx x =
  Log.log "x"
  x

mid : Int -> Int
mid x = fx x

main : Unit -> Unit
main =
  print (mid 1)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);

        let fx_req = plan
            .evidence_requirement_for("fx")
            .expect("fx requirement should exist");
        let mid_req = plan
            .evidence_requirement_for("mid")
            .expect("mid requirement should exist");
        let main_req = plan
            .evidence_requirement_for("main")
            .expect("main requirement should exist");

        assert!(fx_req.passes_evidence());
        assert_eq!(fx_req.required_effect_count(), 1);
        assert_eq!(fx_req.referenced_operation_count(), 1);

        // Callers become effect-boundary via transitive propagation while
        // keeping direct evidence references empty until explicit calls exist.
        assert!(mid_req.passes_evidence());
        assert_eq!(mid_req.required_effect_count(), 0);
        assert_eq!(mid_req.referenced_operation_count(), 0);

        assert!(main_req.passes_evidence());
        assert_eq!(main_req.required_effect_count(), 0);
        assert_eq!(main_req.referenced_operation_count(), 0);
    }

    #[test]
    fn exposes_declaration_lowering_mode_snapshot_for_observability() {
        let source = r#"
fx : Int -> Int can Log
fx x = x

pure : Int -> Int
pure x = x + 1

main : Unit -> Unit
main =
  print (pure 1)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        let snapshot = plan.declaration_lowering_modes();
        assert_eq!(snapshot.len(), 3);
        assert_eq!(snapshot[0].declaration_name, "fx");
        assert_eq!(snapshot[0].style, LoweringStyle::EffectBoundary);
        assert_eq!(snapshot[1].declaration_name, "main");
        assert_eq!(snapshot[1].style, LoweringStyle::DirectStyle);
        assert_eq!(snapshot[2].declaration_name, "pure");
        assert_eq!(snapshot[2].style, LoweringStyle::DirectStyle);
    }

    #[test]
    fn evidence_fingerprint_hint_is_stable_under_declaration_reordering() {
        let source_a = r#"
effect Log
  log: String -> Unit

fx : Int -> Int can Log
fx x = x

pure : Int -> Int
pure x = x + 1

main : Unit -> Unit
main =
  print (pure 1)
"#;
        let source_b = r#"
effect Log
  log: String -> Unit

pure : Int -> Int
pure x = x + 1

fx : Int -> Int can Log
fx x = x

main : Unit -> Unit
main =
  print (pure 1)
"#;
        let module_a = parse_module(source_a).expect("source_a should parse");
        let module_b = parse_module(source_b).expect("source_b should parse");
        let hint_a = build_lowering_plan(&module_a)
            .evidence_shape()
            .fingerprint_hint();
        let hint_b = build_lowering_plan(&module_b)
            .evidence_shape()
            .fingerprint_hint();
        assert_eq!(hint_a, hint_b);
    }
}
