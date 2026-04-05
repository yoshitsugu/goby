use std::collections::{HashMap, HashSet};

use goby_core::ir::{CompExpr, ValueExpr};
use goby_core::{
    Expr, Module, Stmt, ast::InterpolatedPart, find_can_keyword_index, types::strip_effect,
};

use crate::call::flatten_named_call;
use crate::wasm_exec_plan::decl_exec_plan;

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
    let effect_dependency_graph = build_effect_dependency_graph(module);
    let mut handler_resume_present = false;

    for (effect_idx, effect_decl) in module.effect_declarations.iter().enumerate() {
        let effect_id = effect_id_from_index(effect_idx);
        effect_name_to_id.insert(effect_decl.name.clone(), effect_id);
        for (op_idx, member) in effect_decl.members.iter().enumerate() {
            let op_id = op_id_from_index(op_idx);
            let op_ref = EffectOperationRef {
                effect_id,
                op_id,
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
            let declared_effects = parse_effect_clause_effects(annotation);
            let ordered_effects =
                expand_effects_with_dependencies(&declared_effects, &effect_dependency_graph);
            for effect_name in ordered_effects {
                if let Some(effect_id) = effect_name_to_id.get(&effect_name).copied() {
                    required_effects.push(effect_id);
                }
            }
        }

        let exec_plan = decl_exec_plan(decl);
        let (calls, referenced_operations) = if let Some(ir_decl) = exec_plan.ir_decl.as_ref() {
            let mut inspection = IrInspection::default();
            inspect_ir_comp(
                &ir_decl.body,
                &mut inspection,
                &declaration_names,
                &qualified_operation_index,
            );
            if inspection.contains_with_handler || inspection.contains_resume {
                is_effect_boundary = true;
            }
            handler_resume_present |= inspection.handler_resume_present;
            (
                inspection.called_declarations,
                inspection.referenced_operations,
            )
        } else if let Some(stmts) = decl.parsed_body.as_deref() {
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
            handler_resume_present |= stmts_contain_handler_resume(stmts);
            (
                inspection.called_declarations,
                inspection.referenced_operations,
            )
        } else {
            // Missing parsed body forces boundary mode until lowering coverage expands.
            is_effect_boundary = true;
            (HashSet::new(), Vec::new())
        };

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

    LoweringPlan {
        declaration_styles,
        handler_resume_present,
        evidence_shape: EvidencePayloadShape {
            operation_table,
            declaration_requirements,
        },
    }
}

fn effect_id_from_index(effect_idx: usize) -> EffectId {
    let effect_id_u16 = u16::try_from(effect_idx)
        .unwrap_or_else(|_| panic!("too many effects: effect index {effect_idx} exceeds u16"));
    EffectId(effect_id_u16)
}

fn op_id_from_index(op_idx: usize) -> OpId {
    let op_id_u16 = u16::try_from(op_idx)
        .unwrap_or_else(|_| panic!("too many ops in effect: op index {op_idx} exceeds u16"));
    OpId(op_id_u16)
}

#[derive(Default)]
struct StmtInspection {
    contains_using: bool,
    contains_resume: bool,
    called_declarations: HashSet<String>,
    referenced_operations: Vec<EffectOperationRef>,
    referenced_operation_ids: HashSet<(EffectId, OpId)>,
}

#[derive(Default)]
struct IrInspection {
    contains_with_handler: bool,
    contains_resume: bool,
    handler_resume_present: bool,
    called_declarations: HashSet<String>,
    referenced_operations: Vec<EffectOperationRef>,
    referenced_operation_ids: HashSet<(EffectId, OpId)>,
}

fn inspect_ir_comp(
    comp: &CompExpr,
    out: &mut IrInspection,
    declaration_names: &HashSet<String>,
    qualified_operation_index: &HashMap<(String, String), EffectOperationRef>,
) {
    match comp {
        CompExpr::Value(value) => {
            inspect_ir_value(value, out, declaration_names, qualified_operation_index)
        }
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            inspect_ir_comp(value, out, declaration_names, qualified_operation_index);
            inspect_ir_comp(body, out, declaration_names, qualified_operation_index);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                inspect_ir_comp(stmt, out, declaration_names, qualified_operation_index);
            }
            inspect_ir_comp(tail, out, declaration_names, qualified_operation_index);
        }
        CompExpr::If { cond, then_, else_ } => {
            inspect_ir_value(cond, out, declaration_names, qualified_operation_index);
            inspect_ir_comp(then_, out, declaration_names, qualified_operation_index);
            inspect_ir_comp(else_, out, declaration_names, qualified_operation_index);
        }
        CompExpr::Call { callee, args } => {
            inspect_ir_value(callee, out, declaration_names, qualified_operation_index);
            if let ValueExpr::Var(name) = callee.as_ref()
                && declaration_names.contains(name)
            {
                out.called_declarations.insert(name.clone());
            } else if let ValueExpr::GlobalRef { module, name } = callee.as_ref()
                && let Some(op_ref) = qualified_operation_index.get(&(module.clone(), name.clone()))
                && out
                    .referenced_operation_ids
                    .insert((op_ref.effect_id, op_ref.op_id))
            {
                out.referenced_operations.push(op_ref.clone());
            }
            for arg in args {
                inspect_ir_value(arg, out, declaration_names, qualified_operation_index);
            }
        }
        CompExpr::Assign { value, .. } => {
            inspect_ir_comp(value, out, declaration_names, qualified_operation_index);
        }
        CompExpr::Case { scrutinee, arms } => {
            inspect_ir_value(scrutinee, out, declaration_names, qualified_operation_index);
            for arm in arms {
                inspect_ir_comp(&arm.body, out, declaration_names, qualified_operation_index);
            }
        }
        CompExpr::PerformEffect { effect, op, args } => {
            if let Some(op_ref) = qualified_operation_index.get(&(effect.clone(), op.clone()))
                && out
                    .referenced_operation_ids
                    .insert((op_ref.effect_id, op_ref.op_id))
            {
                out.referenced_operations.push(op_ref.clone());
            }
            for arg in args {
                inspect_ir_value(arg, out, declaration_names, qualified_operation_index);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                if ir_comp_contains_resume(&clause.body) {
                    out.handler_resume_present = true;
                }
                inspect_ir_comp(
                    &clause.body,
                    out,
                    declaration_names,
                    qualified_operation_index,
                );
            }
        }
        CompExpr::WithHandler { handler, body } => {
            out.contains_with_handler = true;
            inspect_ir_comp(handler, out, declaration_names, qualified_operation_index);
            inspect_ir_comp(body, out, declaration_names, qualified_operation_index);
        }
        CompExpr::Resume { value } => {
            out.contains_resume = true;
            inspect_ir_value(value, out, declaration_names, qualified_operation_index);
        }
    }
}

fn inspect_ir_value(
    value: &ValueExpr,
    out: &mut IrInspection,
    declaration_names: &HashSet<String>,
    qualified_operation_index: &HashMap<(String, String), EffectOperationRef>,
) {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                inspect_ir_value(element, out, declaration_names, qualified_operation_index);
            }
            if let Some(tail) = spread {
                inspect_ir_value(tail, out, declaration_names, qualified_operation_index);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                inspect_ir_value(item, out, declaration_names, qualified_operation_index);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, value) in fields {
                inspect_ir_value(value, out, declaration_names, qualified_operation_index);
            }
        }
        ValueExpr::Lambda { body, .. } => {
            inspect_ir_comp(body, out, declaration_names, qualified_operation_index);
        }
        ValueExpr::BinOp { left, right, .. } => {
            inspect_ir_value(left, out, declaration_names, qualified_operation_index);
            inspect_ir_value(right, out, declaration_names, qualified_operation_index);
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let goby_core::ir::IrInterpPart::Expr(expr) = part {
                    inspect_ir_value(expr, out, declaration_names, qualified_operation_index);
                }
            }
        }
        ValueExpr::TupleProject { tuple, .. } => {
            inspect_ir_value(tuple, out, declaration_names, qualified_operation_index);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn ir_comp_contains_resume(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::Resume { .. } => true,
        CompExpr::Value(_) | CompExpr::Call { .. } | CompExpr::PerformEffect { .. } => false,
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            ir_comp_contains_resume(value) || ir_comp_contains_resume(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(ir_comp_contains_resume) || ir_comp_contains_resume(tail)
        }
        CompExpr::If { then_, else_, .. } => {
            ir_comp_contains_resume(then_) || ir_comp_contains_resume(else_)
        }
        CompExpr::Assign { value, .. } => ir_comp_contains_resume(value),
        CompExpr::Case { arms, .. } => arms.iter().any(|arm| ir_comp_contains_resume(&arm.body)),
        CompExpr::Handle { clauses } => clauses
            .iter()
            .any(|clause| ir_comp_contains_resume(&clause.body)),
        CompExpr::WithHandler { handler, body } => {
            ir_comp_contains_resume(handler) || ir_comp_contains_resume(body)
        }
    }
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
            Stmt::Binding { value, .. }
            | Stmt::MutBinding { value, .. }
            | Stmt::Assign { value, .. }
            | Stmt::Expr(value, _) => {
                inspect_expr(
                    value,
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
        Expr::Spanned { expr, .. } => inspect_expr(
            expr,
            out,
            declaration_names,
            qualified_operation_index,
            op_name_index,
        ),
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var { name: _, .. }
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
        Expr::ListLit { elements, spread } => {
            for item in elements {
                inspect_expr(
                    item,
                    out,
                    declaration_names,
                    qualified_operation_index,
                    op_name_index,
                );
            }
            if let Some(s) = spread {
                inspect_expr(
                    s,
                    out,
                    declaration_names,
                    qualified_operation_index,
                    op_name_index,
                );
            }
        }
        Expr::TupleLit(items) => {
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
        Expr::UnaryOp { expr, .. } => {
            inspect_expr(
                expr,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
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
        Expr::Call { callee, arg, .. } => {
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
        Expr::Block(stmts) => inspect_stmts(
            stmts,
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
        Expr::ListIndex { list, index } => {
            inspect_expr(
                list,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
            inspect_expr(
                index,
                out,
                declaration_names,
                qualified_operation_index,
                op_name_index,
            );
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

fn stmts_contain_handler_resume(stmts: &[Stmt]) -> bool {
    stmts.iter().any(stmt_contains_handler_resume)
}

fn stmt_contains_handler_resume(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Binding { value, .. }
        | Stmt::MutBinding { value, .. }
        | Stmt::Assign { value, .. }
        | Stmt::Expr(value, _) => expr_contains_handler_resume(value),
    }
}

fn expr_contains_handler_resume(expr: &Expr) -> bool {
    match expr {
        Expr::Spanned { expr, .. } => expr_contains_handler_resume(expr),
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var { name: _, .. }
        | Expr::Qualified { .. } => false,
        Expr::InterpolatedString(parts) => parts.iter().any(|part| match part {
            InterpolatedPart::Text(_) => false,
            InterpolatedPart::Expr(expr) => expr_contains_handler_resume(expr),
        }),
        Expr::ListLit { elements, spread } => {
            elements.iter().any(expr_contains_handler_resume)
                || spread
                    .as_ref()
                    .is_some_and(|s| expr_contains_handler_resume(s))
        }
        Expr::TupleLit(items) => items.iter().any(expr_contains_handler_resume),
        Expr::RecordConstruct { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_contains_handler_resume(value)),
        Expr::UnaryOp { expr, .. } => expr_contains_handler_resume(expr),
        Expr::BinOp { left, right, .. } => {
            expr_contains_handler_resume(left) || expr_contains_handler_resume(right)
        }
        Expr::Call { callee, arg, .. } => {
            expr_contains_handler_resume(callee) || expr_contains_handler_resume(arg)
        }
        Expr::MethodCall { args, .. } => args.iter().any(expr_contains_handler_resume),
        Expr::Pipeline { value, .. } => expr_contains_handler_resume(value),
        Expr::Lambda { body, .. } => expr_contains_handler_resume(body),
        Expr::Handler { clauses } => clauses
            .iter()
            .filter_map(|clause| clause.parsed_body.as_deref())
            .any(stmts_contain_resume),
        Expr::With { handler, body } => {
            expr_contains_handler_resume(handler) || stmts_contain_handler_resume(body)
        }
        Expr::Resume { value } => expr_contains_handler_resume(value),
        Expr::Block(stmts) => stmts_contain_handler_resume(stmts),
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            expr_contains_handler_resume(condition)
                || expr_contains_handler_resume(then_expr)
                || expr_contains_handler_resume(else_expr)
        }
        Expr::Case { scrutinee, arms } => {
            expr_contains_handler_resume(scrutinee)
                || arms
                    .iter()
                    .any(|arm| expr_contains_handler_resume(&arm.body))
        }
        Expr::ListIndex { list, index } => {
            expr_contains_handler_resume(list) || expr_contains_handler_resume(index)
        }
    }
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

fn build_effect_dependency_graph(module: &Module) -> HashMap<String, Vec<String>> {
    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    for effect_decl in &module.effect_declarations {
        let deps = graph.entry(effect_decl.name.clone()).or_default();
        for member in &effect_decl.members {
            for dep in parse_effect_clause_effects(&member.type_annotation) {
                if !deps.contains(&dep) {
                    deps.push(dep);
                }
            }
        }
    }
    graph
}

fn expand_effects_with_dependencies(
    declared_effects: &[String],
    dependency_graph: &HashMap<String, Vec<String>>,
) -> Vec<String> {
    fn dfs(
        effect: &str,
        dependency_graph: &HashMap<String, Vec<String>>,
        visiting: &mut HashSet<String>,
        visited: &mut HashSet<String>,
        out: &mut Vec<String>,
    ) {
        if visited.contains(effect) {
            return;
        }
        if !visiting.insert(effect.to_string()) {
            // Cycle: keep deterministic behavior by stopping this branch.
            return;
        }
        if let Some(deps) = dependency_graph.get(effect) {
            for dep in deps {
                dfs(dep, dependency_graph, visiting, visited, out);
            }
        }
        visiting.remove(effect);
        if visited.insert(effect.to_string()) {
            out.push(effect.to_string());
        }
    }

    let mut out = Vec::new();
    let mut visited = HashSet::new();
    let mut visiting = HashSet::new();
    for effect in declared_effects {
        dfs(
            effect,
            dependency_graph,
            &mut visiting,
            &mut visited,
            &mut out,
        );
    }
    out
}

fn collect_operation_refs(
    expr: &Expr,
    out: &mut StmtInspection,
    qualified_operation_index: &HashMap<(String, String), EffectOperationRef>,
    op_name_index: &HashMap<String, Vec<EffectOperationRef>>,
) {
    if let Expr::Qualified {
        receiver, member, ..
    } = expr
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
    use goby_core::ast::{EffectDecl, EffectMember, Span};
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
    fn marks_with_declaration_as_effect_boundary() {
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
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

main : Unit -> Unit
main =
  with
    next n ->
      resume n
  in
    Iter.next 1
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
    fn expands_required_effects_with_member_declared_dependencies_in_topological_order() {
        let source = r#"
effect Print
  print: String -> Unit

effect Log
  log: String -> Unit

effect Trace
  trace_print: String -> Unit can Print
  trace_log: String -> Unit can Log

main : Unit -> Unit can Trace
main =
  Unit
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        let req = plan
            .evidence_requirement_for("main")
            .expect("main requirement should exist");
        assert_eq!(
            req.required_effects(),
            &[EffectId(0), EffectId(1), EffectId(2)]
        );
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

    #[test]
    #[should_panic(expected = "too many effects")]
    fn panics_when_effect_index_exceeds_u16() {
        let _ = effect_id_from_index(usize::from(u16::MAX) + 1);
    }

    #[test]
    #[should_panic(expected = "too many ops in effect")]
    fn panics_when_operation_index_exceeds_u16() {
        let _ = op_id_from_index(usize::from(u16::MAX) + 1);
    }

    #[test]
    fn build_plan_preserves_boundary_u16_effect_and_op_ids() {
        let effect_decl = EffectDecl {
            name: "Big".to_string(),
            type_params: Vec::new(),
            members: vec![EffectMember {
                name: "op".to_string(),
                type_annotation: "Unit -> Unit".to_string(),
                span: Span::point(1, 1),
            }],
            span: Span::point(1, 1),
        };
        let module = Module {
            imports: Vec::new(),
            embed_declarations: Vec::new(),
            type_declarations: Vec::new(),
            effect_declarations: vec![effect_decl],
            declarations: Vec::new(),
        };
        let plan = build_lowering_plan(&module);
        let ops = plan.evidence_shape.operation_table();
        assert_eq!(ops.len(), 1);
        assert_eq!(ops[0].effect_id, EffectId(0));
        assert_eq!(ops[0].op_id, OpId(0));
    }
}
