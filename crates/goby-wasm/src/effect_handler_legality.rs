use std::collections::{BTreeSet, HashMap};

use goby_core::ast::{AssignTarget, HandlerClause, InterpolatedPart};
use goby_core::ir::{CompExpr, IrDecl, IrHandlerClause, IrInterpPart, ValueExpr};
use goby_core::tail_analysis::{TailWalkConfig, walk_comp};
use goby_core::{Expr, Module, Stmt};

use crate::CodegenError;
use crate::wasm_exec_plan::decl_exec_plan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum HandlerLegalityIssue {
    HandlerExpressionNotStaticallyResolvable,
    ResumeNotInTailPosition,
    DelayedResume,
    MultipleResume,
    ReentrantResume,
    MultiShotCapturesMutableLocals { locals: Vec<String> },
}

impl HandlerLegalityIssue {
    pub(crate) const fn as_str(&self) -> &'static str {
        match self {
            HandlerLegalityIssue::HandlerExpressionNotStaticallyResolvable => {
                "handler expression is not statically resolvable to `Handle { clauses }`"
            }
            HandlerLegalityIssue::ResumeNotInTailPosition => {
                "`resume` appears outside a tail position"
            }
            HandlerLegalityIssue::DelayedResume => {
                "`resume` appears in a nested lambda and would require a delayed continuation"
            }
            HandlerLegalityIssue::MultipleResume => {
                "handler clause may resume the same continuation more than once"
            }
            HandlerLegalityIssue::ReentrantResume => {
                "`resume` appears while evaluating another `resume` argument"
            }
            HandlerLegalityIssue::MultiShotCapturesMutableLocals { .. } => {
                "multi-shot continuation captures ordinary mutable locals"
            }
        }
    }

    pub(crate) fn message(&self) -> String {
        match self {
            HandlerLegalityIssue::MultiShotCapturesMutableLocals { locals } => {
                format!("{}: {:?}", self.as_str(), locals)
            }
            _ => self.as_str().to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum HandlerUseKind {
    Abortive,
    DirectOneShot,
    DelayedOneShot,
    SequentialMultiShot,
    ReentrantLooking,
    UnsupportedMutableCapture,
    Unsupported,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum WithHandlerLegality {
    OneShotTailResumptive,
    Other { issue: HandlerLegalityIssue },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct WithHandlerLegalityRecord {
    pub(crate) decl_name: String,
    pub(crate) clause_ops: Vec<String>,
    pub(crate) kind: HandlerUseKind,
    pub(crate) captured_mutable_locals: Vec<String>,
    pub(crate) legality: WithHandlerLegality,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct HandlerLegalitySummary {
    records: Vec<WithHandlerLegalityRecord>,
}

impl HandlerLegalitySummary {
    pub(crate) fn push(&mut self, record: WithHandlerLegalityRecord) {
        self.records.push(record);
    }

    pub(crate) fn records(&self) -> &[WithHandlerLegalityRecord] {
        &self.records
    }

    pub(crate) fn has_with_handlers(&self) -> bool {
        !self.records.is_empty()
    }

    pub(crate) fn unsupported_records(&self) -> impl Iterator<Item = &WithHandlerLegalityRecord> {
        self.records
            .iter()
            .filter(|record| matches!(record.legality, WithHandlerLegality::Other { .. }))
    }

    pub(crate) fn has_unsupported(&self) -> bool {
        self.unsupported_records().next().is_some()
    }

    pub(crate) fn all_one_shot_tail_resumptive(&self) -> bool {
        self.has_with_handlers() && !self.has_unsupported()
    }

    pub(crate) fn supports_wb3_direct_calling_subset(&self) -> bool {
        !self.has_unsupported()
    }

    pub(crate) fn first_issue(
        &self,
    ) -> Option<(&WithHandlerLegalityRecord, &HandlerLegalityIssue)> {
        self.unsupported_records()
            .find_map(|record| match &record.legality {
                WithHandlerLegality::Other { issue } => Some((record, issue)),
                WithHandlerLegality::OneShotTailResumptive => None,
            })
    }
}

pub(crate) fn analyze_module_handler_legality(
    module: &Module,
) -> Result<HandlerLegalitySummary, CodegenError> {
    let mut summary = HandlerLegalitySummary::default();
    for decl in &module.declarations {
        if let Some(stmts) = decl.parsed_body.as_deref() {
            collect_ast_decl_legality(&decl.name, stmts, &mut summary);
        } else if let Some(ir_decl) = decl_exec_plan(decl).ir_decl.as_ref() {
            collect_decl_legality(ir_decl, &mut summary);
        }
    }
    Ok(summary)
}

fn collect_decl_legality(decl: &IrDecl, summary: &mut HandlerLegalitySummary) {
    collect_comp_legality(
        &decl.name,
        &decl.body,
        &HashMap::new(),
        &BTreeSet::new(),
        summary,
    );
}

fn collect_comp_legality(
    decl_name: &str,
    comp: &CompExpr,
    env: &HashMap<String, Vec<IrHandlerClause>>,
    mutable_scope: &BTreeSet<String>,
    summary: &mut HandlerLegalitySummary,
) {
    match comp {
        CompExpr::Value(value) => {
            collect_value_legality(decl_name, value, env, mutable_scope, summary)
        }
        CompExpr::Let {
            name, value, body, ..
        } => {
            collect_comp_legality(decl_name, value, env, mutable_scope, summary);
            let mut body_env = env.clone();
            if let Some(clauses) = extract_handler_binding(value, env) {
                body_env.insert(name.clone(), clauses);
            } else {
                body_env.remove(name);
            }
            collect_comp_legality(decl_name, body, &body_env, mutable_scope, summary);
        }
        CompExpr::LetMut {
            name, value, body, ..
        } => {
            collect_comp_legality(decl_name, value, env, mutable_scope, summary);
            let mut body_env = env.clone();
            if let Some(clauses) = extract_handler_binding(value, env) {
                body_env.insert(name.clone(), clauses);
            } else {
                body_env.remove(name);
            }
            let mut body_mutable_scope = mutable_scope.clone();
            body_mutable_scope.insert(name.clone());
            collect_comp_legality(decl_name, body, &body_env, &body_mutable_scope, summary);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                collect_comp_legality(decl_name, stmt, env, mutable_scope, summary);
            }
            collect_comp_legality(decl_name, tail, env, mutable_scope, summary);
        }
        CompExpr::If { cond, then_, else_ } => {
            collect_value_legality(decl_name, cond, env, mutable_scope, summary);
            collect_comp_legality(decl_name, then_, env, mutable_scope, summary);
            collect_comp_legality(decl_name, else_, env, mutable_scope, summary);
        }
        CompExpr::Call { callee, args, .. } => {
            collect_value_legality(decl_name, callee, env, mutable_scope, summary);
            for arg in args {
                collect_value_legality(decl_name, arg, env, mutable_scope, summary);
            }
        }
        CompExpr::Assign { value, .. } => {
            collect_comp_legality(decl_name, value, env, mutable_scope, summary);
        }
        CompExpr::Dup { value } | CompExpr::Drop { value } | CompExpr::DropReuse { value, .. } => {
            collect_value_legality(decl_name, value, env, mutable_scope, summary);
        }
        CompExpr::AllocReuse { .. } => {}
        CompExpr::Case { scrutinee, arms } => {
            collect_value_legality(decl_name, scrutinee, env, mutable_scope, summary);
            for arm in arms {
                collect_comp_legality(decl_name, &arm.body, env, mutable_scope, summary);
            }
        }
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                collect_value_legality(decl_name, arg, env, mutable_scope, summary);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                collect_comp_legality(decl_name, &clause.body, env, mutable_scope, summary);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            summary.push(classify_with_handler(
                decl_name,
                handler,
                body,
                env,
                mutable_scope,
            ));
            collect_comp_legality(decl_name, handler, env, mutable_scope, summary);
            collect_comp_legality(decl_name, body, env, mutable_scope, summary);
        }
        CompExpr::Resume { value } => {
            collect_value_legality(decl_name, value, env, mutable_scope, summary);
        }
        CompExpr::AssignIndex { path, value, .. } => {
            for idx in path {
                collect_value_legality(decl_name, idx, env, mutable_scope, summary);
            }
            collect_comp_legality(decl_name, value, env, mutable_scope, summary);
        }
    }
}

fn collect_value_legality(
    decl_name: &str,
    value: &ValueExpr,
    env: &HashMap<String, Vec<IrHandlerClause>>,
    mutable_scope: &BTreeSet<String>,
    summary: &mut HandlerLegalitySummary,
) {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                collect_value_legality(decl_name, element, env, mutable_scope, summary);
            }
            if let Some(spread) = spread {
                collect_value_legality(decl_name, spread, env, mutable_scope, summary);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                collect_value_legality(decl_name, item, env, mutable_scope, summary);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                collect_value_legality(decl_name, field, env, mutable_scope, summary);
            }
        }
        ValueExpr::Lambda { body, .. } => {
            collect_comp_legality(decl_name, body, env, mutable_scope, summary);
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(expr) = part {
                    collect_value_legality(decl_name, expr, env, mutable_scope, summary);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            collect_value_legality(decl_name, left, env, mutable_scope, summary);
            collect_value_legality(decl_name, right, env, mutable_scope, summary);
        }
        ValueExpr::TupleProject { tuple, .. } => {
            collect_value_legality(decl_name, tuple, env, mutable_scope, summary);
        }
        ValueExpr::ListGet { list, index } => {
            collect_value_legality(decl_name, list, env, mutable_scope, summary);
            collect_value_legality(decl_name, index, env, mutable_scope, summary);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn classify_with_handler(
    decl_name: &str,
    handler: &CompExpr,
    body: &CompExpr,
    env: &HashMap<String, Vec<IrHandlerClause>>,
    mutable_scope: &BTreeSet<String>,
) -> WithHandlerLegalityRecord {
    let Some(clauses) = resolve_handler_clauses(handler, env) else {
        return WithHandlerLegalityRecord {
            decl_name: decl_name.to_string(),
            clause_ops: Vec::new(),
            kind: HandlerUseKind::Unsupported,
            captured_mutable_locals: Vec::new(),
            legality: WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::HandlerExpressionNotStaticallyResolvable,
            },
        };
    };

    let clause_ops = clauses
        .iter()
        .map(|clause| clause.op_name.clone())
        .collect::<Vec<_>>();
    let captured_mutable_locals =
        captured_mutable_locals_for_ir_body(body, mutable_scope, &clause_ops);
    let (kind, legality) = classify_ir_clauses(&clauses, captured_mutable_locals.clone());

    WithHandlerLegalityRecord {
        decl_name: decl_name.to_string(),
        clause_ops,
        kind,
        captured_mutable_locals,
        legality,
    }
}

fn extract_handler_binding(
    value: &CompExpr,
    env: &HashMap<String, Vec<IrHandlerClause>>,
) -> Option<Vec<IrHandlerClause>> {
    resolve_handler_clauses(value, env)
}

fn resolve_handler_clauses(
    comp: &CompExpr,
    env: &HashMap<String, Vec<IrHandlerClause>>,
) -> Option<Vec<IrHandlerClause>> {
    match comp {
        CompExpr::Handle { clauses } => Some(clauses.clone()),
        CompExpr::Value(ValueExpr::Var(name)) => env.get(name).cloned(),
        CompExpr::Let {
            name, value, body, ..
        }
        | CompExpr::LetMut {
            name, value, body, ..
        } => {
            let mut body_env = env.clone();
            if let Some(clauses) = extract_handler_binding(value, env) {
                body_env.insert(name.clone(), clauses);
            }
            resolve_handler_clauses(body, &body_env)
        }
        _ => None,
    }
}

#[derive(Debug, Clone, Default)]
struct ClauseResumeStats {
    resume_count: usize,
    non_tail_resume: bool,
    delayed_resume: bool,
    reentrant_resume: bool,
}

impl ClauseResumeStats {
    fn combine(&mut self, other: Self) {
        self.resume_count += other.resume_count;
        self.non_tail_resume |= other.non_tail_resume;
        self.delayed_resume |= other.delayed_resume;
        self.reentrant_resume |= other.reentrant_resume;
    }
}

fn classify_ir_clauses(
    clauses: &[IrHandlerClause],
    captured_mutable_locals: Vec<String>,
) -> (HandlerUseKind, WithHandlerLegality) {
    let mut stats = ClauseResumeStats::default();
    for clause in clauses {
        stats.combine(analyze_ir_clause_body(&clause.body));
    }
    classify_resume_stats(stats, captured_mutable_locals)
}

fn classify_resume_stats(
    stats: ClauseResumeStats,
    captured_mutable_locals: Vec<String>,
) -> (HandlerUseKind, WithHandlerLegality) {
    if stats.resume_count == 0 {
        return (
            HandlerUseKind::Abortive,
            WithHandlerLegality::OneShotTailResumptive,
        );
    }
    if stats.reentrant_resume {
        return (
            HandlerUseKind::ReentrantLooking,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::ReentrantResume,
            },
        );
    }
    if stats.resume_count > 1 {
        if !captured_mutable_locals.is_empty() {
            return (
                HandlerUseKind::UnsupportedMutableCapture,
                WithHandlerLegality::Other {
                    issue: HandlerLegalityIssue::MultiShotCapturesMutableLocals {
                        locals: captured_mutable_locals,
                    },
                },
            );
        }
        return (
            HandlerUseKind::SequentialMultiShot,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::MultipleResume,
            },
        );
    }
    if stats.delayed_resume {
        return (
            HandlerUseKind::DelayedOneShot,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::DelayedResume,
            },
        );
    }
    if stats.non_tail_resume {
        return (
            HandlerUseKind::DelayedOneShot,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::ResumeNotInTailPosition,
            },
        );
    }
    (
        HandlerUseKind::DirectOneShot,
        WithHandlerLegality::OneShotTailResumptive,
    )
}

fn analyze_ir_clause_body(comp: &CompExpr) -> ClauseResumeStats {
    let mut stats = ClauseResumeStats::default();
    walk_comp(
        comp,
        TailWalkConfig::HANDLER_CLAUSE_BODY,
        |node, tail_position| {
            if let CompExpr::Resume { .. } = node {
                stats.resume_count += 1;
                if tail_position.is_tail() {
                    // Tail resume is the fast-path shape.
                } else {
                    stats.non_tail_resume = true;
                }
            }
        },
    );
    stats.delayed_resume = ir_comp_contains_resume_under_lambda(comp);
    stats
}

fn captured_mutable_locals_for_ir_body(
    body: &CompExpr,
    mutable_scope: &BTreeSet<String>,
    handled_ops: &[String],
) -> Vec<String> {
    let mut captured = BTreeSet::new();
    collect_ir_effect_mutable_captures(body, mutable_scope, handled_ops, &mut captured);
    captured.into_iter().collect()
}

fn collect_ir_effect_mutable_captures(
    comp: &CompExpr,
    mutable_scope: &BTreeSet<String>,
    handled_ops: &[String],
    captured: &mut BTreeSet<String>,
) {
    match comp {
        CompExpr::Value(value) => {
            collect_ir_value_effect_mutable_captures(value, mutable_scope, handled_ops, captured);
        }
        CompExpr::Let { value, body, .. } => {
            collect_ir_effect_mutable_captures(value, mutable_scope, handled_ops, captured);
            collect_ir_effect_mutable_captures(body, mutable_scope, handled_ops, captured);
        }
        CompExpr::LetMut {
            name, value, body, ..
        } => {
            collect_ir_effect_mutable_captures(value, mutable_scope, handled_ops, captured);
            let mut body_scope = mutable_scope.clone();
            body_scope.insert(name.clone());
            collect_ir_effect_mutable_captures(body, &body_scope, handled_ops, captured);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                collect_ir_effect_mutable_captures(stmt, mutable_scope, handled_ops, captured);
            }
            collect_ir_effect_mutable_captures(tail, mutable_scope, handled_ops, captured);
        }
        CompExpr::If { cond, then_, else_ } => {
            collect_ir_value_effect_mutable_captures(cond, mutable_scope, handled_ops, captured);
            collect_ir_effect_mutable_captures(then_, mutable_scope, handled_ops, captured);
            collect_ir_effect_mutable_captures(else_, mutable_scope, handled_ops, captured);
        }
        CompExpr::Call { callee, args, .. } => {
            collect_ir_value_effect_mutable_captures(callee, mutable_scope, handled_ops, captured);
            for arg in args {
                collect_ir_value_effect_mutable_captures(arg, mutable_scope, handled_ops, captured);
            }
        }
        CompExpr::Assign { value, .. } => {
            collect_ir_effect_mutable_captures(value, mutable_scope, handled_ops, captured);
        }
        CompExpr::AssignIndex { path, value, .. } => {
            for idx in path {
                collect_ir_value_effect_mutable_captures(idx, mutable_scope, handled_ops, captured);
            }
            collect_ir_effect_mutable_captures(value, mutable_scope, handled_ops, captured);
        }
        CompExpr::Case { scrutinee, arms } => {
            collect_ir_value_effect_mutable_captures(
                scrutinee,
                mutable_scope,
                handled_ops,
                captured,
            );
            for arm in arms {
                collect_ir_effect_mutable_captures(&arm.body, mutable_scope, handled_ops, captured);
            }
        }
        CompExpr::PerformEffect { op, args, .. } => {
            if handled_ops.iter().any(|candidate| candidate == op) {
                captured.extend(mutable_scope.iter().cloned());
            }
            for arg in args {
                collect_ir_value_effect_mutable_captures(arg, mutable_scope, handled_ops, captured);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            collect_ir_effect_mutable_captures(handler, mutable_scope, handled_ops, captured);
            collect_ir_effect_mutable_captures(body, mutable_scope, handled_ops, captured);
        }
        CompExpr::Handle { .. } => {}
        CompExpr::Resume { value }
        | CompExpr::Dup { value }
        | CompExpr::Drop { value }
        | CompExpr::DropReuse { value, .. } => {
            collect_ir_value_effect_mutable_captures(value, mutable_scope, handled_ops, captured);
        }
        CompExpr::AllocReuse { .. } => {}
    }
}

fn collect_ir_value_effect_mutable_captures(
    value: &ValueExpr,
    mutable_scope: &BTreeSet<String>,
    handled_ops: &[String],
    captured: &mut BTreeSet<String>,
) {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                collect_ir_value_effect_mutable_captures(
                    element,
                    mutable_scope,
                    handled_ops,
                    captured,
                );
            }
            if let Some(spread) = spread {
                collect_ir_value_effect_mutable_captures(
                    spread,
                    mutable_scope,
                    handled_ops,
                    captured,
                );
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                collect_ir_value_effect_mutable_captures(
                    item,
                    mutable_scope,
                    handled_ops,
                    captured,
                );
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                collect_ir_value_effect_mutable_captures(
                    field,
                    mutable_scope,
                    handled_ops,
                    captured,
                );
            }
        }
        ValueExpr::Lambda { body, .. } => {
            collect_ir_effect_mutable_captures(body, mutable_scope, handled_ops, captured);
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(expr) = part {
                    collect_ir_value_effect_mutable_captures(
                        expr,
                        mutable_scope,
                        handled_ops,
                        captured,
                    );
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            collect_ir_value_effect_mutable_captures(left, mutable_scope, handled_ops, captured);
            collect_ir_value_effect_mutable_captures(right, mutable_scope, handled_ops, captured);
        }
        ValueExpr::TupleProject { tuple, .. } => {
            collect_ir_value_effect_mutable_captures(tuple, mutable_scope, handled_ops, captured);
        }
        ValueExpr::ListGet { list, index } => {
            collect_ir_value_effect_mutable_captures(list, mutable_scope, handled_ops, captured);
            collect_ir_value_effect_mutable_captures(index, mutable_scope, handled_ops, captured);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn ir_comp_contains_resume_under_lambda(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::Value(value) => ir_value_contains_resume_under_lambda(value),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            ir_comp_contains_resume_under_lambda(value)
                || ir_comp_contains_resume_under_lambda(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(ir_comp_contains_resume_under_lambda)
                || ir_comp_contains_resume_under_lambda(tail)
        }
        CompExpr::If { then_, else_, .. } => {
            ir_comp_contains_resume_under_lambda(then_)
                || ir_comp_contains_resume_under_lambda(else_)
        }
        CompExpr::Assign { value, .. } => ir_comp_contains_resume_under_lambda(value),
        CompExpr::AssignIndex { value, .. } => ir_comp_contains_resume_under_lambda(value),
        CompExpr::Case { arms, .. } => arms
            .iter()
            .any(|arm| ir_comp_contains_resume_under_lambda(&arm.body)),
        CompExpr::Handle { .. } => false,
        CompExpr::WithHandler { handler, body } => {
            ir_comp_contains_resume_under_lambda(handler)
                || ir_comp_contains_resume_under_lambda(body)
        }
        CompExpr::Call { .. }
        | CompExpr::PerformEffect { .. }
        | CompExpr::Resume { .. }
        | CompExpr::Dup { .. }
        | CompExpr::Drop { .. }
        | CompExpr::DropReuse { .. }
        | CompExpr::AllocReuse { .. } => false,
    }
}

fn ir_comp_contains_resume(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::Resume { .. } => true,
        CompExpr::Value(value) => ir_value_contains_resume(value),
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
        CompExpr::AssignIndex { value, .. } => ir_comp_contains_resume(value),
        CompExpr::Case { arms, .. } => arms.iter().any(|arm| ir_comp_contains_resume(&arm.body)),
        CompExpr::Handle { .. } => false,
        CompExpr::WithHandler { handler, body } => {
            ir_comp_contains_resume(handler) || ir_comp_contains_resume(body)
        }
        CompExpr::Call { .. }
        | CompExpr::PerformEffect { .. }
        | CompExpr::Dup { .. }
        | CompExpr::Drop { .. }
        | CompExpr::DropReuse { .. }
        | CompExpr::AllocReuse { .. } => false,
    }
}

fn ir_value_contains_resume(value: &ValueExpr) -> bool {
    match value {
        ValueExpr::Lambda { body, .. } => ir_comp_contains_resume(body),
        ValueExpr::ListLit { elements, spread } => {
            elements.iter().any(ir_value_contains_resume)
                || spread.as_deref().is_some_and(ir_value_contains_resume)
        }
        ValueExpr::TupleLit(items) => items.iter().any(ir_value_contains_resume),
        ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, field)| ir_value_contains_resume(field)),
        ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            IrInterpPart::Expr(expr) => ir_value_contains_resume(expr),
            IrInterpPart::Text(_) => false,
        }),
        ValueExpr::BinOp { left, right, .. } => {
            ir_value_contains_resume(left) || ir_value_contains_resume(right)
        }
        ValueExpr::TupleProject { tuple, .. } => ir_value_contains_resume(tuple),
        ValueExpr::ListGet { list, index } => {
            ir_value_contains_resume(list) || ir_value_contains_resume(index)
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => false,
    }
}

fn ir_value_contains_resume_under_lambda(value: &ValueExpr) -> bool {
    match value {
        ValueExpr::Lambda { body, .. } => ir_comp_contains_resume(body),
        ValueExpr::ListLit { elements, spread } => {
            elements.iter().any(ir_value_contains_resume_under_lambda)
                || spread
                    .as_deref()
                    .is_some_and(ir_value_contains_resume_under_lambda)
        }
        ValueExpr::TupleLit(items) => items.iter().any(ir_value_contains_resume_under_lambda),
        ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, field)| ir_value_contains_resume_under_lambda(field)),
        ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            IrInterpPart::Expr(expr) => ir_value_contains_resume_under_lambda(expr),
            IrInterpPart::Text(_) => false,
        }),
        ValueExpr::BinOp { left, right, .. } => {
            ir_value_contains_resume_under_lambda(left)
                || ir_value_contains_resume_under_lambda(right)
        }
        ValueExpr::TupleProject { tuple, .. } => ir_value_contains_resume_under_lambda(tuple),
        ValueExpr::ListGet { list, index } => {
            ir_value_contains_resume_under_lambda(list)
                || ir_value_contains_resume_under_lambda(index)
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => false,
    }
}

fn collect_ast_decl_legality(
    decl_name: &str,
    stmts: &[Stmt],
    summary: &mut HandlerLegalitySummary,
) {
    collect_ast_stmts_legality(decl_name, stmts, &HashMap::new(), &BTreeSet::new(), summary);
}

fn collect_ast_stmts_legality(
    decl_name: &str,
    stmts: &[Stmt],
    env: &HashMap<String, Vec<HandlerClause>>,
    mutable_scope: &BTreeSet<String>,
    summary: &mut HandlerLegalitySummary,
) {
    let mut local_env = env.clone();
    let mut local_mutable_scope = mutable_scope.clone();
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value, .. } => {
                collect_ast_expr_legality(
                    decl_name,
                    value,
                    &local_env,
                    &local_mutable_scope,
                    summary,
                );
                if let Some(clauses) = resolve_ast_handler_clauses(value, &local_env) {
                    local_env.insert(name.clone(), clauses);
                } else {
                    local_env.remove(name);
                }
            }
            Stmt::MutBinding { name, value, .. } => {
                collect_ast_expr_legality(
                    decl_name,
                    value,
                    &local_env,
                    &local_mutable_scope,
                    summary,
                );
                if let Some(clauses) = resolve_ast_handler_clauses(value, &local_env) {
                    local_env.insert(name.clone(), clauses);
                } else {
                    local_env.remove(name);
                }
                local_mutable_scope.insert(name.clone());
            }
            Stmt::Assign { value, .. } | Stmt::Expr(value, _) => {
                collect_ast_expr_legality(
                    decl_name,
                    value,
                    &local_env,
                    &local_mutable_scope,
                    summary,
                );
            }
        }
    }
}

fn collect_ast_expr_legality(
    decl_name: &str,
    expr: &Expr,
    env: &HashMap<String, Vec<HandlerClause>>,
    mutable_scope: &BTreeSet<String>,
    summary: &mut HandlerLegalitySummary,
) {
    match expr {
        Expr::Spanned { expr, .. } => {
            collect_ast_expr_legality(decl_name, expr, env, mutable_scope, summary);
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    collect_ast_expr_legality(decl_name, expr, env, mutable_scope, summary);
                }
            }
        }
        Expr::ListLit { elements, spread } => {
            for element in elements {
                collect_ast_expr_legality(decl_name, element, env, mutable_scope, summary);
            }
            if let Some(spread) = spread {
                collect_ast_expr_legality(decl_name, spread, env, mutable_scope, summary);
            }
        }
        Expr::TupleLit(items) => {
            for item in items {
                collect_ast_expr_legality(decl_name, item, env, mutable_scope, summary);
            }
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, field) in fields {
                collect_ast_expr_legality(decl_name, field, env, mutable_scope, summary);
            }
        }
        Expr::UnaryOp { expr, .. } => {
            collect_ast_expr_legality(decl_name, expr, env, mutable_scope, summary);
        }
        Expr::BinOp { left, right, .. } => {
            collect_ast_expr_legality(decl_name, left, env, mutable_scope, summary);
            collect_ast_expr_legality(decl_name, right, env, mutable_scope, summary);
        }
        Expr::Call { callee, arg, .. } => {
            collect_ast_expr_legality(decl_name, callee, env, mutable_scope, summary);
            collect_ast_expr_legality(decl_name, arg, env, mutable_scope, summary);
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                collect_ast_expr_legality(decl_name, arg, env, mutable_scope, summary);
            }
        }
        Expr::Pipeline { value, .. } => {
            collect_ast_expr_legality(decl_name, value, env, mutable_scope, summary);
        }
        Expr::Lambda { body, .. } => {
            collect_ast_expr_legality(decl_name, body, env, mutable_scope, summary);
        }
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = clause.parsed_body.as_deref() {
                    collect_ast_stmts_legality(decl_name, stmts, env, mutable_scope, summary);
                }
            }
        }
        Expr::With { handler, body } => {
            summary.push(classify_ast_with_handler(
                decl_name,
                handler,
                body,
                env,
                mutable_scope,
            ));
            collect_ast_expr_legality(decl_name, handler, env, mutable_scope, summary);
            collect_ast_stmts_legality(decl_name, body, env, mutable_scope, summary);
        }
        Expr::Resume { value } => {
            collect_ast_expr_legality(decl_name, value, env, mutable_scope, summary);
        }
        Expr::Block(stmts) => {
            collect_ast_stmts_legality(decl_name, stmts, env, mutable_scope, summary);
        }
        Expr::Case { scrutinee, arms } => {
            collect_ast_expr_legality(decl_name, scrutinee, env, mutable_scope, summary);
            for arm in arms {
                collect_ast_expr_legality(decl_name, &arm.body, env, mutable_scope, summary);
            }
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_ast_expr_legality(decl_name, condition, env, mutable_scope, summary);
            collect_ast_expr_legality(decl_name, then_expr, env, mutable_scope, summary);
            collect_ast_expr_legality(decl_name, else_expr, env, mutable_scope, summary);
        }
        Expr::ListIndex { list, index } => {
            collect_ast_expr_legality(decl_name, list, env, mutable_scope, summary);
            collect_ast_expr_legality(decl_name, index, env, mutable_scope, summary);
        }
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var { .. }
        | Expr::Qualified { .. } => {}
    }
}

fn classify_ast_with_handler(
    decl_name: &str,
    handler: &Expr,
    body: &[Stmt],
    env: &HashMap<String, Vec<HandlerClause>>,
    mutable_scope: &BTreeSet<String>,
) -> WithHandlerLegalityRecord {
    let Some(clauses) = resolve_ast_handler_clauses(handler, env) else {
        return WithHandlerLegalityRecord {
            decl_name: decl_name.to_string(),
            clause_ops: Vec::new(),
            kind: HandlerUseKind::Unsupported,
            captured_mutable_locals: Vec::new(),
            legality: WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::HandlerExpressionNotStaticallyResolvable,
            },
        };
    };
    let clause_ops = clauses
        .iter()
        .map(|clause| clause.name.clone())
        .collect::<Vec<_>>();
    let captured_mutable_locals =
        captured_mutable_locals_for_ast_body(body, mutable_scope, &clause_ops);
    let mut stats = ClauseResumeStats::default();
    for clause in &clauses {
        stats.combine(analyze_ast_clause_body(clause));
    }
    let (kind, legality) = classify_resume_stats(stats, captured_mutable_locals.clone());
    WithHandlerLegalityRecord {
        decl_name: decl_name.to_string(),
        clause_ops,
        kind,
        captured_mutable_locals,
        legality,
    }
}

fn resolve_ast_handler_clauses(
    expr: &Expr,
    env: &HashMap<String, Vec<HandlerClause>>,
) -> Option<Vec<HandlerClause>> {
    match expr {
        Expr::Spanned { expr, .. } => resolve_ast_handler_clauses(expr, env),
        Expr::Handler { clauses } => Some(clauses.clone()),
        Expr::Var { name, .. } => env.get(name).cloned(),
        _ => None,
    }
}

fn analyze_ast_clause_body(clause: &HandlerClause) -> ClauseResumeStats {
    let Some(stmts) = clause.parsed_body.as_deref() else {
        return ClauseResumeStats::default();
    };
    let mut stats = ClauseResumeStats::default();
    analyze_ast_stmts_for_resume(stmts, true, false, &mut stats);
    stats
}

fn analyze_ast_stmts_for_resume(
    stmts: &[Stmt],
    tail_position: bool,
    in_lambda: bool,
    stats: &mut ClauseResumeStats,
) {
    for (idx, stmt) in stmts.iter().enumerate() {
        let stmt_tail = tail_position && idx + 1 == stmts.len();
        match stmt {
            Stmt::Binding { value, .. }
            | Stmt::MutBinding { value, .. }
            | Stmt::Assign { value, .. } => {
                analyze_ast_expr_for_resume(value, false, in_lambda, stats);
            }
            Stmt::Expr(expr, _) => analyze_ast_expr_for_resume(expr, stmt_tail, in_lambda, stats),
        }
    }
}

fn analyze_ast_expr_for_resume(
    expr: &Expr,
    tail_position: bool,
    in_lambda: bool,
    stats: &mut ClauseResumeStats,
) {
    match expr {
        Expr::Spanned { expr, .. } => {
            analyze_ast_expr_for_resume(expr, tail_position, in_lambda, stats);
        }
        Expr::Resume { value } => {
            stats.resume_count += 1;
            stats.non_tail_resume |= !tail_position;
            stats.delayed_resume |= in_lambda;
            stats.reentrant_resume |= ast_expr_contains_resume(value);
            analyze_ast_expr_for_resume(value, false, in_lambda, stats);
        }
        Expr::Lambda { body, .. } => analyze_ast_expr_for_resume(body, false, true, stats),
        Expr::Block(stmts) => analyze_ast_stmts_for_resume(stmts, tail_position, in_lambda, stats),
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            analyze_ast_expr_for_resume(condition, false, in_lambda, stats);
            analyze_ast_expr_for_resume(then_expr, tail_position, in_lambda, stats);
            analyze_ast_expr_for_resume(else_expr, tail_position, in_lambda, stats);
        }
        Expr::Case { scrutinee, arms } => {
            analyze_ast_expr_for_resume(scrutinee, false, in_lambda, stats);
            for arm in arms {
                analyze_ast_expr_for_resume(&arm.body, tail_position, in_lambda, stats);
            }
        }
        Expr::With { handler, body } => {
            analyze_ast_expr_for_resume(handler, false, in_lambda, stats);
            analyze_ast_stmts_for_resume(body, tail_position, in_lambda, stats);
        }
        Expr::Handler { .. } => {}
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    analyze_ast_expr_for_resume(expr, false, in_lambda, stats);
                }
            }
        }
        Expr::ListLit { elements, spread } => {
            for element in elements {
                analyze_ast_expr_for_resume(element, false, in_lambda, stats);
            }
            if let Some(spread) = spread {
                analyze_ast_expr_for_resume(spread, false, in_lambda, stats);
            }
        }
        Expr::TupleLit(items) => {
            for item in items {
                analyze_ast_expr_for_resume(item, false, in_lambda, stats);
            }
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, field) in fields {
                analyze_ast_expr_for_resume(field, false, in_lambda, stats);
            }
        }
        Expr::UnaryOp { expr, .. } => analyze_ast_expr_for_resume(expr, false, in_lambda, stats),
        Expr::BinOp { left, right, .. } => {
            analyze_ast_expr_for_resume(left, false, in_lambda, stats);
            analyze_ast_expr_for_resume(right, false, in_lambda, stats);
        }
        Expr::Call { callee, arg, .. } => {
            analyze_ast_expr_for_resume(callee, false, in_lambda, stats);
            analyze_ast_expr_for_resume(arg, false, in_lambda, stats);
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                analyze_ast_expr_for_resume(arg, false, in_lambda, stats);
            }
        }
        Expr::Pipeline { value, .. } => analyze_ast_expr_for_resume(value, false, in_lambda, stats),
        Expr::ListIndex { list, index } => {
            analyze_ast_expr_for_resume(list, false, in_lambda, stats);
            analyze_ast_expr_for_resume(index, false, in_lambda, stats);
        }
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var { .. }
        | Expr::Qualified { .. } => {}
    }
}

fn ast_expr_contains_resume(expr: &Expr) -> bool {
    match expr {
        Expr::Spanned { expr, .. } => ast_expr_contains_resume(expr),
        Expr::Resume { .. } => true,
        Expr::InterpolatedString(parts) => parts.iter().any(|part| match part {
            InterpolatedPart::Expr(expr) => ast_expr_contains_resume(expr),
            InterpolatedPart::Text(_) => false,
        }),
        Expr::ListLit { elements, spread } => {
            elements.iter().any(ast_expr_contains_resume)
                || spread.as_deref().is_some_and(ast_expr_contains_resume)
        }
        Expr::TupleLit(items) => items.iter().any(ast_expr_contains_resume),
        Expr::RecordConstruct { fields, .. } => fields
            .iter()
            .any(|(_, field)| ast_expr_contains_resume(field)),
        Expr::UnaryOp { expr, .. } => ast_expr_contains_resume(expr),
        Expr::BinOp { left, right, .. } => {
            ast_expr_contains_resume(left) || ast_expr_contains_resume(right)
        }
        Expr::Call { callee, arg, .. } => {
            ast_expr_contains_resume(callee) || ast_expr_contains_resume(arg)
        }
        Expr::MethodCall { args, .. } => args.iter().any(ast_expr_contains_resume),
        Expr::Pipeline { value, .. } => ast_expr_contains_resume(value),
        Expr::Lambda { body, .. } => ast_expr_contains_resume(body),
        Expr::Handler { clauses } => clauses.iter().any(|clause| {
            clause
                .parsed_body
                .as_deref()
                .is_some_and(ast_stmts_contain_resume)
        }),
        Expr::With { handler, body } => {
            ast_expr_contains_resume(handler) || ast_stmts_contain_resume(body)
        }
        Expr::Block(stmts) => ast_stmts_contain_resume(stmts),
        Expr::Case { scrutinee, arms } => {
            ast_expr_contains_resume(scrutinee)
                || arms.iter().any(|arm| ast_expr_contains_resume(&arm.body))
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            ast_expr_contains_resume(condition)
                || ast_expr_contains_resume(then_expr)
                || ast_expr_contains_resume(else_expr)
        }
        Expr::ListIndex { list, index } => {
            ast_expr_contains_resume(list) || ast_expr_contains_resume(index)
        }
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var { .. }
        | Expr::Qualified { .. } => false,
    }
}

fn ast_stmts_contain_resume(stmts: &[Stmt]) -> bool {
    stmts.iter().any(|stmt| match stmt {
        Stmt::Binding { value, .. }
        | Stmt::MutBinding { value, .. }
        | Stmt::Assign { value, .. } => ast_expr_contains_resume(value),
        Stmt::Expr(expr, _) => ast_expr_contains_resume(expr),
    })
}

fn captured_mutable_locals_for_ast_body(
    body: &[Stmt],
    mutable_scope: &BTreeSet<String>,
    handled_ops: &[String],
) -> Vec<String> {
    let mut captured = BTreeSet::new();
    collect_ast_stmts_effect_mutable_captures(body, mutable_scope, handled_ops, &mut captured);
    captured.into_iter().collect()
}

fn collect_ast_stmts_effect_mutable_captures(
    stmts: &[Stmt],
    mutable_scope: &BTreeSet<String>,
    handled_ops: &[String],
    captured: &mut BTreeSet<String>,
) {
    let mut local_scope = mutable_scope.clone();
    for stmt in stmts {
        match stmt {
            Stmt::Binding { value, .. } => {
                collect_ast_expr_effect_mutable_captures(
                    value,
                    &local_scope,
                    handled_ops,
                    captured,
                );
            }
            Stmt::MutBinding { name, value, .. } => {
                collect_ast_expr_effect_mutable_captures(
                    value,
                    &local_scope,
                    handled_ops,
                    captured,
                );
                local_scope.insert(name.clone());
            }
            Stmt::Assign { target, value, .. } => {
                collect_ast_assign_target_effect_mutable_captures(
                    target,
                    &local_scope,
                    handled_ops,
                    captured,
                );
                collect_ast_expr_effect_mutable_captures(
                    value,
                    &local_scope,
                    handled_ops,
                    captured,
                );
            }
            Stmt::Expr(expr, _) => {
                collect_ast_expr_effect_mutable_captures(expr, &local_scope, handled_ops, captured);
            }
        }
    }
}

fn collect_ast_assign_target_effect_mutable_captures(
    target: &AssignTarget,
    mutable_scope: &BTreeSet<String>,
    handled_ops: &[String],
    captured: &mut BTreeSet<String>,
) {
    match target {
        AssignTarget::Var(_) => {}
        AssignTarget::ListIndex { base, index } => {
            collect_ast_assign_target_effect_mutable_captures(
                base,
                mutable_scope,
                handled_ops,
                captured,
            );
            collect_ast_expr_effect_mutable_captures(index, mutable_scope, handled_ops, captured);
        }
    }
}

fn collect_ast_expr_effect_mutable_captures(
    expr: &Expr,
    mutable_scope: &BTreeSet<String>,
    handled_ops: &[String],
    captured: &mut BTreeSet<String>,
) {
    if ast_expr_is_handled_operation_call(expr, handled_ops) {
        captured.extend(mutable_scope.iter().cloned());
    }
    match expr {
        Expr::Spanned { expr, .. } => {
            collect_ast_expr_effect_mutable_captures(expr, mutable_scope, handled_ops, captured);
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    collect_ast_expr_effect_mutable_captures(
                        expr,
                        mutable_scope,
                        handled_ops,
                        captured,
                    );
                }
            }
        }
        Expr::ListLit { elements, spread } => {
            for element in elements {
                collect_ast_expr_effect_mutable_captures(
                    element,
                    mutable_scope,
                    handled_ops,
                    captured,
                );
            }
            if let Some(spread) = spread {
                collect_ast_expr_effect_mutable_captures(
                    spread,
                    mutable_scope,
                    handled_ops,
                    captured,
                );
            }
        }
        Expr::TupleLit(items) => {
            for item in items {
                collect_ast_expr_effect_mutable_captures(
                    item,
                    mutable_scope,
                    handled_ops,
                    captured,
                );
            }
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, field) in fields {
                collect_ast_expr_effect_mutable_captures(
                    field,
                    mutable_scope,
                    handled_ops,
                    captured,
                );
            }
        }
        Expr::UnaryOp { expr, .. } => {
            collect_ast_expr_effect_mutable_captures(expr, mutable_scope, handled_ops, captured);
        }
        Expr::BinOp { left, right, .. } => {
            collect_ast_expr_effect_mutable_captures(left, mutable_scope, handled_ops, captured);
            collect_ast_expr_effect_mutable_captures(right, mutable_scope, handled_ops, captured);
        }
        Expr::Call { callee, arg, .. } => {
            collect_ast_expr_effect_mutable_captures(callee, mutable_scope, handled_ops, captured);
            collect_ast_expr_effect_mutable_captures(arg, mutable_scope, handled_ops, captured);
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                collect_ast_expr_effect_mutable_captures(arg, mutable_scope, handled_ops, captured);
            }
        }
        Expr::Pipeline { value, .. } => {
            collect_ast_expr_effect_mutable_captures(value, mutable_scope, handled_ops, captured);
        }
        Expr::Lambda { body, .. } => {
            collect_ast_expr_effect_mutable_captures(body, mutable_scope, handled_ops, captured);
        }
        Expr::Handler { .. } => {}
        Expr::With { handler, body } => {
            collect_ast_expr_effect_mutable_captures(handler, mutable_scope, handled_ops, captured);
            collect_ast_stmts_effect_mutable_captures(body, mutable_scope, handled_ops, captured);
        }
        Expr::Resume { value } => {
            collect_ast_expr_effect_mutable_captures(value, mutable_scope, handled_ops, captured);
        }
        Expr::Block(stmts) => {
            collect_ast_stmts_effect_mutable_captures(stmts, mutable_scope, handled_ops, captured);
        }
        Expr::Case { scrutinee, arms } => {
            collect_ast_expr_effect_mutable_captures(
                scrutinee,
                mutable_scope,
                handled_ops,
                captured,
            );
            for arm in arms {
                collect_ast_expr_effect_mutable_captures(
                    &arm.body,
                    mutable_scope,
                    handled_ops,
                    captured,
                );
            }
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_ast_expr_effect_mutable_captures(
                condition,
                mutable_scope,
                handled_ops,
                captured,
            );
            collect_ast_expr_effect_mutable_captures(
                then_expr,
                mutable_scope,
                handled_ops,
                captured,
            );
            collect_ast_expr_effect_mutable_captures(
                else_expr,
                mutable_scope,
                handled_ops,
                captured,
            );
        }
        Expr::ListIndex { list, index } => {
            collect_ast_expr_effect_mutable_captures(list, mutable_scope, handled_ops, captured);
            collect_ast_expr_effect_mutable_captures(index, mutable_scope, handled_ops, captured);
        }
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var { .. }
        | Expr::Qualified { .. } => {}
    }
}

fn ast_expr_is_handled_operation_call(expr: &Expr, handled_ops: &[String]) -> bool {
    let Expr::Call { callee, .. } = expr else {
        return false;
    };
    ast_callee_is_handled_operation(callee, handled_ops)
}

fn ast_callee_is_handled_operation(expr: &Expr, handled_ops: &[String]) -> bool {
    match expr {
        Expr::Spanned { expr, .. } => ast_callee_is_handled_operation(expr, handled_ops),
        Expr::Var { name, .. } => handled_ops.iter().any(|op| op == name),
        Expr::Qualified { member, .. } => handled_ops.iter().any(|op| op == member),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use goby_core::ir::{IrType, ValueExpr};
    use goby_core::parse_module;

    fn safe_handler_body() -> CompExpr {
        CompExpr::Resume {
            value: Box::new(ValueExpr::Unit),
        }
    }

    fn collect_test_comp(body: &CompExpr) -> HandlerLegalitySummary {
        let mut summary = HandlerLegalitySummary::default();
        collect_comp_legality(
            "main",
            body,
            &HashMap::new(),
            &BTreeSet::new(),
            &mut summary,
        );
        summary
    }

    #[test]
    fn classifies_tail_resume_as_safe() {
        let body = CompExpr::WithHandler {
            handler: Box::new(CompExpr::Handle {
                clauses: vec![IrHandlerClause {
                    op_name: "tick".to_string(),
                    params: vec!["value".to_string()],
                    body: safe_handler_body(),
                }],
            }),
            body: Box::new(CompExpr::Value(ValueExpr::Unit)),
        };
        let summary = collect_test_comp(&body);
        assert_eq!(summary.records.len(), 1);
        assert_eq!(
            summary.records[0].legality,
            WithHandlerLegality::OneShotTailResumptive
        );
        assert_eq!(summary.records[0].kind, HandlerUseKind::DirectOneShot);
    }

    #[test]
    fn rejects_non_tail_resume_in_seq_stmt() {
        let body = CompExpr::WithHandler {
            handler: Box::new(CompExpr::Handle {
                clauses: vec![IrHandlerClause {
                    op_name: "tick".to_string(),
                    params: vec!["value".to_string()],
                    body: CompExpr::Seq {
                        stmts: vec![CompExpr::Resume {
                            value: Box::new(ValueExpr::Unit),
                        }],
                        tail: Box::new(CompExpr::Value(ValueExpr::Unit)),
                    },
                }],
            }),
            body: Box::new(CompExpr::Value(ValueExpr::Unit)),
        };
        let summary = collect_test_comp(&body);
        assert_eq!(
            summary.records[0].legality,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::ResumeNotInTailPosition
            }
        );
    }

    #[test]
    fn classifies_scope_exit_without_resume_as_safe() {
        let body = CompExpr::WithHandler {
            handler: Box::new(CompExpr::Handle {
                clauses: vec![IrHandlerClause {
                    op_name: "tick".to_string(),
                    params: vec!["value".to_string()],
                    body: CompExpr::Value(ValueExpr::Unit),
                }],
            }),
            body: Box::new(CompExpr::Value(ValueExpr::Unit)),
        };
        let summary = collect_test_comp(&body);
        assert!(summary.all_one_shot_tail_resumptive());
        assert_eq!(summary.records[0].kind, HandlerUseKind::Abortive);
    }

    #[test]
    fn resolves_handler_alias_before_classifying_with() {
        let body = CompExpr::Let {
            name: "h".to_string(),
            ty: IrType::Unknown,
            value: Box::new(CompExpr::Handle {
                clauses: vec![IrHandlerClause {
                    op_name: "tick".to_string(),
                    params: vec!["value".to_string()],
                    body: safe_handler_body(),
                }],
            }),
            body: Box::new(CompExpr::WithHandler {
                handler: Box::new(CompExpr::Value(ValueExpr::Var("h".to_string()))),
                body: Box::new(CompExpr::Value(ValueExpr::Unit)),
            }),
        };
        let summary = collect_test_comp(&body);
        assert!(summary.all_one_shot_tail_resumptive());
        assert_eq!(summary.records[0].kind, HandlerUseKind::DirectOneShot);
    }

    #[test]
    fn resume_inside_lambda_is_rejected_as_non_tail() {
        let body = CompExpr::WithHandler {
            handler: Box::new(CompExpr::Handle {
                clauses: vec![IrHandlerClause {
                    op_name: "tick".to_string(),
                    params: vec!["value".to_string()],
                    body: CompExpr::Value(ValueExpr::Lambda {
                        param: "x".to_string(),
                        body: Box::new(CompExpr::Resume {
                            value: Box::new(ValueExpr::Var("x".to_string())),
                        }),
                    }),
                }],
            }),
            body: Box::new(CompExpr::Value(ValueExpr::Unit)),
        };
        let summary = collect_test_comp(&body);
        assert_eq!(
            summary.records[0].legality,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::DelayedResume
            }
        );
        assert_eq!(summary.records[0].kind, HandlerUseKind::DelayedOneShot);
    }

    fn first_source_record(source: &str) -> WithHandlerLegalityRecord {
        let module = parse_module(source).expect("source should parse");
        let summary =
            analyze_module_handler_legality(&module).expect("legality analysis should succeed");
        summary
            .records()
            .first()
            .cloned()
            .expect("source should contain a with handler")
    }

    #[test]
    fn source_classifier_marks_reentrant_resume_argument() {
        let record = first_source_record(
            r#"
effect Tick
  tick: Int -> Int

main : Unit -> Unit
main =
  with
    tick n ->
      resume (resume 1)
  in
    print (tick 0)
"#,
        );
        assert_eq!(record.kind, HandlerUseKind::ReentrantLooking);
        assert_eq!(
            record.legality,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::ReentrantResume
            }
        );
    }

    #[test]
    fn source_classifier_marks_sequential_multi_shot_without_mutable_capture() {
        let record = first_source_record(
            r#"
effect Tick
  tick: Unit -> Unit

main : Unit -> Unit
main =
  with
    tick _ ->
      resume ()
      resume ()
  in
    tick ()
"#,
        );
        assert_eq!(record.kind, HandlerUseKind::SequentialMultiShot);
        assert_eq!(
            record.legality,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::MultipleResume
            }
        );
    }

    #[test]
    fn source_classifier_marks_multi_shot_mutable_capture_as_distinct_unsupported_shape() {
        let record = first_source_record(
            r#"
effect Tick
  tick: Unit -> Unit

main : Unit -> Unit
main =
  mut counter = 0
  with
    tick _ ->
      resume ()
      resume ()
  in
    tick ()
    counter := counter + 1
"#,
        );
        assert_eq!(record.kind, HandlerUseKind::UnsupportedMutableCapture);
        assert_eq!(record.captured_mutable_locals, vec!["counter"]);
        assert_eq!(
            record.legality,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::MultiShotCapturesMutableLocals {
                    locals: vec!["counter".to_string()]
                }
            }
        );
    }

    #[test]
    fn source_classifier_tracks_mutable_declared_inside_with_body_before_effect_call() {
        let record = first_source_record(
            r#"
effect Tick
  tick: Unit -> Unit

main : Unit -> Unit
main =
  with
    tick _ ->
      resume ()
      resume ()
  in
    mut local = 0
    tick ()
    local := local + 1
"#,
        );
        assert_eq!(record.kind, HandlerUseKind::UnsupportedMutableCapture);
        assert_eq!(record.captured_mutable_locals, vec!["local"]);
    }
}
