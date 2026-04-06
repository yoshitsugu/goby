use std::collections::HashMap;

use goby_core::Module;
use goby_core::ir::{CompExpr, IrDecl, IrHandlerClause, IrInterpPart, ValueExpr};

use crate::CodegenError;
use crate::wasm_exec_plan::decl_exec_plan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum HandlerLegalityIssue {
    HandlerExpressionNotStaticallyResolvable,
    ResumeNotInTailPosition,
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
        }
    }
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
        if let Some(ir_decl) = decl_exec_plan(decl).ir_decl.as_ref() {
            collect_decl_legality(ir_decl, &mut summary);
        }
    }
    Ok(summary)
}

fn collect_decl_legality(decl: &IrDecl, summary: &mut HandlerLegalitySummary) {
    collect_comp_legality(&decl.name, &decl.body, &HashMap::new(), summary);
}

fn collect_comp_legality(
    decl_name: &str,
    comp: &CompExpr,
    env: &HashMap<String, Vec<IrHandlerClause>>,
    summary: &mut HandlerLegalitySummary,
) {
    match comp {
        CompExpr::Value(value) => collect_value_legality(decl_name, value, env, summary),
        CompExpr::Let {
            name, value, body, ..
        }
        | CompExpr::LetMut {
            name, value, body, ..
        } => {
            collect_comp_legality(decl_name, value, env, summary);
            let mut body_env = env.clone();
            if let Some(clauses) = extract_handler_binding(value, env) {
                body_env.insert(name.clone(), clauses);
            } else {
                body_env.remove(name);
            }
            collect_comp_legality(decl_name, body, &body_env, summary);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                collect_comp_legality(decl_name, stmt, env, summary);
            }
            collect_comp_legality(decl_name, tail, env, summary);
        }
        CompExpr::If { cond, then_, else_ } => {
            collect_value_legality(decl_name, cond, env, summary);
            collect_comp_legality(decl_name, then_, env, summary);
            collect_comp_legality(decl_name, else_, env, summary);
        }
        CompExpr::Call { callee, args } => {
            collect_value_legality(decl_name, callee, env, summary);
            for arg in args {
                collect_value_legality(decl_name, arg, env, summary);
            }
        }
        CompExpr::Assign { value, .. } => collect_comp_legality(decl_name, value, env, summary),
        CompExpr::Case { scrutinee, arms } => {
            collect_value_legality(decl_name, scrutinee, env, summary);
            for arm in arms {
                collect_comp_legality(decl_name, &arm.body, env, summary);
            }
        }
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                collect_value_legality(decl_name, arg, env, summary);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                collect_comp_legality(decl_name, &clause.body, env, summary);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            summary.push(classify_with_handler(decl_name, handler, env));
            collect_comp_legality(decl_name, handler, env, summary);
            collect_comp_legality(decl_name, body, env, summary);
        }
        CompExpr::Resume { value } => collect_value_legality(decl_name, value, env, summary),
        CompExpr::AssignIndex { path, value, .. } => {
            for idx in path {
                collect_value_legality(decl_name, idx, env, summary);
            }
            collect_comp_legality(decl_name, value, env, summary);
        }
    }
}

fn collect_value_legality(
    decl_name: &str,
    value: &ValueExpr,
    env: &HashMap<String, Vec<IrHandlerClause>>,
    summary: &mut HandlerLegalitySummary,
) {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                collect_value_legality(decl_name, element, env, summary);
            }
            if let Some(spread) = spread {
                collect_value_legality(decl_name, spread, env, summary);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                collect_value_legality(decl_name, item, env, summary);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                collect_value_legality(decl_name, field, env, summary);
            }
        }
        ValueExpr::Lambda { body, .. } => collect_comp_legality(decl_name, body, env, summary),
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(expr) = part {
                    collect_value_legality(decl_name, expr, env, summary);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            collect_value_legality(decl_name, left, env, summary);
            collect_value_legality(decl_name, right, env, summary);
        }
        ValueExpr::TupleProject { tuple, .. } => {
            collect_value_legality(decl_name, tuple, env, summary);
        }
        ValueExpr::ListGet { list, index } => {
            collect_value_legality(decl_name, list, env, summary);
            collect_value_legality(decl_name, index, env, summary);
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
    env: &HashMap<String, Vec<IrHandlerClause>>,
) -> WithHandlerLegalityRecord {
    let Some(clauses) = resolve_handler_clauses(handler, env) else {
        return WithHandlerLegalityRecord {
            decl_name: decl_name.to_string(),
            clause_ops: Vec::new(),
            legality: WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::HandlerExpressionNotStaticallyResolvable,
            },
        };
    };

    let clause_ops = clauses
        .iter()
        .map(|clause| clause.op_name.clone())
        .collect();
    let legality = clauses
        .iter()
        .find_map(|clause| analyze_clause_body(&clause.body).err())
        .map(|issue| WithHandlerLegality::Other { issue })
        .unwrap_or(WithHandlerLegality::OneShotTailResumptive);

    WithHandlerLegalityRecord {
        decl_name: decl_name.to_string(),
        clause_ops,
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

fn analyze_clause_body(comp: &CompExpr) -> Result<bool, HandlerLegalityIssue> {
    analyze_comp(comp, true)
}

fn analyze_comp(comp: &CompExpr, tail_position: bool) -> Result<bool, HandlerLegalityIssue> {
    match comp {
        CompExpr::Value(value) => {
            analyze_value(value)?;
            Ok(false)
        }
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            analyze_comp(value, false)?;
            analyze_comp(body, tail_position)
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                analyze_comp(stmt, false)?;
            }
            analyze_comp(tail, tail_position)
        }
        CompExpr::If { cond, then_, else_ } => {
            analyze_value(cond)?;
            Ok(analyze_comp(then_, tail_position)? || analyze_comp(else_, tail_position)?)
        }
        CompExpr::Call { callee, args } => {
            analyze_value(callee)?;
            for arg in args {
                analyze_value(arg)?;
            }
            Ok(false)
        }
        CompExpr::Assign { value, .. } => {
            analyze_comp(value, false)?;
            Ok(false)
        }
        CompExpr::Case { scrutinee, arms } => {
            analyze_value(scrutinee)?;
            let mut any_resume = false;
            for arm in arms {
                any_resume |= analyze_comp(&arm.body, tail_position)?;
            }
            Ok(any_resume)
        }
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                analyze_value(arg)?;
            }
            Ok(false)
        }
        CompExpr::Handle { .. } | CompExpr::WithHandler { .. } => Ok(false),
        CompExpr::Resume { value } => {
            analyze_value(value)?;
            if tail_position {
                Ok(true)
            } else {
                Err(HandlerLegalityIssue::ResumeNotInTailPosition)
            }
        }
        CompExpr::AssignIndex { path, value, .. } => {
            for idx in path {
                analyze_value(idx)?;
            }
            analyze_comp(value, false)?;
            Ok(false)
        }
    }
}

fn analyze_value(value: &ValueExpr) -> Result<(), HandlerLegalityIssue> {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                analyze_value(element)?;
            }
            if let Some(spread) = spread {
                analyze_value(spread)?;
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                analyze_value(item)?;
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                analyze_value(field)?;
            }
        }
        ValueExpr::Lambda { body, .. } => {
            analyze_comp(body, false)?;
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(expr) = part {
                    analyze_value(expr)?;
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            analyze_value(left)?;
            analyze_value(right)?;
        }
        ValueExpr::TupleProject { tuple, .. } => {
            analyze_value(tuple)?;
        }
        ValueExpr::ListGet { list, index } => {
            analyze_value(list)?;
            analyze_value(index)?;
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use goby_core::ir::{IrType, ValueExpr};

    fn safe_handler_body() -> CompExpr {
        CompExpr::Resume {
            value: Box::new(ValueExpr::Unit),
        }
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
        let mut summary = HandlerLegalitySummary::default();
        collect_comp_legality("main", &body, &HashMap::new(), &mut summary);
        assert_eq!(summary.records.len(), 1);
        assert_eq!(
            summary.records[0].legality,
            WithHandlerLegality::OneShotTailResumptive
        );
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
        let mut summary = HandlerLegalitySummary::default();
        collect_comp_legality("main", &body, &HashMap::new(), &mut summary);
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
        let mut summary = HandlerLegalitySummary::default();
        collect_comp_legality("main", &body, &HashMap::new(), &mut summary);
        assert!(summary.all_one_shot_tail_resumptive());
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
        let mut summary = HandlerLegalitySummary::default();
        collect_comp_legality("main", &body, &HashMap::new(), &mut summary);
        assert!(summary.all_one_shot_tail_resumptive());
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
        let mut summary = HandlerLegalitySummary::default();
        collect_comp_legality("main", &body, &HashMap::new(), &mut summary);
        assert_eq!(
            summary.records[0].legality,
            WithHandlerLegality::Other {
                issue: HandlerLegalityIssue::ResumeNotInTailPosition
            }
        );
    }
}
