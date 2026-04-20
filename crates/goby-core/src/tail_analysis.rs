use crate::ir::{CompExpr, ValueExpr};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TailPosition {
    Tail,
    NonTail,
}

impl TailPosition {
    pub const fn is_tail(self) -> bool {
        matches!(self, Self::Tail)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NestedBodyPolicy {
    Skip,
    SeparateTailRoot,
    NonTailRoot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TailWalkConfig {
    pub lambda_body: NestedBodyPolicy,
    pub handler_clause_body: NestedBodyPolicy,
}

impl TailWalkConfig {
    pub const DECL_BODY: Self = Self {
        lambda_body: NestedBodyPolicy::Skip,
        handler_clause_body: NestedBodyPolicy::Skip,
    };

    pub const HANDLER_CLAUSE_BODY: Self = Self {
        lambda_body: NestedBodyPolicy::NonTailRoot,
        handler_clause_body: NestedBodyPolicy::Skip,
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectTailCallee {
    Var(String),
    GlobalRef { module: String, name: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectTailCall {
    pub callee: DirectTailCallee,
    pub arity: usize,
}

pub fn walk_comp<F>(comp: &CompExpr, config: TailWalkConfig, mut visit: F)
where
    F: FnMut(&CompExpr, TailPosition),
{
    walk_comp_inner(comp, TailPosition::Tail, config, &mut visit);
}

pub fn collect_direct_tail_calls(comp: &CompExpr, config: TailWalkConfig) -> Vec<DirectTailCall> {
    let mut calls = Vec::new();
    walk_comp(comp, config, |node, tail_position| {
        if !tail_position.is_tail() {
            return;
        }
        if let CompExpr::Call { callee, args } = node {
            let callee = match callee.as_ref() {
                ValueExpr::Var(name) => DirectTailCallee::Var(name.clone()),
                ValueExpr::GlobalRef { module, name } => DirectTailCallee::GlobalRef {
                    module: module.clone(),
                    name: name.clone(),
                },
                _ => return,
            };
            calls.push(DirectTailCall {
                callee,
                arity: args.len(),
            });
        }
    });
    calls
}

fn walk_comp_inner<F>(
    comp: &CompExpr,
    tail_position: TailPosition,
    config: TailWalkConfig,
    visit: &mut F,
) where
    F: FnMut(&CompExpr, TailPosition),
{
    visit(comp, tail_position);
    match comp {
        CompExpr::Value(value) => walk_value(value, config, visit),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            walk_comp_inner(value, TailPosition::NonTail, config, visit);
            walk_comp_inner(body, tail_position, config, visit);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                walk_comp_inner(stmt, TailPosition::NonTail, config, visit);
            }
            walk_comp_inner(tail, tail_position, config, visit);
        }
        CompExpr::If { cond, then_, else_ } => {
            walk_value(cond, config, visit);
            walk_comp_inner(then_, tail_position, config, visit);
            walk_comp_inner(else_, tail_position, config, visit);
        }
        CompExpr::Call { callee, args } => {
            walk_value(callee, config, visit);
            for arg in args {
                walk_value(arg, config, visit);
            }
        }
        CompExpr::Assign { value, .. } => {
            walk_comp_inner(value, TailPosition::NonTail, config, visit);
        }
        CompExpr::AssignIndex { path, value, .. } => {
            for idx in path {
                walk_value(idx, config, visit);
            }
            walk_comp_inner(value, TailPosition::NonTail, config, visit);
        }
        CompExpr::Case { scrutinee, arms } => {
            walk_value(scrutinee, config, visit);
            for arm in arms {
                walk_comp_inner(&arm.body, tail_position, config, visit);
            }
        }
        CompExpr::Dup { value } | CompExpr::Drop { value } => walk_value(value, config, visit),
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                walk_value(arg, config, visit);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                walk_nested_body(&clause.body, config.handler_clause_body, config, visit);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            walk_comp_inner(handler, TailPosition::NonTail, config, visit);
            walk_comp_inner(body, tail_position, config, visit);
        }
        CompExpr::Resume { value } => walk_value(value, config, visit),
    }
}

fn walk_value<F>(value: &ValueExpr, config: TailWalkConfig, visit: &mut F)
where
    F: FnMut(&CompExpr, TailPosition),
{
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                walk_value(element, config, visit);
            }
            if let Some(spread) = spread {
                walk_value(spread, config, visit);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                walk_value(item, config, visit);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                walk_value(field, config, visit);
            }
        }
        ValueExpr::Lambda { body, .. } => {
            walk_nested_body(body, config.lambda_body, config, visit);
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let crate::ir::IrInterpPart::Expr(expr) = part {
                    walk_value(expr, config, visit);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            walk_value(left, config, visit);
            walk_value(right, config, visit);
        }
        ValueExpr::TupleProject { tuple, .. } => walk_value(tuple, config, visit),
        ValueExpr::ListGet { list, index } => {
            walk_value(list, config, visit);
            walk_value(index, config, visit);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn walk_nested_body<F>(
    body: &CompExpr,
    policy: NestedBodyPolicy,
    config: TailWalkConfig,
    visit: &mut F,
) where
    F: FnMut(&CompExpr, TailPosition),
{
    match policy {
        NestedBodyPolicy::Skip => {}
        NestedBodyPolicy::SeparateTailRoot => {
            walk_comp_inner(body, TailPosition::Tail, config, visit);
        }
        NestedBodyPolicy::NonTailRoot => {
            walk_comp_inner(body, TailPosition::NonTail, config, visit);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{CompExpr, IrCaseArm, IrCasePattern, IrHandlerClause};

    fn direct_call(name: &str) -> CompExpr {
        CompExpr::Call {
            callee: Box::new(ValueExpr::Var(name.to_string())),
            args: vec![],
        }
    }

    #[test]
    fn collects_direct_tail_calls_through_let_seq_if_and_case() {
        let comp = CompExpr::Let {
            name: "tmp".to_string(),
            ty: crate::ir::IrType::Unknown,
            value: Box::new(direct_call("prepare")),
            body: Box::new(CompExpr::Seq {
                stmts: vec![direct_call("side_effect")],
                tail: Box::new(CompExpr::If {
                    cond: Box::new(ValueExpr::BoolLit(true)),
                    then_: Box::new(direct_call("then_tail")),
                    else_: Box::new(CompExpr::Case {
                        scrutinee: Box::new(ValueExpr::IntLit(0)),
                        arms: vec![
                            IrCaseArm {
                                pattern: IrCasePattern::IntLit(0),
                                body: direct_call("zero_tail"),
                            },
                            IrCaseArm {
                                pattern: IrCasePattern::Wildcard,
                                body: direct_call("fallback_tail"),
                            },
                        ],
                    }),
                }),
            }),
        };

        let calls = collect_direct_tail_calls(&comp, TailWalkConfig::DECL_BODY);
        assert_eq!(
            calls,
            vec![
                DirectTailCall {
                    callee: DirectTailCallee::Var("then_tail".to_string()),
                    arity: 0,
                },
                DirectTailCall {
                    callee: DirectTailCallee::Var("zero_tail".to_string()),
                    arity: 0,
                },
                DirectTailCall {
                    callee: DirectTailCallee::Var("fallback_tail".to_string()),
                    arity: 0,
                },
            ]
        );
    }

    #[test]
    fn skips_nested_lambda_and_handler_clause_bodies_in_decl_mode() {
        let comp = CompExpr::Seq {
            stmts: vec![
                CompExpr::Value(ValueExpr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(direct_call("lambda_tail")),
                }),
                CompExpr::Handle {
                    clauses: vec![IrHandlerClause {
                        op_name: "tick".to_string(),
                        params: vec!["v".to_string()],
                        body: direct_call("handler_tail"),
                    }],
                },
            ],
            tail: Box::new(direct_call("decl_tail")),
        };

        let calls = collect_direct_tail_calls(&comp, TailWalkConfig::DECL_BODY);
        assert_eq!(
            calls,
            vec![DirectTailCall {
                callee: DirectTailCallee::Var("decl_tail".to_string()),
                arity: 0,
            }]
        );
    }

    #[test]
    fn treats_lambda_body_as_non_tail_for_handler_clause_mode() {
        let comp = CompExpr::Value(ValueExpr::Lambda {
            param: "x".to_string(),
            body: Box::new(direct_call("lambda_call")),
        });

        let mut seen = Vec::new();
        walk_comp(
            &comp,
            TailWalkConfig::HANDLER_CLAUSE_BODY,
            |node, tail_position| {
                if let CompExpr::Call { callee, .. } = node {
                    let ValueExpr::Var(name) = callee.as_ref() else {
                        return;
                    };
                    seen.push((name.clone(), tail_position));
                }
            },
        );

        assert_eq!(
            seen,
            vec![("lambda_call".to_string(), TailPosition::NonTail)]
        );
    }
}
