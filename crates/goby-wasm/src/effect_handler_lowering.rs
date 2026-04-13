use std::collections::HashMap;
use std::rc::Rc;

use goby_core::ir::{CompExpr, IrHandlerClause, IrInterpPart, IrType, ValueExpr};

use crate::CodegenError;

type Build = Rc<dyn Fn(ValueExpr, &mut NameSupply) -> Result<CompExpr, CodegenError>>;

#[derive(Clone)]
struct HandlerScope {
    clauses: Vec<IrHandlerClause>,
    exit_k: Build,
}

#[derive(Default)]
struct NameSupply {
    next_id: usize,
}

impl NameSupply {
    fn fresh(&mut self, prefix: &str) -> String {
        let name = format!("__goby_wb3_{prefix}_{}", self.next_id);
        self.next_id += 1;
        name
    }
}

pub(crate) fn lower_safe_handlers_in_comp(comp: &CompExpr) -> Result<CompExpr, CodegenError> {
    let mut names = NameSupply::default();
    rewrite_comp(comp, &HashMap::new(), &[], identity_k(), None, &mut names)
}

fn rewrite_comp(
    comp: &CompExpr,
    handler_aliases: &HashMap<String, Vec<IrHandlerClause>>,
    handler_scopes: &[HandlerScope],
    k: Build,
    resume_k: Option<Build>,
    names: &mut NameSupply,
) -> Result<CompExpr, CodegenError> {
    match comp {
        CompExpr::Value(value) => k(rewrite_value(value, names)?, names),
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => {
            if let Some(clauses) = resolve_handler_clauses(value, handler_aliases) {
                let mut body_aliases = handler_aliases.clone();
                body_aliases.insert(name.clone(), clauses);
                return rewrite_comp(body, &body_aliases, handler_scopes, k, resume_k, names);
            }
            let name = name.clone();
            let ty = ty.clone();
            let body = body.clone();
            let aliases = handler_aliases.clone();
            let scopes = handler_scopes.to_vec();
            let k_outer = k.clone();
            let resume_outer = resume_k.clone();
            let value_k: Build = Rc::new(move |result, names| {
                let mut body_aliases = aliases.clone();
                if let Some(clauses) = resolve_handler_clauses_from_value_expr(&result, &aliases) {
                    body_aliases.insert(name.clone(), clauses);
                } else {
                    body_aliases.remove(&name);
                }
                Ok(CompExpr::Let {
                    name: name.clone(),
                    ty: ty.clone(),
                    value: Box::new(CompExpr::Value(result)),
                    body: Box::new(rewrite_comp(
                        &body,
                        &body_aliases,
                        &scopes,
                        k_outer.clone(),
                        resume_outer.clone(),
                        names,
                    )?),
                })
            });
            rewrite_comp(
                value,
                handler_aliases,
                handler_scopes,
                value_k,
                resume_k,
                names,
            )
        }
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => {
            if let Some(clauses) = resolve_handler_clauses(value, handler_aliases) {
                let mut body_aliases = handler_aliases.clone();
                body_aliases.insert(name.clone(), clauses);
                return rewrite_comp(body, &body_aliases, handler_scopes, k, resume_k, names);
            }
            let name = name.clone();
            let ty = ty.clone();
            let body = body.clone();
            let aliases = handler_aliases.clone();
            let scopes = handler_scopes.to_vec();
            let k_outer = k.clone();
            let resume_outer = resume_k.clone();
            let value_k: Build = Rc::new(move |result, names| {
                Ok(CompExpr::LetMut {
                    name: name.clone(),
                    ty: ty.clone(),
                    value: Box::new(CompExpr::Value(result)),
                    body: Box::new(rewrite_comp(
                        &body,
                        &aliases,
                        &scopes,
                        k_outer.clone(),
                        resume_outer.clone(),
                        names,
                    )?),
                })
            });
            rewrite_comp(
                value,
                handler_aliases,
                handler_scopes,
                value_k,
                resume_k,
                names,
            )
        }
        CompExpr::Seq { stmts, tail } => rewrite_seq(
            stmts,
            tail,
            handler_aliases,
            handler_scopes,
            k,
            resume_k,
            names,
        ),
        CompExpr::If { cond, then_, else_ } => Ok(CompExpr::If {
            cond: Box::new(rewrite_value(cond, names)?),
            then_: Box::new(rewrite_comp(
                then_,
                handler_aliases,
                handler_scopes,
                k.clone(),
                resume_k.clone(),
                names,
            )?),
            else_: Box::new(rewrite_comp(
                else_,
                handler_aliases,
                handler_scopes,
                k,
                resume_k,
                names,
            )?),
        }),
        CompExpr::Call { callee, args } => {
            if let ValueExpr::Var(op_name) = callee.as_ref()
                && let Some(rewritten) = rewrite_handled_op_invocation(
                    None,
                    op_name,
                    args,
                    handler_aliases,
                    handler_scopes,
                    k.clone(),
                    names,
                )?
            {
                return Ok(rewritten);
            }
            continue_with(
                CompExpr::Call {
                    callee: Box::new(rewrite_value(callee, names)?),
                    args: rewrite_values(args, names)?,
                },
                k,
                names,
            )
        }
        CompExpr::Assign { name, value } => {
            let name = name.clone();
            let k_outer = k.clone();
            let assign_k: Build = Rc::new(move |result, names| {
                continue_with(
                    CompExpr::Assign {
                        name: name.clone(),
                        value: Box::new(CompExpr::Value(result)),
                    },
                    k_outer.clone(),
                    names,
                )
            });
            rewrite_comp(
                value,
                handler_aliases,
                handler_scopes,
                assign_k,
                resume_k,
                names,
            )
        }
        CompExpr::Case { scrutinee, arms } => Ok(CompExpr::Case {
            scrutinee: Box::new(rewrite_value(scrutinee, names)?),
            arms: arms
                .iter()
                .map(|arm| {
                    Ok(goby_core::ir::IrCaseArm {
                        pattern: arm.pattern.clone(),
                        body: rewrite_comp(
                            &arm.body,
                            handler_aliases,
                            handler_scopes,
                            k.clone(),
                            resume_k.clone(),
                            names,
                        )?,
                    })
                })
                .collect::<Result<Vec<_>, CodegenError>>()?,
        }),
        CompExpr::PerformEffect { effect, op, args } => {
            if let Some(rewritten) = rewrite_handled_op_invocation(
                Some(effect.as_str()),
                op,
                args,
                handler_aliases,
                handler_scopes,
                k.clone(),
                names,
            )? {
                Ok(rewritten)
            } else {
                continue_with(
                    CompExpr::PerformEffect {
                        effect: effect.clone(),
                        op: op.clone(),
                        args: rewrite_values(args, names)?,
                    },
                    k,
                    names,
                )
            }
        }
        CompExpr::Handle { .. } => Err(CodegenError {
            message: "handler lowering expected `Handle` to be consumed by `WithHandler`"
                .to_string(),
        }),
        CompExpr::WithHandler { handler, body } => {
            let clauses =
                resolve_handler_clauses(handler, handler_aliases).ok_or_else(|| CodegenError {
                    message: "handler lowering requires statically resolvable `WithHandler`"
                        .to_string(),
                })?;
            let mut scopes = handler_scopes.to_vec();
            scopes.push(HandlerScope {
                clauses,
                exit_k: k.clone(),
            });
            rewrite_comp(body, handler_aliases, &scopes, k, None, names)
        }
        CompExpr::Resume { value } => {
            let Some(resume_k) = resume_k else {
                return Err(CodegenError {
                    message: "handler lowering encountered `resume` without continuation"
                        .to_string(),
                });
            };
            resume_k(rewrite_value(value, names)?, names)
        }
        CompExpr::AssignIndex { .. } => Err(CodegenError {
            message: "effect handler lowering: AssignIndex not yet implemented (LM3c)".to_string(),
        }),
    }
}

fn rewrite_seq(
    stmts: &[CompExpr],
    tail: &CompExpr,
    handler_aliases: &HashMap<String, Vec<IrHandlerClause>>,
    handler_scopes: &[HandlerScope],
    k: Build,
    resume_k: Option<Build>,
    names: &mut NameSupply,
) -> Result<CompExpr, CodegenError> {
    if let Some((first, rest)) = stmts.split_first() {
        let rest_vec = rest.to_vec();
        let tail = tail.clone();
        let aliases = handler_aliases.clone();
        let scopes = handler_scopes.to_vec();
        let k_outer = k.clone();
        let resume_outer = resume_k.clone();
        let stmt_k: Build = Rc::new(move |_ignored, names| {
            rewrite_seq(
                &rest_vec,
                &tail,
                &aliases,
                &scopes,
                k_outer.clone(),
                resume_outer.clone(),
                names,
            )
        });
        rewrite_comp(
            first,
            handler_aliases,
            handler_scopes,
            stmt_k,
            resume_k,
            names,
        )
    } else {
        rewrite_comp(tail, handler_aliases, handler_scopes, k, resume_k, names)
    }
}

fn continue_with(
    comp: CompExpr,
    k: Build,
    names: &mut NameSupply,
) -> Result<CompExpr, CodegenError> {
    match comp {
        CompExpr::Value(value) => k(value, names),
        other => {
            let temp = names.fresh("resume");
            let body = k(ValueExpr::Var(temp.clone()), names)?;
            Ok(CompExpr::Let {
                name: temp,
                ty: IrType::Unknown,
                value: Box::new(other),
                body: Box::new(body),
            })
        }
    }
}

fn rewrite_values(
    values: &[ValueExpr],
    names: &mut NameSupply,
) -> Result<Vec<ValueExpr>, CodegenError> {
    values
        .iter()
        .map(|value| rewrite_value(value, names))
        .collect()
}

fn rewrite_value(value: &ValueExpr, names: &mut NameSupply) -> Result<ValueExpr, CodegenError> {
    match value {
        ValueExpr::ListLit { elements, spread } => Ok(ValueExpr::ListLit {
            elements: rewrite_values(elements, names)?,
            spread: match spread {
                Some(spread) => Some(Box::new(rewrite_value(spread, names)?)),
                None => None,
            },
        }),
        ValueExpr::TupleLit(items) => Ok(ValueExpr::TupleLit(rewrite_values(items, names)?)),
        ValueExpr::RecordLit {
            constructor,
            fields,
        } => Ok(ValueExpr::RecordLit {
            constructor: constructor.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| Ok((name.clone(), rewrite_value(value, names)?)))
                .collect::<Result<Vec<_>, CodegenError>>()?,
        }),
        ValueExpr::Interp(parts) => Ok(ValueExpr::Interp(
            parts
                .iter()
                .map(|part| match part {
                    IrInterpPart::Text(text) => Ok(IrInterpPart::Text(text.clone())),
                    IrInterpPart::Expr(expr) => Ok(IrInterpPart::Expr(rewrite_value(expr, names)?)),
                })
                .collect::<Result<Vec<_>, CodegenError>>()?,
        )),
        ValueExpr::BinOp { op, left, right } => Ok(ValueExpr::BinOp {
            op: op.clone(),
            left: Box::new(rewrite_value(left, names)?),
            right: Box::new(rewrite_value(right, names)?),
        }),
        ValueExpr::Lambda { .. } => Err(CodegenError {
            message: "handler lowering does not yet support lambdas inside handler-lowered code"
                .to_string(),
        }),
        ValueExpr::TupleProject { tuple, index } => Ok(ValueExpr::TupleProject {
            tuple: Box::new(rewrite_value(tuple, names)?),
            index: *index,
        }),
        ValueExpr::ListGet { list, index } => Ok(ValueExpr::ListGet {
            list: Box::new(rewrite_value(list, names)?),
            index: Box::new(rewrite_value(index, names)?),
        }),
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => Ok(value.clone()),
    }
}

fn identity_k() -> Build {
    Rc::new(|value, _names| Ok(CompExpr::Value(value)))
}

// M7-2 ownership note:
// this rewrite owns "yielding" at IR/lowering time by mapping a handled
// operation invocation onto the active handler clause body and continuation
// bridge (`k`/`resume_k`). It does not own collection stepping order itself;
// order comes from the caller-side traversal shape (e.g. list fold / producer recursion).
fn rewrite_handled_op_invocation(
    effect_name: Option<&str>,
    op_name: &str,
    args: &[ValueExpr],
    handler_aliases: &HashMap<String, Vec<IrHandlerClause>>,
    handler_scopes: &[HandlerScope],
    k: Build,
    names: &mut NameSupply,
) -> Result<Option<CompExpr>, CodegenError> {
    let Some((scope, clause)) = find_matching_handler_clause(handler_scopes, op_name) else {
        return Ok(None);
    };

    let mut rewritten = rewrite_comp(
        &clause.body,
        handler_aliases,
        handler_scopes,
        scope.exit_k.clone(),
        Some(k),
        names,
    )?;
    if clause.params.len() != args.len() {
        let call_head = match effect_name {
            Some(effect) => format!("{effect}.{op_name}"),
            None => op_name.to_string(),
        };
        return Err(CodegenError {
            message: format!(
                "handler lowering arity mismatch for '{}': expected {}, got {}",
                call_head,
                clause.params.len(),
                args.len()
            ),
        });
    }
    for (param, arg) in clause.params.iter().zip(rewrite_values(args, names)?).rev() {
        rewritten = CompExpr::Let {
            name: param.clone(),
            ty: IrType::Unknown,
            value: Box::new(CompExpr::Value(arg)),
            body: Box::new(rewritten),
        };
    }
    Ok(Some(rewritten))
}

fn find_matching_handler_clause<'a>(
    scopes: &'a [HandlerScope],
    op_name: &str,
) -> Option<(&'a HandlerScope, &'a IrHandlerClause)> {
    scopes.iter().rev().find_map(|scope| {
        scope
            .clauses
            .iter()
            .find(|clause| {
                clause.op_name == op_name
                    || clause
                        .op_name
                        .rsplit('.')
                        .next()
                        .is_some_and(|short_name| short_name == op_name)
            })
            .map(|clause| (scope, clause))
    })
}

fn resolve_handler_clauses(
    comp: &CompExpr,
    aliases: &HashMap<String, Vec<IrHandlerClause>>,
) -> Option<Vec<IrHandlerClause>> {
    match comp {
        CompExpr::Handle { clauses } => Some(clauses.clone()),
        CompExpr::Value(ValueExpr::Var(name)) => aliases.get(name).cloned(),
        _ => None,
    }
}

fn resolve_handler_clauses_from_value_expr(
    value: &ValueExpr,
    aliases: &HashMap<String, Vec<IrHandlerClause>>,
) -> Option<Vec<IrHandlerClause>> {
    match value {
        ValueExpr::Var(name) => aliases.get(name).cloned(),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lowers_simple_with_handler_sequence_to_handler_free_ir() {
        let original = CompExpr::WithHandler {
            handler: Box::new(CompExpr::Handle {
                clauses: vec![IrHandlerClause {
                    op_name: "yield".to_string(),
                    params: vec!["value".to_string(), "step".to_string()],
                    body: CompExpr::Seq {
                        stmts: vec![CompExpr::PerformEffect {
                            effect: "Print".to_string(),
                            op: "print".to_string(),
                            args: vec![ValueExpr::Var("value".to_string())],
                        }],
                        tail: Box::new(CompExpr::Resume {
                            value: Box::new(ValueExpr::TupleLit(vec![
                                ValueExpr::BoolLit(true),
                                ValueExpr::Var("step".to_string()),
                            ])),
                        }),
                    },
                }],
            }),
            body: Box::new(CompExpr::Seq {
                stmts: vec![CompExpr::PerformEffect {
                    effect: "Iterator".to_string(),
                    op: "yield".to_string(),
                    args: vec![ValueExpr::StrLit("a".to_string()), ValueExpr::Unit],
                }],
                tail: Box::new(CompExpr::Value(ValueExpr::Unit)),
            }),
        };
        let lowered = lower_safe_handlers_in_comp(&original).expect("handler lowering should work");
        let rendered = goby_core::ir::fmt_ir(&goby_core::ir::IrModule {
            decls: vec![goby_core::ir::IrDecl {
                name: "main".to_string(),
                params: Vec::new(),
                result_ty: IrType::Unit,
                residual_effects: Vec::new(),
                body: lowered.clone(),
            }],
        });
        assert!(
            !rendered.contains("WithHandler") && !rendered.contains("Resume"),
            "lowered IR should be handler-free: {rendered}"
        );
    }
}
