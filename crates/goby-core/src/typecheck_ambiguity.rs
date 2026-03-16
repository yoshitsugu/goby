use std::collections::HashSet;

use crate::ast::{Expr, InterpolatedPart, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_check::{
    check_expr, env_with_case_pattern_bindings, parse_tuple_member_index,
};
use crate::typecheck_env::{Ty, TypeEnv};
use crate::typecheck_render::ty_name;

pub(crate) fn ensure_no_ambiguous_refs_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) => Ok(()),
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    ensure_no_ambiguous_refs_in_expr(expr, env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::ListLit { elements, spread } => {
            for item in elements {
                ensure_no_ambiguous_refs_in_expr(item, env, decl_name)?;
            }
            if let Some(s) = spread {
                ensure_no_ambiguous_refs_in_expr(s, env, decl_name)?;
            }
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                ensure_no_ambiguous_refs_in_expr(item, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Var(name) => ensure_name_not_ambiguous(name, env, decl_name),
        Expr::Qualified { receiver, member } => {
            if let Some(index) = parse_tuple_member_index(member) {
                let receiver_ty = env.lookup(receiver);
                let resolved_receiver_ty = env.resolve_alias(&receiver_ty, 0);
                return match resolved_receiver_ty {
                    Ty::Tuple(items) => {
                        if index < items.len() {
                            Ok(())
                        } else {
                            Err(TypecheckError {
                                declaration: Some(decl_name.to_string()),
                                span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                                message: format!(
                                    "tuple member access index `{}` is out of range for receiver `{}` of type `{}`",
                                    index,
                                    receiver,
                                    ty_name(&Ty::Tuple(items))
                                ),
                            })
                        }
                    }
                    Ty::Unknown => {
                        if env.locals.contains_key(receiver) {
                            Ok(())
                        } else {
                            Err(TypecheckError {
                                declaration: Some(decl_name.to_string()),
                                span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                                message: format!(
                                    "tuple member access `{}` requires tuple receiver, but `{}` type is unresolved",
                                    member, receiver
                                ),
                            })
                        }
                    }
                    other => Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                        message: format!(
                            "tuple member access `{}` requires tuple receiver, but `{}` has type `{}`",
                            member,
                            receiver,
                            ty_name(&other)
                        ),
                    }),
                };
            }
            if env.locals.contains_key(receiver) {
                return Ok(());
            }
            ensure_name_not_ambiguous(&format!("{}.{}", receiver, member), env, decl_name)
        }
        Expr::RecordConstruct {
            constructor,
            fields,
        } => {
            ensure_name_not_ambiguous(constructor, env, decl_name)?;
            let Some(record) = env.lookup_record_by_constructor(constructor) else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                    message: format!("unknown record constructor `{}`", constructor),
                });
            };
            let mut seen = HashSet::new();
            for (_, value) in fields {
                ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?;
            }
            for (name, value) in fields {
                if !seen.insert(name.clone()) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                        message: format!(
                            "duplicate field `{}` in constructor call `{}`",
                            name, constructor
                        ),
                    });
                }
                let Some(expected_ty) = record.fields.get(name) else {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                        message: format!(
                            "unknown field `{}` in constructor call `{}`",
                            name, constructor
                        ),
                    });
                };
                let actual_ty = check_expr(value, env);
                if actual_ty != Ty::Unknown && !env.are_compatible(expected_ty, &actual_ty) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                        message: format!(
                            "field `{}` in constructor `{}` has type `{}` but expected `{}`",
                            name,
                            constructor,
                            ty_name(&actual_ty),
                            ty_name(expected_ty),
                        ),
                    });
                }
            }
            if seen.len() != record.fields.len() {
                let mut missing: Vec<String> = record
                    .fields
                    .keys()
                    .filter(|field| !seen.contains(*field))
                    .cloned()
                    .collect();
                missing.sort();
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                    message: format!(
                        "missing field(s) in constructor call `{}`: {}",
                        constructor,
                        missing.join(", ")
                    ),
                });
            }
            Ok(())
        }
        Expr::BinOp { left, right, .. } => {
            ensure_no_ambiguous_refs_in_expr(left, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(right, env, decl_name)
        }
        Expr::Call { callee, arg } => {
            ensure_no_ambiguous_refs_in_expr(callee, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(arg, env, decl_name)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            let qualified = format!("{}.{}", receiver, method);
            ensure_name_not_ambiguous(&qualified, env, decl_name)?;
            for arg in args {
                ensure_no_ambiguous_refs_in_expr(arg, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, callee } => {
            ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?;
            ensure_name_not_ambiguous(callee, env, decl_name)
        }
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            ensure_no_ambiguous_refs_in_expr(body, &child_env, decl_name)
        }
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = &clause.parsed_body {
                    ensure_no_ambiguous_refs_in_stmts(stmts, env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            ensure_no_ambiguous_refs_in_expr(handler, env, decl_name)?;
            ensure_no_ambiguous_refs_in_stmts(body, env, decl_name)
        }
        Expr::Resume { value } => ensure_no_ambiguous_refs_in_expr(value, env, decl_name),
        Expr::Block(stmts) => {
            if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                    message: "block expression must end with an expression".to_string(),
                });
            }
            let mut local_env = env.clone();
            for stmt in stmts {
                match stmt {
                    Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                        let ty = check_expr(value, &local_env);
                        local_env.locals.insert(name.clone(), ty);
                    }
                    Stmt::Assign { value, .. } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                    }
                    Stmt::Expr(expr) => {
                        ensure_no_ambiguous_refs_in_expr(expr, &local_env, decl_name)?;
                    }
                }
            }
            Ok(())
        }
        Expr::Case { scrutinee, arms } => {
            ensure_no_ambiguous_refs_in_expr(scrutinee, env, decl_name)?;
            let scrutinee_ty = check_expr(scrutinee, env);
            for arm in arms {
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                ensure_no_ambiguous_refs_in_expr(&arm.body, &arm_env, decl_name)?;
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            ensure_no_ambiguous_refs_in_expr(condition, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(then_expr, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(else_expr, env, decl_name)
        }
        Expr::ListIndex { list, index } => {
            ensure_no_ambiguous_refs_in_expr(list, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(index, env, decl_name)
        }
    }
}

pub(crate) fn ensure_no_ambiguous_refs_in_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    for stmt in stmts {
        match stmt {
            Stmt::Binding { value, .. }
            | Stmt::MutBinding { value, .. }
            | Stmt::Assign { value, .. } => {
                ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?
            }
            Stmt::Expr(expr) => ensure_no_ambiguous_refs_in_expr(expr, env, decl_name)?,
        }
    }
    Ok(())
}

fn ensure_name_not_ambiguous(
    name: &str,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    if env.locals.contains_key(name) {
        return Ok(());
    }
    if let Some(sources) = env.ambiguous_sources(name) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None, // no span available: requires Expr/Stmt span (D1a-iii)
            message: format!(
                "name `{}` is ambiguous due to name resolution collision: {}",
                name,
                sources.join(", ")
            ),
        });
    }
    Ok(())
}
