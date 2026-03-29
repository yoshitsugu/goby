use crate::ast::{Expr, InterpolatedPart, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_check::check_expr;
use crate::typecheck_diag::err_unknown_callable;
use crate::typecheck_env::{Ty, TypeEnv, TypeSubst};
use crate::typecheck_render::ty_name;
use crate::typecheck_span::{best_available_expr_span, best_available_name_use_span};
use crate::typecheck_unify::{
    apply_type_substitution, instantiate_ty_with_fresh_type_vars_for_call_site,
    match_function_argument_type, unify_types_with_subst,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum CallbackLambdaMismatch {
    Arity {
        required: Ty,
        provided_param_count: usize,
    },
    Result {
        required: Ty,
        provided: Ty,
    },
}

pub(crate) fn check_ordinary_call_arg_types_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    match expr {
        Expr::Call { callee, arg, .. } => {
            check_ordinary_call_arg_types_in_expr(callee, env, decl_name)?;
            check_ordinary_call_arg_types_in_expr(arg, env, decl_name)?;
            validate_call_chain(expr, env, decl_name)
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                check_ordinary_call_arg_types_in_expr(arg, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, .. } => {
            check_ordinary_call_arg_types_in_expr(value, env, decl_name)
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    check_ordinary_call_arg_types_in_expr(expr, env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::ListLit { elements, spread } => {
            for element in elements {
                check_ordinary_call_arg_types_in_expr(element, env, decl_name)?;
            }
            if let Some(spread) = spread {
                check_ordinary_call_arg_types_in_expr(spread, env, decl_name)?;
            }
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                check_ordinary_call_arg_types_in_expr(item, env, decl_name)?;
            }
            Ok(())
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                check_ordinary_call_arg_types_in_expr(value, env, decl_name)?;
            }
            Ok(())
        }
        Expr::UnaryOp { expr, .. } => check_ordinary_call_arg_types_in_expr(expr, env, decl_name),
        Expr::BinOp { left, right, .. } => {
            check_ordinary_call_arg_types_in_expr(left, env, decl_name)?;
            check_ordinary_call_arg_types_in_expr(right, env, decl_name)
        }
        Expr::Lambda { body, .. } => check_ordinary_call_arg_types_in_expr(body, env, decl_name),
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = &clause.parsed_body {
                    for stmt in stmts {
                        check_ordinary_call_arg_types_in_stmt(stmt, env, decl_name)?;
                    }
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            check_ordinary_call_arg_types_in_expr(handler, env, decl_name)?;
            for stmt in body {
                check_ordinary_call_arg_types_in_stmt(stmt, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Resume { value } => check_ordinary_call_arg_types_in_expr(value, env, decl_name),
        Expr::Block(stmts) => {
            for stmt in stmts {
                check_ordinary_call_arg_types_in_stmt(stmt, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Case { scrutinee, arms } => {
            check_ordinary_call_arg_types_in_expr(scrutinee, env, decl_name)?;
            for arm in arms {
                check_ordinary_call_arg_types_in_expr(&arm.body, env, decl_name)?;
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            check_ordinary_call_arg_types_in_expr(condition, env, decl_name)?;
            check_ordinary_call_arg_types_in_expr(then_expr, env, decl_name)?;
            check_ordinary_call_arg_types_in_expr(else_expr, env, decl_name)
        }
        Expr::ListIndex { list, index } => {
            check_ordinary_call_arg_types_in_expr(list, env, decl_name)?;
            check_ordinary_call_arg_types_in_expr(index, env, decl_name)
        }
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var { .. }
        | Expr::Qualified { .. } => Ok(()),
    }
}

pub(crate) fn infer_expr_binding_ty(expr: &Expr, env: &TypeEnv) -> Ty {
    let Some(_) = ordinary_call_target_and_args(expr) else {
        return check_expr(expr, env);
    };
    let mut next_id = 0;
    resolve_function_value_ty(expr, env, &mut next_id)
}

fn check_ordinary_call_arg_types_in_stmt(
    stmt: &Stmt,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    match stmt {
        Stmt::Binding { value, .. }
        | Stmt::MutBinding { value, .. }
        | Stmt::Assign { value, .. }
        | Stmt::Expr(value, _) => check_ordinary_call_arg_types_in_expr(value, env, decl_name),
    }
}

fn validate_call_chain(expr: &Expr, env: &TypeEnv, decl_name: &str) -> Result<(), TypecheckError> {
    let Some((target, args)) = ordinary_call_target_and_args(expr) else {
        return Ok(());
    };
    if resolved_callable_name(target)
        .as_deref()
        .is_some_and(|name| env.is_effect_op(name))
    {
        return Ok(());
    }
    let mut next_id = 0;
    let root_ty = resolve_function_value_ty(target, env, &mut next_id);
    let Ty::Fun { params, .. } =
        instantiate_ty_with_fresh_type_vars_for_call_site(&root_ty, &mut next_id)
    else {
        return Ok(());
    };

    let mut subst = TypeSubst::new();
    for (idx, arg) in args.iter().enumerate() {
        let Some(expected) = params.get(idx) else {
            return Ok(());
        };
        let expected_after_subst = apply_type_substitution(expected, &subst, env);
        if matches!(env.resolve_alias(&expected_after_subst, 0), Ty::Fun { .. }) {
            let actual_ty = match infer_callback_arg_ty(
                arg,
                &expected_after_subst,
                env,
                &mut subst,
                &mut next_id,
            ) {
                Ok(actual_ty) => actual_ty,
                Err(CallbackLambdaMismatch::Arity {
                    required,
                    provided_param_count,
                }) => {
                    let plural = if provided_param_count == 1 { "" } else { "s" };
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: best_available_expr_span(arg),
                        message: format!(
                            "higher-order callback arity mismatch: required `{}` but lambda provides {} parameter{}",
                            ty_name(&required),
                            provided_param_count,
                            plural,
                        ),
                    });
                }
                Err(CallbackLambdaMismatch::Result { required, provided }) => {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: best_available_expr_span(arg),
                        message: format!(
                            "higher-order callback result type mismatch: required `{}` but lambda provides `{}`",
                            ty_name(&required),
                            ty_name(&provided)
                        ),
                    });
                }
            };
            if actual_ty == Ty::Unknown {
                if let Some(name) = unresolved_callable_name(arg, env) {
                    return Err(err_unknown_callable(
                        decl_name,
                        &name,
                        best_available_name_use_span(arg),
                    ));
                }
                return Ok(());
            }
            if matches!(arg, Expr::Lambda { .. }) {
                continue;
            }
            if let Err(mismatch) = match_function_argument_type(
                &expected_after_subst,
                &actual_ty,
                &mut subst,
                env,
                &mut next_id,
            ) {
                let resolved_name = resolved_callable_name(arg);
                let found = match resolved_name {
                    Some(name) => format!("{name} : {}", ty_name(&mismatch.actual)),
                    None => ty_name(&mismatch.actual),
                };
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_expr_span(arg),
                    message: format!(
                        "higher-order argument type mismatch: required `{}` but found `{}`",
                        ty_name(&mismatch.required),
                        found
                    ),
                });
            }
            continue;
        }

        let actual_ty = resolve_function_value_ty(arg, env, &mut next_id);
        if actual_ty == Ty::Unknown {
            continue;
        }
        if !unify_types_with_subst(&expected_after_subst, &actual_ty, &mut subst, env) {
            let target_name = resolved_callable_name(target);
            let call_target = target_name.as_deref().unwrap_or("function");
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: best_available_expr_span(arg),
                message: format!(
                    "`{}` expects argument of type `{}` but got `{}`",
                    call_target,
                    ty_name(&expected_after_subst),
                    ty_name(&actual_ty)
                ),
            });
        }
    }
    Ok(())
}

fn infer_callback_arg_ty(
    expr: &Expr,
    expected: &Ty,
    env: &TypeEnv,
    subst: &mut TypeSubst,
    next_id: &mut usize,
) -> Result<Ty, CallbackLambdaMismatch> {
    match expr {
        Expr::Lambda { .. } => {
            infer_lambda_ty_against_expected(expr, expected, env, subst, next_id)
        }
        _ => Ok(resolve_function_value_ty(expr, env, next_id)),
    }
}

fn infer_lambda_ty_against_expected(
    expr: &Expr,
    expected: &Ty,
    env: &TypeEnv,
    subst: &mut TypeSubst,
    next_id: &mut usize,
) -> Result<Ty, CallbackLambdaMismatch> {
    let Expr::Lambda { param, body } = expr else {
        return Ok(resolve_function_value_ty(expr, env, next_id));
    };

    let required = apply_type_substitution(expected, subst, env);
    let Ty::Fun { params, result } = env.resolve_alias(&required, 0) else {
        return Err(CallbackLambdaMismatch::Arity {
            required,
            provided_param_count: lambda_param_count(expr),
        });
    };

    let Some(expected_param_ty) = params.first() else {
        return Err(CallbackLambdaMismatch::Arity {
            required,
            provided_param_count: lambda_param_count(expr),
        });
    };
    let expected_param_ty = apply_type_substitution(expected_param_ty, subst, env);
    let expected_rest_ty = if params.len() == 1 {
        apply_type_substitution(&result, subst, env)
    } else {
        Ty::Fun {
            params: params[1..]
                .iter()
                .map(|param| apply_type_substitution(param, subst, env))
                .collect(),
            result: Box::new(apply_type_substitution(&result, subst, env)),
        }
    };

    let child_env = env.with_local(param, expected_param_ty.clone());
    let body_ty = match body.as_ref() {
        Expr::Lambda { .. } => {
            let nested_ty = match infer_lambda_ty_against_expected(
                body,
                &expected_rest_ty,
                &child_env,
                subst,
                next_id,
            ) {
                Ok(nested_ty) => nested_ty,
                Err(CallbackLambdaMismatch::Arity { .. }) => {
                    return Err(CallbackLambdaMismatch::Arity {
                        required,
                        provided_param_count: lambda_param_count(expr),
                    });
                }
                Err(CallbackLambdaMismatch::Result { provided, .. }) => {
                    return Err(CallbackLambdaMismatch::Result {
                        required,
                        provided: Ty::Fun {
                            params: vec![expected_param_ty.clone()],
                            result: Box::new(provided),
                        },
                    });
                }
            };
            if !matches!(env.resolve_alias(&expected_rest_ty, 0), Ty::Fun { .. }) {
                return Err(CallbackLambdaMismatch::Arity {
                    required,
                    provided_param_count: lambda_param_count(expr),
                });
            }
            nested_ty
        }
        _ => {
            let inferred = check_expr(body, &child_env);
            if matches!(
                child_env.resolve_alias(&expected_rest_ty, 0),
                Ty::Fun { .. }
            ) {
                return Err(CallbackLambdaMismatch::Arity {
                    required,
                    provided_param_count: lambda_param_count(expr),
                });
            }
            inferred
        }
    };

    let provided = Ty::Fun {
        params: vec![expected_param_ty],
        result: Box::new(body_ty.clone()),
    };

    if !unify_types_with_subst(&expected_rest_ty, &body_ty, subst, &child_env) {
        return Err(CallbackLambdaMismatch::Result { required, provided });
    }

    Ok(apply_type_substitution(&provided, subst, &child_env))
}

fn lambda_param_count(expr: &Expr) -> usize {
    match expr {
        Expr::Lambda { body, .. } => 1 + lambda_param_count(body),
        _ => 0,
    }
}

fn ordinary_call_target_and_args(expr: &Expr) -> Option<(&Expr, Vec<&Expr>)> {
    match expr {
        Expr::Call { callee, arg, .. } => {
            let (target, mut args) = ordinary_call_target_and_args(callee)?;
            args.push(arg.as_ref());
            Some((target, args))
        }
        Expr::Var { .. } | Expr::Qualified { .. } | Expr::Lambda { .. } => Some((expr, Vec::new())),
        _ => None,
    }
}

fn resolve_function_value_ty(expr: &Expr, env: &TypeEnv, next_id: &mut usize) -> Ty {
    let Some((target, args)) = ordinary_call_target_and_args(expr) else {
        return check_expr(expr, env);
    };
    let target_ty = check_expr(target, env);
    let Ty::Fun { params, result } =
        instantiate_ty_with_fresh_type_vars_for_call_site(&target_ty, next_id)
    else {
        return target_ty;
    };

    let mut subst = TypeSubst::new();
    for (idx, arg) in args.iter().enumerate() {
        let Some(expected) = params.get(idx) else {
            return Ty::Unknown;
        };
        let expected_after_subst = apply_type_substitution(expected, &subst, env);
        let actual_ty = resolve_function_value_ty(arg, env, next_id);
        if actual_ty == Ty::Unknown {
            return Ty::Unknown;
        }
        if matches!(env.resolve_alias(&expected_after_subst, 0), Ty::Fun { .. }) {
            if match_function_argument_type(
                &expected_after_subst,
                &actual_ty,
                &mut subst,
                env,
                next_id,
            )
            .is_err()
            {
                return Ty::Unknown;
            }
        } else if !unify_types_with_subst(&expected_after_subst, &actual_ty, &mut subst, env) {
            return Ty::Unknown;
        }
    }

    if args.len() < params.len() {
        let remaining = params[args.len()..]
            .iter()
            .map(|param| apply_type_substitution(param, &subst, env))
            .collect();
        let result = apply_type_substitution(&result, &subst, env);
        Ty::Fun {
            params: remaining,
            result: Box::new(result),
        }
    } else {
        apply_type_substitution(&result, &subst, env)
    }
}

fn resolved_callable_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Var { name, .. } => Some(name.clone()),
        Expr::Qualified {
            receiver, member, ..
        } => Some(format!("{receiver}.{member}")),
        _ => None,
    }
}

fn unresolved_callable_name(expr: &Expr, env: &TypeEnv) -> Option<String> {
    match expr {
        Expr::Var { name, .. } if env.lookup(name) == Ty::Unknown => Some(name.clone()),
        Expr::Qualified {
            receiver, member, ..
        } if env.lookup(&format!("{receiver}.{member}")) == Ty::Unknown => {
            Some(format!("{receiver}.{member}"))
        }
        _ => None,
    }
}
