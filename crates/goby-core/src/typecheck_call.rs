use crate::ast::{Expr, InterpolatedPart, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_check::check_expr;
use crate::typecheck_env::{Ty, TypeEnv, TypeSubst};
use crate::typecheck_render::ty_name;
use crate::typecheck_unify::{
    apply_type_substitution, instantiate_ty_with_fresh_type_vars_for_call_site,
    match_function_argument_type, unify_types_with_subst,
};

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
        let actual_ty = resolve_function_value_ty(arg, env, &mut next_id);
        if matches!(env.resolve_alias(&expected_after_subst, 0), Ty::Fun { .. }) {
            if actual_ty == Ty::Unknown {
                if let Some(name) = unresolved_callable_name(arg, env) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!("unknown function or constructor `{}`", name),
                    });
                }
                return Ok(());
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
                    span: None,
                    message: format!(
                        "higher-order argument type mismatch: required `{}` but found `{}`",
                        ty_name(&mismatch.required),
                        found
                    ),
                });
            }
            continue;
        }

        if actual_ty == Ty::Unknown {
            continue;
        }
        if !unify_types_with_subst(&expected_after_subst, &actual_ty, &mut subst, env) {
            return Ok(());
        }
    }
    Ok(())
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
