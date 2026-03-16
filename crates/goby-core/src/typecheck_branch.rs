use crate::ast::{Expr, InterpolatedPart, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_check::{
    branch_types_compatible, check_expr, env_with_case_pattern_bindings, merge_branch_type,
};
use crate::typecheck_env::{Ty, TypeEnv};
use crate::typecheck_render::ty_name;

pub(crate) fn check_branch_type_consistency_in_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    let mut local_env = env.clone();
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
            }
            Stmt::Assign { value, .. } => {
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
            }
            Stmt::Expr(expr) => {
                check_branch_type_consistency_in_expr(expr, &local_env, decl_name)?;
            }
        }
    }
    Ok(())
}

pub(crate) fn check_branch_type_consistency_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    macro_rules! recurse {
        ($e:expr) => {
            check_branch_type_consistency_in_expr($e, env, decl_name)
        };
        ($e:expr, $child_env:expr) => {
            check_branch_type_consistency_in_expr($e, $child_env, decl_name)
        };
    }

    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) | Expr::Var(_) => Ok(()),
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    recurse!(expr)?;
                }
            }
            Ok(())
        }
        Expr::ListLit { elements, spread } => {
            for item in elements {
                recurse!(item)?;
            }
            if let Some(s) = spread {
                recurse!(s)?;
            }
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                recurse!(item)?;
            }
            Ok(())
        }
        Expr::Qualified { .. } => Ok(()),
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                recurse!(value)?;
            }
            Ok(())
        }
        Expr::BinOp { left, right, .. } => {
            recurse!(left)?;
            recurse!(right)
        }
        Expr::Call { callee, arg } => {
            recurse!(callee)?;
            recurse!(arg)
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                recurse!(arg)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, .. } => recurse!(value),
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            recurse!(body, &child_env)
        }
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = &clause.parsed_body {
                    let mut child_env = TypeEnv {
                        globals: env.globals.clone(),
                        locals: env.locals.clone(),
                        type_aliases: env.type_aliases.clone(),
                        record_types: env.record_types.clone(),
                    };
                    if let Some(first) = clause.params.first() {
                        child_env.locals.insert(first.clone(), Ty::Unknown);
                    }
                    check_branch_type_consistency_in_stmts(stmts, &child_env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            recurse!(handler)?;
            let child_env = TypeEnv {
                globals: env.globals.clone(),
                locals: env.locals.clone(),
                type_aliases: env.type_aliases.clone(),
                record_types: env.record_types.clone(),
            };
            check_branch_type_consistency_in_stmts(body, &child_env, decl_name)
        }
        Expr::Resume { value } => recurse!(value),
        Expr::Block(stmts) => check_branch_type_consistency_in_stmts(stmts, env, decl_name),
        Expr::Case { scrutinee, arms } => {
            recurse!(scrutinee)?;
            let scrutinee_ty = check_expr(scrutinee, env);
            let mut merged: Option<Ty> = None;
            for arm in arms {
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                recurse!(&arm.body, &arm_env)?;
                let arm_ty = check_expr(&arm.body, &arm_env);
                if let Some(prev) = &merged
                    && *prev != Ty::Unknown
                    && arm_ty != Ty::Unknown
                    && !branch_types_compatible(env, prev, &arm_ty)
                {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                        message: format!(
                            "case branch type mismatch: `{}` vs `{}`",
                            ty_name(prev),
                            ty_name(&arm_ty)
                        ),
                    });
                }
                merged = Some(match merged {
                    Some(prev) => merge_branch_type(env, prev, arm_ty),
                    None => arm_ty,
                });
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            recurse!(condition)?;
            recurse!(then_expr)?;
            recurse!(else_expr)?;
            let then_ty = check_expr(then_expr, env);
            let else_ty = check_expr(else_expr, env);
            if then_ty != Ty::Unknown
                && else_ty != Ty::Unknown
                && !branch_types_compatible(env, &then_ty, &else_ty)
            {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                    message: format!(
                        "if branch type mismatch: then is `{}`, else is `{}`",
                        ty_name(&then_ty),
                        ty_name(&else_ty)
                    ),
                });
            }
            Ok(())
        }
        Expr::ListIndex { list, index } => {
            recurse!(list)?;
            recurse!(index)
        }
    }
}
