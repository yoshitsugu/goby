use crate::ast::{Expr, InterpolatedPart, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_check::{
    check_expr, check_list_index_constraints, check_list_spread_constraints,
    env_with_case_pattern_bindings, is_list_case_pattern,
};
use crate::typecheck_env::{ResumeContext, Ty, TypeEnv, TypeSubst};
use crate::typecheck_render::ty_name;
use crate::typecheck_span::best_available_expr_span;
use crate::typecheck_unify::{
    apply_type_substitution, instantiate_handler_clause_signature, ty_contains_type_var,
    type_hole_conflict_note, unify_types_with_subst,
};

pub(crate) fn check_resume_in_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    decl_name: &str,
    param_tys: &[(&str, Ty)],
    resume_ctx: Option<&ResumeContext>,
) -> Result<(), TypecheckError> {
    let mut local_env = TypeEnv {
        globals: env.globals.clone(),
        locals: param_tys
            .iter()
            .map(|(name, ty)| (name.to_string(), ty.clone()))
            .collect(),
        type_aliases: env.type_aliases.clone(),
        record_types: env.record_types.clone(),
    };
    check_resume_in_stmts_with_local_env(stmts, &mut local_env, decl_name, resume_ctx)
}

pub(crate) fn check_resume_in_stmts_with_local_env(
    stmts: &[Stmt],
    local_env: &mut TypeEnv,
    decl_name: &str,
    resume_ctx: Option<&ResumeContext>,
) -> Result<(), TypecheckError> {
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value, .. } | Stmt::MutBinding { name, value, .. } => {
                check_resume_in_expr(value, local_env, decl_name, resume_ctx)?;
                let ty = infer_binding_ty_with_resume_context(value, local_env, resume_ctx);
                local_env.locals.insert(name.clone(), ty);
            }
            Stmt::Assign { value, .. } => {
                check_resume_in_expr(value, local_env, decl_name, resume_ctx)?;
            }
            Stmt::Expr(expr, _) => {
                check_resume_in_expr(expr, local_env, decl_name, resume_ctx)?;
            }
        }
    }
    Ok(())
}

pub(crate) fn infer_binding_ty_with_resume_context(
    value: &Expr,
    env: &TypeEnv,
    resume_ctx: Option<&ResumeContext>,
) -> Ty {
    if let Expr::Resume { .. } = value
        && let Some(ctx) = resume_ctx
        && let Some(expected) = ctx.expected_arg_ty.as_ref()
        && !ty_contains_type_var(expected)
    {
        return expected.clone();
    }
    check_expr(value, env)
}

fn check_resume_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
    resume_ctx: Option<&ResumeContext>,
) -> Result<(), TypecheckError> {
    if let Expr::Var { name, .. } = expr
        && name == "Unit"
    {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: best_available_expr_span(expr),
            message: "legacy_unit_value_syntax: `Unit` is no longer a value expression; use `()`"
                .to_string(),
        });
    }

    macro_rules! recurse {
        ($e:expr) => {
            check_resume_in_expr($e, env, decl_name, resume_ctx)
        };
        ($e:expr, $child_env:expr) => {
            check_resume_in_expr($e, $child_env, decl_name, resume_ctx)
        };
    }

    match expr {
        Expr::Spanned { expr, .. } => recurse!(expr),
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) | Expr::Var { .. } => Ok(()),
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
            check_list_spread_constraints(elements, spread.as_deref(), env, decl_name)?;
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
        Expr::UnaryOp { expr, .. } => recurse!(expr),
        Expr::BinOp { left, right, .. } => {
            recurse!(left)?;
            recurse!(right)
        }
        Expr::Call { callee, arg, .. } => {
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
            let mut fresh_type_counter = 0usize;
            for clause in clauses {
                let instantiated = instantiate_handler_clause_signature(
                    env,
                    &clause.name,
                    &mut fresh_type_counter,
                );
                let mut child_env = TypeEnv {
                    globals: env.globals.clone(),
                    locals: env.locals.clone(),
                    type_aliases: env.type_aliases.clone(),
                    record_types: env.record_types.clone(),
                };
                let expected_arg_ty = instantiated.as_ref().map(|(_, result)| result.clone());
                if let Some((param_tys, _)) = instantiated.as_ref() {
                    for (idx, param_name) in clause.params.iter().enumerate() {
                        let ty = param_tys.get(idx).cloned().unwrap_or(Ty::Unknown);
                        child_env.locals.insert(param_name.clone(), ty);
                    }
                } else if let Some(first) = clause.params.first() {
                    child_env.locals.insert(first.clone(), Ty::Unknown);
                }
                if expected_arg_ty.is_none() {
                    continue;
                }
                if let Some(stmts) = &clause.parsed_body {
                    let ctx = ResumeContext { expected_arg_ty };
                    check_resume_in_stmts_with_local_env(
                        stmts,
                        &mut child_env,
                        decl_name,
                        Some(&ctx),
                    )?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            recurse!(handler)?;
            let mut child_env = TypeEnv {
                globals: env.globals.clone(),
                locals: env.locals.clone(),
                type_aliases: env.type_aliases.clone(),
                record_types: env.record_types.clone(),
            };
            check_resume_in_stmts_with_local_env(body, &mut child_env, decl_name, resume_ctx)
        }
        Expr::Resume { value } => {
            recurse!(value)?;
            let Some(ctx) = resume_ctx else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_expr_span(expr),
                    message: "resume_outside_handler: `resume` can only be used inside handler method bodies".to_string(),
                });
            };
            let Some(expected) = ctx.expected_arg_ty.as_ref() else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_expr_span(expr),
                    message: "resume_in_unknown_operation_context: cannot resolve handler operation signature for this `resume`".to_string(),
                });
            };
            let actual = check_expr(value, env);
            let mut subst = TypeSubst::new();
            let expected_after_subst = apply_type_substitution(expected, &subst, env);
            if actual == Ty::Unknown && ty_contains_type_var(&expected_after_subst) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_expr_span(value),
                    message: format!(
                        "resume_unresolved_generic_constraints: cannot resolve generic constraints for `resume` argument (expected `{}` but got unresolved argument type)",
                        ty_name(&expected_after_subst)
                    ),
                });
            }
            if actual != Ty::Unknown && !unify_types_with_subst(expected, &actual, &mut subst, env)
            {
                let expected_rendered = apply_type_substitution(expected, &subst, env);
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_expr_span(value),
                    message: format!(
                        "resume_arg_type_mismatch: `resume` expects argument of type `{}` but got `{}`{}",
                        ty_name(&expected_rendered),
                        ty_name(&actual),
                        type_hole_conflict_note(&expected_rendered)
                    ),
                });
            }
            Ok(())
        }
        Expr::Block(stmts) => {
            let mut child_env = TypeEnv {
                globals: env.globals.clone(),
                locals: env.locals.clone(),
                type_aliases: env.type_aliases.clone(),
                record_types: env.record_types.clone(),
            };
            check_resume_in_stmts_with_local_env(stmts, &mut child_env, decl_name, resume_ctx)
        }
        Expr::Case { scrutinee, arms } => {
            recurse!(scrutinee)?;
            let scrutinee_ty = check_expr(scrutinee, env);
            let resolved_scrutinee_ty = env.resolve_alias(&scrutinee_ty, 0);
            for arm in arms {
                if is_list_case_pattern(&arm.pattern)
                    && !matches!(resolved_scrutinee_ty, Ty::List(_) | Ty::Unknown)
                {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: Some(arm.span),
                        message: format!(
                            "list case pattern requires `List` scrutinee, but got `{}`",
                            ty_name(&resolved_scrutinee_ty)
                        ),
                    });
                }
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                recurse!(&arm.body, &arm_env)?;
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
            recurse!(else_expr)
        }
        Expr::ListIndex { list, index } => {
            recurse!(list)?;
            recurse!(index)?;
            check_list_index_constraints(list, index, env, decl_name)
        }
    }
}
