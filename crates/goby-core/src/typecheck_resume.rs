use std::collections::HashMap;

use crate::ast::{Expr, InterpolatedPart, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_check::{
    check_expr, check_list_spread_constraints, effect_candidates_for_operation,
    env_with_case_pattern_bindings, is_list_case_pattern, ty_name,
};
use crate::typecheck_env::{ResumeContext, Ty, TypeEnv, TypeSubst};

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
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                check_resume_in_expr(value, local_env, decl_name, resume_ctx)?;
                let ty = infer_binding_ty_with_resume_context(value, local_env, resume_ctx);
                local_env.locals.insert(name.clone(), ty);
            }
            Stmt::Assign { value, .. } => {
                check_resume_in_expr(value, local_env, decl_name, resume_ctx)?;
            }
            Stmt::Expr(expr) => {
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

pub(crate) fn apply_type_substitution(ty: &Ty, subst: &TypeSubst, env: &TypeEnv) -> Ty {
    let resolved = env.resolve_alias(ty, 0);
    match resolved {
        Ty::Var(name) => match subst.get(&name) {
            Some(bound) => apply_type_substitution(bound, subst, env),
            None => Ty::Var(name),
        },
        Ty::List(inner) => Ty::List(Box::new(apply_type_substitution(&inner, subst, env))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .iter()
                .map(|item| apply_type_substitution(item, subst, env))
                .collect(),
        ),
        Ty::Fun { params, result } => Ty::Fun {
            params: params
                .iter()
                .map(|param| apply_type_substitution(param, subst, env))
                .collect(),
            result: Box::new(apply_type_substitution(&result, subst, env)),
        },
        Ty::Con { name, args } => Ty::Con {
            name,
            args: args
                .iter()
                .map(|arg| apply_type_substitution(arg, subst, env))
                .collect(),
        },
        Ty::Handler { covered_ops } => Ty::Handler { covered_ops },
        Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => resolved,
    }
}

fn bind_type_variable(name: &str, ty: &Ty, subst: &mut TypeSubst, env: &TypeEnv) -> bool {
    if let Some(bound) = subst.get(name).cloned() {
        return unify_types_with_subst(&bound, ty, subst, env);
    }
    if matches!(ty, Ty::Var(other) if other == name) {
        return true;
    }
    subst.insert(name.to_string(), apply_type_substitution(ty, subst, env));
    true
}

pub(crate) fn unify_types_with_subst(
    expected: &Ty,
    actual: &Ty,
    subst: &mut TypeSubst,
    env: &TypeEnv,
) -> bool {
    let expected = apply_type_substitution(expected, subst, env);
    let actual = apply_type_substitution(actual, subst, env);
    match (expected, actual) {
        (Ty::Unknown, _) | (_, Ty::Unknown) => true,
        (Ty::Var(name), ty) => bind_type_variable(&name, &ty, subst, env),
        (ty, Ty::Var(name)) => bind_type_variable(&name, &ty, subst, env),
        (Ty::Int, Ty::Int) | (Ty::Bool, Ty::Bool) | (Ty::Str, Ty::Str) | (Ty::Unit, Ty::Unit) => {
            true
        }
        (Ty::List(left), Ty::List(right)) => unify_types_with_subst(&left, &right, subst, env),
        (Ty::Tuple(left), Ty::Tuple(right)) if left.len() == right.len() => left
            .iter()
            .zip(right.iter())
            .all(|(l, r)| unify_types_with_subst(l, r, subst, env)),
        (
            Ty::Fun {
                params: left_params,
                result: left_result,
            },
            Ty::Fun {
                params: right_params,
                result: right_result,
            },
        ) if left_params.len() == right_params.len() => {
            left_params
                .iter()
                .zip(right_params.iter())
                .all(|(l, r)| unify_types_with_subst(l, r, subst, env))
                && unify_types_with_subst(&left_result, &right_result, subst, env)
        }
        (
            Ty::Con {
                name: left_name,
                args: left_args,
            },
            Ty::Con {
                name: right_name,
                args: right_args,
            },
        ) if left_name == right_name && left_args.len() == right_args.len() => left_args
            .iter()
            .zip(right_args.iter())
            .all(|(l, r)| unify_types_with_subst(l, r, subst, env)),
        (
            Ty::Handler {
                covered_ops: left_ops,
            },
            Ty::Handler {
                covered_ops: right_ops,
            },
        ) => left_ops == right_ops,
        _ => false,
    }
}

fn instantiate_ty_with_fresh_type_vars(
    ty: &Ty,
    mapping: &mut HashMap<String, String>,
    next_id: &mut usize,
) -> Ty {
    match ty {
        Ty::Var(name) => {
            let fresh = mapping.entry(name.clone()).or_insert_with(|| {
                let id = *next_id;
                *next_id += 1;
                format!("__goby_fresh_ty_{}", id)
            });
            Ty::Var(fresh.clone())
        }
        Ty::List(inner) => Ty::List(Box::new(instantiate_ty_with_fresh_type_vars(
            inner, mapping, next_id,
        ))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .iter()
                .map(|item| instantiate_ty_with_fresh_type_vars(item, mapping, next_id))
                .collect(),
        ),
        Ty::Fun { params, result } => Ty::Fun {
            params: params
                .iter()
                .map(|param| instantiate_ty_with_fresh_type_vars(param, mapping, next_id))
                .collect(),
            result: Box::new(instantiate_ty_with_fresh_type_vars(
                result, mapping, next_id,
            )),
        },
        Ty::Con { name, args } => Ty::Con {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| instantiate_ty_with_fresh_type_vars(arg, mapping, next_id))
                .collect(),
        },
        Ty::Handler { covered_ops } => Ty::Handler {
            covered_ops: covered_ops.clone(),
        },
        Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => ty.clone(),
    }
}

pub(crate) fn instantiate_handler_clause_signature(
    env: &TypeEnv,
    clause_name: &str,
    next_id: &mut usize,
) -> Option<(Vec<Ty>, Ty)> {
    let op_ty = {
        let effects = effect_candidates_for_operation(env, clause_name);
        if effects.len() == 1 {
            env.lookup(&format!("{}.{}", effects[0], clause_name))
        } else {
            env.lookup(clause_name)
        }
    };
    let Ty::Fun { params, result } = op_ty else {
        return None;
    };
    let mut mapping = HashMap::new();
    let params = params
        .iter()
        .map(|param| instantiate_ty_with_fresh_type_vars(param, &mut mapping, next_id))
        .collect();
    let result = instantiate_ty_with_fresh_type_vars(&result, &mut mapping, next_id);
    Some((params, result))
}

pub(crate) fn ty_contains_type_var(ty: &Ty) -> bool {
    match ty {
        Ty::Var(_) => true,
        Ty::List(inner) => ty_contains_type_var(inner),
        Ty::Tuple(items) => items.iter().any(ty_contains_type_var),
        Ty::Fun { params, result } => {
            params.iter().any(ty_contains_type_var) || ty_contains_type_var(result)
        }
        Ty::Con { args, .. } => args.iter().any(ty_contains_type_var),
        Ty::Handler { .. } => false,
        Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => false,
    }
}

fn ty_contains_anonymous_type_hole(ty: &Ty) -> bool {
    match ty {
        Ty::Var(name) => name.starts_with("__goby_type_hole_"),
        Ty::List(inner) => ty_contains_anonymous_type_hole(inner),
        Ty::Tuple(items) => items.iter().any(ty_contains_anonymous_type_hole),
        Ty::Fun { params, result } => {
            params.iter().any(ty_contains_anonymous_type_hole)
                || ty_contains_anonymous_type_hole(result)
        }
        Ty::Con { args, .. } => args.iter().any(ty_contains_anonymous_type_hole),
        Ty::Handler { .. } | Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => false,
    }
}

pub(crate) fn type_hole_conflict_note(expected: &Ty) -> &'static str {
    if ty_contains_anonymous_type_hole(expected) {
        " (anonymous type-hole `_` constraints conflict)"
    } else {
        ""
    }
}

fn check_resume_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
    resume_ctx: Option<&ResumeContext>,
) -> Result<(), TypecheckError> {
    if let Expr::Var(name) = expr
        && name == "Unit"
    {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
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
                    span: None,
                    message: "resume_outside_handler: `resume` can only be used inside handler method bodies".to_string(),
                });
            };
            let Some(expected) = ctx.expected_arg_ty.as_ref() else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: "resume_in_unknown_operation_context: cannot resolve handler operation signature for this `resume`".to_string(),
                });
            };
            let actual = check_expr(value, env);
            let mut subst = TypeSubst::new();
            let expected_after_subst = apply_type_substitution(expected, &subst, env);
            if actual == Ty::Unknown && ty_contains_type_var(&expected_after_subst) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
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
                    span: None,
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
                        span: None,
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
    }
}
