use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, InterpolatedPart, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_ambiguity::ensure_no_ambiguous_refs_in_expr;
use crate::typecheck_check::{check_expr, env_with_case_pattern_bindings};
use crate::typecheck_env::{EffectDependencyInfo, EffectMap, Ty, TypeEnv, TypeSubst};
use crate::typecheck_render::ty_name;
use crate::typecheck_stmt::check_body_stmts;
use crate::typecheck_unify::{
    apply_type_substitution, instantiate_handler_clause_signature, ty_contains_type_var,
    type_hole_conflict_note, unify_types_with_subst,
};

fn infer_handler_covered_ops_strict(
    handler_expr: &Expr,
    env: &TypeEnv,
    effect_map: &EffectMap,
    decl_name: &str,
) -> Result<HashSet<String>, TypecheckError> {
    match handler_expr {
        Expr::Handler { clauses } => {
            let mut covered = HashSet::new();
            let mut seen_ops = HashSet::new();
            for clause in clauses {
                if !seen_ops.insert(clause.name.clone()) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "duplicate handler clause for operation `{}`",
                            clause.name
                        ),
                    });
                }
                let Some(effects) = effect_map.op_to_effects.get(&clause.name) else {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "unknown effect operation `{}` in handler expression",
                            clause.name
                        ),
                    });
                };
                if effects.len() > 1 {
                    let mut names: Vec<String> = effects.iter().cloned().collect();
                    names.sort();
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "operation '{}' is ambiguous across effects in Handler(...): {}",
                            clause.name,
                            names.join(", ")
                        ),
                    });
                }
                let effect = effects.iter().next().expect("non-empty set");
                covered.insert(clause.name.clone());
                covered.insert(format!("{}.{}", effect, clause.name));
            }
            Ok(covered)
        }
        Expr::Var(name) => match env.lookup(name) {
            Ty::Handler { covered_ops } => Ok(covered_ops),
            _ => Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!(
                    "`with` expects a handler value, but `{}` is not a Handler",
                    name
                ),
            }),
        },
        _ => Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "`with` expects a handler value".to_string(),
        }),
    }
}

fn check_callee_required_effects(
    callee_name: &str,
    required_effects_map: &HashMap<String, Vec<String>>,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    let Some(required) = required_effects_map.get(callee_name) else {
        return Ok(());
    };
    for effect_name in required {
        let Some(ops) = effect_map.effect_to_ops.get(effect_name) else {
            continue;
        };
        let all_covered = ops.iter().all(|op| covered_ops.contains(op));
        if !all_covered {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!(
                    "function `{}` requires effect `{}` which is not handled by any enclosing `with` scope",
                    callee_name, effect_name
                ),
            });
        }
    }
    Ok(())
}

pub(crate) fn check_unhandled_effects_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    effect_dependency_info: &EffectDependencyInfo,
    required_effects_map: &HashMap<String, Vec<String>>,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    fn check_effect_op_call_arg_types_in_handler_scope(
        op_name: &str,
        args: &[&Expr],
        env: &TypeEnv,
        covered_ops: &HashSet<String>,
        decl_name: &str,
    ) -> Result<(), TypecheckError> {
        if env.is_effect_op(op_name)
            && covered_ops.contains(op_name)
            && let Ty::Fun { params, .. } = env.lookup(op_name)
        {
            if args.len() > params.len() {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` expects {} argument(s) but got at least {}",
                        op_name,
                        params.len(),
                        args.len()
                    ),
                });
            }
            let mut subst = TypeSubst::new();
            for (idx, arg) in args.iter().enumerate() {
                let expected = &params[idx];
                if *expected == Ty::Unknown {
                    continue;
                }
                let actual = check_expr(arg, env);
                let expected_after_subst = apply_type_substitution(expected, &subst, env);
                if actual == Ty::Unknown && ty_contains_type_var(&expected_after_subst) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "effect_op_unresolved_generic_constraints: effect operation `{}` argument #{} cannot resolve generic constraints (expected `{}` but argument type is unresolved)",
                            op_name,
                            idx + 1,
                            ty_name(&expected_after_subst)
                        ),
                    });
                }
                if actual != Ty::Unknown
                    && !unify_types_with_subst(expected, &actual, &mut subst, env)
                {
                    let expected_rendered = apply_type_substitution(expected, &subst, env);
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "effect operation `{}` expects argument of type `{}` but got `{}`{}",
                            op_name,
                            ty_name(&expected_rendered),
                            ty_name(&actual),
                            type_hole_conflict_note(&expected_rendered)
                        ),
                    });
                }
            }
        }
        Ok(())
    }

    fn effect_op_call_target_and_args(expr: &Expr) -> Option<(String, Vec<&Expr>)> {
        match expr {
            Expr::Var(name) => Some((name.clone(), Vec::new())),
            Expr::Qualified { receiver, member } => {
                Some((format!("{}.{}", receiver, member), Vec::new()))
            }
            Expr::Call { callee, arg } => {
                let (target, mut args) = effect_op_call_target_and_args(callee)?;
                args.push(arg.as_ref());
                Some((target, args))
            }
            _ => None,
        }
    }

    macro_rules! recurse {
        ($e:expr) => {
            check_unhandled_effects_in_expr(
                $e,
                env,
                effect_dependency_info,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )
        };
        ($e:expr, $child_env:expr) => {
            check_unhandled_effects_in_expr(
                $e,
                $child_env,
                effect_dependency_info,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )
        };
    }

    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) => Ok(()),
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
        Expr::Var(name) => {
            if env.is_effect_op(name) && !covered_ops.contains(name.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `with` scope",
                        name
                    ),
                });
            }
            check_callee_required_effects(
                name,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )
        }
        Expr::Qualified { receiver, member } => {
            let qualified = format!("{}.{}", receiver, member);
            if env.is_effect_op(&qualified) && !covered_ops.contains(qualified.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `with` scope",
                        qualified
                    ),
                });
            }
            Ok(())
        }
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
            if let Expr::Var(name) = callee.as_ref() {
                check_callee_required_effects(
                    name,
                    required_effects_map,
                    effect_map,
                    covered_ops,
                    decl_name,
                )?;
            }
            if let Some((op_name, args)) = effect_op_call_target_and_args(expr) {
                check_effect_op_call_arg_types_in_handler_scope(
                    &op_name,
                    &args,
                    env,
                    covered_ops,
                    decl_name,
                )?;
            }
            recurse!(callee)?;
            recurse!(arg)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            let qualified = format!("{}.{}", receiver, method);
            if env.is_effect_op(&qualified) && !covered_ops.contains(qualified.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `with` scope",
                        qualified
                    ),
                });
            }
            let provided: Vec<&Expr> = args.iter().collect();
            check_effect_op_call_arg_types_in_handler_scope(
                &qualified,
                &provided,
                env,
                covered_ops,
                decl_name,
            )?;
            for arg in args {
                recurse!(arg)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, callee } => {
            if env.is_effect_op(callee) && !covered_ops.contains(callee.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `with` scope",
                        callee
                    ),
                });
            }
            check_effect_op_call_arg_types_in_handler_scope(
                callee,
                &[value.as_ref()],
                env,
                covered_ops,
                decl_name,
            )?;
            recurse!(value)?;
            check_callee_required_effects(
                callee,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )
        }
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            recurse!(body, &child_env)
        }
        Expr::Handler { clauses } => {
            let mut fresh_type_counter = 0usize;
            for clause in clauses {
                let Some(effects) = effect_map.op_to_effects.get(&clause.name) else {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "unknown effect operation `{}` in handler expression",
                            clause.name
                        ),
                    });
                };
                if effects.len() > 1 {
                    let mut names: Vec<String> = effects.iter().cloned().collect();
                    names.sort();
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "operation '{}' is ambiguous across effects in Handler(...): {}",
                            clause.name,
                            names.join(", ")
                        ),
                    });
                }
                let effect_name = effects
                    .iter()
                    .next()
                    .expect("single-effect handler clause resolution");
                let qualified_op = format!("{}.{}", effect_name, clause.name);
                let mut clause_covered_ops = covered_ops.clone();
                if let Some(required_effects) = effect_dependency_info
                    .op_required_effects
                    .get(&qualified_op)
                {
                    for required_effect in required_effects {
                        if let Some(required_ops) = effect_map.effect_to_ops.get(required_effect) {
                            clause_covered_ops.extend(required_ops.iter().cloned());
                        }
                    }
                }
                if let Some(stmts) = &clause.parsed_body {
                    let instantiated = instantiate_handler_clause_signature(
                        env,
                        &clause.name,
                        &mut fresh_type_counter,
                    );
                    let params: Vec<(String, Ty)> = if let Some((param_tys, _)) = instantiated {
                        clause
                            .params
                            .iter()
                            .enumerate()
                            .map(|(idx, name)| {
                                (
                                    name.clone(),
                                    param_tys.get(idx).cloned().unwrap_or(Ty::Unknown),
                                )
                            })
                            .collect()
                    } else {
                        clause
                            .params
                            .iter()
                            .map(|name| (name.clone(), Ty::Unknown))
                            .collect()
                    };
                    let param_refs: Vec<(&str, Ty)> = params
                        .iter()
                        .map(|(name, ty)| (name.as_str(), ty.clone()))
                        .collect();
                    check_body_stmts(
                        stmts,
                        env,
                        effect_map,
                        effect_dependency_info,
                        required_effects_map,
                        decl_name,
                        None,
                        &param_refs,
                        &clause_covered_ops,
                    )?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            recurse!(handler)?;
            let handler_covered =
                infer_handler_covered_ops_strict(handler, env, effect_map, decl_name)?;
            let mut merged = covered_ops.clone();
            merged.extend(handler_covered);
            check_body_stmts(
                body,
                env,
                effect_map,
                effect_dependency_info,
                required_effects_map,
                decl_name,
                None,
                &[],
                &merged,
            )
        }
        Expr::Resume { value } => recurse!(value),
        Expr::Block(stmts) => {
            if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: "block expression must end with an expression".to_string(),
                });
            }
            let mut local_env = env.clone();
            for stmt in stmts {
                match stmt {
                    Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                        check_unhandled_effects_in_expr(
                            value,
                            &local_env,
                            effect_dependency_info,
                            required_effects_map,
                            effect_map,
                            covered_ops,
                            decl_name,
                        )?;
                        let ty = check_expr(value, &local_env);
                        local_env.locals.insert(name.clone(), ty);
                    }
                    Stmt::Assign { value, .. } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                        check_unhandled_effects_in_expr(
                            value,
                            &local_env,
                            effect_dependency_info,
                            required_effects_map,
                            effect_map,
                            covered_ops,
                            decl_name,
                        )?;
                    }
                    Stmt::Expr(expr) => {
                        ensure_no_ambiguous_refs_in_expr(expr, &local_env, decl_name)?;
                        check_unhandled_effects_in_expr(
                            expr,
                            &local_env,
                            effect_dependency_info,
                            required_effects_map,
                            effect_map,
                            covered_ops,
                            decl_name,
                        )?;
                    }
                }
            }
            Ok(())
        }
        Expr::Case { scrutinee, arms } => {
            recurse!(scrutinee)?;
            let scrutinee_ty = check_expr(scrutinee, env);
            for arm in arms {
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
