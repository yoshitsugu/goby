use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, InterpolatedPart, Span, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_ambiguity::ensure_no_ambiguous_refs_in_expr;
use crate::typecheck_check::{check_expr, env_with_case_pattern_bindings};
use crate::typecheck_env::{EffectMap, RowSubst, Ty, TypeEnv, TypeSubst};
use crate::typecheck_render::ty_name;
use crate::typecheck_span::{best_available_expr_span, best_available_name_use_span};
use crate::typecheck_stmt::check_body_stmts;
use crate::typecheck_call::{CallContext, infer_call_effects_at_site};
use crate::typecheck_unify::{
    apply_type_substitution, instantiate_handler_clause_signature, ty_contains_type_var,
    type_hole_conflict_note, unify_types_with_subst,
};

fn resolve_handler_clause_name(
    clause_name: &str,
    effect_map: &EffectMap,
    decl_name: &str,
    clause_span: Option<Span>,
) -> Result<(String, String), TypecheckError> {
    // Returns (bare_op_name, effect_name).
    if let Some((effect, op)) = clause_name.split_once('.') {
        // Qualified form: "Effect.op" — verify the op exists in that effect.
        let Some(ops) = effect_map.effect_to_ops.get(effect) else {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: clause_span,
                message: format!(
                    "unknown effect operation `{}` in handler expression",
                    clause_name
                ),
            });
        };
        // The effect_to_ops set contains both bare and qualified forms (e.g. "log" and "Log.log").
        let bare = op.to_string();
        let qualified = clause_name.to_string();
        if !ops.contains(&bare) && !ops.contains(&qualified) {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: clause_span,
                message: format!(
                    "unknown effect operation `{}` in handler expression",
                    clause_name
                ),
            });
        }
        return Ok((bare, effect.to_string()));
    }

    // Bare form: look up via op_to_effects.
    let Some(effects) = effect_map.op_to_effects.get(clause_name) else {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: clause_span,
            message: format!(
                "unknown effect operation `{}` in handler expression",
                clause_name
            ),
        });
    };
    if effects.len() > 1 {
        let mut names: Vec<String> = effects.iter().cloned().collect();
        names.sort();
        let first_effect = names[0].clone();
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: clause_span,
            message: format!(
                "handler clause '{}' is ambiguous (defined in effects: {}); use qualified form e.g. '{}.{}'",
                clause_name,
                names.join(", "),
                first_effect,
                clause_name
            ),
        });
    }
    let effect = effects.iter().next().expect("non-empty set");
    Ok((clause_name.to_string(), effect.clone()))
}

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
                let (bare_name, effect) = resolve_handler_clause_name(
                    &clause.name,
                    effect_map,
                    decl_name,
                    Some(clause.span),
                )?;
                // Duplicate check uses bare name so "log" and "Log.log" conflict.
                if !seen_ops.insert(bare_name.clone()) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: Some(clause.span),
                        message: format!("duplicate handler clause for operation `{}`", bare_name),
                    });
                }
                covered.insert(bare_name.clone());
                covered.insert(format!("{}.{}", effect, bare_name));
            }
            Ok(covered)
        }
        Expr::Var { name, .. } => match env.lookup(name) {
            Ty::Handler { covered_ops } => Ok(covered_ops),
            _ => Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: best_available_name_use_span(handler_expr),
                message: format!(
                    "`with` expects a handler value, but `{}` is not a Handler",
                    name
                ),
            }),
        },
        _ => Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: best_available_expr_span(handler_expr),
            message: "`with` expects a handler value".to_string(),
        }),
    }
}

/// Mirrors `typecheck_effect::effect_fully_covered`: an effect leaks out
/// unless every one of its operations is in `covered_ops`.
fn effect_fully_covered_for_diag(
    effect_name: &str,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
) -> bool {
    let Some(ops) = effect_map.effect_to_ops.get(effect_name) else {
        return false;
    };
    if ops.is_empty() {
        return false;
    }
    ops.iter().all(|op| covered_ops.contains(op))
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
                span: None, // expr span not yet available
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
    local_mutability: Option<&HashMap<String, bool>>,
    required_effects_map: &HashMap<String, Vec<String>>,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
    decl_can_ops: &HashSet<String>,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    // EP-3 root-cause split: the *incoming* `covered_ops` carries only ops
    // discharged by an enclosing `with`, while `decl_can_ops` carries ops
    // permitted by the surrounding decl's `can` clause. We keep a separate
    // `with_only_covered_ops` for lambda body effect inference (so callback
    // effects surface to the row variable) and locally shadow `covered_ops`
    // with the effective union for op-call validation, preserving the
    // existing "can clause permits this op" behaviour.
    let with_only_covered_ops = covered_ops;
    let covered_ops_owned: HashSet<String> = covered_ops
        .union(decl_can_ops)
        .cloned()
        .collect();
    let covered_ops: &HashSet<String> = &covered_ops_owned;
    fn check_effect_op_call_arg_types_in_handler_scope(
        op_name: &str,
        args: &[&Expr],
        call_site: &Expr,
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
                    span: best_available_name_use_span(call_site),
                    message: format!(
                        "effect operation `{}` expects {} argument(s) but got at least {}",
                        op_name,
                        params.len(),
                        args.len()
                    ),
                });
            }
            let mut subst = TypeSubst::new();
            let mut row_subst = RowSubst::new();
            let mut next_id = 0usize;
            for (idx, arg) in args.iter().enumerate() {
                let expected = &params[idx];
                if *expected == Ty::Unknown {
                    continue;
                }
                let actual = check_expr(arg, env);
                let expected_after_subst =
                    apply_type_substitution(expected, &subst, &row_subst, env);
                if actual == Ty::Unknown && ty_contains_type_var(&expected_after_subst) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: best_available_expr_span(arg),
                        message: format!(
                            "effect operation `{}` argument #{} has unresolved type (expected `{}`; provide a concrete argument or annotate the type)",
                            op_name,
                            idx + 1,
                            ty_name(&expected_after_subst)
                        ),
                    });
                }
                if actual != Ty::Unknown
                    && !unify_types_with_subst(
                        expected,
                        &actual,
                        &mut subst,
                        &mut row_subst,
                        env,
                        &mut next_id,
                    )
                {
                    // EP-1d follow-up: render the *pre-unification* snapshot
                    // so a partially-mutated subst from the failed unify call
                    // cannot bleed into the diagnostic. `expected_after_subst`
                    // was computed earlier with the same expected type.
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: best_available_expr_span(arg),
                        message: format!(
                            "effect operation `{}` expects argument of type `{}` but got `{}`{}",
                            op_name,
                            ty_name(&expected_after_subst),
                            ty_name(&actual),
                            type_hole_conflict_note(&expected_after_subst)
                        ),
                    });
                }
            }
        }
        Ok(())
    }

    fn effect_op_call_target_and_args(expr: &Expr) -> Option<(String, Vec<&Expr>)> {
        match expr {
            Expr::Var { name, .. } => Some((name.clone(), Vec::new())),
            Expr::Qualified {
                receiver, member, ..
            } => Some((format!("{}.{}", receiver, member), Vec::new())),
            Expr::Call { callee, arg, .. } => {
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
                local_mutability,
                required_effects_map,
                effect_map,
                with_only_covered_ops,
                decl_can_ops,
                decl_name,
            )
        };
        ($e:expr, $child_env:expr) => {
            check_unhandled_effects_in_expr(
                $e,
                $child_env,
                local_mutability,
                required_effects_map,
                effect_map,
                with_only_covered_ops,
                decl_can_ops,
                decl_name,
            )
        };
    }

    match expr {
        Expr::Spanned { expr, .. } => recurse!(expr),
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
        Expr::Var { name, .. } => {
            if env.is_effect_op(name) && !covered_ops.contains(name.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_name_use_span(expr),
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
        Expr::Qualified {
            receiver, member, ..
        } => {
            let qualified = format!("{}.{}", receiver, member);
            if env.is_effect_op(&qualified) && !covered_ops.contains(qualified.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_expr_span(expr),
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
        Expr::UnaryOp { expr, .. } => recurse!(expr),
        Expr::BinOp { left, right, .. } => {
            recurse!(left)?;
            recurse!(right)
        }
        Expr::Call { callee, arg, .. } => {
            if let Expr::Var { name, .. } = callee.as_ref() {
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
                    expr,
                    env,
                    covered_ops,
                    decl_name,
                )?;
            }
            // EP-2 Step 3b: row-based effect check at fully-applied call sites.
            // Lambda callbacks may have bound the callee's row variable to
            // their inferred effects, so the resolved row's `fixed` set
            // names the effects this call requires beyond the callee's
            // declared `required_effects_map` entry.
            //
            // EP-3: pass `with_only_covered_ops` so lambda body inference does
            // not silently swallow effects that the outer decl's `can` clause
            // permits — those effects should surface to the row variable so
            // they can be (re)bound at the call site here.
            let ctx = CallContext {
                effect_map,
                required_effects_map,
                covered_ops: with_only_covered_ops,
            };
            if let Some(call_effects) = infer_call_effects_at_site(expr, env, ctx) {
                for effect_name in &call_effects {
                    if !effect_fully_covered_for_diag(effect_name, effect_map, covered_ops) {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: best_available_expr_span(expr),
                            message: format!(
                                "callback effect `{}` is not handled by any enclosing `with` scope",
                                effect_name
                            ),
                        });
                    }
                }
            }
            recurse!(callee)?;
            recurse!(arg)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            let qualified = format!("{}.{}", receiver, method);
            if env.is_effect_op(&qualified) && !covered_ops.contains(qualified.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_name_use_span(expr),
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
                expr,
                env,
                covered_ops,
                decl_name,
            )?;
            for arg in args {
                recurse!(arg)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, callee, .. } => {
            if env.is_effect_op(callee) && !covered_ops.contains(callee.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None, // expr span not yet available
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `with` scope",
                        callee
                    ),
                });
            }
            check_effect_op_call_arg_types_in_handler_scope(
                callee,
                &[value.as_ref()],
                expr,
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
                let (bare_name, _effect) = resolve_handler_clause_name(
                    &clause.name,
                    effect_map,
                    decl_name,
                    Some(clause.span),
                )?;
                if let Some(stmts) = &clause.parsed_body {
                    let instantiated = instantiate_handler_clause_signature(
                        env,
                        &bare_name,
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
                        local_mutability,
                        effect_map,
                        required_effects_map,
                        decl_name,
                        None,
                        &param_refs,
                        with_only_covered_ops,
                        decl_can_ops,
                    )?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            recurse!(handler)?;
            let handler_covered =
                infer_handler_covered_ops_strict(handler, env, effect_map, decl_name)?;
            // EP-3: extend the *with-derived* set only. `decl_can_ops` is
            // immutable per declaration and is threaded through unchanged.
            let mut merged = with_only_covered_ops.clone();
            merged.extend(handler_covered);
            check_body_stmts(
                body,
                env,
                local_mutability,
                effect_map,
                required_effects_map,
                decl_name,
                None,
                &[],
                &merged,
                decl_can_ops,
            )
        }
        Expr::Resume { value } => recurse!(value),
        Expr::Block(stmts) => {
            if !matches!(stmts.last(), Some(Stmt::Expr(_, _))) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_expr_span(expr),
                    message: "block expression must end with an expression".to_string(),
                });
            }
            let mut local_env = env.clone();
            for stmt in stmts {
                match stmt {
                    Stmt::Binding { name, value, .. } | Stmt::MutBinding { name, value, .. } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                        check_unhandled_effects_in_expr(
                            value,
                            &local_env,
                            local_mutability,
                            required_effects_map,
                            effect_map,
                            with_only_covered_ops,
                            decl_can_ops,
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
                            local_mutability,
                            required_effects_map,
                            effect_map,
                            with_only_covered_ops,
                            decl_can_ops,
                            decl_name,
                        )?;
                    }
                    Stmt::Expr(expr, _) => {
                        ensure_no_ambiguous_refs_in_expr(expr, &local_env, decl_name)?;
                        check_unhandled_effects_in_expr(
                            expr,
                            &local_env,
                            local_mutability,
                            required_effects_map,
                            effect_map,
                            with_only_covered_ops,
                            decl_can_ops,
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
        Expr::ListIndex { list, index } => {
            recurse!(list)?;
            recurse!(index)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use super::*;
    use crate::ast::Span;
    use crate::typecheck_env::EffectMap;

    fn effect_map_with_op(effect: &str, op: &str) -> EffectMap {
        let mut effect_to_ops = HashMap::new();
        let mut ops = HashSet::new();
        ops.insert(op.to_string());
        ops.insert(format!("{}.{}", effect, op));
        effect_to_ops.insert(effect.to_string(), ops);

        let mut op_to_effects = HashMap::new();
        let mut effects = HashSet::new();
        effects.insert(effect.to_string());
        op_to_effects.insert(op.to_string(), effects.clone());
        op_to_effects.insert(format!("{}.{}", effect, op), effects);

        EffectMap {
            effect_to_ops,
            op_to_effects,
        }
    }

    fn effect_map_with_ambiguous_op(op: &str, effects: &[&str]) -> EffectMap {
        let mut effect_to_ops = HashMap::new();
        for effect in effects {
            let mut ops = HashSet::new();
            ops.insert(op.to_string());
            effect_to_ops.insert(effect.to_string(), ops);
        }

        let mut op_to_effects = HashMap::new();
        let effect_set: HashSet<String> = effects.iter().map(|s| s.to_string()).collect();
        op_to_effects.insert(op.to_string(), effect_set);

        EffectMap {
            effect_to_ops,
            op_to_effects,
        }
    }

    #[test]
    fn unknown_handler_clause_name_error_carries_span() {
        let clause_span = Span::new(2, 1, 2, 12);
        let effect_map = effect_map_with_op("Log", "log");

        let err =
            resolve_handler_clause_name("no_such_op", &effect_map, "my_decl", Some(clause_span))
                .expect_err("should be unknown op");

        assert_eq!(err.span, Some(clause_span));
        assert!(err.message.contains("unknown effect operation"));
    }

    #[test]
    fn ambiguous_handler_clause_name_error_carries_span() {
        let clause_span = Span::new(3, 1, 3, 8);
        let effect_map = effect_map_with_ambiguous_op("send", &["Http", "Smtp"]);

        let err = resolve_handler_clause_name("send", &effect_map, "my_decl", Some(clause_span))
            .expect_err("should be ambiguous");

        assert_eq!(err.span, Some(clause_span));
        assert!(err.message.contains("ambiguous"));
    }

    #[test]
    fn duplicate_handler_clause_error_carries_span() {
        let clause_span = Span::new(4, 1, 4, 8);
        let effect_map = effect_map_with_op("Log", "log");
        let handler_expr = Expr::Handler {
            clauses: vec![
                crate::ast::HandlerClause {
                    name: "log".to_string(),
                    params: vec![],
                    body: "()".to_string(),
                    parsed_body: None,
                    span: Span::new(2, 1, 2, 8),
                },
                crate::ast::HandlerClause {
                    name: "log".to_string(),
                    params: vec![],
                    body: "()".to_string(),
                    parsed_body: None,
                    span: clause_span,
                },
            ],
        };
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        };

        let err = infer_handler_covered_ops_strict(&handler_expr, &env, &effect_map, "my_decl")
            .expect_err("should be duplicate");

        assert_eq!(err.span, Some(clause_span));
        assert!(err.message.contains("duplicate"));
    }
}
