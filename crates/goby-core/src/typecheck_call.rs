use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, InterpolatedPart, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_check::check_expr;
use crate::typecheck_diag::err_unknown_callable;
use crate::typecheck_effect::{infer_curried_lambda_body_effects, infer_expr_effects};
use crate::typecheck_env::{EffectMap, EffectRow, RowSubst, Ty, TypeEnv, TypeSubst};
use crate::typecheck_render::ty_name;
use crate::typecheck_span::{best_available_expr_span, best_available_name_use_span};
use crate::typecheck_unify::{
    apply_type_substitution, instantiate_ty_with_fresh_type_vars_for_call_site,
    match_function_argument_type, unify_effect_rows, unify_types_with_subst,
};

/// EP-2 Step 3a: per-call-validation context bundling the maps needed for
/// lambda effect inference. Threaded through `check_ordinary_call_arg_types_in_expr`
/// so callbacks land at `infer_lambda_ty_against_expected` with the same
/// `effect_map` / `required_effects_map` / `covered_ops` view that
/// `check_unhandled_effects_in_expr` uses.
#[derive(Clone, Copy)]
pub(crate) struct CallContext<'a> {
    pub(crate) effect_map: &'a EffectMap,
    pub(crate) required_effects_map: &'a HashMap<String, Vec<String>>,
    pub(crate) covered_ops: &'a HashSet<String>,
}

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
    ctx: CallContext<'_>,
) -> Result<(), TypecheckError> {
    match expr {
        Expr::Spanned { expr, .. } => {
            check_ordinary_call_arg_types_in_expr(expr, env, decl_name, ctx)
        }
        Expr::Call { callee, arg, .. } => {
            check_ordinary_call_arg_types_in_expr(callee, env, decl_name, ctx)?;
            check_ordinary_call_arg_types_in_expr(arg, env, decl_name, ctx)?;
            validate_call_chain(expr, env, decl_name, ctx)
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                check_ordinary_call_arg_types_in_expr(arg, env, decl_name, ctx)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, .. } => {
            check_ordinary_call_arg_types_in_expr(value, env, decl_name, ctx)
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    check_ordinary_call_arg_types_in_expr(expr, env, decl_name, ctx)?;
                }
            }
            Ok(())
        }
        Expr::ListLit { elements, spread } => {
            for element in elements {
                check_ordinary_call_arg_types_in_expr(element, env, decl_name, ctx)?;
            }
            if let Some(spread) = spread {
                check_ordinary_call_arg_types_in_expr(spread, env, decl_name, ctx)?;
            }
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                check_ordinary_call_arg_types_in_expr(item, env, decl_name, ctx)?;
            }
            Ok(())
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                check_ordinary_call_arg_types_in_expr(value, env, decl_name, ctx)?;
            }
            Ok(())
        }
        Expr::UnaryOp { expr, .. } => {
            check_ordinary_call_arg_types_in_expr(expr, env, decl_name, ctx)
        }
        Expr::BinOp { left, right, .. } => {
            check_ordinary_call_arg_types_in_expr(left, env, decl_name, ctx)?;
            check_ordinary_call_arg_types_in_expr(right, env, decl_name, ctx)
        }
        Expr::Lambda { body, .. } => {
            check_ordinary_call_arg_types_in_expr(body, env, decl_name, ctx)
        }
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = &clause.parsed_body {
                    for stmt in stmts {
                        check_ordinary_call_arg_types_in_stmt(stmt, env, decl_name, ctx)?;
                    }
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            check_ordinary_call_arg_types_in_expr(handler, env, decl_name, ctx)?;
            // EP-3 Codex follow-up: a callback inside `with H in ... cb ...`
            // must see the handler's covered ops on `CallContext.covered_ops`,
            // otherwise lambda body inference (`infer_expr_effects`) reports
            // effects that the enclosing handler already discharges. Mirror
            // the merge that `check_unhandled_effects_in_expr` does on the
            // `Expr::With` branch.
            let handler_covered =
                crate::typecheck_effect::covered_ops_from_handler(
                    handler,
                    env,
                    ctx.effect_map,
                );
            let mut merged = ctx.covered_ops.clone();
            merged.extend(handler_covered);
            let inner_ctx = CallContext {
                effect_map: ctx.effect_map,
                required_effects_map: ctx.required_effects_map,
                covered_ops: &merged,
            };
            for stmt in body {
                check_ordinary_call_arg_types_in_stmt(stmt, env, decl_name, inner_ctx)?;
            }
            Ok(())
        }
        Expr::Resume { value } => {
            check_ordinary_call_arg_types_in_expr(value, env, decl_name, ctx)
        }
        Expr::Block(stmts) => {
            for stmt in stmts {
                check_ordinary_call_arg_types_in_stmt(stmt, env, decl_name, ctx)?;
            }
            Ok(())
        }
        Expr::Case { scrutinee, arms } => {
            check_ordinary_call_arg_types_in_expr(scrutinee, env, decl_name, ctx)?;
            for arm in arms {
                check_ordinary_call_arg_types_in_expr(&arm.body, env, decl_name, ctx)?;
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            check_ordinary_call_arg_types_in_expr(condition, env, decl_name, ctx)?;
            check_ordinary_call_arg_types_in_expr(then_expr, env, decl_name, ctx)?;
            check_ordinary_call_arg_types_in_expr(else_expr, env, decl_name, ctx)
        }
        Expr::ListIndex { list, index } => {
            check_ordinary_call_arg_types_in_expr(list, env, decl_name, ctx)?;
            check_ordinary_call_arg_types_in_expr(index, env, decl_name, ctx)
        }
        Expr::IntLit(_)
        | Expr::FloatLit(_)
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
    ctx: CallContext<'_>,
) -> Result<(), TypecheckError> {
    match stmt {
        Stmt::Binding { value, .. }
        | Stmt::MutBinding { value, .. }
        | Stmt::Assign { value, .. }
        | Stmt::Expr(value, _) => {
            check_ordinary_call_arg_types_in_expr(value, env, decl_name, ctx)
        }
    }
}

fn validate_call_chain(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
    ctx: CallContext<'_>,
) -> Result<(), TypecheckError> {
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
    let mut row_subst = RowSubst::new();
    for (idx, arg) in args.iter().enumerate() {
        let Some(expected) = params.get(idx) else {
            return Ok(());
        };
        let expected_after_subst = apply_type_substitution(expected, &subst, &row_subst, env);
        if matches!(env.resolve_alias(&expected_after_subst, 0), Ty::Fun { .. }) {
            let actual_ty = match infer_callback_arg_ty(
                arg,
                &expected_after_subst,
                env,
                &mut subst,
                &mut row_subst,
                &mut next_id,
                ctx,
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
                // EP-1d / EP-2 Step 3a: structural unification of the
                // lambda body's params/result already happened inside
                // `infer_lambda_ty_against_expected`. We deliberately do not
                // re-run a full `match_function_argument_type` here because
                // historically that has been skipped to tolerate curried vs
                // flat callback shapes (`(b -> a -> b)` vs `(b, a) -> b`).
                //
                // What Step 3a still needs is the *effect row* binding: the
                // callee's row variable on the outermost callback `Ty::Fun`
                // must absorb the lambda's inferred effects. We extract both
                // outermost rows after subst and run `unify_effect_rows`
                // directly, leaving param/result mismatches to the existing
                // structural skip.
                //
                // KNOWN LIMITATION (Step 3b): for a curried lambda passed to
                // a multi-arity callback (e.g. `fold xs init (fn acc -> fn x ->
                // print x; acc)`), the *outermost* lambda's effects are
                // closed-empty because `infer_expr_effects` does not descend
                // into nested `Expr::Lambda` bodies (Step 1's
                // `nested_lambda_body_does_not_leak_into_outer_row`
                // invariant). The expected callback type is flat
                // (`(b, a) -> b can {e}`), so the row variable still binds,
                // but to closed-empty rather than the body's `{Print}`. Step 3b
                // closes this by either flattening curried lambda inferred
                // types or carrying the body's row through nested `Ty::Fun`
                // segments before this point.
                let expected_outer_effects = match env
                    .resolve_alias(&apply_type_substitution(
                        &expected_after_subst,
                        &subst,
                        &row_subst,
                        env,
                    ), 0)
                {
                    Ty::Fun { effects, .. } => effects,
                    _ => EffectRow::closed_empty(),
                };
                // EP-2 Step 3b: when the expected callback type is flat
                // (`(b, a) -> b can {e}`) but the lambda is written curried
                // (`fn acc -> fn x -> ...`), `actual_ty.effects` only carries
                // the outermost layer's effects (closed-empty: returning a
                // nested lambda value is itself effect-free). Aggregate the
                // *terminal* body's effects by walking through nested
                // `Expr::Lambda` AST nodes so the row variable absorbs what
                // the callback emits when fully applied.
                let actual_outer_effects = if expected_arity_flat(
                    &expected_after_subst,
                    env,
                ) > 1
                {
                    infer_curried_lambda_body_effects(
                        arg,
                        env,
                        ctx.effect_map,
                        ctx.required_effects_map,
                        ctx.covered_ops,
                    )
                } else {
                    match env.resolve_alias(
                        &apply_type_substitution(&actual_ty, &subst, &row_subst, env),
                        0,
                    ) {
                        Ty::Fun { effects, .. } => effects,
                        _ => EffectRow::closed_empty(),
                    }
                };
                if !unify_effect_rows(
                    &expected_outer_effects,
                    &actual_outer_effects,
                    &mut row_subst,
                    &mut next_id,
                ) {
                    // LANGUAGE_SPEC §5: closed effect rows are exact, so a
                    // callback typed `(a -> b)` (closed-empty) cannot accept
                    // a lambda whose body produces effects. Reject here so
                    // an effectful body is not silently accepted just because
                    // the surrounding declaration happens to list `can E`.
                    let target_name = resolved_callable_name(target);
                    let call_target = target_name.as_deref().unwrap_or("function");
                    let resolved_expected =
                        crate::typecheck_unify::apply_row_substitution(
                            &expected_outer_effects,
                            &row_subst,
                        )
                        .unwrap_or_else(|| expected_outer_effects.clone());
                    let resolved_actual =
                        crate::typecheck_unify::apply_row_substitution(
                            &actual_outer_effects,
                            &row_subst,
                        )
                        .unwrap_or_else(|| actual_outer_effects.clone());
                    let detail = classify_effect_row_mismatch(
                        &resolved_expected,
                        &resolved_actual,
                    );
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: best_available_expr_span(arg),
                        message: format!(
                            "`{}` callback effect row mismatch: required `{}` but lambda has `{}`{}",
                            call_target,
                            ty_name(&Ty::Fun {
                                params: vec![Ty::Unknown],
                                result: Box::new(Ty::Unknown),
                                effects: expected_outer_effects.clone(),
                            }),
                            ty_name(&Ty::Fun {
                                params: vec![Ty::Unknown],
                                result: Box::new(Ty::Unknown),
                                effects: actual_outer_effects.clone(),
                            }),
                            detail,
                        ),
                    });
                }
                continue;
            }
            if let Err(mismatch) = match_function_argument_type(
                &expected_after_subst,
                &actual_ty,
                &mut subst,
                &mut row_subst,
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
        if !unify_types_with_subst(
            &expected_after_subst,
            &actual_ty,
            &mut subst,
            &mut row_subst,
            env,
            &mut next_id,
        ) {
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
    row_subst: &mut RowSubst,
    next_id: &mut usize,
    ctx: CallContext<'_>,
) -> Result<Ty, CallbackLambdaMismatch> {
    match expr {
        Expr::Lambda { .. } => {
            infer_lambda_ty_against_expected(expr, expected, env, subst, row_subst, next_id, ctx)
        }
        _ => Ok(resolve_function_value_ty(expr, env, next_id)),
    }
}

fn infer_lambda_ty_against_expected(
    expr: &Expr,
    expected: &Ty,
    env: &TypeEnv,
    subst: &mut TypeSubst,
    row_subst: &mut RowSubst,
    next_id: &mut usize,
    ctx: CallContext<'_>,
) -> Result<Ty, CallbackLambdaMismatch> {
    let Expr::Lambda { param, body } = expr else {
        return Ok(resolve_function_value_ty(expr, env, next_id));
    };

    let required = apply_type_substitution(expected, subst, row_subst, env);
    let Ty::Fun {
        params,
        result,
        effects,
    } = env.resolve_alias(&required, 0)
    else {
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
    let expected_param_ty = apply_type_substitution(expected_param_ty, subst, row_subst, env);
    let expected_rest_ty = if params.len() == 1 {
        apply_type_substitution(&result, subst, row_subst, env)
    } else {
        Ty::Fun {
            params: params[1..]
                .iter()
                .map(|param| apply_type_substitution(param, subst, row_subst, env))
                .collect(),
            result: Box::new(apply_type_substitution(&result, subst, row_subst, env)),
            // EP-1d: carry through the original callable's residual row;
            // apply_type_substitution above already routed any bound row
            // tail through the row substitution.
            effects: effects.clone(),
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
                row_subst,
                next_id,
                ctx,
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
                            effects: EffectRow::closed_empty(),
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

    // EP-2 Step 3a: synthesise the lambda's effect row from its body. This
    // turns previously-silent callback effects into a proper row that
    // `unify_types_with_subst` can match against the callee's row variable.
    let inferred_effects = infer_expr_effects(
        body.as_ref(),
        &child_env,
        ctx.effect_map,
        ctx.required_effects_map,
        ctx.covered_ops,
    );
    let provided = Ty::Fun {
        params: vec![expected_param_ty],
        result: Box::new(body_ty.clone()),
        effects: inferred_effects,
    };

    if !unify_types_with_subst(
        &expected_rest_ty,
        &body_ty,
        subst,
        row_subst,
        &child_env,
        next_id,
    ) {
        return Err(CallbackLambdaMismatch::Result { required, provided });
    }

    Ok(apply_type_substitution(
        &provided, subst, row_subst, &child_env,
    ))
}

/// EP-2 Step 3b: at a call site, determine which fixed effect names the
/// callee's outermost row resolves to once the lambda arguments contribute
/// their inferred rows. Returns `Some(set)` only when the call is *fully
/// applied* (so that a row variable in the callee's signature has actually
/// been bound through unification); partial applications return `None`
/// because the residual `Ty::Fun.effects` describes the future call, not
/// the current one.
pub(crate) fn infer_call_effects_at_site(
    expr: &Expr,
    env: &TypeEnv,
    ctx: CallContext<'_>,
) -> Option<HashSet<String>> {
    let (target, args) = ordinary_call_target_and_args(expr)?;
    if args.is_empty() {
        return None;
    }
    if resolved_callable_name(target)
        .as_deref()
        .is_some_and(|name| env.is_effect_op(name))
    {
        return None;
    }
    let mut next_id = 0usize;
    let target_ty = check_expr(target, env);
    let Ty::Fun { params, effects, .. } =
        instantiate_ty_with_fresh_type_vars_for_call_site(&target_ty, &mut next_id)
    else {
        return None;
    };
    if args.len() < params.len() {
        // Partial application: skip — the residual function's row stays on
        // the resulting `Ty::Fun` value and will be re-checked at the call
        // that finally supplies the missing argument(s).
        return None;
    }

    let mut subst = TypeSubst::new();
    let mut row_subst = RowSubst::new();
    for (idx, arg) in args.iter().enumerate() {
        let Some(expected) = params.get(idx) else {
            break;
        };
        let expected_after_subst = apply_type_substitution(expected, &subst, &row_subst, env);
        if matches!(env.resolve_alias(&expected_after_subst, 0), Ty::Fun { .. }) {
            // Lambda argument: bind the callee's row variable to the lambda's
            // inferred row. Same outermost-effects approach as Step 3a, plus
            // the curried-aggregation fallback when the expected callback is
            // multi-arity.
            if matches!(arg, Expr::Lambda { .. }) {
                let actual_outer_effects = if expected_arity_flat(&expected_after_subst, env) > 1 {
                    crate::typecheck_effect::infer_curried_lambda_body_effects(
                        arg,
                        env,
                        ctx.effect_map,
                        ctx.required_effects_map,
                        ctx.covered_ops,
                    )
                } else {
                    let mut local_env = env.clone();
                    let mut current = *arg;
                    while let Expr::Spanned { expr, .. } = current {
                        current = expr.as_ref();
                    }
                    if let Expr::Lambda { param, body } = current {
                        local_env = local_env.with_local(param, Ty::Unknown);
                        crate::typecheck_effect::infer_expr_effects(
                            body.as_ref(),
                            &local_env,
                            ctx.effect_map,
                            ctx.required_effects_map,
                            ctx.covered_ops,
                        )
                    } else {
                        EffectRow::closed_empty()
                    }
                };
                let expected_outer_effects = match env.resolve_alias(&expected_after_subst, 0) {
                    Ty::Fun { effects, .. } => effects,
                    _ => EffectRow::closed_empty(),
                };
                // Failures here are non-fatal for *this* helper: the Step 3a
                // validator (`check_ordinary_call_arg_types_in_expr`) runs
                // before `check_unhandled_effects_in_expr` and reports row
                // mismatches with the user-facing `callback effect row
                // mismatch` diagnostic. If we reach this point, Step 3a
                // already accepted the call, so unification will normally
                // succeed; on the off-chance it does not, leaving
                // `row_subst` partially updated only widens the leak set we
                // compute, which is a safe over-approximation for diag.
                let _ = unify_effect_rows(
                    &expected_outer_effects,
                    &actual_outer_effects,
                    &mut row_subst,
                    &mut next_id,
                );
            } else {
                // Named function or other expression: unify against its own
                // type (carrying any declared effects) like the validator.
                // Same safety argument as above — Step 3a is the source of
                // truth for argument-type compatibility.
                let actual_ty = resolve_function_value_ty(arg, env, &mut next_id);
                if actual_ty != Ty::Unknown {
                    let _ = match_function_argument_type(
                        &expected_after_subst,
                        &actual_ty,
                        &mut subst,
                        &mut row_subst,
                        env,
                        &mut next_id,
                    );
                }
            }
        } else {
            // Non-function argument: same Step 3a-first safety story.
            let actual_ty = resolve_function_value_ty(arg, env, &mut next_id);
            if actual_ty != Ty::Unknown {
                let _ = unify_types_with_subst(
                    &expected_after_subst,
                    &actual_ty,
                    &mut subst,
                    &mut row_subst,
                    env,
                    &mut next_id,
                );
            }
        }
    }

    // After unification, walk the callee's outermost row through `row_subst`
    // and collect fixed effect names. `apply_row_substitution` only returns
    // None on a cycle in the substitution (see `typecheck_unify`); an
    // unbound tail simply stays as `Some(var)` and we drop it, since the
    // tail represents future flexibility, not a *currently required* effect.
    let resolved = crate::typecheck_unify::apply_row_substitution(&effects, &row_subst)?;
    Some(resolved.fixed.into_iter().collect())
}

/// Number of parameters in the *flat* callback type (`(a, b) -> r` returns 2).
/// Curried encodings like `(a -> b -> r)` parse as flat `Fun { params: [a, b], result: r }`
/// in this codebase (see `parse_function_type`), so a single `Ty::Fun` node
/// suffices.
fn expected_arity_flat(ty: &Ty, env: &TypeEnv) -> usize {
    match env.resolve_alias(ty, 0) {
        Ty::Fun { params, .. } => params.len(),
        _ => 0,
    }
}

/// EP-3 diagnostic refinement (LANGUAGE_SPEC §5 line 283-284): produce a
/// trailing hint that distinguishes "missing effect in closed row" from
/// "row variable cannot be unified" once the base "callback effect row
/// mismatch" message has rendered the offending rows.
///
/// Returns an empty string when no specific classification applies; callers
/// always append the result, so the unmodified diagnostic remains a valid
/// fallback for cases the helper does not (yet) distinguish.
fn classify_effect_row_mismatch(expected: &EffectRow, actual: &EffectRow) -> String {
    match (&expected.tail, &actual.tail) {
        (None, None) => {
            // closed/closed: report effects present in `actual` but missing
            // from `expected`, and effects required by `expected` but absent
            // from `actual`. Both fall under LANGUAGE_SPEC §5's
            // "missing effect in closed row" classification — closed rows
            // are exact-match.
            let format_set = |names: &[String]| -> String {
                names
                    .iter()
                    .map(|name| format!("`{}`", name))
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            let extras: Vec<String> = actual
                .fixed
                .difference(&expected.fixed)
                .cloned()
                .collect();
            let missing: Vec<String> = expected
                .fixed
                .difference(&actual.fixed)
                .cloned()
                .collect();
            match (extras.is_empty(), missing.is_empty()) {
                (true, true) => String::new(),
                (false, true) => format!(
                    " (effect{} {} not allowed by required closed row)",
                    if extras.len() == 1 { "" } else { "s" },
                    format_set(&extras),
                ),
                (true, false) => format!(
                    " (required closed row demands effect{} {} that the lambda does not produce)",
                    if missing.len() == 1 { "" } else { "s" },
                    format_set(&missing),
                ),
                (false, false) => format!(
                    " (closed row mismatch: lambda lacks {}, has extra {})",
                    format_set(&missing),
                    format_set(&extras),
                ),
            }
        }
        (Some(_), Some(_)) | (Some(_), None) | (None, Some(_)) => {
            // After row substitution, a tail that survived means the row
            // variable could not bind (open/open with mismatched fixed sets,
            // or closed/open subset failure). LANGUAGE_SPEC §5 calls this
            // class "row variable cannot be unified".
            " (callback row variable cannot be unified)".to_string()
        }
    }
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
    let Ty::Fun {
        params,
        result,
        effects,
    } = instantiate_ty_with_fresh_type_vars_for_call_site(&target_ty, next_id)
    else {
        return target_ty;
    };

    let mut subst = TypeSubst::new();
    let mut row_subst = RowSubst::new();
    for (idx, arg) in args.iter().enumerate() {
        let Some(expected) = params.get(idx) else {
            return Ty::Unknown;
        };
        let expected_after_subst = apply_type_substitution(expected, &subst, &row_subst, env);
        let actual_ty = resolve_function_value_ty(arg, env, next_id);
        if actual_ty == Ty::Unknown {
            return Ty::Unknown;
        }
        if matches!(env.resolve_alias(&expected_after_subst, 0), Ty::Fun { .. }) {
            if match_function_argument_type(
                &expected_after_subst,
                &actual_ty,
                &mut subst,
                &mut row_subst,
                env,
                next_id,
            )
            .is_err()
            {
                return Ty::Unknown;
            }
        } else if !unify_types_with_subst(
            &expected_after_subst,
            &actual_ty,
            &mut subst,
            &mut row_subst,
            env,
            next_id,
        ) {
            return Ty::Unknown;
        }
    }

    if args.len() < params.len() {
        let remaining = params[args.len()..]
            .iter()
            .map(|param| apply_type_substitution(param, &subst, &row_subst, env))
            .collect();
        let result = apply_type_substitution(&result, &subst, &row_subst, env);
        Ty::Fun {
            params: remaining,
            result: Box::new(result),
            // EP-1d: partial application preserves the original callable's
            // residual row, but bindings introduced during arg unification
            // must be applied. Without `apply_row_substitution`, a
            // row-polymorphic callable would return a Ty::Fun whose `effects`
            // tail still pointed at the pre-binding fresh row variable.
            effects: crate::typecheck_unify::apply_row_substitution(&effects, &row_subst)
                .unwrap_or_else(EffectRow::closed_empty),
        }
    } else {
        apply_type_substitution(&result, &subst, &row_subst, env)
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
