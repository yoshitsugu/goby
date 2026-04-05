use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_ambiguity::ensure_no_ambiguous_refs_in_expr;
use crate::typecheck_branch::check_branch_type_consistency_in_expr;
use crate::typecheck_call::{check_ordinary_call_arg_types_in_expr, infer_expr_binding_ty};
use crate::typecheck_check::check_expr;
use crate::typecheck_diag::err_unknown_callable;
use crate::typecheck_effect_usage::check_unhandled_effects_in_expr;
use crate::typecheck_env::{EffectMap, Ty, TypeEnv};
use crate::typecheck_render::ty_name;
use crate::typecheck_span::best_available_name_use_span;

#[allow(clippy::too_many_arguments)]
pub(crate) fn check_body_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    inherited_mutability: Option<&HashMap<String, bool>>,
    effect_map: &EffectMap,
    required_effects_map: &HashMap<String, Vec<String>>,
    decl_name: &str,
    declared_return_ty: Option<Ty>,
    param_tys: &[(&str, Ty)],
    covered_ops: &HashSet<String>,
) -> Result<(), TypecheckError> {
    let mut local_env = TypeEnv {
        globals: env.globals.clone(),
        locals: env.locals.clone(),
        type_aliases: env.type_aliases.clone(),
        record_types: env.record_types.clone(),
    };
    for (name, ty) in param_tys {
        local_env.locals.insert((*name).to_string(), ty.clone());
    }
    let mut local_mutability: HashMap<String, bool> =
        inherited_mutability.cloned().unwrap_or_default();
    for (name, _) in param_tys {
        local_mutability.insert((*name).to_string(), false);
    }

    check_statement_sequence(
        stmts,
        env,
        &mut local_env,
        &mut local_mutability,
        effect_map,
        required_effects_map,
        decl_name,
        covered_ops,
    )?;
    check_declared_return_type(stmts, env, &local_env, decl_name, declared_return_ty)
}

#[allow(clippy::too_many_arguments)]
fn check_statement_sequence(
    stmts: &[Stmt],
    env: &TypeEnv,
    local_env: &mut TypeEnv,
    local_mutability: &mut HashMap<String, bool>,
    effect_map: &EffectMap,
    required_effects_map: &HashMap<String, Vec<String>>,
    decl_name: &str,
    covered_ops: &HashSet<String>,
) -> Result<(), TypecheckError> {
    for stmt in stmts {
        check_stmt(
            stmt,
            env,
            local_env,
            local_mutability,
            effect_map,
            required_effects_map,
            decl_name,
            covered_ops,
        )?;
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn check_stmt(
    stmt: &Stmt,
    env: &TypeEnv,
    local_env: &mut TypeEnv,
    local_mutability: &mut HashMap<String, bool>,
    effect_map: &EffectMap,
    required_effects_map: &HashMap<String, Vec<String>>,
    decl_name: &str,
    covered_ops: &HashSet<String>,
) -> Result<(), TypecheckError> {
    match stmt {
        Stmt::Binding { name, value, span } => {
            if local_mutability.contains_key(name) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: *span,
                    message: format!(
                        "duplicate declaration `{}` in the same scope; use `:=` for mutation",
                        name
                    ),
                });
            }
            validate_stmt_value(
                value,
                local_env,
                local_mutability,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )?;
            let ty = infer_expr_binding_ty(value, local_env);
            local_env.locals.insert(name.clone(), ty);
            local_mutability.insert(name.clone(), false);
            Ok(())
        }
        Stmt::MutBinding { name, value, span } => {
            if local_mutability.contains_key(name) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: *span,
                    message: format!(
                        "duplicate declaration `{}` in the same scope; use `:=` for mutation",
                        name
                    ),
                });
            }
            validate_stmt_value(
                value,
                local_env,
                local_mutability,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )?;
            let ty = infer_expr_binding_ty(value, local_env);
            local_env.locals.insert(name.clone(), ty);
            local_mutability.insert(name.clone(), true);
            Ok(())
        }
        Stmt::Assign { target, value, span } => {
            match target {
                crate::ast::AssignTarget::Var(name) => {
                    if !local_mutability.contains_key(name) {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: *span,
                            message: format!("cannot assign to undeclared variable `{}`", name),
                        });
                    }
                    if !local_mutability.get(name).copied().unwrap_or(false) {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: *span,
                            message: format!(
                                "cannot assign to immutable variable `{}`; declare it with `mut` first",
                                name
                            ),
                        });
                    }
                    validate_stmt_value(
                        value,
                        local_env,
                        local_mutability,
                        required_effects_map,
                        effect_map,
                        covered_ops,
                        decl_name,
                    )?;
                    let current_ty = local_env.locals.get(name).cloned().unwrap_or(Ty::Unknown);
                    let assigned_ty = check_expr(value, local_env);
                    if current_ty != Ty::Unknown
                        && assigned_ty != Ty::Unknown
                        && !env.are_compatible(&current_ty, &assigned_ty)
                    {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: *span,
                            message: format!(
                                "assignment type `{}` does not match variable `{}` type `{}`",
                                ty_name(&assigned_ty),
                                name,
                                ty_name(&current_ty)
                            ),
                        });
                    }
                    if current_ty == Ty::Unknown {
                        local_env
                            .locals
                            .insert(name.clone(), infer_expr_binding_ty(value, local_env));
                    }
                    Ok(())
                }
                crate::ast::AssignTarget::ListIndex { .. } => {
                    // Full type-checking for list-index assignment targets is implemented in LM2.
                    // This includes the root-variable mutability check (`root_name()` is available
                    // but intentionally not applied yet), List/index type checks, and element type
                    // compatibility. Only the RHS is validated here to catch effect/resume errors.
                    validate_stmt_value(
                        value,
                        local_env,
                        local_mutability,
                        required_effects_map,
                        effect_map,
                        covered_ops,
                        decl_name,
                    )
                }
            }
        }
        Stmt::Expr(expr, _) => validate_stmt_value(
            expr,
            local_env,
            local_mutability,
            required_effects_map,
            effect_map,
            covered_ops,
            decl_name,
        ),
    }
}

fn validate_stmt_value(
    expr: &Expr,
    local_env: &TypeEnv,
    local_mutability: &HashMap<String, bool>,
    required_effects_map: &HashMap<String, Vec<String>>,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    ensure_known_call_targets_in_expr(expr, local_env, decl_name)?;
    ensure_no_ambiguous_refs_in_expr(expr, local_env, decl_name)?;
    check_ordinary_call_arg_types_in_expr(expr, local_env, decl_name)?;
    check_unhandled_effects_in_expr(
        expr,
        local_env,
        Some(local_mutability),
        required_effects_map,
        effect_map,
        covered_ops,
        decl_name,
    )?;
    check_branch_type_consistency_in_expr(expr, local_env, decl_name)
}

fn ensure_known_call_targets_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    match expr {
        Expr::Spanned { expr, .. } => ensure_known_call_targets_in_expr(expr, env, decl_name),
        Expr::Call { callee, arg, .. } => {
            if let Expr::Var { name, .. } = callee.as_ref() {
                let callee_ty = env.lookup(name);
                if callee_ty == Ty::Unknown {
                    return Err(err_unknown_callable(
                        decl_name,
                        name,
                        best_available_name_use_span(callee),
                    ));
                }
            }
            ensure_known_call_targets_in_expr(callee, env, decl_name)?;
            ensure_known_call_targets_in_expr(arg, env, decl_name)
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                ensure_known_call_targets_in_expr(arg, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, callee, .. } => {
            if env.lookup(callee) == Ty::Unknown {
                return Err(err_unknown_callable(
                    decl_name,
                    callee,
                    best_available_name_use_span(expr),
                ));
            }
            ensure_known_call_targets_in_expr(value, env, decl_name)
        }
        Expr::ListLit { elements, spread } => {
            for element in elements {
                ensure_known_call_targets_in_expr(element, env, decl_name)?;
            }
            if let Some(spread) = spread {
                ensure_known_call_targets_in_expr(spread, env, decl_name)?;
            }
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                ensure_known_call_targets_in_expr(item, env, decl_name)?;
            }
            Ok(())
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                ensure_known_call_targets_in_expr(value, env, decl_name)?;
            }
            Ok(())
        }
        Expr::UnaryOp { expr, .. } => ensure_known_call_targets_in_expr(expr, env, decl_name),
        Expr::BinOp { left, right, .. } => {
            ensure_known_call_targets_in_expr(left, env, decl_name)?;
            ensure_known_call_targets_in_expr(right, env, decl_name)
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let crate::ast::InterpolatedPart::Expr(expr) = part {
                    ensure_known_call_targets_in_expr(expr, env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::Lambda { body, .. } => ensure_known_call_targets_in_expr(body, env, decl_name),
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = &clause.parsed_body {
                    for stmt in stmts {
                        match stmt {
                            Stmt::Binding { value, .. }
                            | Stmt::MutBinding { value, .. }
                            | Stmt::Assign { value, .. }
                            | Stmt::Expr(value, _) => {
                                ensure_known_call_targets_in_expr(value, env, decl_name)?;
                            }
                        }
                    }
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            ensure_known_call_targets_in_expr(handler, env, decl_name)?;
            for stmt in body {
                match stmt {
                    Stmt::Binding { value, .. }
                    | Stmt::MutBinding { value, .. }
                    | Stmt::Assign { value, .. }
                    | Stmt::Expr(value, _) => {
                        ensure_known_call_targets_in_expr(value, env, decl_name)?;
                    }
                }
            }
            Ok(())
        }
        Expr::Resume { value } => ensure_known_call_targets_in_expr(value, env, decl_name),
        Expr::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Binding { value, .. }
                    | Stmt::MutBinding { value, .. }
                    | Stmt::Assign { value, .. }
                    | Stmt::Expr(value, _) => {
                        ensure_known_call_targets_in_expr(value, env, decl_name)?;
                    }
                }
            }
            Ok(())
        }
        Expr::Case { scrutinee, arms } => {
            ensure_known_call_targets_in_expr(scrutinee, env, decl_name)?;
            for arm in arms {
                ensure_known_call_targets_in_expr(&arm.body, env, decl_name)?;
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            ensure_known_call_targets_in_expr(condition, env, decl_name)?;
            ensure_known_call_targets_in_expr(then_expr, env, decl_name)?;
            ensure_known_call_targets_in_expr(else_expr, env, decl_name)
        }
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var { .. }
        | Expr::Qualified { .. } => Ok(()),
        Expr::ListIndex { list, index } => {
            ensure_known_call_targets_in_expr(list, env, decl_name)?;
            ensure_known_call_targets_in_expr(index, env, decl_name)
        }
    }
}

fn check_declared_return_type(
    stmts: &[Stmt],
    env: &TypeEnv,
    local_env: &TypeEnv,
    decl_name: &str,
    declared_return_ty: Option<Ty>,
) -> Result<(), TypecheckError> {
    let Some(declared) = declared_return_ty.filter(|d| *d != Ty::Unknown) else {
        return Ok(());
    };

    let inferred = stmts
        .iter()
        .rev()
        .find_map(|s| {
            if let Stmt::Expr(expr, _) = s {
                Some(check_expr(expr, local_env))
            } else {
                None
            }
        })
        .unwrap_or(Ty::Unit);

    if inferred != Ty::Unknown && !env.are_compatible(&declared, &inferred) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None, // body-relative span; file-relative offset not yet wired up
            message: format!(
                "body type `{}` does not match declared return type `{}`",
                ty_name(&inferred),
                ty_name(&declared),
            ),
        });
    }
    Ok(())
}
