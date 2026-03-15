use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_ambiguity::ensure_no_ambiguous_refs_in_expr;
use crate::typecheck_branch::check_branch_type_consistency_in_expr;
use crate::typecheck_check::check_expr;
use crate::typecheck_effect_usage::check_unhandled_effects_in_expr;
use crate::typecheck_env::{EffectMap, Ty, TypeEnv};
use crate::typecheck_render::ty_name;

pub(crate) fn check_body_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    effect_map: &EffectMap,
    required_effects_map: &HashMap<String, Vec<String>>,
    decl_name: &str,
    declared_return_ty: Option<Ty>,
    param_tys: &[(&str, Ty)],
    covered_ops: &HashSet<String>,
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
    let mut local_mutability: HashMap<String, bool> = param_tys
        .iter()
        .map(|(name, _)| (name.to_string(), false))
        .collect();

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
        Stmt::Binding { name, value } => {
            if local_mutability.contains_key(name) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "duplicate declaration `{}` in the same scope; use `:=` for mutation",
                        name
                    ),
                });
            }
            validate_stmt_value(
                value,
                local_env,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )?;
            let ty = check_expr(value, local_env);
            local_env.locals.insert(name.clone(), ty);
            local_mutability.insert(name.clone(), false);
            Ok(())
        }
        Stmt::MutBinding { name, value } => {
            if local_mutability.contains_key(name) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "duplicate declaration `{}` in the same scope; use `:=` for mutation",
                        name
                    ),
                });
            }
            validate_stmt_value(
                value,
                local_env,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )?;
            let ty = check_expr(value, local_env);
            local_env.locals.insert(name.clone(), ty);
            local_mutability.insert(name.clone(), true);
            Ok(())
        }
        Stmt::Assign { name, value } => {
            if !local_mutability.contains_key(name) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!("cannot assign to undeclared variable `{}`", name),
                });
            }
            if !local_mutability.get(name).copied().unwrap_or(false) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "cannot assign to immutable variable `{}`; declare it with `mut` first",
                        name
                    ),
                });
            }
            validate_stmt_value(
                value,
                local_env,
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
                    span: None,
                    message: format!(
                        "assignment type `{}` does not match variable `{}` type `{}`",
                        ty_name(&assigned_ty),
                        name,
                        ty_name(&current_ty)
                    ),
                });
            }
            if current_ty == Ty::Unknown {
                local_env.locals.insert(name.clone(), assigned_ty);
            }
            Ok(())
        }
        Stmt::Expr(expr) => validate_stmt_value(
            expr,
            local_env,
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
    required_effects_map: &HashMap<String, Vec<String>>,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    ensure_known_call_targets_in_expr(expr, local_env, decl_name)?;
    ensure_no_ambiguous_refs_in_expr(expr, local_env, decl_name)?;
    check_unhandled_effects_in_expr(
        expr,
        local_env,
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
        Expr::Call { callee, arg } => {
            match callee.as_ref() {
                Expr::Var(name) => {
                    if matches!(name.as_str(), "print" | "println")
                        && env.lookup(name) == Ty::Unknown
                    {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: None,
                            message: format!("unknown function or constructor `{}`", name),
                        });
                    }
                }
                _ => {}
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
        Expr::Pipeline { value, callee } => {
            if matches!(callee.as_str(), "print" | "println") && env.lookup(callee) == Ty::Unknown {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!("unknown function or constructor `{}`", callee),
                });
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
                            | Stmt::Expr(value) => {
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
                    | Stmt::Expr(value) => {
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
                    | Stmt::Expr(value) => {
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
        | Expr::Var(_)
        | Expr::Qualified { .. } => Ok(()),
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
            if let Stmt::Expr(expr) = s {
                Some(check_expr(expr, local_env))
            } else {
                None
            }
        })
        .unwrap_or(Ty::Unit);

    if inferred != Ty::Unknown && !env.are_compatible(&declared, &inferred) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: format!(
                "body type `{}` does not match declared return type `{}`",
                ty_name(&inferred),
                ty_name(&declared),
            ),
        });
    }
    Ok(())
}
