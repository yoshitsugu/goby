use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_ambiguity::ensure_no_ambiguous_refs_in_expr;
use crate::typecheck_branch::check_branch_type_consistency_in_expr;
use crate::typecheck_check::check_expr;
use crate::typecheck_effect_usage::check_unhandled_effects_in_expr;
use crate::typecheck_env::{EffectDependencyInfo, EffectMap, Ty, TypeEnv};
use crate::typecheck_render::ty_name;

#[allow(clippy::too_many_arguments)]
pub(crate) fn check_body_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    effect_map: &EffectMap,
    effect_dependency_info: &EffectDependencyInfo,
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
        effect_dependency_info,
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
    effect_dependency_info: &EffectDependencyInfo,
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
            effect_dependency_info,
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
    effect_dependency_info: &EffectDependencyInfo,
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
                effect_dependency_info,
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
                effect_dependency_info,
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
                effect_dependency_info,
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
            effect_dependency_info,
            required_effects_map,
            effect_map,
            covered_ops,
            decl_name,
        ),
    }
}

#[allow(clippy::too_many_arguments)]
fn validate_stmt_value(
    expr: &Expr,
    local_env: &TypeEnv,
    effect_dependency_info: &EffectDependencyInfo,
    required_effects_map: &HashMap<String, Vec<String>>,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    ensure_no_ambiguous_refs_in_expr(expr, local_env, decl_name)?;
    check_unhandled_effects_in_expr(
        expr,
        local_env,
        effect_dependency_info,
        required_effects_map,
        effect_map,
        covered_ops,
        decl_name,
    )?;
    check_branch_type_consistency_in_expr(expr, local_env, decl_name)
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
