use std::collections::{HashMap, HashSet};

use crate::ast::{BinOpKind, CasePattern, Expr, ListPatternItem, ListPatternTail, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_ambiguity::ensure_no_ambiguous_refs_in_expr;
use crate::typecheck_branch::check_branch_type_consistency_in_expr;
use crate::typecheck_effect_usage::check_unhandled_effects_in_expr;
use crate::typecheck_env::{EffectDependencyInfo, EffectMap, GlobalBinding, Ty, TypeEnv};
use crate::typecheck_resume::ty_contains_type_var;

pub(crate) fn check_expr(expr: &Expr, env: &TypeEnv) -> Ty {
    match expr {
        Expr::IntLit(_) => Ty::Int,
        Expr::BoolLit(_) => Ty::Bool,
        Expr::StringLit(_) => Ty::Str,
        Expr::InterpolatedString(_) => Ty::Str,
        Expr::ListLit { elements, spread } => {
            if elements.is_empty() {
                return Ty::List(Box::new(Ty::Unknown));
            }
            let mut item_ty = check_expr(&elements[0], env);
            for item in &elements[1..] {
                item_ty = merge_branch_type(env, item_ty, check_expr(item, env));
            }
            if let Some(tail) = spread {
                let tail_ty = check_expr(tail, env);
                if let Ty::List(tail_inner) = env.resolve_alias(&tail_ty, 0) {
                    item_ty = merge_branch_type(env, item_ty, *tail_inner);
                }
            }
            Ty::List(Box::new(item_ty))
        }
        Expr::TupleLit(items) => {
            if items.is_empty() {
                return Ty::Unit;
            }
            let tys: Vec<Ty> = items.iter().map(|i| check_expr(i, env)).collect();
            Ty::Tuple(tys)
        }
        Expr::Var(name) => env.lookup(name),
        Expr::Qualified { receiver, member } => {
            let receiver_ty = env.lookup(receiver);
            let resolved_receiver_ty = env.resolve_alias(&receiver_ty, 0);
            if let Ty::Tuple(items) = &resolved_receiver_ty
                && let Some(index) = parse_tuple_member_index(member)
            {
                return items.get(index).cloned().unwrap_or(Ty::Unknown);
            }
            if let Some(receiver_ty) = env.locals.get(receiver) {
                let resolved_receiver_ty = env.resolve_alias(receiver_ty, 0);
                if let Ty::Con { name, .. } = &resolved_receiver_ty
                    && let Some(field_ty) = env.record_field_ty(name, member)
                {
                    return env.resolve_alias(&field_ty, 0);
                }
            }
            env.lookup(&format!("{}.{}", receiver, member))
        }
        Expr::RecordConstruct {
            constructor,
            fields,
        } => {
            let Some(record) = env.lookup_record_by_constructor(constructor) else {
                return Ty::Unknown;
            };
            if fields.len() != record.fields.len() {
                return Ty::Unknown;
            }
            for (name, value) in fields {
                let Some(expected_ty) = record.fields.get(name) else {
                    return Ty::Unknown;
                };
                let actual_ty = check_expr(value, env);
                if actual_ty != Ty::Unknown && !env.are_compatible(expected_ty, &actual_ty) {
                    return Ty::Unknown;
                }
            }
            Ty::Con {
                name: record.type_name.clone(),
                args: Vec::new(),
            }
        }
        Expr::BinOp { op, left, right } => {
            let lt = check_expr(left, env);
            let rt = check_expr(right, env);
            match (op, &lt, &rt) {
                (BinOpKind::And, Ty::Bool, Ty::Bool) => Ty::Bool,
                (BinOpKind::Add, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Mul, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Eq, Ty::Int, Ty::Int) => Ty::Bool,
                (BinOpKind::Eq, Ty::Str, Ty::Str) => Ty::Bool,
                (BinOpKind::Lt, Ty::Int, Ty::Int) => Ty::Bool,
                (BinOpKind::Gt, Ty::Int, Ty::Int) => Ty::Bool,
                (_, Ty::Unknown, _) | (_, _, Ty::Unknown) => Ty::Unknown,
                _ => Ty::Unknown,
            }
        }
        Expr::Call { callee, arg } => {
            if let Expr::Var(name) = callee.as_ref()
                && let Some(record) = env.lookup_record_by_constructor(name)
                && record.fields.len() == 1
            {
                let field_name = record.fields.keys().next().unwrap().clone();
                let rewritten = Expr::RecordConstruct {
                    constructor: name.clone(),
                    fields: vec![(field_name, *arg.clone())],
                };
                return check_expr(&rewritten, env);
            }
            let callee_ty = check_expr(callee, env);
            match callee_ty {
                Ty::Fun { result, .. } => *result,
                _ => Ty::Unknown,
            }
        }
        Expr::MethodCall {
            receiver, method, ..
        } => {
            let qualified = format!("{}.{}", receiver, method);
            match env.lookup(&qualified) {
                Ty::Fun { result, .. } => *result,
                _ => Ty::Unknown,
            }
        }
        Expr::Pipeline { value: _, callee } => {
            let callee_ty = env.lookup(callee);
            match callee_ty {
                Ty::Fun { result, .. } => *result,
                _ => Ty::Unknown,
            }
        }
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            let result = check_expr(body, &child_env);
            Ty::Fun {
                params: vec![Ty::Unknown],
                result: Box::new(result),
            }
        }
        Expr::Handler { clauses } => Ty::Handler {
            covered_ops: infer_handler_covered_ops_lenient(clauses, env),
        },
        Expr::With { .. } => Ty::Unknown,
        Expr::Resume { value } => {
            let _ = check_expr(value, env);
            Ty::Unknown
        }
        Expr::Block(stmts) => infer_block_expr_ty(stmts, env),
        Expr::Case { scrutinee, arms } => {
            let scrutinee_ty = check_expr(scrutinee, env);
            let mut merged: Option<Ty> = None;
            for arm in arms {
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                let arm_ty = check_expr(&arm.body, &arm_env);
                merged = Some(match merged {
                    Some(prev) => merge_branch_type(env, prev, arm_ty),
                    None => arm_ty,
                });
            }
            merged.unwrap_or(Ty::Unknown)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            let _ = check_expr(condition, env);
            let then_ty = check_expr(then_expr, env);
            let else_ty = check_expr(else_expr, env);
            merge_branch_type(env, then_ty, else_ty)
        }
    }
}

pub(crate) fn check_list_spread_constraints(
    elements: &[Expr],
    spread: Option<&Expr>,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    let Some(tail_expr) = spread else {
        return Ok(());
    };

    let mut merged_prefix_ty: Option<Ty> = None;
    for (idx, element) in elements.iter().enumerate() {
        let element_ty = env.resolve_alias(&check_expr(element, env), 0);
        if element_ty == Ty::Unknown || ty_contains_type_var(&element_ty) {
            continue;
        }
        if let Some(expected) = &merged_prefix_ty
            && !env.are_compatible(expected, &element_ty)
            && !env.are_compatible(&element_ty, expected)
        {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!(
                    "list spread prefix element type mismatch: element #{} is `{}` but earlier prefix elements require `{}`",
                    idx + 1,
                    ty_name(&element_ty),
                    ty_name(expected)
                ),
            });
        }
        merged_prefix_ty = Some(match merged_prefix_ty {
            Some(prev) => merge_branch_type(env, prev, element_ty),
            None => element_ty,
        });
    }

    let tail_ty = env.resolve_alias(&check_expr(tail_expr, env), 0);
    match tail_ty {
        Ty::List(tail_item_ty) => {
            let tail_item_ty = *tail_item_ty;
            if tail_item_ty == Ty::Unknown || ty_contains_type_var(&tail_item_ty) {
                return Ok(());
            }
            if let Some(expected_prefix_ty) = merged_prefix_ty
                && expected_prefix_ty != Ty::Unknown
                && !ty_contains_type_var(&expected_prefix_ty)
                && !env.are_compatible(&expected_prefix_ty, &tail_item_ty)
                && !env.are_compatible(&tail_item_ty, &expected_prefix_ty)
            {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "list spread tail element type mismatch: expected `List {}` but got `List {}`",
                        ty_name(&expected_prefix_ty),
                        ty_name(&tail_item_ty)
                    ),
                });
            }
            Ok(())
        }
        Ty::Unknown | Ty::Var(_) => Ok(()),
        other => {
            let expected = merged_prefix_ty.unwrap_or(Ty::Unknown);
            Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!(
                    "list spread tail must be `List {}` but got `{}`",
                    ty_name(&expected),
                    ty_name(&other)
                ),
            })
        }
    }
}

pub(crate) fn merge_branch_type(env: &TypeEnv, left: Ty, right: Ty) -> Ty {
    let left = env.resolve_alias(&left, 0);
    let right = env.resolve_alias(&right, 0);
    match (left, right) {
        (Ty::Unknown, ty) | (ty, Ty::Unknown) => ty,
        (Ty::List(l), Ty::List(r)) => Ty::List(Box::new(merge_branch_type(env, *l, *r))),
        (Ty::Tuple(ls), Ty::Tuple(rs)) if ls.len() == rs.len() => {
            let merged = ls
                .into_iter()
                .zip(rs)
                .map(|(l, r)| merge_branch_type(env, l, r))
                .collect();
            Ty::Tuple(merged)
        }
        (
            Ty::Fun {
                params: lps,
                result: lr,
            },
            Ty::Fun {
                params: rps,
                result: rr,
            },
        ) if lps.len() == rps.len() => {
            let merged_params = lps
                .into_iter()
                .zip(rps)
                .map(|(l, r)| merge_branch_type(env, l, r))
                .collect();
            Ty::Fun {
                params: merged_params,
                result: Box::new(merge_branch_type(env, *lr, *rr)),
            }
        }
        (Ty::Con { name: ln, args: la }, Ty::Con { name: rn, args: ra })
            if ln == rn && la.len() == ra.len() =>
        {
            let merged_args = la
                .into_iter()
                .zip(ra)
                .map(|(l, r)| merge_branch_type(env, l, r))
                .collect();
            Ty::Con {
                name: ln,
                args: merged_args,
            }
        }
        (l, r) if env.are_compatible(&l, &r) => l,
        (l, r) if env.are_compatible(&r, &l) => r,
        _ => Ty::Unknown,
    }
}

pub(crate) fn branch_types_compatible(env: &TypeEnv, left: &Ty, right: &Ty) -> bool {
    let left = env.resolve_alias(left, 0);
    let right = env.resolve_alias(right, 0);
    match (left, right) {
        (Ty::Unknown, _) | (_, Ty::Unknown) => true,
        (Ty::List(l), Ty::List(r)) => branch_types_compatible(env, &l, &r),
        (Ty::Tuple(ls), Ty::Tuple(rs)) if ls.len() == rs.len() => ls
            .iter()
            .zip(rs.iter())
            .all(|(l, r)| branch_types_compatible(env, l, r)),
        (
            Ty::Fun {
                params: lps,
                result: lr,
            },
            Ty::Fun {
                params: rps,
                result: rr,
            },
        ) if lps.len() == rps.len() => {
            lps.iter()
                .zip(rps.iter())
                .all(|(l, r)| branch_types_compatible(env, l, r))
                && branch_types_compatible(env, &lr, &rr)
        }
        (Ty::Con { name: ln, args: la }, Ty::Con { name: rn, args: ra })
            if ln == rn && la.len() == ra.len() =>
        {
            la.iter()
                .zip(ra.iter())
                .all(|(l, r)| branch_types_compatible(env, l, r))
        }
        (l, r) => env.are_compatible(&l, &r) || env.are_compatible(&r, &l),
    }
}

fn infer_block_expr_ty(stmts: &[Stmt], env: &TypeEnv) -> Ty {
    let mut local_env = env.clone();
    let mut last_expr_ty = Ty::Unknown;
    let mut has_tail_expr = false;
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
                has_tail_expr = false;
            }
            Stmt::Assign { value, .. } => {
                let _ = check_expr(value, &local_env);
                has_tail_expr = false;
            }
            Stmt::Expr(expr) => {
                last_expr_ty = check_expr(expr, &local_env);
                has_tail_expr = true;
            }
        }
    }
    if has_tail_expr {
        last_expr_ty
    } else {
        Ty::Unknown
    }
}

pub(crate) fn is_list_case_pattern(pattern: &CasePattern) -> bool {
    matches!(
        pattern,
        CasePattern::EmptyList | CasePattern::ListPattern { .. }
    )
}

fn list_item_ty_for_case_scrutinee(env: &TypeEnv, scrutinee_ty: &Ty) -> Ty {
    match env.resolve_alias(scrutinee_ty, 0) {
        Ty::List(inner) => *inner,
        Ty::Unknown => Ty::Unknown,
        _ => Ty::Unknown,
    }
}

pub(crate) fn env_with_case_pattern_bindings(
    env: &TypeEnv,
    pattern: &CasePattern,
    scrutinee_ty: &Ty,
) -> TypeEnv {
    let mut child = env.clone();
    if let CasePattern::ListPattern { items, tail } = pattern {
        let item_ty = list_item_ty_for_case_scrutinee(env, scrutinee_ty);
        for item in items {
            if let ListPatternItem::Bind(name) = item
                && name != "_"
            {
                child.locals.insert(name.clone(), item_ty.clone());
            }
        }
        if let Some(ListPatternTail::Bind(name)) = tail
            && name != "_"
        {
            child
                .locals
                .insert(name.clone(), Ty::List(Box::new(item_ty)));
        }
    }
    child
}

fn infer_handler_covered_ops_lenient(
    clauses: &[crate::ast::HandlerClause],
    env: &TypeEnv,
) -> HashSet<String> {
    let mut covered = HashSet::new();
    for clause in clauses {
        let effects = effect_candidates_for_operation(env, &clause.name);
        if effects.len() != 1 {
            continue;
        }
        let effect = &effects[0];
        covered.insert(clause.name.clone());
        covered.insert(format!("{}.{}", effect, clause.name));
    }
    covered
}

pub(crate) fn effect_candidates_for_operation(env: &TypeEnv, op_name: &str) -> Vec<String> {
    let mut out = Vec::new();
    let Some(binding) = env.globals.get(op_name) else {
        return out;
    };
    match binding {
        GlobalBinding::Resolved { source, .. } => {
            if let Some(effect) = extract_effect_name_from_source(source) {
                out.push(effect);
            }
        }
        GlobalBinding::Ambiguous { sources } => {
            for source in sources {
                if let Some(effect) = extract_effect_name_from_source(source) {
                    out.push(effect);
                }
            }
            out.sort();
            out.dedup();
        }
    }
    out
}

fn extract_effect_name_from_source(source: &str) -> Option<String> {
    let prefix = "effect `";
    let suffix = "` member";
    if !source.starts_with(prefix) || !source.ends_with(suffix) {
        return None;
    }
    let start = prefix.len();
    let end = source.len() - suffix.len();
    Some(source[start..end].to_string())
}

pub(crate) fn parse_tuple_member_index(member: &str) -> Option<usize> {
    if member.is_empty() || !member.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    member.parse::<usize>().ok()
}

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

    for stmt in stmts {
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
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
                local_mutability.insert(name.clone(), false);
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
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
                local_mutability.insert(name.clone(), true);
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
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
                let current_ty = local_env.locals.get(name).cloned().unwrap_or(Ty::Unknown);
                let assigned_ty = check_expr(value, &local_env);
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
                check_branch_type_consistency_in_expr(expr, &local_env, decl_name)?;
            }
        }
    }

    if let Some(declared) = declared_return_ty.filter(|d| *d != Ty::Unknown) {
        let inferred = stmts
            .iter()
            .rev()
            .find_map(|s| {
                if let Stmt::Expr(expr) = s {
                    Some(check_expr(expr, &local_env))
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
    }

    Ok(())
}

pub(crate) fn ty_name(ty: &Ty) -> String {
    match ty {
        Ty::Int => "Int".to_string(),
        Ty::Bool => "Bool".to_string(),
        Ty::Str => "String".to_string(),
        Ty::Unit => "Unit".to_string(),
        Ty::List(inner) => format!("List {}", ty_name(inner)),
        Ty::Tuple(items) => {
            let inner: Vec<String> = items.iter().map(ty_name).collect();
            format!("({})", inner.join(", "))
        }
        Ty::Fun { params, result } => {
            let mut parts: Vec<String> = params.iter().map(format_fun_segment).collect();
            parts.push(ty_name(result));
            parts.join(" -> ")
        }
        Ty::Var(name) => {
            if name.starts_with("__goby_type_hole_") {
                "_".to_string()
            } else {
                name.clone()
            }
        }
        Ty::Con { name, args } => {
            if args.is_empty() {
                name.clone()
            } else {
                let rendered_args: Vec<String> =
                    args.iter().map(format_type_application_arg).collect();
                format!("{} {}", name, rendered_args.join(" "))
            }
        }
        Ty::Handler { .. } => "Handler".to_string(),
        Ty::Unknown => "Unknown".to_string(),
    }
}

fn format_type_application_arg(ty: &Ty) -> String {
    match ty {
        Ty::Con { args, .. } if !args.is_empty() => format!("({})", ty_name(ty)),
        Ty::Fun { .. } => format!("({})", ty_name(ty)),
        _ => ty_name(ty),
    }
}

fn format_fun_segment(ty: &Ty) -> String {
    match ty {
        Ty::Fun { .. } => format!("({})", ty_name(ty)),
        _ => ty_name(ty),
    }
}
