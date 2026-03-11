use std::collections::{HashMap, HashSet};

use crate::ast::{
    BinOpKind, CasePattern, Expr, InterpolatedPart, ListPatternItem, ListPatternTail, Stmt,
};
use crate::typecheck::TypecheckError;
use crate::typecheck_env::{
    EffectDependencyInfo, EffectMap, GlobalBinding, ResumeContext, Ty, TypeEnv, TypeSubst,
};

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

fn check_list_spread_constraints(
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

fn merge_branch_type(env: &TypeEnv, left: Ty, right: Ty) -> Ty {
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

fn branch_types_compatible(env: &TypeEnv, left: &Ty, right: &Ty) -> bool {
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

fn is_list_case_pattern(pattern: &CasePattern) -> bool {
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

fn env_with_case_pattern_bindings(
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

fn effect_candidates_for_operation(env: &TypeEnv, op_name: &str) -> Vec<String> {
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

fn check_resume_in_stmts_with_local_env(
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

fn apply_type_substitution(ty: &Ty, subst: &TypeSubst, env: &TypeEnv) -> Ty {
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

fn unify_types_with_subst(
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

fn instantiate_handler_clause_signature(
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

fn ty_contains_type_var(ty: &Ty) -> bool {
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

fn parse_tuple_member_index(member: &str) -> Option<usize> {
    if member.is_empty() || !member.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    member.parse::<usize>().ok()
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

fn type_hole_conflict_note(expected: &Ty) -> &'static str {
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

fn check_unhandled_effects_in_expr(
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

fn ensure_no_ambiguous_refs_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) => Ok(()),
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    ensure_no_ambiguous_refs_in_expr(expr, env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::ListLit { elements, spread } => {
            for item in elements {
                ensure_no_ambiguous_refs_in_expr(item, env, decl_name)?;
            }
            if let Some(s) = spread {
                ensure_no_ambiguous_refs_in_expr(s, env, decl_name)?;
            }
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                ensure_no_ambiguous_refs_in_expr(item, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Var(name) => ensure_name_not_ambiguous(name, env, decl_name),
        Expr::Qualified { receiver, member } => {
            if let Some(index) = parse_tuple_member_index(member) {
                let receiver_ty = env.lookup(receiver);
                let resolved_receiver_ty = env.resolve_alias(&receiver_ty, 0);
                return match resolved_receiver_ty {
                    Ty::Tuple(items) => {
                        if index < items.len() {
                            Ok(())
                        } else {
                            Err(TypecheckError {
                                declaration: Some(decl_name.to_string()),
                                span: None,
                                message: format!(
                                    "tuple member access index `{}` is out of range for receiver `{}` of type `{}`",
                                    index,
                                    receiver,
                                    ty_name(&Ty::Tuple(items))
                                ),
                            })
                        }
                    }
                    Ty::Unknown => {
                        if env.locals.contains_key(receiver) {
                            Ok(())
                        } else {
                            Err(TypecheckError {
                                declaration: Some(decl_name.to_string()),
                                span: None,
                                message: format!(
                                    "tuple member access `{}` requires tuple receiver, but `{}` type is unresolved",
                                    member, receiver
                                ),
                            })
                        }
                    }
                    other => Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "tuple member access `{}` requires tuple receiver, but `{}` has type `{}`",
                            member,
                            receiver,
                            ty_name(&other)
                        ),
                    }),
                };
            }
            if env.locals.contains_key(receiver) {
                return Ok(());
            }
            ensure_name_not_ambiguous(&format!("{}.{}", receiver, member), env, decl_name)
        }
        Expr::RecordConstruct {
            constructor,
            fields,
        } => {
            ensure_name_not_ambiguous(constructor, env, decl_name)?;
            let Some(record) = env.lookup_record_by_constructor(constructor) else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!("unknown record constructor `{}`", constructor),
                });
            };
            let mut seen = HashSet::new();
            for (_, value) in fields {
                ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?;
            }
            for (name, value) in fields {
                if !seen.insert(name.clone()) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "duplicate field `{}` in constructor call `{}`",
                            name, constructor
                        ),
                    });
                }
                let Some(expected_ty) = record.fields.get(name) else {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "unknown field `{}` in constructor call `{}`",
                            name, constructor
                        ),
                    });
                };
                let actual_ty = check_expr(value, env);
                if actual_ty != Ty::Unknown && !env.are_compatible(expected_ty, &actual_ty) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "field `{}` in constructor `{}` has type `{}` but expected `{}`",
                            name,
                            constructor,
                            ty_name(&actual_ty),
                            ty_name(expected_ty),
                        ),
                    });
                }
            }
            if seen.len() != record.fields.len() {
                let mut missing: Vec<String> = record
                    .fields
                    .keys()
                    .filter(|field| !seen.contains(*field))
                    .cloned()
                    .collect();
                missing.sort();
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "missing field(s) in constructor call `{}`: {}",
                        constructor,
                        missing.join(", ")
                    ),
                });
            }
            Ok(())
        }
        Expr::BinOp { left, right, .. } => {
            ensure_no_ambiguous_refs_in_expr(left, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(right, env, decl_name)
        }
        Expr::Call { callee, arg } => {
            ensure_no_ambiguous_refs_in_expr(callee, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(arg, env, decl_name)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            let qualified = format!("{}.{}", receiver, method);
            ensure_name_not_ambiguous(&qualified, env, decl_name)?;
            for arg in args {
                ensure_no_ambiguous_refs_in_expr(arg, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, callee } => {
            ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?;
            ensure_name_not_ambiguous(callee, env, decl_name)
        }
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            ensure_no_ambiguous_refs_in_expr(body, &child_env, decl_name)
        }
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = &clause.parsed_body {
                    ensure_no_ambiguous_refs_in_stmts(stmts, env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            ensure_no_ambiguous_refs_in_expr(handler, env, decl_name)?;
            ensure_no_ambiguous_refs_in_stmts(body, env, decl_name)
        }
        Expr::Resume { value } => ensure_no_ambiguous_refs_in_expr(value, env, decl_name),
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
                        let ty = check_expr(value, &local_env);
                        local_env.locals.insert(name.clone(), ty);
                    }
                    Stmt::Assign { value, .. } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                    }
                    Stmt::Expr(expr) => {
                        ensure_no_ambiguous_refs_in_expr(expr, &local_env, decl_name)?;
                    }
                }
            }
            Ok(())
        }
        Expr::Case { scrutinee, arms } => {
            ensure_no_ambiguous_refs_in_expr(scrutinee, env, decl_name)?;
            let scrutinee_ty = check_expr(scrutinee, env);
            for arm in arms {
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                ensure_no_ambiguous_refs_in_expr(&arm.body, &arm_env, decl_name)?;
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            ensure_no_ambiguous_refs_in_expr(condition, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(then_expr, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(else_expr, env, decl_name)
        }
    }
}

fn ensure_no_ambiguous_refs_in_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    for stmt in stmts {
        match stmt {
            Stmt::Binding { value, .. }
            | Stmt::MutBinding { value, .. }
            | Stmt::Assign { value, .. } => {
                ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?
            }
            Stmt::Expr(expr) => ensure_no_ambiguous_refs_in_expr(expr, env, decl_name)?,
        }
    }
    Ok(())
}

fn check_branch_type_consistency_in_stmts(
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

fn check_branch_type_consistency_in_expr(
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
                        span: None,
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
                    span: None,
                    message: format!(
                        "if branch type mismatch: then is `{}`, else is `{}`",
                        ty_name(&then_ty),
                        ty_name(&else_ty)
                    ),
                });
            }
            Ok(())
        }
    }
}

fn ensure_name_not_ambiguous(
    name: &str,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    if env.locals.contains_key(name) {
        return Ok(());
    }
    if let Some(sources) = env.ambiguous_sources(name) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: format!(
                "name `{}` is ambiguous due to name resolution collision: {}",
                name,
                sources.join(", ")
            ),
        });
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
