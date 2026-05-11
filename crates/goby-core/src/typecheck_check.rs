use std::collections::{HashMap, HashSet};

use crate::ast::{
    BinOpKind, CasePattern, CtorPatternArg, Expr, ListPatternItem, ListPatternTail, Stmt,
};
use crate::typecheck::TypecheckError;
use crate::typecheck_env::{EffectRow, GlobalBinding, Ty, TypeEnv};
use crate::typecheck_render::ty_name;
use crate::typecheck_span::best_available_expr_span;
use crate::typecheck_unify::ty_contains_type_var;

/// CA-3a: 公開 wrapper。`next_id` を `next_fresh_ty_id_seed(env)` で
/// seed して `infer_expr_ty` を呼び出す。outer caller (`typecheck_stmt.rs` 等)
/// が `check_expr` を呼ぶたびに、`env` 内の既存 `__goby_fresh_ty_*` /
/// `__goby_fresh_row_*` と衝突しない初期値が得られる。
pub(crate) fn check_expr(expr: &Expr, env: &TypeEnv) -> Ty {
    let mut next_id = crate::typecheck_unify::next_fresh_ty_id_seed(env);
    infer_expr_ty(expr, env, &mut next_id)
}

/// CA-3a: `next_id` を取る版の expression 型推論本体。`Expr::Call` arm が
/// `typecheck_call::infer_call_result_ty` に counter を渡し、call resolver
/// 経由で生まれる fresh 名と本関数の他の経路で生まれる fresh 名が干渉
/// しないようにする。本コミットでは plumbing 変更のみで挙動不変
/// (各 arm の振る舞いは CA-1/CA-2 と同じ)。
pub(crate) fn infer_expr_ty(expr: &Expr, env: &TypeEnv, next_id: &mut usize) -> Ty {
    match expr {
        Expr::Spanned { expr, .. } => infer_expr_ty(expr, env, next_id),
        Expr::IntLit(_) => Ty::Int,
        Expr::FloatLit(_) => Ty::Float,
        Expr::BoolLit(_) => Ty::Bool,
        Expr::StringLit(_) => Ty::Str,
        Expr::InterpolatedString(_) => Ty::Str,
        Expr::ListLit { elements, spread } => {
            if elements.is_empty() {
                return Ty::List(Box::new(Ty::Unknown));
            }
            let mut item_ty = infer_expr_ty(&elements[0], env, next_id);
            for item in &elements[1..] {
                item_ty = merge_branch_type(env, item_ty, infer_expr_ty(item, env, next_id));
            }
            if let Some(tail) = spread {
                let tail_ty = infer_expr_ty(tail, env, next_id);
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
            let tys: Vec<Ty> = items
                .iter()
                .map(|i| infer_expr_ty(i, env, next_id))
                .collect();
            Ty::Tuple(tys)
        }
        Expr::Var { name, .. } => lookup_and_maybe_freshen_ctor(env, name, next_id),
        Expr::Qualified {
            receiver, member, ..
        } => {
            let receiver_ty = env.lookup(receiver);
            let resolved_receiver_ty = env.resolve_alias(&receiver_ty, 0);
            if let Ty::Tuple(items) = &resolved_receiver_ty
                && let Some(index) = parse_tuple_member_index(member)
            {
                return items.get(index).cloned().unwrap_or(Ty::Unknown);
            }
            if let Some(receiver_ty) = env.locals.get(receiver) {
                let resolved_receiver_ty = env.resolve_alias(receiver_ty, 0);
                if let Some(field_ty) = resolve_record_field_ty(env, &resolved_receiver_ty, member)
                {
                    return env.resolve_alias(&field_ty, 0);
                }
            }
            let qualified = format!("{}.{}", receiver, member);
            lookup_and_maybe_freshen_ctor(env, &qualified, next_id)
        }
        Expr::RecordConstruct {
            constructor,
            fields,
            ..
        } => infer_record_construct_ty(env, constructor, fields, next_id),
        Expr::UnaryOp { op, expr } => {
            let inner = infer_expr_ty(expr, env, next_id);
            match (op, &inner) {
                (crate::ast::UnaryOpKind::Not, Ty::Bool) => Ty::Bool,
                (_, Ty::Unknown) => Ty::Unknown,
                _ => Ty::Unknown,
            }
        }
        Expr::BinOp { op, left, right } => {
            let lt = infer_expr_ty(left, env, next_id);
            let rt = infer_expr_ty(right, env, next_id);
            match (op, &lt, &rt) {
                (BinOpKind::Or, Ty::Bool, Ty::Bool) => Ty::Bool,
                (BinOpKind::And, Ty::Bool, Ty::Bool) => Ty::Bool,
                (BinOpKind::Add, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Sub, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Mul, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Div, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Mod, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::BitXor, Ty::Int, Ty::Int) => Ty::Int,
                // `+`, `-`, `*`, `/` overload on operand type:
                // `Float, Float -> Float`; the integer arithmetic above
                // is unchanged. Mixed `Int` / `Float` operands fall
                // through to the diagnostic path.
                (BinOpKind::Add, Ty::Float, Ty::Float) => Ty::Float,
                (BinOpKind::Sub, Ty::Float, Ty::Float) => Ty::Float,
                (BinOpKind::Mul, Ty::Float, Ty::Float) => Ty::Float,
                (BinOpKind::Div, Ty::Float, Ty::Float) => Ty::Float,
                (BinOpKind::Eq, _, _) if is_equality_comparable(env, &lt, &rt) => Ty::Bool,
                (BinOpKind::Lt, Ty::Int, Ty::Int) => Ty::Bool,
                (BinOpKind::Gt, Ty::Int, Ty::Int) => Ty::Bool,
                (BinOpKind::Le, Ty::Int, Ty::Int) => Ty::Bool,
                (BinOpKind::Ge, Ty::Int, Ty::Int) => Ty::Bool,
                // Ordering comparisons on `Float` produce `Bool`. NaN
                // handling follows IEEE 754 at runtime.
                (BinOpKind::Lt, Ty::Float, Ty::Float) => Ty::Bool,
                (BinOpKind::Gt, Ty::Float, Ty::Float) => Ty::Bool,
                (BinOpKind::Le, Ty::Float, Ty::Float) => Ty::Bool,
                (BinOpKind::Ge, Ty::Float, Ty::Float) => Ty::Bool,
                (_, Ty::Unknown, _) | (_, _, Ty::Unknown) => Ty::Unknown,
                _ => Ty::Unknown,
            }
        }
        Expr::Call { callee, arg, .. } => {
            if let Expr::Var { name, .. } = callee.as_ref()
                && let Some(record) = env.lookup_record_by_constructor(name)
                && record.fields.len() == 1
            {
                let field_name = record.fields.keys().next().unwrap().clone();
                let rewritten = Expr::RecordConstruct {
                    constructor: name.clone(),
                    fields: vec![(field_name, *arg.clone())],
                    span: None,
                };
                return infer_expr_ty(&rewritten, env, next_id);
            }
            // CA-1: callee の `Ty::Fun.result` 直返しでは call-site unify が
            // 反映されないため、`typecheck_call::infer_call_result_ty` を経由
            // して引数を unify した結果を返す。後続 CA-3b で `Just 42` が
            // `Maybe Int` に具体化される前提となる経路。
            // CA-3a: `next_id` を共有して call resolver と本関数の他の経路
            // が同じ counter を見るようにする。
            crate::typecheck_call::infer_call_result_ty(expr, env, next_id)
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
        Expr::Pipeline {
            value: _, callee, ..
        } => {
            let callee_ty = env.lookup(callee);
            match callee_ty {
                Ty::Fun { result, .. } => *result,
                _ => Ty::Unknown,
            }
        }
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            let result = infer_expr_ty(body, &child_env, next_id);
            Ty::Fun {
                params: vec![Ty::Unknown],
                result: Box::new(result),
                // EP-1c: lambda effect inference is not implemented yet.
                // EP-1d/EP-2 will infer the residual row from the body.
                effects: EffectRow::closed_empty(),
            }
        }
        Expr::Handler { clauses } => Ty::Handler {
            covered_ops: infer_handler_covered_ops_lenient(clauses, env),
        },
        Expr::With { .. } => Ty::Unknown,
        Expr::Resume { value } => {
            let _ = infer_expr_ty(value, env, next_id);
            Ty::Unknown
        }
        Expr::Block(stmts) => infer_block_expr_ty(stmts, env, next_id),
        Expr::Case { scrutinee, arms } => {
            let scrutinee_ty = infer_expr_ty(scrutinee, env, next_id);
            let mut merged: Option<Ty> = None;
            for arm in arms {
                // CP (Codex pass-2): pattern binder の freshen で消費した
                // counter を arm body に引き継ぐため、`*_using` 版を使う。
                let arm_env =
                    env_with_case_pattern_bindings_using(env, &arm.pattern, &scrutinee_ty, next_id);
                let arm_ty = infer_expr_ty(&arm.body, &arm_env, next_id);
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
            let _ = infer_expr_ty(condition, env, next_id);
            let then_ty = infer_expr_ty(then_expr, env, next_id);
            let else_ty = infer_expr_ty(else_expr, env, next_id);
            merge_branch_type(env, then_ty, else_ty)
        }
        Expr::ListIndex { list, index } => {
            let list_ty = env.resolve_alias(&infer_expr_ty(list, env, next_id), 0);
            let _index_ty = infer_expr_ty(index, env, next_id);
            match list_ty {
                Ty::List(elem_ty) => *elem_ty,
                Ty::Unknown => Ty::Unknown,
                _ => Ty::Unknown,
            }
        }
    }
}

/// GU-S3 CA-3b: ctor lookup を `freshen_type_scheme` で 1 回 freshen する。
///
/// non-generic ctor (例: `Red`, `Box`) は `ty_contains_type_var` が false の
/// ため freshen は no-op、戻り値は lookup そのもの。generic ctor (例: `Just`,
/// `Nothing` for `Maybe a`) は declaration-side `Ty::Var(a)` 等を含む
/// `Ty::Fun { params, result }` または `Ty::Con { name, args }` で登録
/// されているので、各 use site で fresh な `__goby_fresh_ty_N` に置換される。
///
/// 二重 freshen 回避は `instantiate_ty_with_fresh_type_vars` の Var arm で
/// idempotent 化 (CA-3b 同コミット): prefix 一致の Var は no-op で返るので、
/// 後続の call resolver `instantiate_ty_with_fresh_type_vars_for_call_site`
/// が同じ Var を再度 freshen しようとしても影響しない。
///
/// locals shadowing は `is_ctor_binding` が `locals.contains_key(name)` で
/// 先に false を返すため、ローカル束縛で同名 binding がある場合は freshen
/// しない (= 通常の `env.lookup(name)`)。
fn lookup_and_maybe_freshen_ctor(env: &TypeEnv, name: &str, next_id: &mut usize) -> Ty {
    let ty = env.lookup(name);
    if !env.is_ctor_binding(name) {
        return ty;
    }
    if !crate::typecheck_unify::ty_contains_type_var(&ty) {
        return ty;
    }
    crate::typecheck_unify::freshen_type_scheme(&[ty], next_id)
        .pop()
        .expect("freshen_type_scheme always returns one ty per template")
}

fn is_equality_comparable(env: &TypeEnv, left: &Ty, right: &Ty) -> bool {
    let left = env.resolve_alias(left, 0);
    let right = env.resolve_alias(right, 0);
    if !env.are_compatible(&left, &right) || !env.are_compatible(&right, &left) {
        return false;
    }
    equality_type_supported(&left)
}

fn equality_type_supported(ty: &Ty) -> bool {
    match ty {
        // `Float` participates in `==`, `<`, etc. via the operator
        // dispatch above. NaN comparisons follow IEEE 754
        // (`NaN == NaN == False`); the typechecker only verifies type
        // compatibility here.
        Ty::Int | Ty::Float | Ty::Bool | Ty::Str | Ty::Unit => true,
        Ty::List(inner) => equality_type_supported(inner),
        Ty::Tuple(items) => items.iter().all(equality_type_supported),
        Ty::Con { args, .. } => args.iter().all(equality_type_supported),
        Ty::Var(_) | Ty::Fun { .. } | Ty::Handler { .. } | Ty::Unknown => false,
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
                span: best_available_expr_span(element),
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
                    span: best_available_expr_span(tail_expr),
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
                span: best_available_expr_span(tail_expr),
                message: format!(
                    "list spread tail must be `List {}` but got `{}`",
                    ty_name(&expected),
                    ty_name(&other)
                ),
            })
        }
    }
}

pub(crate) fn check_list_index_constraints(
    list: &Expr,
    index: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    let list_ty = env.resolve_alias(&check_expr(list, env), 0);
    match &list_ty {
        Ty::List(_) | Ty::Unknown | Ty::Var(_) => {}
        other => {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: best_available_expr_span(list),
                message: format!(
                    "list index requires a `List` receiver, but got `{}`",
                    ty_name(other)
                ),
            });
        }
    }

    let index_ty = env.resolve_alias(&check_expr(index, env), 0);
    match &index_ty {
        Ty::Int | Ty::Unknown | Ty::Var(_) => {}
        other => {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: best_available_expr_span(index),
                message: format!("list index must be `Int`, but got `{}`", ty_name(other)),
            });
        }
    }

    Ok(())
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
                effects: l_eff,
            },
            Ty::Fun {
                params: rps,
                result: rr,
                effects: r_eff,
            },
        ) if lps.len() == rps.len() => {
            let merged_params = lps
                .into_iter()
                .zip(rps)
                .map(|(l, r)| merge_branch_type(env, l, r))
                .collect();
            // EP-1d: keep the row only when both branches agree on it.
            // Row union is not yet specified (LANGUAGE_SPEC §5 leaves
            // multi-branch joins implicit), so when rows differ we collapse
            // the entire merged Ty::Fun to Unknown rather than silently
            // dropping effects.
            if l_eff != r_eff {
                return Ty::Unknown;
            }
            Ty::Fun {
                params: merged_params,
                result: Box::new(merge_branch_type(env, *lr, *rr)),
                effects: l_eff,
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
                effects: l_eff,
            },
            Ty::Fun {
                params: rps,
                result: rr,
                effects: r_eff,
            },
        ) if lps.len() == rps.len() => {
            // EP-1d: branch types must agree on params, result, and the
            // residual effect row. Row union is not part of LANGUAGE_SPEC §5
            // yet, so we require equality for now.
            l_eff == r_eff
                && lps
                    .iter()
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

fn infer_block_expr_ty(stmts: &[Stmt], env: &TypeEnv, next_id: &mut usize) -> Ty {
    let mut local_env = env.clone();
    let mut last_expr_ty = Ty::Unknown;
    let mut has_tail_expr = false;
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value, .. } | Stmt::MutBinding { name, value, .. } => {
                let ty = infer_expr_ty(value, &local_env, next_id);
                local_env.locals.insert(name.clone(), ty);
                has_tail_expr = false;
            }
            Stmt::Assign { value, .. } => {
                let _ = infer_expr_ty(value, &local_env, next_id);
                has_tail_expr = false;
            }
            Stmt::Expr(expr, _) => {
                last_expr_ty = infer_expr_ty(expr, &local_env, next_id);
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

/// CP: pattern binder の env を作る入り口 (旧シグネチャ)。
///
/// `next_id` を持たない caller (`typecheck_ambiguity` / `typecheck_branch` /
/// `typecheck_effect_usage` / `typecheck_resume`) はこちらを使う。内部で
/// ローカル `next_id` を seed して使い切り、消費結果は外に伝わらない。
/// これらの caller は pattern bindings の **env を作るだけ** で、その後の
/// expr 推論には流れないため、`next_id` を共有する必要がない。
pub(crate) fn env_with_case_pattern_bindings(
    env: &TypeEnv,
    pattern: &CasePattern,
    scrutinee_ty: &Ty,
) -> TypeEnv {
    let mut next_id = crate::typecheck_unify::next_fresh_ty_id_seed(env)
        .max(crate::typecheck_unify::max_fresh_ty_id_in_ty(scrutinee_ty))
        .max(crate::typecheck_env::max_fresh_row_id(scrutinee_ty));
    env_with_case_pattern_bindings_using(env, pattern, scrutinee_ty, &mut next_id)
}

/// CP (Codex pass-2 指摘): caller の `&mut next_id` を thread して、
/// pattern binder で消費した fresh counter を arm body 推論に引き継ぐ
/// 入り口。`infer_expr_ty` の `Expr::Case` arm が使う。
///
/// 旧 entry (`env_with_case_pattern_bindings`) はこちらの薄い wrapper で
/// あり、`next_id` を持たない caller も挙動を変えずに通すために残す。
pub(crate) fn env_with_case_pattern_bindings_using(
    env: &TypeEnv,
    pattern: &CasePattern,
    scrutinee_ty: &Ty,
    next_id: &mut usize,
) -> TypeEnv {
    // Codex CP pass-2 指摘: 渡された counter よりさらに進んだ最大値で
    // seed しなおす。caller 側が CP の binder freshen で消費した fresh
    // 名を使い回さないように。env / scrutinee_ty の両方を加味する。
    *next_id = (*next_id)
        .max(crate::typecheck_unify::next_fresh_ty_id_seed(env))
        .max(crate::typecheck_unify::max_fresh_ty_id_in_ty(scrutinee_ty))
        .max(crate::typecheck_env::max_fresh_row_id(scrutinee_ty));
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
    if let CasePattern::Ctor {
        type_qualifier,
        ctor,
        args,
    } = pattern
    {
        let resolved = resolve_ctor_pattern_binder_tys(
            env,
            type_qualifier.as_deref(),
            ctor,
            args,
            scrutinee_ty,
            next_id,
        );
        for (arg, resolved_ty) in args.iter().zip(resolved.iter()) {
            if let CtorPatternArg::Bind(name) = arg
                && name != "_"
            {
                child.locals.insert(name.clone(), resolved_ty.clone());
            }
        }
    }
    child
}

/// GU-S3 constructor-pattern binder inference: given a `CasePattern::Ctor`
/// and the scrutinee's type, return a `Vec<Ty>` (one per pattern arg)
/// holding each binder's inferred type.
///
/// Implementation:
/// 1. Look up the union variant via `TypeEnv::lookup_union_variant`. Unknown
///    ctor → all-`Ty::Unknown` (= pre-GU-S3 behaviour).
/// 2. Wrong-arity (`args.len() != variant.arg_types.len()`) also falls back
///    to all-`Ty::Unknown`, matching the existing tolerant pattern walker
///    (the dedicated wrong-arity diagnostic is a follow-up).
/// 3. Build a single declaration-side scheme `[result_template,
///    arg_type_0, arg_type_1, ...]` and feed it to `freshen_type_scheme`
///    so every `Ty::Var(a)` shared between the result template and any arg
///    type maps to one fresh name.
/// 4. Try `unify_types_with_subst(freshened_result, scrutinee_ty, ...)`.
///    On success, `apply_type_substitution` rewrites the freshened arg
///    types into concrete binder types. On failure (or when the scrutinee
///    is `Ty::Unknown` / a different shape), the freshened arg types are
///    returned as-is. For non-generic unions the freshen is a no-op so
///    binders come back exactly as the original `arg_types` declared, and
///    behaviour is unchanged.
fn resolve_ctor_pattern_binder_tys(
    env: &TypeEnv,
    type_qualifier: Option<&str>,
    ctor: &str,
    args: &[CtorPatternArg],
    scrutinee_ty: &Ty,
    next_id: &mut usize,
) -> Vec<Ty> {
    let Some((union_info, variant_info)) = env.lookup_union_variant(type_qualifier, ctor) else {
        return vec![Ty::Unknown; args.len()];
    };
    if args.len() != variant_info.arg_types.len() {
        return vec![Ty::Unknown; args.len()];
    }
    let result_template = Ty::Con {
        name: union_info.type_name.clone(),
        args: union_info
            .type_params
            .iter()
            .map(|p| Ty::Var(p.clone()))
            .collect(),
    };
    let mut scheme: Vec<Ty> = Vec::with_capacity(1 + variant_info.arg_types.len());
    scheme.push(result_template);
    scheme.extend(variant_info.arg_types.iter().cloned());
    // Codex CP pass-1/pass-2 指摘: seed は env と scrutinee_ty の両方を
    // 加味した最大値からスタートする。caller (`Expr::Case` arm in
    // `infer_expr_ty`) は同じ `&mut next_id` を渡して、消費後の counter を
    // arm body 推論に引き継ぐ。これにより CP binder が消費した fresh ID
    // を後続の generic ctor/function freshen が再発行することがなくなる。
    *next_id = (*next_id)
        .max(crate::typecheck_unify::next_fresh_ty_id_seed(env))
        .max(crate::typecheck_unify::max_fresh_ty_id_in_ty(scrutinee_ty))
        .max(crate::typecheck_env::max_fresh_row_id(scrutinee_ty));
    let freshened = crate::typecheck_unify::freshen_type_scheme(&scheme, next_id);
    let freshened_result = freshened[0].clone();
    let freshened_arg_tys: Vec<Ty> = freshened[1..].to_vec();

    let mut subst = crate::typecheck_env::TypeSubst::new();
    let mut row_subst = crate::typecheck_env::RowSubst::new();
    let scrutinee_resolved = env.resolve_alias(scrutinee_ty, 0);
    let _ = crate::typecheck_unify::unify_types_with_subst(
        &freshened_result,
        &scrutinee_resolved,
        &mut subst,
        &mut row_subst,
        env,
        next_id,
    );
    freshened_arg_tys
        .into_iter()
        .map(|ty| crate::typecheck_unify::apply_type_substitution(&ty, &subst, &row_subst, env))
        .collect()
}

/// GU-S3 GR-2: infer the type of an `Expr::RecordConstruct`.
///
/// Mirrors the union-side `CA-3b` / `CP` flow but for a record constructor
/// expression. Builds a single declaration-side scheme
/// `[result_template, field_ty_a, field_ty_b, ...]` where the result
/// template is `Ty::Con { name: record.type_name, args: type_params as
/// Ty::Var }` and the field types come from `record.fields` taken in a
/// **name-sorted order** so the iteration is deterministic across
/// `HashMap` rehashes (matches the union side's `type_name`-sorted
/// disambiguation). `freshen_type_scheme` then maps every shared
/// `Ty::Var(p)` to one fresh name, and each field expression is unified
/// against its freshened template via `unify_types_with_subst`. The
/// resulting `TypeSubst`/`RowSubst` rewrites the freshened result
/// template into `Ty::Con { name, args: [resolved...] }`.
///
/// Non-generic records (`type_params` empty) fall through with a
/// trivially-substituted result of `Ty::Con { name, args: vec![] }`,
/// which matches the pre-GR shape exactly. Wrong-arity (provided fields
/// count mismatch) or unknown field name still returns `Ty::Unknown`,
/// preserving the existing tolerant behaviour. Wrong field type
/// (unification failure) also collapses to `Ty::Unknown`.
fn infer_record_construct_ty(
    env: &TypeEnv,
    constructor: &str,
    fields: &[(String, crate::ast::Expr)],
    next_id: &mut usize,
) -> Ty {
    let Some(record) = env.lookup_record_by_constructor(constructor) else {
        return Ty::Unknown;
    };
    if fields.len() != record.fields.len() {
        return Ty::Unknown;
    }
    // Sort field names so the iteration order is stable across HashMap
    // rehashes (analogous to the union-side type_name sort).
    let mut field_names: Vec<&String> = record.fields.keys().collect();
    field_names.sort();

    // Build the scheme: [result_template, field_ty_for_name_0, ...].
    let result_template = Ty::Con {
        name: record.type_name.clone(),
        args: record
            .type_params
            .iter()
            .map(|p| Ty::Var(p.clone()))
            .collect(),
    };
    let mut scheme: Vec<Ty> = Vec::with_capacity(1 + field_names.len());
    scheme.push(result_template);
    for fname in &field_names {
        scheme.push(record.fields[*fname].clone());
    }

    // Seed `next_id` past existing fresh names in env (CA-3a contract);
    // there is no scrutinee to consider here, unlike the pattern side.
    *next_id = (*next_id).max(crate::typecheck_unify::next_fresh_ty_id_seed(env));
    let freshened = crate::typecheck_unify::freshen_type_scheme(&scheme, next_id);
    let freshened_result = freshened[0].clone();
    let freshened_field_tys: Vec<Ty> = freshened[1..].to_vec();

    let mut subst = crate::typecheck_env::TypeSubst::new();
    let mut row_subst = crate::typecheck_env::RowSubst::new();

    // Build a name -> provided-expr map so we can iterate in the sorted
    // order while still picking up whatever ordering the source used.
    let mut provided: HashMap<&str, &crate::ast::Expr> = HashMap::new();
    for (name, value) in fields {
        provided.insert(name.as_str(), value);
    }

    for (fname, freshened_field_ty) in field_names.iter().zip(freshened_field_tys.iter()) {
        let Some(value) = provided.get(fname.as_str()) else {
            // A declared field is not provided by the source — treat as
            // wrong-arity / unknown-field (consistent with the legacy
            // `record.fields.get(name)` miss case).
            return Ty::Unknown;
        };
        let actual_ty = infer_expr_ty(value, env, next_id);
        if actual_ty == Ty::Unknown {
            // Treat `Ty::Unknown` actual as a wildcard; the legacy
            // `are_compatible` short-circuited on `Ty::Unknown` in the
            // same direction, so preserve that to avoid spurious rejects.
            continue;
        }
        let expected_after_subst = crate::typecheck_unify::apply_type_substitution(
            freshened_field_ty,
            &subst,
            &row_subst,
            env,
        );
        if !crate::typecheck_unify::unify_types_with_subst(
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

    crate::typecheck_unify::apply_type_substitution(&freshened_result, &subst, &row_subst, env)
}

/// GU-S3 GR-3: resolve a record field type against a concrete receiver
/// `Ty::Con { name, args }`.
///
/// For a non-generic record (`type_params` empty), this returns the
/// declared field type unchanged — matching the pre-GR behaviour exactly.
///
/// For a generic record (e.g. `Box(value: a)` against a receiver
/// `Ty::Con { name: "Box", args: [Ty::Int] }`), the helper builds a
/// `TypeSubst` mapping each declared `type_params[i]` to `args[i]`, then
/// applies it to the field template via `apply_type_substitution`. So
/// `v.value` where `v : Box Int` resolves to `Int` instead of leaving
/// `Ty::Var("a")` leaking out.
///
/// Returns `None` when the receiver is not a `Ty::Con` shape known to
/// have a record declaration with the requested field; callers preserve
/// their existing fallthrough (Qualified arm falls back to
/// `env.lookup(qualified)` which routes ctor lookups through the CA-3b
/// freshen path).
fn resolve_record_field_ty(env: &TypeEnv, receiver_ty: &Ty, field: &str) -> Option<Ty> {
    let Ty::Con { name, args } = receiver_ty else {
        return None;
    };
    let record_info = env
        .record_types
        .values()
        .find(|info| info.type_name == *name)?;
    let field_template = record_info.fields.get(field)?.clone();
    if record_info.type_params.is_empty() {
        return Some(field_template);
    }
    if record_info.type_params.len() != args.len() {
        // Arity mismatch — fall back to the un-substituted template. This
        // matches the legacy `record_field_ty` behaviour (no substitution
        // was performed before GR-3 either).
        return Some(field_template);
    }
    let mut subst = crate::typecheck_env::TypeSubst::new();
    for (param, arg) in record_info.type_params.iter().zip(args.iter()) {
        subst.insert(param.clone(), arg.clone());
    }
    let row_subst = crate::typecheck_env::RowSubst::new();
    Some(crate::typecheck_unify::apply_type_substitution(
        &field_template,
        &subst,
        &row_subst,
        env,
    ))
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
        // For qualified names like "Log.log", extract the bare op name.
        let bare_name = if let Some((_e, op)) = clause.name.split_once('.') {
            op
        } else {
            &clause.name
        };
        covered.insert(bare_name.to_string());
        covered.insert(format!("{}.{}", effect, bare_name));
    }
    covered
}

pub(crate) fn effect_candidates_for_operation(env: &TypeEnv, op_name: &str) -> Vec<String> {
    // If this is a qualified name like "Effect.op", resolve directly without env lookup.
    if let Some((effect, _op)) = op_name.split_once('.') {
        return vec![effect.to_string()];
    }

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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::ast::Expr;
    use crate::parse_module;
    use crate::typecheck::typecheck_module;
    use crate::typecheck_env::{RecordTypeInfo, Ty, TypeEnv};

    use super::{check_expr, infer_expr_ty};

    #[test]
    fn resolves_record_field_access_for_alias_receiver_type() {
        let mut type_aliases = HashMap::new();
        type_aliases.insert(
            "UserAlias".to_string(),
            Ty::Con {
                name: "User".to_string(),
                args: Vec::new(),
            },
        );
        let mut fields = HashMap::new();
        fields.insert("name".to_string(), Ty::Str);
        let mut record_types = HashMap::new();
        record_types.insert(
            "User".to_string(),
            RecordTypeInfo {
                type_name: "User".to_string(),
                type_params: Vec::new(),
                constructor: "User".to_string(),
                fields,
            },
        );
        let mut locals = HashMap::new();
        locals.insert(
            "user".to_string(),
            Ty::Con {
                name: "UserAlias".to_string(),
                args: Vec::new(),
            },
        );
        let env = TypeEnv {
            locals,
            type_aliases,
            record_types,
            ..TypeEnv::empty()
        };
        let expr = Expr::Qualified {
            receiver: "user".to_string(),
            member: "name".to_string(),
            span: None,
        };
        assert_eq!(check_expr(&expr, &env), Ty::Str);
    }

    #[test]
    fn resolves_tuple_member_access_by_index() {
        let mut locals = HashMap::new();
        locals.insert("pair".to_string(), Ty::Tuple(vec![Ty::Bool, Ty::Int]));
        let env = TypeEnv {
            locals,
            ..TypeEnv::empty()
        };
        let expr = Expr::Qualified {
            receiver: "pair".to_string(),
            member: "1".to_string(),
            span: None,
        };
        assert_eq!(check_expr(&expr, &env), Ty::Int);
    }

    #[test]
    fn tuple_member_access_out_of_range_is_unknown() {
        let mut locals = HashMap::new();
        locals.insert("pair".to_string(), Ty::Tuple(vec![Ty::Bool]));
        let env = TypeEnv {
            locals,
            ..TypeEnv::empty()
        };
        let expr = Expr::Qualified {
            receiver: "pair".to_string(),
            member: "1".to_string(),
            span: None,
        };
        assert_eq!(check_expr(&expr, &env), Ty::Unknown);
    }

    #[test]
    fn rejects_numeric_qualified_access_on_non_tuple_receiver() {
        let source = "\
type Status = Ready | Busy
main : Unit -> Unit
main =
  print Status.0
";
        let module = parse_module(source).expect("should parse");
        let err =
            typecheck_module(&module).expect_err("numeric member access on non-tuple should fail");
        assert!(
            err.message.contains("tuple member access"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn accepts_numeric_qualified_access_on_global_tuple_receiver() {
        let source = "\
import goby/int as int
pair : (Bool, Int)
pair = (True, 42)
main : Unit -> Unit can Print
main =
  if pair.0
    print (int.to_string pair.1)
  else
    print (int.to_string 0)
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("numeric member access on tuple-typed global should typecheck");
    }

    #[test]
    fn accepts_list_spread_annotation_matching_list_literal_body() {
        let module = parse_module("xs : List Int\nxs = [1, ..[2, 3]]\n").expect("should parse");
        typecheck_module(&module).expect("list spread body should typecheck");
    }

    #[test]
    fn rejects_list_spread_tail_when_tail_is_not_list() {
        let module = parse_module("xs : List Int\nxs = [1, ..2]\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("non-list spread tail should fail");
        assert_eq!(err.declaration.as_deref(), Some("xs"));
        assert!(
            err.message.contains("list spread tail must be"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_list_spread_prefix_element_type_mismatch() {
        let module = parse_module("xs : List Int\nxs = [1, \"x\", ..[2]]\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("spread prefix mismatch should fail");
        assert_eq!(err.declaration.as_deref(), Some("xs"));
        assert!(
            err.message
                .contains("list spread prefix element type mismatch"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_list_spread_tail_element_type_mismatch() {
        let module = parse_module("xs : List Int\nxs = [1, ..[\"x\"]]\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("spread tail element mismatch should fail");
        assert_eq!(err.declaration.as_deref(), Some("xs"));
        assert!(
            err.message
                .contains("list spread tail element type mismatch"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn infers_string_literal_type() {
        let module = parse_module("s : String\ns = \"hello\"\n").expect("should parse");
        typecheck_module(&module).expect("string literal body should typecheck");
    }

    #[test]
    fn infers_interpolated_string_type() {
        let module = parse_module("s : String\ns = \"n=${1}\"\n").expect("should parse");
        typecheck_module(&module).expect("interpolated string body should typecheck as String");
    }

    #[test]
    fn list_index_infers_element_type() {
        let module = parse_module("xs : List Int\nxs = [1, 2, 3]\nv : Int\nv = xs[0]\n")
            .expect("should parse");
        typecheck_module(&module).expect("list index result should typecheck as Int");
    }

    #[test]
    fn list_index_rejects_non_int_index() {
        let module = parse_module("xs : List Int\nxs = [1, 2]\nv : Int\nv = xs[True]\n")
            .expect("should parse");
        let err = typecheck_module(&module).expect_err("bool index should fail");
        assert!(
            err.message.contains("list index must be `Int`"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn list_index_rejects_non_list_receiver() {
        let module = parse_module("n : Int\nn = 42\nv : Int\nv = n[0]\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("int receiver index should fail");
        assert!(
            err.message
                .contains("list index requires a `List` receiver"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn infers_equality_as_bool() {
        let module = parse_module("flag : Bool\nflag = 1 == 1\n").expect("should parse");
        typecheck_module(&module).expect("equality result should typecheck as Bool");
    }

    #[test]
    fn infers_string_equality_as_bool() {
        let module = parse_module("flag : Bool\nflag = \"a\" == \"a\"\n").expect("should parse");
        typecheck_module(&module).expect("string equality result should typecheck as Bool");
    }

    #[test]
    fn infers_bool_equality_as_bool() {
        let module = parse_module("flag : Bool\nflag = True == False\n").expect("should parse");
        typecheck_module(&module).expect("bool equality result should typecheck as Bool");
    }

    #[test]
    fn infers_int_comparison_family() {
        let module = parse_module(
            "a : Bool\na = 3 > 2\nb : Bool\nb = 3 < 2\nc : Bool\nc = 3 >= 2\nd : Bool\nd = 3 <= 2\n",
        )
        .expect("should parse");
        typecheck_module(&module).expect("comparison family should typecheck as Bool");
    }

    #[test]
    fn infers_sub_div_mod_as_int() {
        let module = parse_module("a : Int\na = 5 - 1\nb : Int\nb = 5 / 2\nc : Int\nc = 5 % 2\n")
            .expect("should parse");
        typecheck_module(&module).expect("sub/div/mod should typecheck as Int");
    }

    #[test]
    fn check_expr_infers_addition() {
        let env = TypeEnv::empty();
        let expr = Expr::BinOp {
            op: crate::BinOpKind::Add,
            left: Box::new(Expr::IntLit(1)),
            right: Box::new(Expr::IntLit(2)),
        };
        assert_eq!(check_expr(&expr, &env), Ty::Int);
    }

    // ---- GU-S3 CA-1: `Expr::Call` arm が call-site unify を経由することを pin ----
    //
    // 旧 `match callee_ty { Ty::Fun { result, .. } => *result, _ => Ty::Unknown }`
    // は partial application / multi-arg ctor を粗く unwrap して latent な
    // 不正パターンを `Ty::Unknown` 経由で素通ししていた。新 entry
    // `typecheck_call::infer_call_result_ty` は `resolve_function_value_ty` を
    // 経由するので、partial application は残り params を `Ty::Fun` として返し、
    // 引数不一致は `Ty::Unknown` を返す。本コミットでは、CA-2/CA-3b で
    // ctor 登録 template を入れる前段としての挙動を以下で固定する。

    #[test]
    fn call_partial_application_returns_function_type_not_final_result() {
        // GU-S3 CA-1: 2-引数関数を 1 引数だけ与えて部分適用すると、戻り値
        // 型は「残り params -> result」の `Ty::Fun` として返る。旧
        // `match callee_ty { Ty::Fun { result, .. } => *result, _ =>
        // Ty::Unknown }` は最終 result (`Int`) を返してしまい partial
        // application を識別できなかった。AST 直書きで `check_expr` の
        // 戻り値を assert することで、`infer_call_result_ty` 経由の
        // partial application 挙動を pin する。
        let mut globals = HashMap::new();
        globals.insert(
            "add".to_string(),
            crate::typecheck_env::GlobalBinding::Resolved {
                ty: Ty::Fun {
                    params: vec![Ty::Int, Ty::Int],
                    result: Box::new(Ty::Int),
                    effects: crate::typecheck_env::EffectRow::closed_empty(),
                },
                source: "test".to_string(),
            },
        );
        let env = TypeEnv {
            globals,
            ..TypeEnv::empty()
        };
        let expr = Expr::Call {
            callee: Box::new(Expr::Var {
                name: "add".to_string(),
                span: None,
            }),
            arg: Box::new(Expr::IntLit(1)),
            span: None,
        };
        let result = check_expr(&expr, &env);
        match result {
            Ty::Fun { params, result, .. } => {
                assert_eq!(params, vec![Ty::Int]);
                assert_eq!(*result, Ty::Int);
            }
            other => panic!(
                "expected `Int -> Int` partial application type, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn check_expr_seeds_next_id_from_env_locals_and_globals() {
        // GU-S3 CA-3a: `check_expr` wrapper は `next_fresh_ty_id_seed(env)`
        // で seed する。env の locals / globals に既存の `__goby_fresh_ty_N`
        // が含まれていたら、新規 fresh は N+1 以上を取る (= 衝突しない)。
        // ここでは `add` の partial application で counter が回り、結果型
        // (`Int -> Int`) は fresh 名を含まないため挙動は不変。env に既存
        // fresh 名を含めても外向きの戻り値は同じ。
        let mut globals = HashMap::new();
        globals.insert(
            "add".to_string(),
            crate::typecheck_env::GlobalBinding::Resolved {
                ty: Ty::Fun {
                    params: vec![Ty::Int, Ty::Int],
                    result: Box::new(Ty::Int),
                    effects: crate::typecheck_env::EffectRow::closed_empty(),
                },
                source: "test".to_string(),
            },
        );
        let mut locals = HashMap::new();
        locals.insert(
            "spilled".to_string(),
            Ty::Var("__goby_fresh_ty_7".to_string()),
        );
        let env = TypeEnv {
            globals,
            locals,
            ..TypeEnv::empty()
        };
        let expr = Expr::Call {
            callee: Box::new(Expr::Var {
                name: "add".to_string(),
                span: None,
            }),
            arg: Box::new(Expr::IntLit(1)),
            span: None,
        };
        match check_expr(&expr, &env) {
            Ty::Fun { params, result, .. } => {
                assert_eq!(params, vec![Ty::Int]);
                assert_eq!(*result, Ty::Int);
            }
            other => panic!("expected `Int -> Int` partial app, got {:?}", other),
        }
    }

    #[test]
    fn infer_expr_ty_advances_shared_next_id_through_nested_calls() {
        // GU-S3 CA-3a: `Expr::Call` arm が `next_id` を `infer_call_result_ty`
        // に渡すことで、ネストした Call の resolver 経由 freshening が同じ
        // counter を共有する。
        // `add` ctor を `Var "a"` 由来のテンプレートを持つ偽装 ctor として
        // 登録し、call resolver の `instantiate_ty_with_fresh_type_vars_for_call_site`
        // が `__goby_fresh_ty_N` を作る挙動を pin。next_id は 0 → 1 (a の
        // freshening 1 件 + apply_type_substitution で消費) と進む。
        let mut globals = HashMap::new();
        globals.insert(
            "id".to_string(),
            crate::typecheck_env::GlobalBinding::Resolved {
                ty: Ty::Fun {
                    params: vec![Ty::Var("a".to_string())],
                    result: Box::new(Ty::Var("a".to_string())),
                    effects: crate::typecheck_env::EffectRow::closed_empty(),
                },
                source: "test".to_string(),
            },
        );
        let env = TypeEnv {
            globals,
            ..TypeEnv::empty()
        };
        // `id 1` — `a` は `Int` に bind され、結果は `Int`。
        let expr = Expr::Call {
            callee: Box::new(Expr::Var {
                name: "id".to_string(),
                span: None,
            }),
            arg: Box::new(Expr::IntLit(1)),
            span: None,
        };
        let mut next_id = 0;
        assert_eq!(infer_expr_ty(&expr, &env, &mut next_id), Ty::Int);
        // resolver の `instantiate_ty_with_fresh_type_vars_for_call_site`
        // が `a` を 1 つ freshen するので next_id が前進している。
        assert!(
            next_id > 0,
            "next_id must advance past 0 after freshening `a`"
        );
    }

    #[test]
    fn call_invalid_arg_type_yields_unknown_through_resolver() {
        // GU-S3 CA-1: `resolve_function_value_ty` で引数型が unify できない
        // call は `Ty::Unknown` を返す。`check_expr` を AST 直書きで呼んで
        // 戻り値そのものを assert する。`validate_call_chain` 経路を経ない
        // 単独評価で、`infer_call_result_ty` の `Ty::Unknown` 帰着を pin。
        let mut globals = HashMap::new();
        globals.insert(
            "add".to_string(),
            crate::typecheck_env::GlobalBinding::Resolved {
                ty: Ty::Fun {
                    params: vec![Ty::Int, Ty::Int],
                    result: Box::new(Ty::Int),
                    effects: crate::typecheck_env::EffectRow::closed_empty(),
                },
                source: "test".to_string(),
            },
        );
        let env = TypeEnv {
            globals,
            ..TypeEnv::empty()
        };
        // `add True` — Bool を Int 期待 param に渡す、partial application。
        let expr = Expr::Call {
            callee: Box::new(Expr::Var {
                name: "add".to_string(),
                span: None,
            }),
            arg: Box::new(Expr::BoolLit(true)),
            span: None,
        };
        assert_eq!(check_expr(&expr, &env), Ty::Unknown);
    }

    // ---- GU-S3 CA-3b: generic union ctor application を pin ----

    #[test]
    fn typechecks_generic_union_ctor_just_int_against_maybe_int() {
        // GU-S3 CA-3b acceptance: `Just 42 : Maybe Int` が typecheck される。
        let source = "\
type Maybe a = Just(a) | Nothing
v : Maybe Int
v = Just 42
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`Just 42 : Maybe Int` should typecheck");
    }

    #[test]
    fn typechecks_generic_union_nullary_ctor_against_maybe_int() {
        // GU-S3 CA-3b acceptance: `Nothing : Maybe Int` が typecheck される。
        // Nothing は nullary なので Var arm の freshen が `Ty::Con { name:
        // "Maybe", args: [__goby_fresh_ty_0] }` を返し、`Maybe Int` 注釈
        // との `unifies_with_annotation` で `Int` に bind される。
        let source = "\
type Maybe a = Just(a) | Nothing
v : Maybe Int
v = Nothing
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`Nothing : Maybe Int` should typecheck");
    }

    #[test]
    fn typechecks_two_independent_just_call_sites_in_same_module() {
        // GU-S3 CA-3b 必須要件 (Codex pass-3): `Just 1 : Maybe Int` と
        // `Just "x" : Maybe String` を同一モジュール内で独立に typecheck。
        // 各 call site で `freshen_type_scheme` が異なる `__goby_fresh_ty_N`
        // を取り、互いの bind が干渉しない。
        let source = "\
type Maybe a = Just(a) | Nothing
ints : Maybe Int
ints = Just 1
strs : Maybe String
strs = Just \"x\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("two `Just` call sites should typecheck independently");
    }

    #[test]
    fn typechecks_nested_generic_union_ctor_just_nothing() {
        // GU-S3 CA-3b acceptance: `Just Nothing : Maybe (Maybe Int)`。
        // ネストした ctor 適用で内側 `Nothing` と外側 `Just` がそれぞれ別
        // scope の fresh 名を取る。
        let source = "\
type Maybe a = Just(a) | Nothing
v : Maybe (Maybe Int)
v = Just Nothing
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`Just Nothing : Maybe (Maybe Int)` should typecheck");
    }

    #[test]
    fn typechecks_generic_union_with_two_type_parameters() {
        // GU-S3 CA-3b acceptance: 多パラメータ template (`Pair Int String`)。
        let source = "\
type Pair a b = Pair(a, b)
v : Pair Int String
v = Pair 1 \"x\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`Pair 1 \"x\" : Pair Int String` should typecheck");
    }

    #[test]
    fn typechecks_qualified_generic_union_ctor() {
        // GU-S3 CA-3b: `Maybe.Just 42 : Maybe Int` (Qualified arm)。
        let source = "\
type Maybe a = Just(a) | Nothing
v : Maybe Int
v = Maybe.Just 42
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`Maybe.Just 42 : Maybe Int` should typecheck");
    }

    #[test]
    fn typechecks_qualified_generic_union_nullary_ctor() {
        // GU-S3 CA-3b: `Maybe.Nothing : Maybe Int` (Qualified arm, nullary)。
        let source = "\
type Maybe a = Just(a) | Nothing
v : Maybe Int
v = Maybe.Nothing
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`Maybe.Nothing : Maybe Int` should typecheck");
    }

    #[test]
    fn non_generic_union_ctor_still_typechecks_as_before() {
        // GU-S3 CA-3b regression: non-generic union ctor は freshen 経路を
        // 通らず (ty_contains_type_var が false)、挙動完全不変。
        let source = "\
type Color = Red | Blue
v : Color
v = Red
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("non-generic ctor `Red : Color` should still typecheck");
    }

    #[test]
    fn non_generic_arg_bearing_union_ctor_still_typechecks_as_before() {
        // GU-S3 CA-3b regression: arg-bearing non-generic ctor も挙動不変。
        let source = "\
type Box = Box(Int)
v : Box
v = Box 42
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`Box 42 : Box` should still typecheck");
    }

    #[test]
    fn rejects_generic_union_ctor_with_wrong_arg_type() {
        // GU-S3 CA-3b 必須要件 (Codex pass-3): generic ctor の wrong-type
        // が reject される。文言の厳密一致は要求しない (CA-1 と同じく汎用
        // function-call 診断を再利用)。
        let source = "\
type Maybe a = Just(a) | Nothing
v : Maybe Int
v = Just \"oops\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("`Just \"oops\" : Maybe Int` should be rejected by call-site unify");
        // CA-3b 時点では具体的な reject 経路 (validate_call_chain のような
        // call validator が拾うか、return-type unification が拾うか) は
        // 実装の都合で揺れる。reject されることだけを pin。
        assert!(
            !err.message.is_empty(),
            "should produce a non-empty error message"
        );
    }

    #[test]
    fn generic_union_ctor_extra_args_currently_pass_silently() {
        // GU-S3 CA-3b deferred (Codex pass-3 で out-of-scope 化された
        // "generic ctor の arity/wrong-arg diagnostic 文言改善")。`Just 1 2`
        // のように params 数を超える引数を渡すと、`validate_call_chain` の
        // `args.get(idx)` が `None` を返した時点で `Ok(())` で抜ける現状
        // 挙動 (= silent pass) は GU-S3 以前からの既存挙動。本テストは
        // CA-3b で挙動を変えていないことの regression pin。reject 化は
        // 後続サブタスクで取り組む。
        let source = "\
type Maybe a = Just(a) | Nothing
v : Maybe Int
v = Just 1 2
";
        let module = parse_module(source).expect("should parse");
        // 現状 silent pass する。CA-3b 後続で reject 化されたら本テストは
        // 反転 (expect_err に書き換え) する。
        typecheck_module(&module).expect(
            "wrong-arity generic ctor application currently passes silently (pre-existing \
             validator gap; reject is a deferred follow-up)",
        );
    }

    // ---- GU-S3 CP: constructor-pattern binder inference を pin ----

    #[test]
    fn case_ctor_pattern_binds_int_for_maybe_int_scrutinee() {
        // CP acceptance: `case xs ... Just(x) -> x + 1` で `x : Int` に
        // bind される。旧挙動 (binder = `Ty::Unknown`) では `x + 1` が
        // `BinOp(Unknown, Int) -> Unknown` になるが、arm body の戻り値
        // 型注釈 `Int` と annotation 比較は `Ty::Unknown` ワイルドカード
        // で通っていた。新挙動では `x : Int` で `BinOp(Int, Int) -> Int`
        // が正しく推論される。どちらでも typecheck は通るが、本テストは
        // パターン側 binder が `Int` に解決されることを式戻り値経由で間接
        // 的に pin する (binder = `Ty::Unknown` で `x + 1 : Unknown` が
        // 返ったとしても annotation `Int` には通るので、より厳密な pin
        // は CP-2 の追加リファクタで Var lookup を観察する必要がある —
        // 後続テストで行う)。
        let source = "\
type Maybe a = Just(a) | Nothing
to_int : Maybe Int -> Int
to_int m =
  case m
    Just(x) -> x + 1
    Nothing -> 0
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("`Just(x)` against `Maybe Int` scrutinee should bind `x : Int`");
    }

    #[test]
    fn case_ctor_pattern_binds_string_for_maybe_string_scrutinee() {
        // CP acceptance: 別 type argument でも独立に binder を推論する。
        // `Just(x) -> x` で arm body type が `String` になることを宣言
        // 戻り値 `String` 注釈で確認。binder が `Ty::Unknown` のままだと
        // arm 型は `Ty::Unknown` で `String` annotation は wildcard で
        // 通るが、CP-3 で AST-level 直接検査するテストを別途追加する。
        let source = "\
type Maybe a = Just(a) | Nothing
from_some : Maybe String -> String
from_some m =
  case m
    Just(x) -> x
    Nothing -> \"empty\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("`Just(x)` against `Maybe String` scrutinee should bind `x : String`");
    }

    #[test]
    fn case_ctor_pattern_binds_pair_components() {
        // CP acceptance: 多パラメータ template でも各 binder が独立に
        // 解決される。`Pair(x, y)` against `Pair Int String` で `x : Int`、
        // `y : String`。
        let source = "\
type Pair a b = Pair(a, b)
fst : Pair Int String -> Int
fst p =
  case p
    Pair(x, _) -> x + 0
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("`Pair(x, _)` against `Pair Int String` should bind `x : Int`");
    }

    #[test]
    fn case_ctor_pattern_binder_is_int_at_var_lookup() {
        // CP acceptance (直接観察): pattern 後の env で `Var "x"` を
        // 評価した結果が `Ty::Int` であることを `check_expr` で直接確認。
        // 旧挙動なら `Ty::Unknown` が返るところを `Ty::Int` に。
        use crate::ast::{CaseArm, CasePattern, CtorPatternArg};
        // env: union "Maybe" with `Just(a)` / `Nothing`、scrutinee local
        // `xs : Maybe Int` を仕込む。
        let union_info = crate::typecheck_env::UnionTypeInfo {
            type_name: "Maybe".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![
                crate::typecheck_env::UnionVariantInfo {
                    ctor: "Just".to_string(),
                    variant_index: 0,
                    arg_types: vec![Ty::Var("a".to_string())],
                },
                crate::typecheck_env::UnionVariantInfo {
                    ctor: "Nothing".to_string(),
                    variant_index: 1,
                    arg_types: vec![],
                },
            ],
        };
        let mut union_types = HashMap::new();
        union_types.insert("Maybe".to_string(), union_info);
        let mut locals = HashMap::new();
        locals.insert(
            "xs".to_string(),
            Ty::Con {
                name: "Maybe".to_string(),
                args: vec![Ty::Int],
            },
        );
        let env = TypeEnv {
            locals,
            union_types,
            ..TypeEnv::empty()
        };
        let pattern = CasePattern::Ctor {
            type_qualifier: None,
            ctor: "Just".to_string(),
            args: vec![CtorPatternArg::Bind("x".to_string())],
        };
        let scrutinee_ty = Ty::Con {
            name: "Maybe".to_string(),
            args: vec![Ty::Int],
        };
        let child_env = super::env_with_case_pattern_bindings(&env, &pattern, &scrutinee_ty);
        let body = Expr::Var {
            name: "x".to_string(),
            span: None,
        };
        assert_eq!(
            check_expr(&body, &child_env),
            Ty::Int,
            "binder `x` should resolve to `Int` after `Just(x)` against `Maybe Int`"
        );
        // arm walker contract: `CaseArm` is unused here directly; the
        // helper is what `infer_expr_ty` calls. Avoid unused import warning.
        let _ = std::mem::size_of::<CaseArm>();
    }

    #[test]
    fn case_ctor_pattern_wildcard_does_not_bind() {
        // CP acceptance: `Just(_)` は env に何も追加しない。
        use crate::ast::{CasePattern, CtorPatternArg};
        let union_info = crate::typecheck_env::UnionTypeInfo {
            type_name: "Maybe".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![crate::typecheck_env::UnionVariantInfo {
                ctor: "Just".to_string(),
                variant_index: 0,
                arg_types: vec![Ty::Var("a".to_string())],
            }],
        };
        let mut union_types = HashMap::new();
        union_types.insert("Maybe".to_string(), union_info);
        let env = TypeEnv {
            union_types,
            ..TypeEnv::empty()
        };
        let pattern = CasePattern::Ctor {
            type_qualifier: None,
            ctor: "Just".to_string(),
            args: vec![CtorPatternArg::Wildcard],
        };
        let scrutinee_ty = Ty::Con {
            name: "Maybe".to_string(),
            args: vec![Ty::Int],
        };
        let child_env = super::env_with_case_pattern_bindings(&env, &pattern, &scrutinee_ty);
        assert!(
            child_env.locals.is_empty(),
            "wildcard binder should not insert anything into the env"
        );
    }

    #[test]
    fn case_ctor_pattern_non_generic_box_arg_still_binds_int() {
        // CP regression: non-generic single-arg ctor pattern (`Box(x)` for
        // `type Box = Box(Int)`) を `Box` scrutinee 上で評価して `x : Int`
        // を pin。`type_params` 空 → freshen 結果は `arg_types` の clone
        // (no fresh ids 消費) → subst も同じ Con shape の unify で成立
        // しないが、freshened_arg_tys がそのまま Int で返るので OK。
        use crate::ast::{CasePattern, CtorPatternArg};
        let union_info = crate::typecheck_env::UnionTypeInfo {
            type_name: "Box".to_string(),
            type_params: vec![],
            variants: vec![crate::typecheck_env::UnionVariantInfo {
                ctor: "Box".to_string(),
                variant_index: 0,
                arg_types: vec![Ty::Int],
            }],
        };
        let mut union_types = HashMap::new();
        union_types.insert("Box".to_string(), union_info);
        let env = TypeEnv {
            union_types,
            ..TypeEnv::empty()
        };
        let pattern = CasePattern::Ctor {
            type_qualifier: None,
            ctor: "Box".to_string(),
            args: vec![CtorPatternArg::Bind("x".to_string())],
        };
        let scrutinee_ty = Ty::Con {
            name: "Box".to_string(),
            args: vec![],
        };
        let child_env = super::env_with_case_pattern_bindings(&env, &pattern, &scrutinee_ty);
        assert_eq!(child_env.locals.get("x"), Some(&Ty::Int));
    }

    #[test]
    fn case_ctor_pattern_unknown_ctor_falls_back_to_unknown_binders() {
        // CP regression: unknown ctor は all-`Ty::Unknown` で fallback。
        use crate::ast::{CasePattern, CtorPatternArg};
        let env = TypeEnv::empty();
        let pattern = CasePattern::Ctor {
            type_qualifier: None,
            ctor: "Unknown".to_string(),
            args: vec![CtorPatternArg::Bind("x".to_string())],
        };
        let scrutinee_ty = Ty::Unknown;
        let child_env = super::env_with_case_pattern_bindings(&env, &pattern, &scrutinee_ty);
        assert_eq!(child_env.locals.get("x"), Some(&Ty::Unknown));
    }

    #[test]
    fn case_ctor_pattern_wrong_arity_falls_back_to_unknown_binders() {
        // CP regression: wrong-arity (binder 数 > variant arg 数 等) は
        // all-`Ty::Unknown` で fallback (CA-3b と同じ deferred diagnostic
        // policy)。
        use crate::ast::{CasePattern, CtorPatternArg};
        let union_info = crate::typecheck_env::UnionTypeInfo {
            type_name: "Maybe".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![crate::typecheck_env::UnionVariantInfo {
                ctor: "Just".to_string(),
                variant_index: 0,
                arg_types: vec![Ty::Var("a".to_string())],
            }],
        };
        let mut union_types = HashMap::new();
        union_types.insert("Maybe".to_string(), union_info);
        let env = TypeEnv {
            union_types,
            ..TypeEnv::empty()
        };
        // `Just(x, y)` — variant は 1-arg なのに pattern は 2 binder。
        let pattern = CasePattern::Ctor {
            type_qualifier: None,
            ctor: "Just".to_string(),
            args: vec![
                CtorPatternArg::Bind("x".to_string()),
                CtorPatternArg::Bind("y".to_string()),
            ],
        };
        let scrutinee_ty = Ty::Con {
            name: "Maybe".to_string(),
            args: vec![Ty::Int],
        };
        let child_env = super::env_with_case_pattern_bindings(&env, &pattern, &scrutinee_ty);
        assert_eq!(child_env.locals.get("x"), Some(&Ty::Unknown));
        assert_eq!(child_env.locals.get("y"), Some(&Ty::Unknown));
    }

    #[test]
    fn env_with_case_pattern_bindings_using_advances_caller_next_id() {
        // Codex CP pass-2 指摘: `*_using` 版に caller の `&mut next_id` を
        // 渡したとき、CP 側で消費した fresh counter が外に伝わることを pin。
        // pattern binder で freshen が走れば next_id は最低 1 回 advance
        // するはず (`Ty::Var("a")` を `__goby_fresh_ty_N` に置換)。
        use crate::ast::{CasePattern, CtorPatternArg};
        let union_info = crate::typecheck_env::UnionTypeInfo {
            type_name: "Maybe".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![crate::typecheck_env::UnionVariantInfo {
                ctor: "Just".to_string(),
                variant_index: 0,
                arg_types: vec![Ty::Var("a".to_string())],
            }],
        };
        let mut union_types = HashMap::new();
        union_types.insert("Maybe".to_string(), union_info);
        let env = TypeEnv {
            union_types,
            ..TypeEnv::empty()
        };
        let pattern = CasePattern::Ctor {
            type_qualifier: None,
            ctor: "Just".to_string(),
            args: vec![CtorPatternArg::Bind("x".to_string())],
        };
        // 未解決の scrutinee — Maybe `__goby_fresh_ty_2` を渡す。
        let scrutinee_ty = Ty::Con {
            name: "Maybe".to_string(),
            args: vec![Ty::Var("__goby_fresh_ty_2".to_string())],
        };
        let mut next_id = 0usize;
        let _ = super::env_with_case_pattern_bindings_using(
            &env,
            &pattern,
            &scrutinee_ty,
            &mut next_id,
        );
        // 入力 scrutinee に `__goby_fresh_ty_2` があるので seed は 3 以上に
        // jump し、CP の freshen で `a` を `__goby_fresh_ty_3` に置換する。
        // 結果として next_id は 4 以上 (3 を消費して advance) — Codex
        // pass-3 nit 反映でより強い pin に。
        assert!(
            next_id >= 4,
            "next_id must advance past scrutinee fresh max + 1 (got {})",
            next_id
        );
    }

    #[test]
    fn case_ctor_pattern_seeds_next_id_past_scrutinee_fresh_vars() {
        // Codex CP pass-1 指摘 regression: scrutinee に既存
        // `__goby_fresh_ty_N` が含まれていても CP の freshen が同じ ID を
        // 再利用しないことを pin。例として scrutinee
        // `Pair __goby_fresh_ty_1 __goby_fresh_ty_0` (param 順を入れ替えて
        // ある) で `Pair(x, y)` 評価 — もし seed が env のみだと CP が
        // `__goby_fresh_ty_0` / `__goby_fresh_ty_1` を再利用して
        // `a -> __goby_fresh_ty_0`, `b -> __goby_fresh_ty_1` に freshen し
        // unify で `a` も `b` も同じ `__goby_fresh_ty_1` に bind してしまう
        // 可能性がある。scrutinee 側の fresh も seed すれば 2 以降から
        // 新規 fresh を取るため衝突しない。
        use crate::ast::{CasePattern, CtorPatternArg};
        let union_info = crate::typecheck_env::UnionTypeInfo {
            type_name: "Pair".to_string(),
            type_params: vec!["a".to_string(), "b".to_string()],
            variants: vec![crate::typecheck_env::UnionVariantInfo {
                ctor: "Pair".to_string(),
                variant_index: 0,
                arg_types: vec![Ty::Var("a".to_string()), Ty::Var("b".to_string())],
            }],
        };
        let mut union_types = HashMap::new();
        union_types.insert("Pair".to_string(), union_info);
        let env = TypeEnv {
            union_types,
            ..TypeEnv::empty()
        };
        let pattern = CasePattern::Ctor {
            type_qualifier: None,
            ctor: "Pair".to_string(),
            args: vec![
                CtorPatternArg::Bind("x".to_string()),
                CtorPatternArg::Bind("y".to_string()),
            ],
        };
        // scrutinee `Pair __goby_fresh_ty_1 __goby_fresh_ty_0` — 内側で
        // 順序を反転させてあるので、CP の freshen が同じ ID を再利用すると
        // collapse する。
        let scrutinee_ty = Ty::Con {
            name: "Pair".to_string(),
            args: vec![
                Ty::Var("__goby_fresh_ty_1".to_string()),
                Ty::Var("__goby_fresh_ty_0".to_string()),
            ],
        };
        let child_env = super::env_with_case_pattern_bindings(&env, &pattern, &scrutinee_ty);
        let x_ty = child_env.locals.get("x").cloned().expect("x must be bound");
        let y_ty = child_env.locals.get("y").cloned().expect("y must be bound");
        // x は scrutinee の 1 番目 = `__goby_fresh_ty_1`、y は 2 番目 =
        // `__goby_fresh_ty_0`。2 つの binder が別の fresh 名に bind されて
        // いることが本テストの core 不変条件 (= ID 衝突で collapse して
        // いない)。
        assert_ne!(
            x_ty, y_ty,
            "two independent template params must not collapse to one fresh id"
        );
        assert_eq!(x_ty, Ty::Var("__goby_fresh_ty_1".to_string()));
        assert_eq!(y_ty, Ty::Var("__goby_fresh_ty_0".to_string()));
    }

    #[test]
    fn case_ctor_pattern_qualified_ctor_resolves_via_lookup_union_variant() {
        // CP: `Maybe.Just(x)` Qualified pattern も同じ経路で binder 推論。
        use crate::ast::{CasePattern, CtorPatternArg};
        let union_info = crate::typecheck_env::UnionTypeInfo {
            type_name: "Maybe".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![crate::typecheck_env::UnionVariantInfo {
                ctor: "Just".to_string(),
                variant_index: 0,
                arg_types: vec![Ty::Var("a".to_string())],
            }],
        };
        let mut union_types = HashMap::new();
        union_types.insert("Maybe".to_string(), union_info);
        let env = TypeEnv {
            union_types,
            ..TypeEnv::empty()
        };
        let pattern = CasePattern::Ctor {
            type_qualifier: Some("Maybe".to_string()),
            ctor: "Just".to_string(),
            args: vec![CtorPatternArg::Bind("x".to_string())],
        };
        let scrutinee_ty = Ty::Con {
            name: "Maybe".to_string(),
            args: vec![Ty::Str],
        };
        let child_env = super::env_with_case_pattern_bindings(&env, &pattern, &scrutinee_ty);
        assert_eq!(child_env.locals.get("x"), Some(&Ty::Str));
    }

    // ---- GU-S3 GR: generic-record ctor application + field access ----

    #[test]
    fn typechecks_generic_record_ctor_application_box_int() {
        // GR-2 acceptance: `Box(value: 42) : Box Int` (single-field
        // generic record)。
        let source = "\
type Box a = Box(value: a)
v : Box Int
v = Box(value: 42)
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`Box(value: 42) : Box Int` should typecheck");
    }

    #[test]
    fn typechecks_generic_record_ctor_application_box_string_via_call_rewrite() {
        // GR-2 acceptance: `Box "hello" : Box String` (single-field、Call
        // rewrite 経路、typecheck_check.rs:150-162)。
        let source = "\
type Box a = Box(value: a)
v : Box String
v = Box \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`Box \"hello\" : Box String` should typecheck");
    }

    #[test]
    fn typechecks_generic_record_ctor_with_two_type_parameters() {
        // GR-2 acceptance: 多パラメータ generic record。
        let source = "\
type Pair a b = Pair(first: a, second: b)
v : Pair Int String
v = Pair(first: 1, second: \"x\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("`Pair(first: 1, second: \"x\") : Pair Int String` should typecheck");
    }

    #[test]
    fn typechecks_generic_record_field_access_box_int() {
        // GR-3 acceptance: field access。`v : Box Int` から `v.value : Int`。
        let source = "\
type Box a = Box(value: a)
v : Box Int
v = Box(value: 42)
n : Int
n = v.value
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`v.value : Int` should typecheck for `v : Box Int`");
    }

    #[test]
    fn typechecks_generic_record_field_access_nested_box_maybe_int() {
        // GR-3 acceptance: nested `Box (Maybe Int)` で `v.value : Maybe Int`。
        let source = "\
type Maybe a = Just(a) | Nothing
type Box a = Box(value: a)
v : Box (Maybe Int)
v = Box(value: Nothing)
inner : Maybe Int
inner = v.value
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("`v.value : Maybe Int` should typecheck for `v : Box (Maybe Int)`");
    }

    #[test]
    fn non_generic_record_ctor_application_still_typechecks_as_before() {
        // GR-2 regression: non-generic record はの挙動完全不変。
        let source = "\
type Point = Point(x: Int, y: Int)
p : Point
p = Point(x: 1, y: 2)
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("non-generic `Point(x: 1, y: 2) : Point` should still typecheck");
    }

    #[test]
    fn non_generic_record_field_access_still_typechecks_as_before() {
        // GR-3 regression: non-generic record field access も挙動不変。
        let source = "\
type Point = Point(x: Int, y: Int)
p : Point
p = Point(x: 1, y: 2)
n : Int
n = p.x
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("non-generic `p.x : Int` should still typecheck");
    }

    #[test]
    fn record_construct_returns_ty_con_with_resolved_type_args() {
        // GR-2 AST-level pin: `infer_expr_ty(RecordConstruct)` の戻り値が
        // `Ty::Con { name, args: [resolved] }` になることを直接 assert。
        // 旧挙動では `args: vec![]` を返していた。
        let mut record_types = HashMap::new();
        let mut box_fields = HashMap::new();
        box_fields.insert("value".to_string(), Ty::Var("a".to_string()));
        record_types.insert(
            "Box".to_string(),
            crate::typecheck_env::RecordTypeInfo {
                type_name: "Box".to_string(),
                type_params: vec!["a".to_string()],
                constructor: "Box".to_string(),
                fields: box_fields,
            },
        );
        let env = TypeEnv {
            record_types,
            ..TypeEnv::empty()
        };
        let expr = Expr::RecordConstruct {
            constructor: "Box".to_string(),
            fields: vec![("value".to_string(), Expr::IntLit(42))],
            span: None,
        };
        match check_expr(&expr, &env) {
            Ty::Con { name, args } => {
                assert_eq!(name, "Box");
                assert_eq!(args, vec![Ty::Int]);
            }
            other => panic!(
                "expected `Ty::Con {{ name: \"Box\", args: [Int] }}`, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn rejects_generic_record_with_inconsistent_shared_type_param() {
        // Codex GR pass-1 指摘 regression: 同じ `type_params[i]` を複数
        // field が共有しているときの mismatch は `infer_record_construct_ty`
        // が `Ty::Unknown` を返すだけで宣言型比較が wildcard pass する
        // ため、`typecheck_ambiguity` の generic-record validator で
        // 明示的に reject する必要がある。
        // `Same a = Same(left: a, right: a)` に対して `Same(left: 1,
        // right: "x")` を渡すと `a = Int` と `a = String` で矛盾するため
        // 拒否されるべき。
        let source = "\
type Same a = Same(left: a, right: a)
v : Same Int
v = Same(left: 1, right: \"x\")
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err(
            "`Same(left: 1, right: \"x\")` should be rejected — shared type param mismatch",
        );
        assert!(
            !err.message.is_empty(),
            "should produce a non-empty error message"
        );
    }

    #[test]
    fn rejects_generic_record_with_wrong_field_type() {
        // GR-2 acceptance: wrong-type field を reject。
        let source = "\
type Box a = Box(value: a)
v : Box String
v = Box(value: 42)
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("`Box(value: 42) : Box String` should be rejected (Int vs String)");
        assert!(
            !err.message.is_empty(),
            "should produce a non-empty error message"
        );
    }
}
