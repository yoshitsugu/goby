use std::collections::{HashMap, HashSet};

use crate::typecheck_check::effect_candidates_for_operation;
use crate::typecheck_env::{EffectRow, RowSubst, RowVarId, Ty, TypeEnv, TypeSubst};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionArgMismatch {
    pub(crate) required: Ty,
    pub(crate) actual: Ty,
}

// EP-1d: row substitution and unification helpers.
//
// `apply_row_substitution` walks `tail` through `row_subst` until a closed
// row, an unbound variable, or a cycle is reached. Cycles are reported as
// `None` so callers (e.g. `unify_effect_rows`) can propagate failure rather
// than silently terminating with a truncated row.
//
// `unify_effect_rows` implements the rules locked in `LANGUAGE_SPEC.md` §5:
//   1. closed{S1} ~ closed{S2}: ok iff S1 == S2.
//   2. closed{S1} ~ open{S2; e}: ok iff S2 ⊆ S1; bind e := closed(S1 \ S2).
//   3. open{S1; e1} ~ open{S2; e2}:
//        - same var: ok iff S1 == S2.
//        - distinct vars: fresh row var r; bind e1 := open(S2 \ S1; r) and
//          e2 := open(S1 \ S2; r); occurs check on each bind.
pub(crate) fn apply_row_substitution(row: &EffectRow, row_subst: &RowSubst) -> Option<EffectRow> {
    let mut visited: HashSet<RowVarId> = HashSet::new();
    let mut fixed = row.fixed.clone();
    let mut tail = row.tail.clone();
    while let Some(var) = tail.clone() {
        if !visited.insert(var.clone()) {
            // Direct or indirect cycle through the substitution.
            return None;
        }
        match row_subst.get(&var) {
            Some(bound) => {
                fixed.extend(bound.fixed.iter().cloned());
                tail = bound.tail.clone();
            }
            None => {
                // Unbound variable — leave the tail as-is.
                tail = Some(var);
                break;
            }
        }
    }
    Some(EffectRow { fixed, tail })
}

fn occurs_in_row(name: &RowVarId, row: &EffectRow, row_subst: &RowSubst) -> bool {
    let mut visited: HashSet<RowVarId> = HashSet::new();
    let mut tail = row.tail.clone();
    while let Some(var) = tail {
        if &var == name {
            return true;
        }
        if !visited.insert(var.clone()) {
            return true;
        }
        match row_subst.get(&var) {
            Some(bound) => {
                tail = bound.tail.clone();
            }
            None => return false,
        }
    }
    false
}

fn bind_row_variable(name: &RowVarId, row: &EffectRow, row_subst: &mut RowSubst) -> bool {
    if occurs_in_row(name, row, row_subst) {
        return false;
    }
    row_subst.insert(name.clone(), row.clone());
    true
}

fn fresh_row_var(next_id: &mut usize) -> RowVarId {
    let id = *next_id;
    *next_id += 1;
    RowVarId(format!("__goby_fresh_row_{}", id))
}

pub(crate) fn unify_effect_rows(
    left: &EffectRow,
    right: &EffectRow,
    row_subst: &mut RowSubst,
    next_id: &mut usize,
) -> bool {
    let Some(left) = apply_row_substitution(left, row_subst) else {
        return false;
    };
    let Some(right) = apply_row_substitution(right, row_subst) else {
        return false;
    };
    match (left.tail.clone(), right.tail.clone()) {
        (None, None) => left.fixed == right.fixed,
        (None, Some(rv)) => unify_closed_with_open(&left.fixed, &right.fixed, &rv, row_subst),
        (Some(lv), None) => unify_closed_with_open(&right.fixed, &left.fixed, &lv, row_subst),
        (Some(lv), Some(rv)) => {
            if lv == rv {
                left.fixed == right.fixed
            } else {
                unify_open_with_open(&left.fixed, &lv, &right.fixed, &rv, row_subst, next_id)
            }
        }
    }
}

/// closed{S_closed} ~ open{S_open; var}: ok iff S_open ⊆ S_closed; binds
/// `var := closed(S_closed \ S_open)`.
fn unify_closed_with_open(
    s_closed: &std::collections::BTreeSet<String>,
    s_open: &std::collections::BTreeSet<String>,
    var: &RowVarId,
    row_subst: &mut RowSubst,
) -> bool {
    if !s_open.is_subset(s_closed) {
        return false;
    }
    let residual: std::collections::BTreeSet<String> =
        s_closed.difference(s_open).cloned().collect();
    bind_row_variable(
        var,
        &EffectRow {
            fixed: residual,
            tail: None,
        },
        row_subst,
    )
}

/// open{S1; e1} ~ open{S2; e2} with distinct vars: introduce a fresh row var
/// `r`, then bind e1 := open(S2 \ S1; r) and e2 := open(S1 \ S2; r).
///
/// Both binds are applied transactionally: if the second bind's occurs check
/// fails, the first bind is rolled back so callers do not observe a partial
/// substitution.
fn unify_open_with_open(
    s1: &std::collections::BTreeSet<String>,
    e1: &RowVarId,
    s2: &std::collections::BTreeSet<String>,
    e2: &RowVarId,
    row_subst: &mut RowSubst,
    next_id: &mut usize,
) -> bool {
    let fresh = fresh_row_var(next_id);
    let from_e1 = EffectRow {
        fixed: s2.difference(s1).cloned().collect(),
        tail: Some(fresh.clone()),
    };
    let from_e2 = EffectRow {
        fixed: s1.difference(s2).cloned().collect(),
        tail: Some(fresh),
    };
    let prior_e1 = row_subst.get(e1).cloned();
    if !bind_row_variable(e1, &from_e1, row_subst) {
        return false;
    }
    if !bind_row_variable(e2, &from_e2, row_subst) {
        // Roll back the first bind so the substitution stays consistent.
        match prior_e1 {
            Some(prev) => {
                row_subst.insert(e1.clone(), prev);
            }
            None => {
                row_subst.remove(e1);
            }
        }
        return false;
    }
    true
}

pub(crate) fn apply_type_substitution(
    ty: &Ty,
    type_subst: &TypeSubst,
    row_subst: &RowSubst,
    env: &TypeEnv,
) -> Ty {
    let resolved = env.resolve_alias(ty, 0);
    match resolved {
        Ty::Var(name) => match type_subst.get(&name) {
            Some(bound) => apply_type_substitution(bound, type_subst, row_subst, env),
            None => Ty::Var(name),
        },
        Ty::List(inner) => Ty::List(Box::new(apply_type_substitution(
            &inner, type_subst, row_subst, env,
        ))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .iter()
                .map(|item| apply_type_substitution(item, type_subst, row_subst, env))
                .collect(),
        ),
        Ty::Fun {
            params,
            result,
            effects,
        } => Ty::Fun {
            params: params
                .iter()
                .map(|param| apply_type_substitution(param, type_subst, row_subst, env))
                .collect(),
            result: Box::new(apply_type_substitution(&result, type_subst, row_subst, env)),
            // EP-1d: resolve the residual row through the row substitution so
            // bindings introduced by `unify_effect_rows` propagate. Cycles
            // (which `apply_row_substitution` reports as None) collapse to
            // closed-empty here; the cycle itself is reported by the unifier.
            effects: apply_row_substitution(&effects, row_subst)
                .unwrap_or_else(EffectRow::closed_empty),
        },
        Ty::Con { name, args } => Ty::Con {
            name,
            args: args
                .iter()
                .map(|arg| apply_type_substitution(arg, type_subst, row_subst, env))
                .collect(),
        },
        Ty::Handler { covered_ops } => Ty::Handler { covered_ops },
        Ty::Int | Ty::Float | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => resolved,
    }
}

fn bind_type_variable(
    name: &str,
    ty: &Ty,
    type_subst: &mut TypeSubst,
    row_subst: &mut RowSubst,
    env: &TypeEnv,
    next_id: &mut usize,
) -> bool {
    if let Some(bound) = type_subst.get(name).cloned() {
        return unify_types_with_subst(&bound, ty, type_subst, row_subst, env, next_id);
    }
    if matches!(ty, Ty::Var(other) if other == name) {
        return true;
    }
    let value = apply_type_substitution(ty, type_subst, row_subst, env);
    type_subst.insert(name.to_string(), value);
    true
}

pub(crate) fn unify_types_with_subst(
    expected: &Ty,
    actual: &Ty,
    type_subst: &mut TypeSubst,
    row_subst: &mut RowSubst,
    env: &TypeEnv,
    next_id: &mut usize,
) -> bool {
    let expected = apply_type_substitution(expected, type_subst, row_subst, env);
    let actual = apply_type_substitution(actual, type_subst, row_subst, env);
    match (expected, actual) {
        (Ty::Unknown, _) | (_, Ty::Unknown) => true,
        (Ty::Var(name), ty) => bind_type_variable(&name, &ty, type_subst, row_subst, env, next_id),
        (ty, Ty::Var(name)) => bind_type_variable(&name, &ty, type_subst, row_subst, env, next_id),
        (Ty::Int, Ty::Int)
        | (Ty::Float, Ty::Float)
        | (Ty::Bool, Ty::Bool)
        | (Ty::Str, Ty::Str)
        | (Ty::Unit, Ty::Unit) => true,
        (Ty::List(left), Ty::List(right)) => {
            unify_types_with_subst(&left, &right, type_subst, row_subst, env, next_id)
        }
        (Ty::Tuple(left), Ty::Tuple(right)) if left.len() == right.len() => left
            .iter()
            .zip(right.iter())
            .all(|(l, r)| unify_types_with_subst(l, r, type_subst, row_subst, env, next_id)),
        (
            Ty::Fun {
                params: left_params,
                result: left_result,
                effects: left_effects,
            },
            Ty::Fun {
                params: right_params,
                result: right_result,
                effects: right_effects,
            },
        ) if left_params.len() == right_params.len() => {
            left_params
                .iter()
                .zip(right_params.iter())
                .all(|(l, r)| unify_types_with_subst(l, r, type_subst, row_subst, env, next_id))
                && unify_types_with_subst(
                    &left_result,
                    &right_result,
                    type_subst,
                    row_subst,
                    env,
                    next_id,
                )
                // EP-1d: row unification per LANGUAGE_SPEC §5.
                && unify_effect_rows(&left_effects, &right_effects, row_subst, next_id)
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
            .all(|(l, r)| unify_types_with_subst(l, r, type_subst, row_subst, env, next_id)),
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

/// GU-S3 CA-2: annotation 比較用の unify。
///
/// `unify_types_with_subst` は呼び出し側で freshen 済みの safe context
/// (callee の freshen 後など) を前提に、すべての `Ty::Var` を flexible
/// として bind する。一方、宣言戻り値比較 (`typecheck_stmt::check_declared_return_type`)
/// のように **ユーザーが書いた rigid な型変数 `a` (`type a -> Int`)** と
/// 推論で生まれた **flexible な `__goby_fresh_ty_*`** が同居する文脈では、
/// rigid を flexible に降格して `f : a -> Int; f x = x` を通してしまう。
///
/// 本 helper はそれを防ぐため、**`Ty::Var(name)` の `name` が
/// `__goby_fresh_ty_` で始まる場合のみ flexible として bind し、それ以外
/// (rigid) は等価比較のみ**を行う。判定は **side-independent**
/// (expected/actual のどちらに来ても prefix だけで決まる)。
///
/// `Ty::Unknown` は両側でワイルドカード扱い (`are_compatible` 互換)、
/// `Ty::Fun` / `Ty::List` / `Ty::Tuple` / `Ty::Con` / effect row は
/// `unify_types_with_subst` と同じ規則で構造再帰する。
///
/// `next_id` は呼び出し側で
/// `max_fresh_ty_id_in_ty(expected).max(max_fresh_ty_id_in_ty(actual))
///  .max(typecheck_env::max_fresh_row_id(expected))
///  .max(typecheck_env::max_fresh_row_id(actual))`
/// を seed として渡すこと。`Ty::Fun` 経路の `unify_effect_rows` が新規
/// `__goby_fresh_row_N` を作る可能性があるため、両 prefix を加味する。
///
/// **Reserved prefix (GU-S3 全コミット共通)**:
/// `__goby_fresh_ty_` および `__goby_fresh_row_` は内部 freshening のために
/// 予約された prefix。Goby surface annotation で `_` 始まりの型変数を
/// 書くこと自体は許される (`typecheck_types::is_type_variable_name`) ため、
/// ユーザーが `__goby_fresh_ty_0` のような identifier を annotation に
/// 書くと意味論が壊れる可能性がある (= unspecified behaviour)。診断による
/// 予約名拒否は本サブタスクのスコープ外で、後続改善とする。
pub(crate) fn unifies_with_annotation(
    expected: &Ty,
    actual: &Ty,
    env: &TypeEnv,
    next_id: &mut usize,
) -> bool {
    let mut type_subst = TypeSubst::new();
    let mut row_subst = RowSubst::new();
    unify_with_rigid_vars(
        expected,
        actual,
        &mut type_subst,
        &mut row_subst,
        env,
        next_id,
    )
}

/// CA-2 helper: `unifies_with_annotation` の内部実装。`unify_types_with_subst`
/// と構造はほぼ同じだが、`Ty::Var` 分岐で rigid (= prefix が
/// `__goby_fresh_ty_` で **始まらない** 名前) は等価比較のみ、flexible
/// (prefix 一致) のみ `bind_type_variable` で束縛する。
fn unify_with_rigid_vars(
    expected: &Ty,
    actual: &Ty,
    type_subst: &mut TypeSubst,
    row_subst: &mut RowSubst,
    env: &TypeEnv,
    next_id: &mut usize,
) -> bool {
    let expected = apply_type_substitution(expected, type_subst, row_subst, env);
    let actual = apply_type_substitution(actual, type_subst, row_subst, env);
    match (expected, actual) {
        (Ty::Unknown, _) | (_, Ty::Unknown) => true,
        (Ty::Var(left_name), Ty::Var(right_name)) => {
            let left_flex = is_flexible_fresh_ty_var(&left_name);
            let right_flex = is_flexible_fresh_ty_var(&right_name);
            if left_name == right_name {
                return true;
            }
            if left_flex {
                bind_type_variable(
                    &left_name,
                    &Ty::Var(right_name),
                    type_subst,
                    row_subst,
                    env,
                    next_id,
                )
            } else if right_flex {
                bind_type_variable(
                    &right_name,
                    &Ty::Var(left_name),
                    type_subst,
                    row_subst,
                    env,
                    next_id,
                )
            } else {
                // どちらも rigid: 等価比較 (上で `==` 判定済みなので false)。
                false
            }
        }
        (Ty::Var(name), ty) => {
            if is_flexible_fresh_ty_var(&name) {
                bind_type_variable(&name, &ty, type_subst, row_subst, env, next_id)
            } else {
                false
            }
        }
        (ty, Ty::Var(name)) => {
            if is_flexible_fresh_ty_var(&name) {
                bind_type_variable(&name, &ty, type_subst, row_subst, env, next_id)
            } else {
                false
            }
        }
        (Ty::Int, Ty::Int)
        | (Ty::Float, Ty::Float)
        | (Ty::Bool, Ty::Bool)
        | (Ty::Str, Ty::Str)
        | (Ty::Unit, Ty::Unit) => true,
        (Ty::List(left), Ty::List(right)) => {
            unify_with_rigid_vars(&left, &right, type_subst, row_subst, env, next_id)
        }
        (Ty::Tuple(left), Ty::Tuple(right)) if left.len() == right.len() => left
            .iter()
            .zip(right.iter())
            .all(|(l, r)| unify_with_rigid_vars(l, r, type_subst, row_subst, env, next_id)),
        (
            Ty::Fun {
                params: left_params,
                result: left_result,
                effects: left_effects,
            },
            Ty::Fun {
                params: right_params,
                result: right_result,
                effects: right_effects,
            },
        ) if left_params.len() == right_params.len() => {
            left_params
                .iter()
                .zip(right_params.iter())
                .all(|(l, r)| unify_with_rigid_vars(l, r, type_subst, row_subst, env, next_id))
                && unify_with_rigid_vars(
                    &left_result,
                    &right_result,
                    type_subst,
                    row_subst,
                    env,
                    next_id,
                )
                && unify_effect_rows(&left_effects, &right_effects, row_subst, next_id)
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
            .all(|(l, r)| unify_with_rigid_vars(l, r, type_subst, row_subst, env, next_id)),
        (
            Ty::Handler {
                covered_ops: left_ops,
            },
            Ty::Handler {
                covered_ops: right_ops,
            },
        ) => left_ops == right_ops,
        // `are_compatible` 互換: `Handler E1 E2 ...` annotation と
        // `Ty::Handler { covered_ops }` actual の特殊照合。effect 名集合が
        // 一致すれば許容。両側どちらに `Con("Handler", ..)` が来てもよい。
        (Ty::Con { name, args }, Ty::Handler { covered_ops })
        | (Ty::Handler { covered_ops }, Ty::Con { name, args })
            if name == "Handler" =>
        {
            if let Some(expected_ops) = env.handler_ops_from_type_args(&args) {
                expected_ops == covered_ops
            } else {
                false
            }
        }
        _ => false,
    }
}

/// CA-2 helper: `Ty::Var(name)` が GU-S3 で予約された flexible prefix
/// (`__goby_fresh_ty_`) で始まる識別子かを判定する。
pub(crate) fn is_flexible_fresh_ty_var(name: &str) -> bool {
    name.starts_with("__goby_fresh_ty_")
}

/// CA-2 helper: 任意の `Ty` を walk して `__goby_fresh_ty_N` の最大 N に
/// `+1` した one-past-max を返す。caller が `next_id` を seed するときに
/// 使う。`max_fresh_row_id` と同じ "one past max" コントラクト。
pub(crate) fn max_fresh_ty_id_in_ty(ty: &Ty) -> usize {
    fn extract(name: &str) -> Option<usize> {
        const PREFIX: &str = "__goby_fresh_ty_";
        name.strip_prefix(PREFIX)
            .and_then(|rest| rest.parse::<usize>().ok())
            .map(|n| n + 1)
    }
    fn walk(ty: &Ty, acc: &mut usize) {
        match ty {
            Ty::Var(name) => {
                if let Some(next) = extract(name)
                    && next > *acc
                {
                    *acc = next;
                }
            }
            Ty::List(inner) => walk(inner, acc),
            Ty::Tuple(items) => items.iter().for_each(|t| walk(t, acc)),
            Ty::Fun { params, result, .. } => {
                params.iter().for_each(|p| walk(p, acc));
                walk(result, acc);
            }
            Ty::Con { args, .. } => args.iter().for_each(|a| walk(a, acc)),
            Ty::Handler { .. }
            | Ty::Int
            | Ty::Float
            | Ty::Bool
            | Ty::Str
            | Ty::Unit
            | Ty::Unknown => {}
        }
    }
    let mut acc = 0usize;
    walk(ty, &mut acc);
    acc
}

pub(crate) fn instantiate_ty_with_fresh_type_vars(
    ty: &Ty,
    ty_mapping: &mut HashMap<String, String>,
    row_mapping: &mut HashMap<RowVarId, RowVarId>,
    next_id: &mut usize,
) -> Ty {
    match ty {
        Ty::Var(name) => {
            let fresh = ty_mapping.entry(name.clone()).or_insert_with(|| {
                let id = *next_id;
                *next_id += 1;
                format!("__goby_fresh_ty_{}", id)
            });
            Ty::Var(fresh.clone())
        }
        Ty::List(inner) => Ty::List(Box::new(instantiate_ty_with_fresh_type_vars(
            inner,
            ty_mapping,
            row_mapping,
            next_id,
        ))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .iter()
                .map(|item| {
                    instantiate_ty_with_fresh_type_vars(item, ty_mapping, row_mapping, next_id)
                })
                .collect(),
        ),
        Ty::Fun {
            params,
            result,
            effects,
        } => Ty::Fun {
            params: params
                .iter()
                .map(|param| {
                    instantiate_ty_with_fresh_type_vars(param, ty_mapping, row_mapping, next_id)
                })
                .collect(),
            result: Box::new(instantiate_ty_with_fresh_type_vars(
                result,
                ty_mapping,
                row_mapping,
                next_id,
            )),
            // EP-1d: refresh the row tail so independent call sites of a
            // row-polymorphic function get independent row variables.
            // Same-named row vars within one instantiation map to the same
            // fresh var so a callback's `{e}` and the result row's `{e}` stay
            // unified.
            effects: instantiate_effect_row(effects, row_mapping, next_id),
        },
        Ty::Con { name, args } => Ty::Con {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| {
                    instantiate_ty_with_fresh_type_vars(arg, ty_mapping, row_mapping, next_id)
                })
                .collect(),
        },
        Ty::Handler { covered_ops } => Ty::Handler {
            covered_ops: covered_ops.clone(),
        },
        Ty::Int | Ty::Float | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => ty.clone(),
    }
}

fn instantiate_effect_row(
    row: &EffectRow,
    row_mapping: &mut HashMap<RowVarId, RowVarId>,
    next_id: &mut usize,
) -> EffectRow {
    let tail = row.tail.as_ref().map(|var| {
        row_mapping
            .entry(var.clone())
            .or_insert_with(|| fresh_row_var(next_id))
            .clone()
    });
    EffectRow {
        fixed: row.fixed.clone(),
        tail,
    }
}

pub(crate) fn instantiate_ty_with_fresh_type_vars_for_call_site(
    ty: &Ty,
    next_id: &mut usize,
) -> Ty {
    let mut ty_mapping = HashMap::new();
    let mut row_mapping: HashMap<RowVarId, RowVarId> = HashMap::new();
    instantiate_ty_with_fresh_type_vars(ty, &mut ty_mapping, &mut row_mapping, next_id)
}

/// Freshen a "type scheme" — a slice of declaration-side `Ty` templates that
/// share one type-parameter / row-variable scope — into a fresh instantiation.
///
/// All `Ty::Var(name)` occurrences with the same name across `templates` map to
/// the same fresh variable; ditto for row-variable tails. Different calls (with
/// the caller's `next_id` continuing to advance) yield disjoint fresh names.
///
/// Inputs are expected to be declaration-side templates — `Ty::Var(...)` stands
/// for a *declared* type parameter (e.g. `a` in `Maybe a`), not for an existing
/// inference variable. Passing a `Ty` that already contains inference variables
/// will rename those, which is almost certainly a bug at the caller.
///
/// Used by:
/// - effect-member instantiation (one handler clause's params + result share
///   the same type-parameter scope), and
/// - (forthcoming, GU-S3) generic union / record constructor instantiation and
///   generic-record field access — one constructor's arg-types + result
///   `Ty::Con` share the same scope, as do a record-field template + its
///   receiver `Ty::Con`.
pub(crate) fn freshen_type_scheme(templates: &[Ty], next_id: &mut usize) -> Vec<Ty> {
    let mut ty_mapping = HashMap::new();
    let mut row_mapping: HashMap<RowVarId, RowVarId> = HashMap::new();
    templates
        .iter()
        .map(|template| {
            instantiate_ty_with_fresh_type_vars(
                template,
                &mut ty_mapping,
                &mut row_mapping,
                next_id,
            )
        })
        .collect()
}

pub(crate) fn match_function_argument_type(
    required: &Ty,
    actual: &Ty,
    type_subst: &mut TypeSubst,
    row_subst: &mut RowSubst,
    env: &TypeEnv,
    next_id: &mut usize,
) -> Result<(), FunctionArgMismatch> {
    let required = apply_type_substitution(required, type_subst, row_subst, env);
    let actual = instantiate_ty_with_fresh_type_vars_for_call_site(actual, next_id);
    if unify_types_with_subst(&required, &actual, type_subst, row_subst, env, next_id) {
        return Ok(());
    }
    Err(FunctionArgMismatch {
        required,
        actual: apply_type_substitution(&actual, type_subst, row_subst, env),
    })
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
    let Ty::Fun {
        params,
        result,
        effects: _,
    } = op_ty
    else {
        return None;
    };
    let mut scheme = params;
    scheme.push(*result);
    let mut freshened = freshen_type_scheme(&scheme, next_id);
    let result = freshened
        .pop()
        .expect("handler clause scheme always contains a result template");
    Some((freshened, result))
}

pub(crate) fn ty_contains_type_var(ty: &Ty) -> bool {
    match ty {
        Ty::Var(_) => true,
        Ty::List(inner) => ty_contains_type_var(inner),
        Ty::Tuple(items) => items.iter().any(ty_contains_type_var),
        Ty::Fun {
            params,
            result,
            effects: _,
        } => params.iter().any(ty_contains_type_var) || ty_contains_type_var(result),
        Ty::Con { args, .. } => args.iter().any(ty_contains_type_var),
        Ty::Handler { .. } => false,
        Ty::Int | Ty::Float | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => false,
    }
}

fn ty_contains_anonymous_type_hole(ty: &Ty) -> bool {
    match ty {
        Ty::Var(name) => name.starts_with("__goby_type_hole_"),
        Ty::List(inner) => ty_contains_anonymous_type_hole(inner),
        Ty::Tuple(items) => items.iter().any(ty_contains_anonymous_type_hole),
        Ty::Fun {
            params,
            result,
            effects: _,
        } => {
            params.iter().any(ty_contains_anonymous_type_hole)
                || ty_contains_anonymous_type_hole(result)
        }
        Ty::Con { args, .. } => args.iter().any(ty_contains_anonymous_type_hole),
        Ty::Handler { .. } | Ty::Int | Ty::Float | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => {
            false
        }
    }
}

pub(crate) fn type_hole_conflict_note(expected: &Ty) -> &'static str {
    if ty_contains_anonymous_type_hole(expected) {
        " (anonymous type-hole `_` constraints conflict)"
    } else {
        ""
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_env() -> TypeEnv {
        TypeEnv::empty()
    }

    #[test]
    fn matches_generic_callback_by_instantiating_actual_type() {
        let env = empty_env();
        let required = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow::closed_empty(),
        };
        let actual = Ty::Fun {
            params: vec![Ty::Var("a".to_string())],
            result: Box::new(Ty::Var("a".to_string())),
            effects: EffectRow::closed_empty(),
        };
        let mut subst = TypeSubst::new();
        let mut row_subst = RowSubst::new();
        let mut next_id = 0;

        let result = match_function_argument_type(
            &required,
            &actual,
            &mut subst,
            &mut row_subst,
            &env,
            &mut next_id,
        );

        assert!(
            result.is_ok(),
            "generic callback should instantiate successfully"
        );
    }

    #[test]
    fn reports_required_and_actual_function_types_on_mismatch() {
        let env = empty_env();
        let required = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Unit),
            effects: EffectRow::closed_empty(),
        };
        let actual = Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Unit),
            effects: EffectRow::closed_empty(),
        };
        let mut subst = TypeSubst::new();
        let mut row_subst = RowSubst::new();
        let mut next_id = 0;

        let mismatch = match_function_argument_type(
            &required,
            &actual,
            &mut subst,
            &mut row_subst,
            &env,
            &mut next_id,
        )
        .expect_err("incompatible callback types should mismatch");

        assert_eq!(mismatch.required, required);
        assert_eq!(mismatch.actual, actual);
    }

    #[test]
    fn ep1d_function_unify_rejects_differing_closed_rows() {
        // EP-1d: replaces EP-1c's "ignore effects" placeholder. Two closed
        // rows that disagree must now fail unification.
        let env = empty_env();
        let mut subst = TypeSubst::new();
        let mut row_subst = RowSubst::new();
        let mut next_id = 0;
        let lhs = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Unit),
            effects: EffectRow::closed_empty(),
        };
        let rhs = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Unit),
            effects: EffectRow::closed_from(["Print".to_string()]),
        };
        assert!(
            !unify_types_with_subst(&lhs, &rhs, &mut subst, &mut row_subst, &env, &mut next_id),
            "EP-1d: differing closed rows must fail unification"
        );
    }

    #[test]
    fn ep1d_function_unify_accepts_open_against_closed_via_binding() {
        // closed_empty ~ open{; e} succeeds and binds `e := closed_empty`.
        let env = empty_env();
        let mut subst = TypeSubst::new();
        let mut row_subst = RowSubst::new();
        let mut next_id = 0;
        let closed = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow::closed_empty(),
        };
        let open = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow {
                fixed: std::collections::BTreeSet::new(),
                tail: Some(RowVarId("e".to_string())),
            },
        };
        assert!(unify_types_with_subst(
            &closed,
            &open,
            &mut subst,
            &mut row_subst,
            &env,
            &mut next_id
        ));
        assert_eq!(
            row_subst.get(&RowVarId("e".to_string())).cloned(),
            Some(EffectRow::closed_empty())
        );
    }

    #[test]
    fn ep1d_are_compatible_rejects_differing_closed_rows() {
        // EP-1d (was EP-1c "ignore" placeholder): are_compatible now consults
        // unify_effect_rows so a closed-empty annotation cannot satisfy a
        // closed{Print} expectation.
        let env = empty_env();
        let lhs = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Unit),
            effects: EffectRow::closed_empty(),
        };
        let rhs = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Unit),
            effects: EffectRow::closed_from(["Print".to_string()]),
        };
        assert!(
            !env.are_compatible(&lhs, &rhs),
            "EP-1d: closed_empty must not match closed{{Print}}"
        );
    }

    #[test]
    fn ep1d_are_compatible_accepts_closed_with_open_row_variable() {
        let env = empty_env();
        let closed = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow::closed_empty(),
        };
        let open = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow {
                fixed: std::collections::BTreeSet::new(),
                tail: Some(RowVarId("e".to_string())),
            },
        };
        assert!(env.are_compatible(&closed, &open));
        assert!(env.are_compatible(&open, &closed));
    }

    #[test]
    fn ep1d_are_compatible_avoids_fresh_row_collision_with_input_types() {
        // Pass2 regression: are_compatible used to start `next_id` at 0.
        // If either input already contained `__goby_fresh_row_0` (e.g. a
        // residue of an earlier instantiation), unify_open_with_open could
        // reissue that same name and yield an accidental occurs failure.
        // are_compatible now seeds next_id past any fresh_row_N already
        // present, so this comparison succeeds.
        let env = empty_env();
        let lhs = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow {
                fixed: std::collections::BTreeSet::new(),
                tail: Some(RowVarId("__goby_fresh_row_0".to_string())),
            },
        };
        let rhs = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow {
                fixed: std::collections::BTreeSet::new(),
                tail: Some(RowVarId("__goby_fresh_row_1".to_string())),
            },
        };
        assert!(env.are_compatible(&lhs, &rhs));
    }

    #[test]
    fn ep1d_are_compatible_matches_identical_closed_rows() {
        let env = empty_env();
        let lhs = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Unit),
            effects: EffectRow::closed_from(["Print".to_string()]),
        };
        let rhs = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Unit),
            effects: EffectRow::closed_from(["Print".to_string()]),
        };
        assert!(env.are_compatible(&lhs, &rhs));
    }
}

#[cfg(test)]
mod effect_rows_unify_tests {
    use std::collections::BTreeSet;

    use super::*;

    fn closed(effects: &[&str]) -> EffectRow {
        EffectRow::closed_from(effects.iter().map(|s| s.to_string()))
    }

    fn open(effects: &[&str], var: &str) -> EffectRow {
        EffectRow {
            fixed: effects.iter().map(|s| s.to_string()).collect(),
            tail: Some(RowVarId(var.to_string())),
        }
    }

    fn rv(name: &str) -> RowVarId {
        RowVarId(name.to_string())
    }

    #[test]
    fn closed_closed_equal_unifies() {
        let mut subst = RowSubst::new();
        let mut next_id = 0usize;
        assert!(unify_effect_rows(
            &closed(&["Print"]),
            &closed(&["Print"]),
            &mut subst,
            &mut next_id
        ));
    }

    #[test]
    fn closed_closed_different_fails() {
        let mut subst = RowSubst::new();
        let mut next_id = 0usize;
        assert!(!unify_effect_rows(
            &closed(&["Print"]),
            &closed(&["Read"]),
            &mut subst,
            &mut next_id
        ));
    }

    #[test]
    fn closed_closed_subset_fails() {
        // EP-0: closed rows are exact, no widening.
        let mut subst = RowSubst::new();
        let mut next_id = 0usize;
        assert!(!unify_effect_rows(
            &closed(&["Print"]),
            &closed(&["Print", "Read"]),
            &mut subst,
            &mut next_id
        ));
    }

    #[test]
    fn closed_closed_empty_unifies() {
        let mut subst = RowSubst::new();
        let mut next_id = 0usize;
        assert!(unify_effect_rows(
            &EffectRow::closed_empty(),
            &EffectRow::closed_empty(),
            &mut subst,
            &mut next_id
        ));
    }

    #[test]
    fn closed_open_subset_binds_residual() {
        // closed{Print, Read} ~ open{Print; e}: binds e := closed{Read}.
        let mut subst = RowSubst::new();
        let mut next_id = 0usize;
        assert!(unify_effect_rows(
            &closed(&["Print", "Read"]),
            &open(&["Print"], "e"),
            &mut subst,
            &mut next_id
        ));
        let bound = subst.get(&rv("e")).cloned().expect("e must be bound");
        assert_eq!(bound, closed(&["Read"]));
    }

    #[test]
    fn open_closed_subset_binds_residual_symmetric() {
        // Same as above but reversed argument order.
        let mut subst = RowSubst::new();
        let mut next_id = 0usize;
        assert!(unify_effect_rows(
            &open(&["Print"], "e"),
            &closed(&["Print", "Read"]),
            &mut subst,
            &mut next_id
        ));
        assert_eq!(subst.get(&rv("e")).cloned().unwrap(), closed(&["Read"]));
    }

    #[test]
    fn closed_open_violates_subset_fails() {
        // open side requires `Print, Read` but closed side only has `Print`.
        let mut subst = RowSubst::new();
        let mut next_id = 0usize;
        assert!(!unify_effect_rows(
            &closed(&["Print"]),
            &open(&["Print", "Read"], "e"),
            &mut subst,
            &mut next_id
        ));
    }

    #[test]
    fn closed_open_with_empty_open_binds_var_to_closed() {
        // closed{Print} ~ open{; e}: binds e := closed{Print}.
        let mut subst = RowSubst::new();
        let mut next_id = 0usize;
        assert!(unify_effect_rows(
            &closed(&["Print"]),
            &open(&[], "e"),
            &mut subst,
            &mut next_id
        ));
        assert_eq!(subst.get(&rv("e")).cloned().unwrap(), closed(&["Print"]));
    }

    #[test]
    fn open_open_distinct_vars_introduces_fresh_tail() {
        // open{Print; e1} ~ open{Read; e2}:
        //   e1 := open{Read; r}, e2 := open{Print; r} for fresh r.
        let mut subst = RowSubst::new();
        let mut next_id = 7usize; // arbitrary starting id
        assert!(unify_effect_rows(
            &open(&["Print"], "e1"),
            &open(&["Read"], "e2"),
            &mut subst,
            &mut next_id
        ));
        let fresh = RowVarId("__goby_fresh_row_7".to_string());
        let bind_e1 = subst.get(&rv("e1")).cloned().expect("e1 bound");
        let bind_e2 = subst.get(&rv("e2")).cloned().expect("e2 bound");
        assert_eq!(bind_e1.fixed, BTreeSet::from(["Read".to_string()]));
        assert_eq!(bind_e1.tail, Some(fresh.clone()));
        assert_eq!(bind_e2.fixed, BTreeSet::from(["Print".to_string()]));
        assert_eq!(bind_e2.tail, Some(fresh));
        assert_eq!(next_id, 8);
    }

    #[test]
    fn open_open_same_var_requires_fixed_equality() {
        // EP-1d clarification: same row var on both sides demands matching
        // fixed sets. (LANGUAGE_SPEC §5 update lands in Step 6.)
        let mut subst = RowSubst::new();
        let mut next_id = 0usize;
        assert!(unify_effect_rows(
            &open(&["Print"], "e"),
            &open(&["Print"], "e"),
            &mut subst,
            &mut next_id
        ));
        assert!(
            subst.is_empty(),
            "no bindings needed for same-var equal rows"
        );

        let mut subst2 = RowSubst::new();
        let mut next_id2 = 0usize;
        assert!(!unify_effect_rows(
            &open(&["Print"], "e"),
            &open(&["Read"], "e"),
            &mut subst2,
            &mut next_id2
        ));
    }

    #[test]
    fn occurs_check_rejects_self_referential_binding() {
        // Pre-populate `e := open{; e}` to simulate a cycle, then attempt
        // to dereference it. apply_row_substitution should report None.
        let mut subst = RowSubst::new();
        subst.insert(
            rv("e"),
            EffectRow {
                fixed: BTreeSet::new(),
                tail: Some(rv("e")),
            },
        );
        let resolved = apply_row_substitution(&open(&[], "e"), &subst);
        assert!(resolved.is_none(), "direct cycle must be reported");
    }

    #[test]
    fn occurs_check_rejects_indirect_cycle() {
        // a -> b -> a: indirect cycle must also be reported.
        let mut subst = RowSubst::new();
        subst.insert(
            rv("a"),
            EffectRow {
                fixed: BTreeSet::new(),
                tail: Some(rv("b")),
            },
        );
        subst.insert(
            rv("b"),
            EffectRow {
                fixed: BTreeSet::new(),
                tail: Some(rv("a")),
            },
        );
        assert!(apply_row_substitution(&open(&[], "a"), &subst).is_none());
        assert!(apply_row_substitution(&open(&[], "b"), &subst).is_none());
    }

    #[test]
    fn bind_row_variable_rejects_self_reference() {
        let mut subst = RowSubst::new();
        // Trying to bind e := open{; e} must fail the occurs check.
        let row = EffectRow {
            fixed: BTreeSet::new(),
            tail: Some(rv("e")),
        };
        assert!(!bind_row_variable(&rv("e"), &row, &mut subst));
        assert!(subst.is_empty());
    }

    #[test]
    fn instantiate_ty_freshens_row_variable_per_call_site() {
        // EP-1d: a Ty::Fun whose effects.tail is `e` must come back with a
        // distinct fresh tail every time
        // `instantiate_ty_with_fresh_type_vars_for_call_site` is used.
        let original = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: open(&[], "e"),
        };
        let mut next_id = 0usize;
        let first = instantiate_ty_with_fresh_type_vars_for_call_site(&original, &mut next_id);
        let second = instantiate_ty_with_fresh_type_vars_for_call_site(&original, &mut next_id);
        let first_tail = match first {
            Ty::Fun { effects, .. } => effects.tail.expect("fresh tail must be set"),
            _ => panic!("expected Ty::Fun"),
        };
        let second_tail = match second {
            Ty::Fun { effects, .. } => effects.tail.expect("fresh tail must be set"),
            _ => panic!("expected Ty::Fun"),
        };
        assert_ne!(
            first_tail, second_tail,
            "independent call sites must get distinct fresh row variables"
        );
        assert!(first_tail.0.starts_with("__goby_fresh_row_"));
        assert!(second_tail.0.starts_with("__goby_fresh_row_"));
    }

    #[test]
    fn instantiate_ty_reuses_fresh_row_var_within_one_call_site() {
        // Same row var shared between callback effects and result effects must
        // receive the same fresh row var so they continue to unify.
        // Shape: (Int -> Int can {e}) -> Int can {e}
        let original = Ty::Fun {
            params: vec![Ty::Fun {
                params: vec![Ty::Int],
                result: Box::new(Ty::Int),
                effects: open(&[], "e"),
            }],
            result: Box::new(Ty::Int),
            effects: open(&[], "e"),
        };
        let mut next_id = 0usize;
        let inst = instantiate_ty_with_fresh_type_vars_for_call_site(&original, &mut next_id);
        let Ty::Fun {
            params,
            effects: outer,
            ..
        } = inst
        else {
            panic!("expected Ty::Fun");
        };
        let callback_effects = match &params[0] {
            Ty::Fun { effects, .. } => effects.clone(),
            _ => panic!("expected callback Ty::Fun"),
        };
        assert_eq!(
            outer.tail, callback_effects.tail,
            "shared `{{e}}` must map to the same fresh tail"
        );
    }

    // GU-S3 D-6: `freshen_type_scheme` is the shared declaration-side scheme
    // freshener that effect-member instantiation (and forthcoming union /
    // record constructor instantiation) routes through. The contract these
    // tests pin is what GU-S3 will rely on when wiring constructor schemes.

    #[test]
    fn freshen_type_scheme_shares_mapping_across_templates() {
        // Templates: [a, a -> b]
        // Expectation: the `Ty::Var("a")` in the first slot and the param
        // `Ty::Var("a")` in the second slot map to the same fresh name; `b`
        // gets a distinct fresh name.
        let templates = vec![
            Ty::Var("a".to_string()),
            Ty::Fun {
                params: vec![Ty::Var("a".to_string())],
                result: Box::new(Ty::Var("b".to_string())),
                effects: closed(&[]),
            },
        ];
        let mut next_id = 0usize;
        let out = freshen_type_scheme(&templates, &mut next_id);
        let fresh_a_outer = match &out[0] {
            Ty::Var(name) => name.clone(),
            _ => panic!("expected Ty::Var at slot 0"),
        };
        let (fresh_a_inner, fresh_b) = match &out[1] {
            Ty::Fun { params, result, .. } => {
                let a = match &params[0] {
                    Ty::Var(name) => name.clone(),
                    _ => panic!("expected Ty::Var inside params"),
                };
                let b = match result.as_ref() {
                    Ty::Var(name) => name.clone(),
                    _ => panic!("expected Ty::Var as result"),
                };
                (a, b)
            }
            _ => panic!("expected Ty::Fun at slot 1"),
        };
        assert_eq!(
            fresh_a_outer, fresh_a_inner,
            "same `a` across templates must share the same fresh name"
        );
        assert_ne!(
            fresh_a_outer, fresh_b,
            "distinct template vars must receive distinct fresh names"
        );
        assert!(fresh_a_outer.starts_with("__goby_fresh_ty_"));
        assert!(fresh_b.starts_with("__goby_fresh_ty_"));
    }

    #[test]
    fn freshen_type_scheme_shares_row_var_across_templates() {
        // Templates: two `Ty::Fun` values that both carry an open row tail `e`.
        // Expectation: both row tails resolve to the same fresh row var.
        let template_one = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: open(&[], "e"),
        };
        let template_two = Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Str),
            effects: open(&[], "e"),
        };
        let mut next_id = 0usize;
        let out = freshen_type_scheme(&[template_one, template_two], &mut next_id);
        let tail_one = match &out[0] {
            Ty::Fun { effects, .. } => effects.tail.clone().expect("fresh tail must be set"),
            _ => panic!("expected Ty::Fun"),
        };
        let tail_two = match &out[1] {
            Ty::Fun { effects, .. } => effects.tail.clone().expect("fresh tail must be set"),
            _ => panic!("expected Ty::Fun"),
        };
        assert_eq!(
            tail_one, tail_two,
            "shared row tail `e` must map to the same fresh row var across templates"
        );
        assert!(tail_one.0.starts_with("__goby_fresh_row_"));
    }

    #[test]
    fn freshen_type_scheme_distinct_calls_yield_disjoint_fresh_names() {
        // Two successive calls with a continuing `next_id` produce disjoint
        // fresh names — i.e. the per-call mapping is local, not memoised.
        let templates = vec![Ty::Var("a".to_string())];
        let mut next_id = 0usize;
        let first = freshen_type_scheme(&templates, &mut next_id);
        let second = freshen_type_scheme(&templates, &mut next_id);
        let first_name = match &first[0] {
            Ty::Var(name) => name.clone(),
            _ => panic!("expected Ty::Var"),
        };
        let second_name = match &second[0] {
            Ty::Var(name) => name.clone(),
            _ => panic!("expected Ty::Var"),
        };
        assert_ne!(
            first_name, second_name,
            "successive calls must produce distinct fresh names"
        );
    }

    #[test]
    fn freshen_type_scheme_advances_next_id_once_per_unique_var() {
        // Templates: [a, a, b]
        // Two distinct template names (`a`, `b`) should each consume exactly
        // one fresh id. Duplicate occurrences of `a` reuse the same fresh
        // name and must not bump `next_id` again.
        let templates = vec![
            Ty::Var("a".to_string()),
            Ty::Var("a".to_string()),
            Ty::Var("b".to_string()),
        ];
        let mut next_id = 0usize;
        let _ = freshen_type_scheme(&templates, &mut next_id);
        assert_eq!(
            next_id, 2,
            "next_id must advance exactly once per unique template var"
        );
    }

    // GU-S3 D-6: pin the `instantiate_handler_clause_signature` rewrite that
    // routes through `freshen_type_scheme`. The two cases below are the ones
    // Codex pass-1 flagged as worth covering directly so future edits cannot
    // silently break the shared-mapping / nullary-op contracts.

    fn env_with_handler_op(op_name: &str, op_ty: Ty) -> TypeEnv {
        // The real handler-clause lookup goes through
        // `effect_candidates_for_operation`, which inspects the global
        // binding's `source` to learn which effect declares `op_name`.
        // When exactly one effect is identified, the helper looks up the
        // qualified name `Eff.op` instead of the bare op. Register both
        // entries so the test path mirrors the real resolver output.
        let mut env = TypeEnv::empty();
        let binding = crate::typecheck_env::GlobalBinding::Resolved {
            ty: op_ty,
            source: "effect `TestEff` member".to_string(),
        };
        env.globals.insert(op_name.to_string(), binding.clone());
        env.globals.insert(format!("TestEff.{}", op_name), binding);
        env
    }

    #[test]
    fn instantiate_handler_clause_signature_shares_var_between_param_and_result() {
        // Operation type: `a -> a can {}` (declared parametric).
        // The freshened scheme must use the same fresh name for both the
        // param and the result so the typechecker can later unify them.
        let op_ty = Ty::Fun {
            params: vec![Ty::Var("a".to_string())],
            result: Box::new(Ty::Var("a".to_string())),
            effects: closed(&[]),
        };
        let env = env_with_handler_op("op", op_ty);
        let mut next_id = 0usize;
        let (params, result) = instantiate_handler_clause_signature(&env, "op", &mut next_id)
            .expect("op declared as Ty::Fun must yield a freshened signature");
        let fresh_param = match &params[0] {
            Ty::Var(name) => name.clone(),
            _ => panic!("expected Ty::Var in params[0]"),
        };
        let fresh_result = match &result {
            Ty::Var(name) => name.clone(),
            _ => panic!("expected Ty::Var in result"),
        };
        assert_eq!(
            fresh_param, fresh_result,
            "shared `a` between param and result must map to a single fresh name"
        );
        assert!(fresh_param.starts_with("__goby_fresh_ty_"));
    }

    #[test]
    fn instantiate_handler_clause_signature_handles_nullary_op() {
        // Operation type: `() -> Int can {}` (no params, concrete result).
        // The freshened scheme must come back with empty params and an
        // unchanged result; no fresh ids are consumed.
        let op_ty = Ty::Fun {
            params: vec![],
            result: Box::new(Ty::Int),
            effects: closed(&[]),
        };
        let env = env_with_handler_op("op", op_ty);
        let mut next_id = 0usize;
        let (params, result) = instantiate_handler_clause_signature(&env, "op", &mut next_id)
            .expect("op declared as Ty::Fun must yield a freshened signature");
        assert!(
            params.is_empty(),
            "nullary op must come back with no params"
        );
        assert_eq!(result, Ty::Int);
        assert_eq!(
            next_id, 0,
            "a fully-concrete nullary op must not consume any fresh ids"
        );
    }

    // ---- GU-S3 CA-2: `unifies_with_annotation` の rigid/flexible 区別を pin ----

    fn empty_env() -> TypeEnv {
        TypeEnv::empty()
    }

    fn fresh_ty(n: usize) -> Ty {
        Ty::Var(format!("__goby_fresh_ty_{}", n))
    }

    fn rigid_ty(name: &str) -> Ty {
        Ty::Var(name.to_string())
    }

    #[test]
    fn unifies_with_annotation_rejects_rigid_var_against_concrete() {
        // `f : a -> Int; f x = x` shape: declared returns `a` (rigid),
        // body returns `Ty::Int` (concrete). Must be rejected.
        let env = empty_env();
        let mut next_id = 0;
        assert!(
            !unifies_with_annotation(&rigid_ty("a"), &Ty::Int, &env, &mut next_id),
            "rigid `a` must not bind to concrete `Int`"
        );
    }

    #[test]
    fn unifies_with_annotation_rejects_const_unit_to_rigid_var() {
        // `const : Unit -> a; const = 1` shape: declared returns `a` (rigid),
        // body returns `Ty::Int`. Same rejection from the opposite side
        // (rigid on expected, concrete on actual) — side-independent.
        let env = empty_env();
        let mut next_id = 0;
        assert!(
            !unifies_with_annotation(&Ty::Int, &rigid_ty("a"), &env, &mut next_id),
            "rigid `a` must not be bound by a concrete actual either"
        );
    }

    #[test]
    fn unifies_with_annotation_accepts_concrete_maybe_against_flexible() {
        // `Maybe Int` vs `Maybe __goby_fresh_ty_0`: flexible bind ok.
        let env = empty_env();
        let mut next_id = 1;
        let expected = Ty::Con {
            name: "Maybe".to_string(),
            args: vec![Ty::Int],
        };
        let actual = Ty::Con {
            name: "Maybe".to_string(),
            args: vec![fresh_ty(0)],
        };
        assert!(
            unifies_with_annotation(&expected, &actual, &env, &mut next_id),
            "flexible `__goby_fresh_ty_0` should bind to `Int` under `Maybe`"
        );
    }

    #[test]
    fn unifies_with_annotation_accepts_two_flexible_vars_either_side() {
        // `__goby_fresh_ty_0` ~ `__goby_fresh_ty_1`: both flexible, ok.
        let env = empty_env();
        let mut next_id = 2;
        assert!(
            unifies_with_annotation(&fresh_ty(0), &fresh_ty(1), &env, &mut next_id),
            "two flexible vars should unify"
        );
    }

    #[test]
    fn unifies_with_annotation_treats_unknown_as_wildcard() {
        // `Ty::Unknown` on either side is a wildcard (`are_compatible` 互換)。
        let env = empty_env();
        let mut next_id = 0;
        assert!(unifies_with_annotation(
            &Ty::Unknown,
            &Ty::Int,
            &env,
            &mut next_id
        ));
        assert!(unifies_with_annotation(
            &rigid_ty("a"),
            &Ty::Unknown,
            &env,
            &mut next_id
        ));
    }

    #[test]
    fn unifies_with_annotation_concrete_concrete_mismatch_rejects() {
        // Sanity: ordinary concrete mismatch still rejects.
        let env = empty_env();
        let mut next_id = 0;
        assert!(
            !unifies_with_annotation(&Ty::Int, &Ty::Str, &env, &mut next_id),
            "Int / Str must not unify"
        );
    }

    #[test]
    fn unifies_with_annotation_same_rigid_var_accepts() {
        // Identical rigid `a` ~ `a` is just equality.
        let env = empty_env();
        let mut next_id = 0;
        assert!(unifies_with_annotation(
            &rigid_ty("a"),
            &rigid_ty("a"),
            &env,
            &mut next_id
        ));
    }

    #[test]
    fn unifies_with_annotation_flexible_var_against_concrete_either_side() {
        // Codex pass-1 指摘: flexible `__goby_fresh_ty_0` ~ concrete の
        // 双方向 bind を pin。expected 側 / actual 側どちらに flexible が
        // 来ても束縛されることを確認。
        let env = empty_env();
        let mut next_id = 1;
        assert!(unifies_with_annotation(
            &fresh_ty(0),
            &Ty::Int,
            &env,
            &mut next_id
        ));
        let mut next_id = 1;
        assert!(unifies_with_annotation(
            &Ty::Int,
            &fresh_ty(0),
            &env,
            &mut next_id
        ));
    }

    #[test]
    fn unifies_with_annotation_handler_special_case_either_side() {
        // Codex pass-1 指摘: `Handler` annotation 特例の対称性を pin する。
        // 宣言戻り値比較では通常 `declared = Con("Handler", [..])` /
        // `inferred = Ty::Handler { .. }` (annotation 側に `Ty::Handler` 形
        // は構文上書けないが、内部表現としては片方向に偏らせない方が安全)。
        let env = empty_env();
        let mut next_id = 0;
        let handler_con = Ty::Con {
            name: "Handler".to_string(),
            args: vec![Ty::Con {
                name: "Log".to_string(),
                args: vec![],
            }],
        };
        let handler_value = Ty::Handler {
            covered_ops: ["log".to_string()].into_iter().collect(),
        };
        // Note: handler_ops_from_type_args walks the env for `effect Log` to
        // expand `Log` into its op set; without that env the unification
        // returns `None`. Test the symmetry instead by ensuring the two
        // shapes match an empty handler trivially (`Handler` with no args
        // → expected_ops = empty set).
        let _ = (handler_con, handler_value);
        let empty_handler_con = Ty::Con {
            name: "Handler".to_string(),
            args: vec![],
        };
        let empty_handler_value = Ty::Handler {
            covered_ops: Default::default(),
        };
        assert!(unifies_with_annotation(
            &empty_handler_con,
            &empty_handler_value,
            &env,
            &mut next_id
        ));
        assert!(unifies_with_annotation(
            &empty_handler_value,
            &empty_handler_con,
            &env,
            &mut next_id
        ));
    }

    #[test]
    fn unifies_with_annotation_seeds_next_id_for_effect_rows() {
        // Codex pass-1 指摘: `Ty::Fun` の effect row が unification で fresh
        // row を作るとき、`next_id` seed に既存の `__goby_fresh_row_N` が
        // 含まれていないと衝突する。caller (`typecheck_stmt.rs`) は
        // `max_fresh_row_id` を加味する。helper 単体テストで row var 経由
        // でも fail しないことを pin。
        //
        // expected = `Int -> Int can __goby_fresh_row_3`,
        // actual   = `Int -> Int can __goby_fresh_row_5`. 別 var の open ~ open
        // は新しい fresh row を作って両方を bind。`next_id` を 6 で seed
        // しておけば衝突しない。
        let env = empty_env();
        let mut next_id = 6;
        let expected = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow {
                fixed: Default::default(),
                tail: Some(RowVarId("__goby_fresh_row_3".to_string())),
            },
        };
        let actual = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow {
                fixed: Default::default(),
                tail: Some(RowVarId("__goby_fresh_row_5".to_string())),
            },
        };
        assert!(
            unifies_with_annotation(&expected, &actual, &env, &mut next_id),
            "open ~ open effect rows must unify under non-colliding next_id seed"
        );
    }

    #[test]
    fn max_fresh_ty_id_returns_one_past_max() {
        // Walks Fun / Con / List / Tuple structurally. Returns max + 1.
        let ty = Ty::Fun {
            params: vec![
                Ty::List(Box::new(fresh_ty(2))),
                Ty::Con {
                    name: "Maybe".to_string(),
                    args: vec![fresh_ty(5)],
                },
            ],
            result: Box::new(fresh_ty(1)),
            effects: EffectRow::closed_empty(),
        };
        assert_eq!(max_fresh_ty_id_in_ty(&ty), 6);
    }

    #[test]
    fn max_fresh_ty_id_ignores_rigid_and_non_var_types() {
        // Rigid `a` and concrete types contribute nothing.
        let ty = Ty::Fun {
            params: vec![rigid_ty("a"), Ty::Int],
            result: Box::new(Ty::Str),
            effects: EffectRow::closed_empty(),
        };
        assert_eq!(max_fresh_ty_id_in_ty(&ty), 0);
    }
}
