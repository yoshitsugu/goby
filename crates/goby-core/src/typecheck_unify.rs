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
    let mut ty_mapping = HashMap::new();
    let mut row_mapping: HashMap<RowVarId, RowVarId> = HashMap::new();
    let params = params
        .iter()
        .map(|param| {
            instantiate_ty_with_fresh_type_vars(param, &mut ty_mapping, &mut row_mapping, next_id)
        })
        .collect();
    let result =
        instantiate_ty_with_fresh_type_vars(&result, &mut ty_mapping, &mut row_mapping, next_id);
    Some((params, result))
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
}
