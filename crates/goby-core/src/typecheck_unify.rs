use std::collections::HashMap;

use crate::typecheck_check::effect_candidates_for_operation;
#[cfg(test)]
use crate::typecheck_env::{EffectRow, RowVarId};
use crate::typecheck_env::{Ty, TypeEnv, TypeSubst};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionArgMismatch {
    pub(crate) required: Ty,
    pub(crate) actual: Ty,
}

pub(crate) fn apply_type_substitution(ty: &Ty, subst: &TypeSubst, env: &TypeEnv) -> Ty {
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
        Ty::Fun {
            params,
            result,
            effects,
        } => Ty::Fun {
            params: params
                .iter()
                .map(|param| apply_type_substitution(param, subst, env))
                .collect(),
            result: Box::new(apply_type_substitution(&result, subst, env)),
            effects,
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

pub(crate) fn unify_types_with_subst(
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
                effects: _,
            },
            Ty::Fun {
                params: right_params,
                result: right_result,
                effects: _,
            },
        ) if left_params.len() == right_params.len() => {
            // EP-1c: effects field is intentionally ignored here. Row
            // unification will arrive in EP-1d together with `unify_effect_rows`.
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

pub(crate) fn instantiate_ty_with_fresh_type_vars(
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
        Ty::Fun {
            params,
            result,
            effects,
        } => Ty::Fun {
            params: params
                .iter()
                .map(|param| instantiate_ty_with_fresh_type_vars(param, mapping, next_id))
                .collect(),
            result: Box::new(instantiate_ty_with_fresh_type_vars(
                result, mapping, next_id,
            )),
            // EP-1c: row variables are not yet freshened per call site. EP-1d
            // must extend this with a row-var mapping so independent calls of
            // a row-polymorphic function do not accidentally unify their row
            // tails through a shared `RowVarId`.
            effects: effects.clone(),
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

pub(crate) fn instantiate_ty_with_fresh_type_vars_for_call_site(
    ty: &Ty,
    next_id: &mut usize,
) -> Ty {
    let mut mapping = HashMap::new();
    instantiate_ty_with_fresh_type_vars(ty, &mut mapping, next_id)
}

pub(crate) fn match_function_argument_type(
    required: &Ty,
    actual: &Ty,
    subst: &mut TypeSubst,
    env: &TypeEnv,
    next_id: &mut usize,
) -> Result<(), FunctionArgMismatch> {
    let required = apply_type_substitution(required, subst, env);
    let actual = instantiate_ty_with_fresh_type_vars_for_call_site(actual, next_id);
    if unify_types_with_subst(&required, &actual, subst, env) {
        return Ok(());
    }
    Err(FunctionArgMismatch {
        required,
        actual: apply_type_substitution(&actual, subst, env),
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
    let mut mapping = HashMap::new();
    let params = params
        .iter()
        .map(|param| instantiate_ty_with_fresh_type_vars(param, &mut mapping, next_id))
        .collect();
    let result = instantiate_ty_with_fresh_type_vars(&result, &mut mapping, next_id);
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
        Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => false,
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
        Ty::Handler { .. } | Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => false,
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
    use std::collections::HashMap;

    use super::*;

    fn empty_env() -> TypeEnv {
        TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        }
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
        let mut next_id = 0;

        let result =
            match_function_argument_type(&required, &actual, &mut subst, &env, &mut next_id);

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
        let mut next_id = 0;

        let mismatch =
            match_function_argument_type(&required, &actual, &mut subst, &env, &mut next_id)
                .expect_err("incompatible callback types should mismatch");

        assert_eq!(mismatch.required, required);
        assert_eq!(mismatch.actual, actual);
    }

    #[test]
    fn ep1c_function_unify_ignores_residual_effect_rows() {
        // EP-1c contract: unify_types_with_subst treats two function types as
        // compatible if their params/result agree, regardless of residual
        // effect rows. EP-1d will replace this branch with proper row
        // unification — this test will need updating then.
        let env = empty_env();
        let mut subst = TypeSubst::new();
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
            unify_types_with_subst(&lhs, &rhs, &mut subst, &env),
            "EP-1c: differing residual rows must still unify"
        );
    }

    #[test]
    fn ep1c_function_unify_ignores_open_versus_closed_rows() {
        let env = empty_env();
        let mut subst = TypeSubst::new();
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
        assert!(
            unify_types_with_subst(&closed, &open, &mut subst, &env),
            "EP-1c: closed vs open rows must still unify"
        );
    }

    #[test]
    fn ep1c_are_compatible_ignores_residual_effect_rows() {
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
            env.are_compatible(&lhs, &rhs),
            "EP-1c: are_compatible must currently ignore the effect row"
        );
    }
}
