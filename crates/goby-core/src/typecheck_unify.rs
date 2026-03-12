use std::collections::HashMap;

use crate::typecheck_check::effect_candidates_for_operation;
use crate::typecheck_env::{Ty, TypeEnv, TypeSubst};

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

pub(crate) fn ty_contains_type_var(ty: &Ty) -> bool {
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

pub(crate) fn type_hole_conflict_note(expected: &Ty) -> &'static str {
    if ty_contains_anonymous_type_hole(expected) {
        " (anonymous type-hole `_` constraints conflict)"
    } else {
        ""
    }
}
