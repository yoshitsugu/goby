use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, InterpolatedPart, Span, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_check::{
    check_expr, env_with_case_pattern_bindings, parse_tuple_member_index,
};
use crate::typecheck_diag::err_name_ambiguous;
use crate::typecheck_env::{Ty, TypeEnv};
use crate::typecheck_render::ty_name;
use crate::typecheck_span::{best_available_expr_span, best_available_name_use_span};

pub(crate) fn ensure_no_ambiguous_refs_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    match expr {
        Expr::Spanned { expr, .. } => ensure_no_ambiguous_refs_in_expr(expr, env, decl_name),
        Expr::IntLit(_) | Expr::FloatLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) => Ok(()),
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
        Expr::Var { name, .. } => {
            ensure_name_not_ambiguous(name, env, decl_name, best_available_name_use_span(expr))
        }
        Expr::Qualified {
            receiver, member, ..
        } => {
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
                                span: best_available_name_use_span(expr),
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
                                span: best_available_name_use_span(expr),
                                message: format!(
                                    "tuple member access `{}` requires tuple receiver, but `{}` type is unresolved",
                                    member, receiver
                                ),
                            })
                        }
                    }
                    other => Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: best_available_name_use_span(expr),
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
            ensure_name_not_ambiguous(
                &format!("{}.{}", receiver, member),
                env,
                decl_name,
                best_available_name_use_span(expr),
            )
        }
        Expr::RecordConstruct {
            constructor,
            fields,
            ..
        } => {
            ensure_name_not_ambiguous(constructor, env, decl_name, best_available_expr_span(expr))?;
            let Some(record) = env.lookup_record_by_constructor(constructor) else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: best_available_expr_span(expr),
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
                        span: best_available_expr_span(expr),
                        message: format!(
                            "duplicate field `{}` in constructor call `{}`",
                            name, constructor
                        ),
                    });
                }
                let Some(expected_ty) = record.fields.get(name) else {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: best_available_expr_span(expr),
                        message: format!(
                            "unknown field `{}` in constructor call `{}`",
                            name, constructor
                        ),
                    });
                };
                let actual_ty = check_expr(value, env);
                if actual_ty == Ty::Unknown {
                    // `Ty::Unknown` actual is a wildcard — preserve the
                    // pre-GR behaviour of skipping the mismatch check.
                    continue;
                }
                if record.type_params.is_empty() {
                    if !env.are_compatible(expected_ty, &actual_ty) {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: best_available_expr_span(expr),
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
                // GU-S3 GR-2 (Codex pass-1): generic record では field
                // template `Ty::Var(a)` をそのまま `are_compatible` で
                // 比較すると `Int vs a` が reject されてしまうので、
                // ここでは個別の field を pass し、generic record 全体
                // の field-vs-field 整合 (e.g. `Same a = Same(left: a,
                // right: a)` で `Same(left: 1, right: "x")` を reject)
                // は下記の generic-record validator で扱う。
            }
            // GU-S3 GR-2 (Codex pass-1 follow-up): generic record の場合、
            // 同一 `type_params[i]` を複数 field が共有しているときの
            // mismatch (`Same(left: 1, right: "x")` を `Same a = Same(
            // left: a, right: a)` に対して reject) を確実に拾う。
            // `infer_record_construct_ty` は失敗時に `Ty::Unknown` を
            // 返すだけで、後段の宣言型比較で wildcard pass してしまうため、
            // ambiguity walker 側で diagnostic を生成する。
            if !record.type_params.is_empty() {
                if let Some((failing_field, expected_ty, actual_ty)) =
                    find_generic_record_field_mismatch(env, &record, fields)
                {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: best_available_expr_span(expr),
                        message: format!(
                            "field `{}` in constructor `{}` has type `{}` but expected `{}`",
                            failing_field,
                            constructor,
                            ty_name(&actual_ty),
                            ty_name(&expected_ty),
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
                    span: best_available_expr_span(expr),
                    message: format!(
                        "missing field(s) in constructor call `{}`: {}",
                        constructor,
                        missing.join(", ")
                    ),
                });
            }
            Ok(())
        }
        Expr::UnaryOp { expr, .. } => ensure_no_ambiguous_refs_in_expr(expr, env, decl_name),
        Expr::BinOp { left, right, .. } => {
            ensure_no_ambiguous_refs_in_expr(left, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(right, env, decl_name)
        }
        Expr::Call { callee, arg, .. } => {
            ensure_no_ambiguous_refs_in_expr(callee, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(arg, env, decl_name)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            let qualified = format!("{}.{}", receiver, method);
            ensure_name_not_ambiguous(
                &qualified,
                env,
                decl_name,
                best_available_name_use_span(expr),
            )?;
            for arg in args {
                ensure_no_ambiguous_refs_in_expr(arg, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, callee, .. } => {
            ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?;
            ensure_name_not_ambiguous(callee, env, decl_name, best_available_name_use_span(expr))
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
            if !matches!(stmts.last(), Some(Stmt::Expr(_, _))) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None, // expr span not yet available
                    message: "block expression must end with an expression".to_string(),
                });
            }
            let mut local_env = env.clone();
            for stmt in stmts {
                match stmt {
                    Stmt::Binding { name, value, .. } | Stmt::MutBinding { name, value, .. } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                        let ty = check_expr(value, &local_env);
                        local_env.locals.insert(name.clone(), ty);
                    }
                    Stmt::Assign { value, .. } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                    }
                    Stmt::Expr(expr, _) => {
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
        Expr::ListIndex { list, index } => {
            ensure_no_ambiguous_refs_in_expr(list, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(index, env, decl_name)
        }
    }
}

pub(crate) fn ensure_no_ambiguous_refs_in_stmts(
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
            Stmt::Expr(expr, _) => ensure_no_ambiguous_refs_in_expr(expr, env, decl_name)?,
        }
    }
    Ok(())
}

/// GU-S3 GR-2 (Codex pass-1 follow-up): for a generic record (`type_params`
/// non-empty), validate that the provided field values agree on the shared
/// `Ty::Var(p)` template params across fields.
///
/// Returns `Some((field_name, expected_ty, actual_ty))` for the first field
/// whose value cannot be unified against the freshened field template under
/// the accumulating substitution, so the caller can produce a diagnostic
/// equivalent to the non-generic mismatch error. Non-generic records and
/// fully-`Ty::Unknown` value lists return `None`.
///
/// Mirrors `infer_record_construct_ty`'s scheme-freshen + per-field
/// `unify_types_with_subst` walk, but stops at the first failure and
/// reports the un-substituted field template (`Ty::Var(p)`) and the
/// concrete actual type so the diagnostic mentions the user-facing
/// template name rather than the internal `__goby_fresh_ty_N`.
fn find_generic_record_field_mismatch(
    env: &TypeEnv,
    record: &crate::typecheck_env::RecordTypeInfo,
    provided_fields: &[(String, crate::ast::Expr)],
) -> Option<(String, crate::typecheck_env::Ty, crate::typecheck_env::Ty)> {
    if record.type_params.is_empty() {
        return None;
    }
    // Iterate declared fields in name-sorted order so the diagnostic is
    // stable across HashMap rehashes — same contract as
    // `infer_record_construct_ty`.
    let mut field_names: Vec<&String> = record.fields.keys().collect();
    field_names.sort();

    let result_template = crate::typecheck_env::Ty::Con {
        name: record.type_name.clone(),
        args: record
            .type_params
            .iter()
            .map(|p| crate::typecheck_env::Ty::Var(p.clone()))
            .collect(),
    };
    let mut scheme: Vec<crate::typecheck_env::Ty> = Vec::with_capacity(1 + field_names.len());
    scheme.push(result_template);
    for fname in &field_names {
        scheme.push(record.fields[*fname].clone());
    }
    let mut next_id = crate::typecheck_unify::next_fresh_ty_id_seed(env);
    let freshened = crate::typecheck_unify::freshen_type_scheme(&scheme, &mut next_id);
    let freshened_field_tys: Vec<crate::typecheck_env::Ty> = freshened[1..].to_vec();

    let mut subst = crate::typecheck_env::TypeSubst::new();
    let mut row_subst = crate::typecheck_env::RowSubst::new();

    let mut provided_map: HashMap<&str, &crate::ast::Expr> = HashMap::new();
    for (name, value) in provided_fields {
        provided_map.insert(name.as_str(), value);
    }

    for (fname, freshened_field_ty) in field_names.iter().zip(freshened_field_tys.iter()) {
        let Some(value) = provided_map.get(fname.as_str()) else {
            continue;
        };
        let actual_ty = check_expr(value, env);
        if actual_ty == crate::typecheck_env::Ty::Unknown {
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
            &mut next_id,
        ) {
            // Report the original declared template (the user-facing
            // `Ty::Var("a")` etc.) rather than the freshened name.
            let original_field_ty = record.fields[*fname].clone();
            return Some(((*fname).clone(), original_field_ty, actual_ty));
        }
    }
    None
}

fn ensure_name_not_ambiguous(
    name: &str,
    env: &TypeEnv,
    decl_name: &str,
    span: Option<Span>,
) -> Result<(), TypecheckError> {
    if env.locals.contains_key(name) {
        return Ok(());
    }
    if let Some(sources) = env.ambiguous_sources(name) {
        return Err(err_name_ambiguous(Some(decl_name), name, sources, span));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ast::Span;
    use crate::typecheck_env::{GlobalBinding, Ty, TypeEnv};

    fn env_with_ambiguous(name: &str, sources: &[&str]) -> TypeEnv {
        let mut globals = HashMap::new();
        globals.insert(
            name.to_string(),
            GlobalBinding::Ambiguous {
                sources: sources.iter().map(|s| s.to_string()).collect(),
            },
        );
        TypeEnv {
            globals,
            ..TypeEnv::empty()
        }
    }

    fn env_with_tuple_local(name: &str, items: Vec<Ty>) -> TypeEnv {
        let mut locals = HashMap::new();
        locals.insert(name.to_string(), Ty::Tuple(items));
        TypeEnv {
            locals,
            ..TypeEnv::empty()
        }
    }

    #[test]
    fn ambiguous_bare_name_error_carries_span() {
        let span = Span::new(3, 5, 3, 8);
        let expr = Expr::Var {
            name: "foo".to_string(),
            span: Some(span),
        };
        let env = env_with_ambiguous("foo", &["mod_a", "mod_b"]);

        let err =
            ensure_no_ambiguous_refs_in_expr(&expr, &env, "decl").expect_err("should be ambiguous");

        assert_eq!(err.span, Some(span));
        assert!(err.message.contains("foo"));
    }

    #[test]
    fn ambiguous_qualified_name_error_carries_span() {
        let span = Span::new(5, 1, 5, 9);
        let expr = Expr::Qualified {
            receiver: "list".to_string(),
            member: "map".to_string(),
            span: Some(span),
        };
        let env = env_with_ambiguous("list.map", &["mod_a", "mod_b"]);

        let err =
            ensure_no_ambiguous_refs_in_expr(&expr, &env, "decl").expect_err("should be ambiguous");

        assert_eq!(err.span, Some(span));
        assert!(err.message.contains("list.map"));
    }

    #[test]
    fn tuple_member_out_of_range_error_carries_span() {
        let span = Span::new(7, 3, 7, 9);
        let expr = Expr::Qualified {
            receiver: "pair".to_string(),
            member: "2".to_string(), // tuple member index: digits only, no dot prefix
            span: Some(span),
        };
        // pair: (Int, Int) — index 2 is out of range
        let env = env_with_tuple_local("pair", vec![Ty::Int, Ty::Int]);

        let err = ensure_no_ambiguous_refs_in_expr(&expr, &env, "decl")
            .expect_err("should be out of range");

        assert_eq!(err.span, Some(span));
        assert!(err.message.contains("out of range"));
    }

    #[test]
    fn ambiguous_method_call_error_carries_span() {
        let span = Span::new(2, 5, 2, 21);
        let expr = Expr::MethodCall {
            receiver: "list".to_string(),
            method: "map".to_string(),
            args: vec![],
            span: Some(span),
        };
        let env = env_with_ambiguous("list.map", &["mod_a", "mod_b"]);

        let err =
            ensure_no_ambiguous_refs_in_expr(&expr, &env, "decl").expect_err("should be ambiguous");

        assert_eq!(err.span, Some(span));
    }

    #[test]
    fn ambiguous_pipeline_callee_error_carries_span() {
        let callee_span = Span::new(1, 7, 1, 10);
        let expr = Expr::Pipeline {
            value: Box::new(Expr::Var {
                name: "xs".to_string(),
                span: None,
            }),
            callee: "foo".to_string(),
            callee_span: Some(callee_span),
        };
        let env = env_with_ambiguous("foo", &["mod_a", "mod_b"]);

        let err =
            ensure_no_ambiguous_refs_in_expr(&expr, &env, "decl").expect_err("should be ambiguous");

        assert_eq!(err.span, Some(callee_span));
    }
}
