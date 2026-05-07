use std::collections::{BTreeSet, HashMap, HashSet};

use crate::{
    Module,
    ast::{Expr, HandlerClause, InterpolatedPart, Stmt},
    typecheck::TypecheckError,
    typecheck_annotation::{find_can_keyword_index, parse_can_clause},
    typecheck_env::{EffectMap, EffectRow, ImportedEffectDecl, Ty, TypeEnv},
    typecheck_types::is_type_variable_name,
    types::{TypeExpr, parse_type_expr},
};

pub(crate) fn builtin_effect_names() -> &'static [&'static str] {
    &[]
}

pub(crate) fn build_required_effects_map(module: &Module) -> HashMap<String, Vec<String>> {
    let mut map = HashMap::new();
    for decl in &module.declarations {
        let Some(ann) = decl.type_annotation.as_deref() else {
            continue;
        };
        let clause = match parse_can_clause(ann) {
            Ok(Some(clause)) => clause,
            Ok(None) => continue,
            Err(err) => {
                debug_assert!(
                    false,
                    "build_required_effects_map: malformed `can` clause survived validate phase: {:?} (decl: {}, annotation: {:?})",
                    err, decl.name, ann
                );
                continue;
            }
        };
        if !clause.fixed.is_empty() {
            map.insert(decl.name.clone(), clause.fixed);
        }
    }
    map
}

pub(crate) fn build_effect_map(
    module: &Module,
    imported_effects: &[ImportedEffectDecl],
) -> EffectMap {
    let mut effect_to_ops: HashMap<String, HashSet<String>> = HashMap::new();
    for effect_decl in imported_effects
        .iter()
        .map(|imported| &imported.decl)
        .chain(module.effect_declarations.iter())
    {
        let ops = effect_to_ops.entry(effect_decl.name.clone()).or_default();
        for member in &effect_decl.members {
            let _ = ops.insert(format!("{}.{}", effect_decl.name, member.name));
            let _ = ops.insert(member.name.clone());
        }
    }

    EffectMap {
        effect_to_ops,
        op_to_effects: build_op_to_effects(module, imported_effects),
    }
}

pub(crate) fn ops_from_can_clause(
    annotation: Option<&str>,
    effect_map: &EffectMap,
) -> HashSet<String> {
    let Some(ann) = annotation else {
        return HashSet::new();
    };
    let clause = match parse_can_clause(ann) {
        Ok(Some(clause)) => clause,
        Ok(None) => return HashSet::new(),
        Err(err) => {
            debug_assert!(
                false,
                "ops_from_can_clause: malformed `can` clause survived validate phase: {:?} (annotation: {:?})",
                err, ann
            );
            return HashSet::new();
        }
    };
    clause
        .fixed
        .iter()
        .flat_map(|effect_name| {
            effect_map
                .effect_to_ops
                .get(effect_name.as_str())
                .into_iter()
                .flat_map(|ops| ops.iter().cloned())
        })
        .collect()
}

pub(crate) fn validate_effect_declarations(module: &Module) -> Result<(), TypecheckError> {
    let mut seen = HashSet::new();
    for effect_decl in &module.effect_declarations {
        if !seen.insert(effect_decl.name.clone()) {
            return Err(TypecheckError {
                declaration: Some(effect_decl.name.clone()),
                span: Some(effect_decl.span),
                message: format!("duplicate effect declaration `{}`", effect_decl.name),
            });
        }
    }
    Ok(())
}

pub(crate) fn validate_effect_member_effect_clauses(module: &Module) -> Result<(), TypecheckError> {
    for effect_decl in &module.effect_declarations {
        let declared_type_params: HashSet<String> =
            effect_decl.type_params.iter().cloned().collect();
        for member in &effect_decl.members {
            if find_can_keyword_index(&member.type_annotation).is_some() {
                return Err(TypecheckError {
                    declaration: Some(effect_decl.name.clone()),
                    span: Some(member.span),
                    message: format!(
                        "can clauses on effect members are not supported in `{}.{}`",
                        effect_decl.name, member.name
                    ),
                });
            }
            let parsed =
                parse_type_expr(&member.type_annotation).ok_or_else(|| TypecheckError {
                    declaration: Some(effect_decl.name.clone()),
                    span: Some(member.span),
                    message: format!(
                        "invalid effect member type annotation in `{}.{}`",
                        effect_decl.name, member.name
                    ),
                })?;
            let mut used_type_vars = HashSet::new();
            collect_type_variable_names_in_type_expr(&parsed, &mut used_type_vars);
            for type_var in used_type_vars {
                if type_var == "_" {
                    continue;
                }
                if !declared_type_params.contains(&type_var) {
                    return Err(TypecheckError {
                        declaration: Some(effect_decl.name.clone()),
                        span: Some(member.span),
                        message: format!(
                            "unknown effect type parameter `{}` in `{}.{}`",
                            type_var, effect_decl.name, member.name
                        ),
                    });
                }
            }
        }
    }
    Ok(())
}

fn build_op_to_effects(
    module: &Module,
    imported_effects: &[ImportedEffectDecl],
) -> HashMap<String, HashSet<String>> {
    let mut map: HashMap<String, HashSet<String>> = HashMap::new();
    for effect_decl in imported_effects
        .iter()
        .map(|imported| &imported.decl)
        .chain(module.effect_declarations.iter())
    {
        for member in &effect_decl.members {
            map.entry(member.name.clone())
                .or_default()
                .insert(effect_decl.name.clone());
        }
    }
    map
}

/// EP-2 Step 1: Infer the closed effect-row a lambda body would produce when
/// executed. The result drives `Ty::Fun.effects` for the synthetic lambda type
/// (Step 2 wires the result into call-site row unification).
///
/// Scope of this implementation:
/// - effect operation calls / refs whose op is not in `covered_ops` contribute
///   their declaring effect's name.
/// - calls into known declared functions contribute that function's
///   `required_effects_map` fixed effects (likewise filtered by `covered_ops`).
/// - `Expr::With` extends `covered_ops` for its body, so handler-discharged
///   effects do not leak out (LANGUAGE_SPEC §5).
/// - row tail propagation from called functions' `Ty::Fun.effects.tail`
///   is intentionally deferred to Step 3 (`infer_call_effects` at the call
///   site). The lambda-side helper only synthesizes the closed-row component.
pub(crate) fn infer_expr_effects(
    expr: &Expr,
    env: &TypeEnv,
    effect_map: &EffectMap,
    required_effects_map: &HashMap<String, Vec<String>>,
    covered_ops: &HashSet<String>,
) -> EffectRow {
    let mut fixed: BTreeSet<String> = BTreeSet::new();
    walk_expr_for_effects(
        expr,
        env,
        effect_map,
        required_effects_map,
        covered_ops,
        &mut fixed,
    );
    EffectRow { fixed, tail: None }
}

/// EP-2 Step 3b: aggregate the effect row produced by the *innermost* body of
/// a possibly-curried lambda. Walks through nested `Expr::Lambda` layers (and
/// `Expr::Spanned` wrappers) without crossing into other lambdas' inner
/// expressions, then runs the standard `walk_expr_for_effects` on the actual
/// terminal body.
///
/// Use this from call-site validation when the callee declares a multi-arity
/// callback (`(b -> a -> b can {e})`); the caller passes the *outer* curried
/// lambda whose Goby semantics is "apply both args, then the body runs and
/// emits these effects". The single-lambda invariant in `infer_expr_effects`
/// (nested lambdas are values, not bodies) is intentional and unchanged; this
/// helper is the call-site complement that knows the curried lambda is
/// destined to be fully applied.
pub(crate) fn infer_curried_lambda_body_effects(
    expr: &Expr,
    env: &TypeEnv,
    effect_map: &EffectMap,
    required_effects_map: &HashMap<String, Vec<String>>,
    covered_ops: &HashSet<String>,
) -> EffectRow {
    let mut current = expr;
    let mut local_env = env.clone();
    loop {
        match current {
            Expr::Spanned { expr, .. } => current = expr,
            Expr::Lambda { param, body } => {
                local_env = local_env.with_local(param, Ty::Unknown);
                current = body;
            }
            _ => break,
        }
    }
    let mut fixed: BTreeSet<String> = BTreeSet::new();
    walk_expr_for_effects(
        current,
        &local_env,
        effect_map,
        required_effects_map,
        covered_ops,
        &mut fixed,
    );
    EffectRow { fixed, tail: None }
}

fn walk_expr_for_effects(
    expr: &Expr,
    env: &TypeEnv,
    effect_map: &EffectMap,
    required_effects_map: &HashMap<String, Vec<String>>,
    covered_ops: &HashSet<String>,
    out: &mut BTreeSet<String>,
) {
    match expr {
        Expr::Spanned { expr, .. } => walk_expr_for_effects(
            expr,
            env,
            effect_map,
            required_effects_map,
            covered_ops,
            out,
        ),
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) => {}
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(inner) = part {
                    walk_expr_for_effects(
                        inner,
                        env,
                        effect_map,
                        required_effects_map,
                        covered_ops,
                        out,
                    );
                }
            }
        }
        Expr::ListLit { elements, spread } => {
            for el in elements {
                walk_expr_for_effects(
                    el,
                    env,
                    effect_map,
                    required_effects_map,
                    covered_ops,
                    out,
                );
            }
            if let Some(s) = spread {
                walk_expr_for_effects(
                    s,
                    env,
                    effect_map,
                    required_effects_map,
                    covered_ops,
                    out,
                );
            }
        }
        Expr::TupleLit(items) => {
            for item in items {
                walk_expr_for_effects(
                    item,
                    env,
                    effect_map,
                    required_effects_map,
                    covered_ops,
                    out,
                );
            }
        }
        Expr::Var { name, .. } => {
            collect_op_or_callee_effects(name, env, effect_map, required_effects_map, covered_ops, out);
        }
        Expr::Qualified { receiver, member, .. } => {
            let qualified = format!("{}.{}", receiver, member);
            collect_op_or_callee_effects(
                &qualified,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                walk_expr_for_effects(
                    value,
                    env,
                    effect_map,
                    required_effects_map,
                    covered_ops,
                    out,
                );
            }
        }
        Expr::UnaryOp { expr, .. } => walk_expr_for_effects(
            expr,
            env,
            effect_map,
            required_effects_map,
            covered_ops,
            out,
        ),
        Expr::BinOp { left, right, .. } => {
            walk_expr_for_effects(
                left,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
            walk_expr_for_effects(
                right,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
        }
        Expr::Call { callee, arg, .. } => {
            walk_expr_for_effects(
                callee,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
            walk_expr_for_effects(
                arg,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            let qualified = format!("{}.{}", receiver, method);
            collect_op_or_callee_effects(
                &qualified,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
            for arg in args {
                walk_expr_for_effects(
                    arg,
                    env,
                    effect_map,
                    required_effects_map,
                    covered_ops,
                    out,
                );
            }
        }
        Expr::Pipeline { value, callee, .. } => {
            walk_expr_for_effects(
                value,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
            collect_op_or_callee_effects(
                callee,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
        }
        Expr::Lambda { body, .. } => {
            // Nested lambda: its body's effects are encapsulated in the inner
            // lambda type, not the outer body's row. Skip the body to avoid
            // double-counting.
            let _ = body;
        }
        Expr::Handler { .. } => {
            // A bare handler value has no execution effects.
        }
        Expr::With { handler, body } => {
            walk_expr_for_effects(
                handler,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
            let extra = covered_ops_from_handler(handler, env, effect_map);
            let mut merged: HashSet<String> = covered_ops.clone();
            merged.extend(extra);
            walk_stmts_for_effects(
                body,
                env,
                effect_map,
                required_effects_map,
                &merged,
                out,
            );
        }
        Expr::Resume { value } => walk_expr_for_effects(
            value,
            env,
            effect_map,
            required_effects_map,
            covered_ops,
            out,
        ),
        Expr::Block(stmts) => walk_stmts_for_effects(
            stmts,
            env,
            effect_map,
            required_effects_map,
            covered_ops,
            out,
        ),
        Expr::Case { scrutinee, arms } => {
            walk_expr_for_effects(
                scrutinee,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
            for arm in arms {
                walk_expr_for_effects(
                    &arm.body,
                    env,
                    effect_map,
                    required_effects_map,
                    covered_ops,
                    out,
                );
            }
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            walk_expr_for_effects(
                condition,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
            walk_expr_for_effects(
                then_expr,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
            walk_expr_for_effects(
                else_expr,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
        }
        Expr::ListIndex { list, index } => {
            walk_expr_for_effects(
                list,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
            walk_expr_for_effects(
                index,
                env,
                effect_map,
                required_effects_map,
                covered_ops,
                out,
            );
        }
    }
}

fn walk_stmts_for_effects(
    stmts: &[Stmt],
    env: &TypeEnv,
    effect_map: &EffectMap,
    required_effects_map: &HashMap<String, Vec<String>>,
    covered_ops: &HashSet<String>,
    out: &mut BTreeSet<String>,
) {
    let mut local_env = env.clone();
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value, .. } | Stmt::MutBinding { name, value, .. } => {
                walk_expr_for_effects(
                    value,
                    &local_env,
                    effect_map,
                    required_effects_map,
                    covered_ops,
                    out,
                );
                // Shadowing: subsequent stmts must not see a global effect op
                // by the same name. Real type is irrelevant for effect inference.
                local_env.locals.insert(name.clone(), Ty::Unknown);
            }
            Stmt::Assign { value, .. } => {
                walk_expr_for_effects(
                    value,
                    &local_env,
                    effect_map,
                    required_effects_map,
                    covered_ops,
                    out,
                );
            }
            Stmt::Expr(expr, _) => {
                walk_expr_for_effects(
                    expr,
                    &local_env,
                    effect_map,
                    required_effects_map,
                    covered_ops,
                    out,
                );
            }
        }
    }
}

fn collect_op_or_callee_effects(
    name: &str,
    env: &TypeEnv,
    effect_map: &EffectMap,
    required_effects_map: &HashMap<String, Vec<String>>,
    covered_ops: &HashSet<String>,
    out: &mut BTreeSet<String>,
) {
    if covered_ops.contains(name) {
        return;
    }
    if env.is_effect_op(name) {
        // `op_to_effects` is keyed by the bare operation name (see
        // `build_op_to_effects`); strip a qualifying effect prefix so
        // `Log.log` resolves the same as `log`.
        let bare = name.rsplit_once('.').map(|(_, op)| op).unwrap_or(name);
        if let Some(effects) = effect_map.op_to_effects.get(bare) {
            for eff in effects {
                out.insert(eff.clone());
            }
        }
        return;
    }
    if let Some(effects) = required_effects_map.get(name) {
        for eff in effects {
            // Skip effects that the surrounding `with` discharges entirely:
            // an effect whose every operation is covered cannot leak out.
            if effect_fully_covered(eff, effect_map, covered_ops) {
                continue;
            }
            out.insert(eff.clone());
        }
    }
}

fn effect_fully_covered(
    effect_name: &str,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
) -> bool {
    let Some(ops) = effect_map.effect_to_ops.get(effect_name) else {
        return false;
    };
    if ops.is_empty() {
        return false;
    }
    ops.iter().all(|op| covered_ops.contains(op))
}

/// Best-effort handler-coverage extraction for `infer_expr_effects` and the
/// EP-3 `validate_call_chain` `Expr::With` branch.
///
/// Diagnostics are produced elsewhere; here we silently treat malformed or
/// unresolved handlers as covering nothing so we do not under-report effects.
pub(crate) fn covered_ops_from_handler(
    handler: &Expr,
    env: &TypeEnv,
    effect_map: &EffectMap,
) -> HashSet<String> {
    let mut out = HashSet::new();
    let unwrapped = match handler {
        Expr::Spanned { expr, .. } => expr.as_ref(),
        other => other,
    };
    match unwrapped {
        Expr::Handler { clauses } => {
            for clause in clauses {
                extend_covered_for_clause(clause, effect_map, &mut out);
            }
        }
        Expr::Var { name, .. } => {
            if let Ty::Handler { covered_ops } = env.lookup(name) {
                out.extend(covered_ops);
            }
        }
        _ => {}
    }
    out
}

fn extend_covered_for_clause(
    clause: &HandlerClause,
    effect_map: &EffectMap,
    out: &mut HashSet<String>,
) {
    let raw = clause.name.as_str();
    if let Some((effect_name, op_name)) = raw.split_once('.') {
        // Validate the qualifier resolves to a known effect with this op
        // before recording bare-name coverage; otherwise an unrelated `Bogus.log`
        // clause could mask a real `log` op elsewhere.
        let qualified_known = effect_map
            .effect_to_ops
            .get(effect_name)
            .is_some_and(|ops| ops.contains(op_name));
        if qualified_known {
            out.insert(op_name.to_string());
        }
        out.insert(format!("{}.{}", effect_name, op_name));
        return;
    }
    out.insert(raw.to_string());
    if let Some(effects) = effect_map.op_to_effects.get(raw) {
        for effect_name in effects {
            out.insert(format!("{}.{}", effect_name, raw));
        }
    }
}

#[cfg(test)]
mod infer_expr_effects_tests {
    use super::*;
    use crate::ast::{HandlerClause, Span, Stmt};
    use crate::typecheck_env::{GlobalBinding, Ty, TypeEnv};

    fn empty_env() -> TypeEnv {
        TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        }
    }

    /// Mirror `build_op_to_effects` / `build_effect_map`: `op_to_effects` is
    /// keyed by the bare op name only; `effect_to_ops` carries both bare and
    /// qualified forms (matching `build_effect_map` in `typecheck_effect.rs`).
    fn effect_map_with(effect: &str, ops: &[&str]) -> EffectMap {
        let mut effect_to_ops: HashMap<String, HashSet<String>> = HashMap::new();
        let mut op_to_effects: HashMap<String, HashSet<String>> = HashMap::new();
        let mut op_set: HashSet<String> = HashSet::new();
        for op in ops {
            op_set.insert((*op).to_string());
            op_set.insert(format!("{}.{}", effect, op));
            let mut effects = HashSet::new();
            effects.insert(effect.to_string());
            op_to_effects.insert((*op).to_string(), effects);
        }
        effect_to_ops.insert(effect.to_string(), op_set);
        EffectMap {
            effect_to_ops,
            op_to_effects,
        }
    }

    fn env_with_effect_op(effect: &str, op: &str) -> TypeEnv {
        let mut env = empty_env();
        let binding = GlobalBinding::Resolved {
            ty: Ty::Fun {
                params: vec![Ty::Unknown],
                result: Box::new(Ty::Unknown),
                effects: EffectRow::closed_empty(),
            },
            source: format!("effect `{}`", effect),
        };
        env.globals.insert(op.to_string(), binding.clone());
        env.globals.insert(format!("{}.{}", effect, op), binding);
        env
    }

    fn var(name: &str) -> Expr {
        Expr::Var {
            name: name.to_string(),
            span: None,
        }
    }

    fn call(callee: Expr, arg: Expr) -> Expr {
        Expr::Call {
            callee: Box::new(callee),
            arg: Box::new(arg),
            span: None,
        }
    }

    #[test]
    fn direct_effect_op_call_contributes_its_effect() {
        // Body: `log "hi"`. Op `log` is in effect Log; covered_ops empty.
        let env = env_with_effect_op("Log", "log");
        let effect_map = effect_map_with("Log", &["log"]);
        let body = call(var("log"), Expr::StringLit("hi".to_string()));

        let row = infer_expr_effects(
            &body,
            &env,
            &effect_map,
            &HashMap::new(),
            &HashSet::new(),
        );

        let expected: BTreeSet<String> = ["Log".to_string()].into_iter().collect();
        assert_eq!(row.fixed, expected);
        assert!(row.tail.is_none());
    }

    #[test]
    fn nested_lambda_body_does_not_leak_into_outer_row() {
        // Outer body: a lambda value `fn _ -> log "hi"`.
        // The inner lambda's effects belong to its own type, not the outer body.
        let env = env_with_effect_op("Log", "log");
        let effect_map = effect_map_with("Log", &["log"]);
        let inner_body = call(var("log"), Expr::StringLit("hi".to_string()));
        let outer_body = Expr::Lambda {
            param: "_".to_string(),
            body: Box::new(inner_body),
        };

        let row = infer_expr_effects(
            &outer_body,
            &env,
            &effect_map,
            &HashMap::new(),
            &HashSet::new(),
        );

        assert!(row.fixed.is_empty());
        assert!(row.tail.is_none());
    }

    #[test]
    fn handler_in_with_discharges_its_effect() {
        // Body: `with handler { log _ -> () } in log "hi"`. Effect Log is
        // discharged inside the with, so the inferred row stays empty.
        let env = env_with_effect_op("Log", "log");
        let effect_map = effect_map_with("Log", &["log"]);
        let handler = Expr::Handler {
            clauses: vec![HandlerClause {
                name: "log".to_string(),
                params: vec!["_".to_string()],
                body: "()".to_string(),
                parsed_body: Some(vec![Stmt::Expr(Expr::IntLit(0), None)]),
                span: Span::new(1, 1, 1, 8),
            }],
        };
        let body = vec![Stmt::Expr(
            call(var("log"), Expr::StringLit("hi".to_string())),
            None,
        )];
        let with_expr = Expr::With {
            handler: Box::new(handler),
            body,
        };

        let row = infer_expr_effects(
            &with_expr,
            &env,
            &effect_map,
            &HashMap::new(),
            &HashSet::new(),
        );

        assert!(
            row.fixed.is_empty(),
            "handler should discharge Log: got {:?}",
            row.fixed
        );
    }

    #[test]
    fn pure_body_has_closed_empty_row() {
        let env = empty_env();
        let effect_map = effect_map_with("Log", &["log"]);
        let body = Expr::BinOp {
            op: crate::ast::BinOpKind::Add,
            left: Box::new(Expr::IntLit(1)),
            right: Box::new(Expr::IntLit(2)),
        };

        let row = infer_expr_effects(
            &body,
            &env,
            &effect_map,
            &HashMap::new(),
            &HashSet::new(),
        );

        assert!(row.is_empty_closed());
    }

    #[test]
    fn known_callee_required_effects_are_picked_up() {
        // Body: `do_log "hi"` where do_log : ... can Log (recorded in
        // required_effects_map). Outer scope has no covered ops.
        let env = empty_env();
        let effect_map = effect_map_with("Log", &["log"]);
        let mut required = HashMap::new();
        required.insert("do_log".to_string(), vec!["Log".to_string()]);
        let body = call(var("do_log"), Expr::StringLit("hi".to_string()));

        let row = infer_expr_effects(&body, &env, &effect_map, &required, &HashSet::new());

        let expected: BTreeSet<String> = ["Log".to_string()].into_iter().collect();
        assert_eq!(row.fixed, expected);
    }

    #[test]
    fn qualified_effect_op_call_resolves_via_bare_name() {
        // Body: `Log.log "hi"`. `op_to_effects` is keyed by bare op names only,
        // so the resolver must strip the qualifier.
        let env = env_with_effect_op("Log", "log");
        let effect_map = effect_map_with("Log", &["log"]);
        let body = Expr::Call {
            callee: Box::new(Expr::Qualified {
                receiver: "Log".to_string(),
                member: "log".to_string(),
                span: None,
            }),
            arg: Box::new(Expr::StringLit("hi".to_string())),
            span: None,
        };

        let row = infer_expr_effects(
            &body,
            &env,
            &effect_map,
            &HashMap::new(),
            &HashSet::new(),
        );

        let expected: BTreeSet<String> = ["Log".to_string()].into_iter().collect();
        assert_eq!(row.fixed, expected);
    }

    #[test]
    fn qualified_handler_clause_discharges_effect() {
        // `with handler { Log.log _ -> () } in log "hi"` → Log discharged.
        let env = env_with_effect_op("Log", "log");
        let effect_map = effect_map_with("Log", &["log"]);
        let handler = Expr::Handler {
            clauses: vec![HandlerClause {
                name: "Log.log".to_string(),
                params: vec!["_".to_string()],
                body: "()".to_string(),
                parsed_body: Some(vec![Stmt::Expr(Expr::IntLit(0), None)]),
                span: Span::new(1, 1, 1, 12),
            }],
        };
        let body = vec![Stmt::Expr(
            call(var("log"), Expr::StringLit("hi".to_string())),
            None,
        )];
        let with_expr = Expr::With {
            handler: Box::new(handler),
            body,
        };

        let row = infer_expr_effects(
            &with_expr,
            &env,
            &effect_map,
            &HashMap::new(),
            &HashSet::new(),
        );

        assert!(row.fixed.is_empty(), "qualified clause should cover bare op");
    }

    #[test]
    fn known_callee_effect_skipped_when_handler_covers_all_ops() {
        // `with handler { log _ -> () } in do_log "hi"` where do_log requires Log.
        // `do_log` itself isn't in covered_ops, but Log's only op `log` is fully
        // covered, so the effect should be dropped.
        let env = env_with_effect_op("Log", "log");
        let effect_map = effect_map_with("Log", &["log"]);
        let mut required = HashMap::new();
        required.insert("do_log".to_string(), vec!["Log".to_string()]);
        let handler = Expr::Handler {
            clauses: vec![HandlerClause {
                name: "log".to_string(),
                params: vec!["_".to_string()],
                body: "()".to_string(),
                parsed_body: Some(vec![Stmt::Expr(Expr::IntLit(0), None)]),
                span: Span::new(1, 1, 1, 8),
            }],
        };
        let body = vec![Stmt::Expr(
            call(var("do_log"), Expr::StringLit("hi".to_string())),
            None,
        )];
        let with_expr = Expr::With {
            handler: Box::new(handler),
            body,
        };

        let row = infer_expr_effects(
            &with_expr,
            &env,
            &effect_map,
            &required,
            &HashSet::new(),
        );

        assert!(
            row.fixed.is_empty(),
            "Log should be discharged even via known callee: got {:?}",
            row.fixed
        );
    }

    #[test]
    fn local_binding_shadows_global_effect_op() {
        // Body: `{ let log = 1; log }` — the local `log` shadows the global
        // effect op of the same name; no Log effect should be reported.
        let env = env_with_effect_op("Log", "log");
        let effect_map = effect_map_with("Log", &["log"]);
        let body = Expr::Block(vec![
            Stmt::Binding {
                name: "log".to_string(),
                value: Expr::IntLit(1),
                span: None,
            },
            Stmt::Expr(var("log"), None),
        ]);

        let row = infer_expr_effects(
            &body,
            &env,
            &effect_map,
            &HashMap::new(),
            &HashSet::new(),
        );

        assert!(
            row.fixed.is_empty(),
            "shadowed `log` must not yield Log effect: got {:?}",
            row.fixed
        );
    }

    #[test]
    fn handler_value_in_with_extends_covered_ops() {
        // `with my_handler in log "hi"` where my_handler : Handler { log }.
        let mut env = env_with_effect_op("Log", "log");
        let mut covered = HashSet::new();
        covered.insert("log".to_string());
        covered.insert("Log.log".to_string());
        env.globals.insert(
            "my_handler".to_string(),
            GlobalBinding::Resolved {
                ty: Ty::Handler {
                    covered_ops: covered,
                },
                source: "let my_handler".to_string(),
            },
        );
        let effect_map = effect_map_with("Log", &["log"]);
        let body = vec![Stmt::Expr(
            call(var("log"), Expr::StringLit("hi".to_string())),
            None,
        )];
        let with_expr = Expr::With {
            handler: Box::new(var("my_handler")),
            body,
        };

        let row = infer_expr_effects(
            &with_expr,
            &env,
            &effect_map,
            &HashMap::new(),
            &HashSet::new(),
        );

        assert!(
            row.fixed.is_empty(),
            "handler value should discharge: got {:?}",
            row.fixed
        );
    }
}

fn collect_type_variable_names_in_type_expr(expr: &TypeExpr, out: &mut HashSet<String>) {
    match expr {
        TypeExpr::Name(name) => {
            if is_type_variable_name(name) {
                out.insert(name.clone());
            }
        }
        TypeExpr::Tuple(items) => {
            for item in items {
                collect_type_variable_names_in_type_expr(item, out);
            }
        }
        TypeExpr::Function { arguments, result } => {
            for arg in arguments {
                collect_type_variable_names_in_type_expr(arg, out);
            }
            collect_type_variable_names_in_type_expr(result, out);
        }
        TypeExpr::Apply { head, args } => {
            collect_type_variable_names_in_type_expr(head, out);
            for arg in args {
                collect_type_variable_names_in_type_expr(arg, out);
            }
        }
    }
}
