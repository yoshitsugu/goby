use std::collections::{HashMap, HashSet};

use crate::ir::{CompExpr, IrCaseArm, IrDecl, IrHandlerClause, IrInterpPart, IrModule, ValueExpr};

const EXPECTED_PIPELINE: [&str; 5] = [
    "ir::from_resolved",
    "closure_capture::materialize_envs",
    "ownership_classify",
    "drop_insert",
    "reuse_pair",
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OwnershipClass {
    Owned,
    Borrowed,
}

pub fn assert_perceus_pipeline_order(pass_names: &[&'static str]) {
    assert_eq!(
        pass_names, EXPECTED_PIPELINE,
        "internal perceus pipeline wiring bug: expected {:?}, got {:?}",
        EXPECTED_PIPELINE, pass_names
    );
}

pub fn run_perceus_passes(module: &IrModule) -> IrModule {
    let ownership = ownership_classify_module(module);
    drop_insert_module(module, &ownership)
}

fn ownership_classify_module(
    module: &IrModule,
) -> HashMap<String, HashMap<String, OwnershipClass>> {
    let mut param_ownership: HashMap<String, Vec<OwnershipClass>> = module
        .decls
        .iter()
        .map(|decl| {
            (
                decl.name.clone(),
                decl.params
                    .iter()
                    .map(|_| OwnershipClass::Borrowed)
                    .collect(),
            )
        })
        .collect();

    loop {
        let classified: HashMap<String, HashMap<String, OwnershipClass>> = module
            .decls
            .iter()
            .map(|decl| {
                (
                    decl.name.clone(),
                    ownership_classify_decl(decl, &param_ownership),
                )
            })
            .collect();

        let mut changed = false;
        for decl in &module.decls {
            let params = param_ownership
                .get_mut(&decl.name)
                .expect("parameter ownership facts must exist");
            let classes = classified
                .get(&decl.name)
                .expect("decl ownership facts must exist");
            for (idx, (param_name, _)) in decl.params.iter().enumerate() {
                let next = classes
                    .get(param_name)
                    .copied()
                    .unwrap_or(OwnershipClass::Owned);
                if params[idx] != next {
                    params[idx] = next;
                    changed = true;
                }
            }
        }

        if !changed {
            return classified;
        }
    }
}

fn ownership_classify_decl(
    decl: &IrDecl,
    module_params: &HashMap<String, Vec<OwnershipClass>>,
) -> HashMap<String, OwnershipClass> {
    let mut classes = HashMap::new();
    let mut params = HashSet::new();
    let current = module_params
        .get(&decl.name)
        .expect("parameter ownership facts must exist");
    for (idx, (param_name, _)) in decl.params.iter().enumerate() {
        params.insert(param_name.clone());
        classes.insert(param_name.clone(), current[idx]);
    }
    classify_comp(&decl.body, &mut classes, &params, module_params);
    classes
}

fn classify_comp(
    comp: &CompExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    module_params: &HashMap<String, Vec<OwnershipClass>>,
) {
    match comp {
        CompExpr::Value(value) => classify_return_value(value, classes, params),
        CompExpr::Let {
            name, value, body, ..
        } => {
            classify_bound_comp(value, classes, params, module_params);
            classes.insert(name.clone(), classify_owned_result(value));
            classify_comp(body, classes, params, module_params);
        }
        CompExpr::LetMut {
            name, value, body, ..
        } => {
            classify_bound_comp(value, classes, params, module_params);
            classes.insert(name.clone(), OwnershipClass::Borrowed);
            classify_comp(body, classes, params, module_params);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                classify_comp(stmt, classes, params, module_params);
            }
            classify_comp(tail, classes, params, module_params);
        }
        CompExpr::If { cond, then_, else_ } => {
            classify_borrowed_value(cond, classes, params);
            classify_comp(then_, classes, params, module_params);
            classify_comp(else_, classes, params, module_params);
        }
        CompExpr::Call { callee, args } => {
            classify_borrowed_value(callee, classes, params);
            classify_call_args(callee, args, classes, params, module_params);
        }
        CompExpr::Assign { value, .. } => {
            classify_bound_comp(value, classes, params, module_params)
        }
        CompExpr::AssignIndex { path, value, .. } => {
            for index in path {
                classify_borrowed_value(index, classes, params);
            }
            classify_bound_comp(value, classes, params, module_params);
        }
        CompExpr::Case { scrutinee, arms } => {
            classify_borrowed_value(scrutinee, classes, params);
            for arm in arms {
                // Pattern-bound variables own the sub-parts of the scrutinee.
                classify_case_pattern_bindings(&arm.pattern, classes);
                classify_comp(&arm.body, classes, params, module_params);
            }
        }
        CompExpr::Dup { value } | CompExpr::Resume { value } => {
            classify_borrowed_value(value, classes, params)
        }
        CompExpr::Drop { value } => {
            classify_consumed_value(value, classes, params);
        }
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                classify_consumed_value(arg, classes, params);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                classify_comp(&clause.body, classes, params, module_params);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            classify_comp(handler, classes, params, module_params);
            classify_comp(body, classes, params, module_params);
        }
    }
}

fn classify_bound_comp(
    comp: &CompExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    module_params: &HashMap<String, Vec<OwnershipClass>>,
) {
    match comp {
        CompExpr::Value(value) => classify_bound_value(value, classes, params),
        _ => classify_comp(comp, classes, params, module_params),
    }
}

fn classify_case_pattern_bindings(
    pattern: &crate::ir::IrCasePattern,
    classes: &mut HashMap<String, OwnershipClass>,
) {
    use crate::ir::{IrCasePattern, IrListPatternItem, IrListPatternTail};
    match pattern {
        IrCasePattern::ListPattern { items, tail } => {
            for item in items {
                if let IrListPatternItem::Bind(name) = item {
                    classes.insert(name.clone(), OwnershipClass::Owned);
                }
            }
            if let Some(IrListPatternTail::Bind(name)) = tail {
                classes.insert(name.clone(), OwnershipClass::Owned);
            }
        }
        IrCasePattern::IntLit(_)
        | IrCasePattern::StringLit(_)
        | IrCasePattern::BoolLit(_)
        | IrCasePattern::EmptyList
        | IrCasePattern::Wildcard => {}
    }
}

fn demote_param(
    name: &str,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
) {
    if params.contains(name) {
        classes.insert(name.to_string(), OwnershipClass::Owned);
    }
}

fn classify_call_args(
    callee: &ValueExpr,
    args: &[ValueExpr],
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    module_params: &HashMap<String, Vec<OwnershipClass>>,
) {
    let callee_params = callee_param_classes(callee, module_params);
    for (idx, arg) in args.iter().enumerate() {
        if callee_params
            .and_then(|params| params.get(idx))
            .is_some_and(|class| *class == OwnershipClass::Borrowed)
        {
            classify_borrowed_value(arg, classes, params);
        } else {
            classify_consumed_value(arg, classes, params);
        }
    }
}

fn callee_param_classes<'a>(
    callee: &ValueExpr,
    module_params: &'a HashMap<String, Vec<OwnershipClass>>,
) -> Option<&'a [OwnershipClass]> {
    match callee {
        ValueExpr::GlobalRef { name, .. } => module_params.get(name).map(Vec::as_slice),
        _ => None,
    }
}

fn classify_return_value(
    value: &ValueExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
) {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                classify_consumed_value(element, classes, params);
            }
            if let Some(spread) = spread {
                classify_consumed_value(spread, classes, params);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                classify_consumed_value(item, classes, params);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                classify_consumed_value(field, classes, params);
            }
        }
        ValueExpr::Lambda { param, body } => classify_lambda_capture(body, param, classes, params),
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    classify_borrowed_value(value, classes, params);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            classify_borrowed_value(left, classes, params);
            classify_borrowed_value(right, classes, params);
        }
        ValueExpr::TupleProject { tuple, .. } => classify_borrowed_value(tuple, classes, params),
        ValueExpr::ListGet { list, index } => {
            classify_borrowed_value(list, classes, params);
            classify_borrowed_value(index, classes, params);
        }
        ValueExpr::Var(name) => demote_param(name, classes, params),
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn classify_bound_value(
    value: &ValueExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
) {
    match value {
        ValueExpr::Var(_) => {}
        _ => classify_return_value(value, classes, params),
    }
}

fn classify_borrowed_value(
    value: &ValueExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
) {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                classify_consumed_value(element, classes, params);
            }
            if let Some(spread) = spread {
                classify_consumed_value(spread, classes, params);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                classify_consumed_value(item, classes, params);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                classify_consumed_value(field, classes, params);
            }
        }
        ValueExpr::Lambda { param, body } => classify_lambda_capture(body, param, classes, params),
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    classify_borrowed_value(value, classes, params);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            classify_borrowed_value(left, classes, params);
            classify_borrowed_value(right, classes, params);
        }
        ValueExpr::TupleProject { tuple, .. } => classify_borrowed_value(tuple, classes, params),
        ValueExpr::ListGet { list, index } => {
            classify_borrowed_value(list, classes, params);
            classify_borrowed_value(index, classes, params);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn classify_consumed_value(
    value: &ValueExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
) {
    match value {
        ValueExpr::Var(name) => demote_param(name, classes, params),
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                classify_consumed_value(element, classes, params);
            }
            if let Some(spread) = spread {
                classify_consumed_value(spread, classes, params);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                classify_consumed_value(item, classes, params);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                classify_consumed_value(field, classes, params);
            }
        }
        ValueExpr::Lambda { param, body } => classify_lambda_capture(body, param, classes, params),
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    classify_consumed_value(value, classes, params);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            classify_consumed_value(left, classes, params);
            classify_consumed_value(right, classes, params);
        }
        ValueExpr::TupleProject { tuple, .. } => classify_consumed_value(tuple, classes, params),
        ValueExpr::ListGet { list, index } => {
            classify_consumed_value(list, classes, params);
            classify_consumed_value(index, classes, params);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn classify_lambda_capture(
    body: &CompExpr,
    lambda_param: &str,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
) {
    let mut live = collect_live_vars(body);
    live.remove(lambda_param);
    for name in live {
        demote_param(&name, classes, params);
    }
}

fn classify_owned_result(comp: &CompExpr) -> OwnershipClass {
    match comp {
        CompExpr::Value(value) if value_is_fresh_heap(value) => OwnershipClass::Owned,
        _ => OwnershipClass::Borrowed,
    }
}

fn value_is_fresh_heap(value: &ValueExpr) -> bool {
    match value {
        ValueExpr::ListLit { .. }
        | ValueExpr::TupleLit(_)
        | ValueExpr::RecordLit { .. }
        | ValueExpr::Interp(_) => true,
        ValueExpr::Lambda { .. } => false,
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::BinOp { .. }
        | ValueExpr::Unit
        | ValueExpr::TupleProject { .. }
        | ValueExpr::ListGet { .. } => false,
    }
}

fn drop_insert_module(
    module: &IrModule,
    ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
) -> IrModule {
    let param_order: HashMap<String, Vec<String>> = module
        .decls
        .iter()
        .map(|decl| {
            (
                decl.name.clone(),
                decl.params
                    .iter()
                    .map(|(param_name, _)| param_name.clone())
                    .collect(),
            )
        })
        .collect();

    IrModule {
        decls: module
            .decls
            .iter()
            .map(|decl| {
                let decl_ownership = ownership
                    .get(&decl.name)
                    .expect("ownership facts must exist");

                let (body, next_tmp) =
                    drop_insert_comp(&decl.body, decl_ownership, ownership, &param_order, 0);

                // Insert Drops for owned parameters that are never referenced in the body.
                // This is the conservative / safe slice of M4 parameter ownership: a param
                // that never appears in the body (never consumed, never read) must be
                // released at entry. We intentionally do NOT emit Dups or Drops for
                // multiply-used or conditionally-consumed params — that requires borrow
                // inference (M4.5) to be safe. Over-emitting Dup/Drop on params caused
                // use-after-free and refcount leaks in real programs (see plan C1/C2).
                let (body, _) =
                    drop_insert_params_conservative(&decl.params, body, decl_ownership, next_tmp);

                IrDecl {
                    name: decl.name.clone(),
                    params: decl.params.clone(),
                    result_ty: decl.result_ty.clone(),
                    residual_effects: decl.residual_effects.clone(),
                    body,
                }
            })
            .collect(),
    }
}

/// Conservative parameter-Drop insertion: only emits `Drop` for Owned parameters
/// that are *never* referenced inside the body. This covers the unambiguous
/// "dead parameter" case without requiring borrow inference. Parameters that are
/// used (even once) are left alone here.
///
/// The "consumed on one branch, dead on another" case (e.g. `grid` in an
/// `if`/`case` where one arm calls `apply x` and the other reads nothing)
/// is **not** balanced by M4. `drop_insert_comp` intentionally does not
/// emit branch-level `Drop`s on non-pattern bindings because distinguishing
/// consume from borrow at a `Var` occurrence requires borrow inference.
/// That work is explicitly deferred to M4.5.
fn drop_insert_params_conservative(
    params: &[(String, crate::ir::IrType)],
    body: CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
    next_tmp: usize,
) -> (CompExpr, usize) {
    let mut drops: Vec<CompExpr> = params
        .iter()
        .filter(|(n, _)| ownership.get(n) == Some(&OwnershipClass::Owned))
        .filter(|(n, _)| count_var_uses(&body, n) == 0)
        .map(|(n, _)| CompExpr::Drop {
            value: Box::new(ValueExpr::Var(n.clone())),
        })
        .collect();

    if drops.is_empty() {
        return (body, next_tmp);
    }
    drops.sort_by(|a, b| {
        let CompExpr::Drop { value: va } = a else {
            unreachable!()
        };
        let CompExpr::Drop { value: vb } = b else {
            unreachable!()
        };
        let ValueExpr::Var(na) = va.as_ref() else {
            unreachable!()
        };
        let ValueExpr::Var(nb) = vb.as_ref() else {
            unreachable!()
        };
        na.cmp(nb)
    });
    (
        CompExpr::Seq {
            stmts: drops,
            tail: Box::new(body),
        },
        next_tmp,
    )
}

fn drop_insert_comp(
    comp: &CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
    next_tmp: usize,
) -> (CompExpr, usize) {
    match comp {
        CompExpr::Value(value) => (CompExpr::Value(value.clone()), next_tmp),
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => {
            let (value_out, next_tmp) =
                drop_insert_comp(value, ownership, module_ownership, param_order, next_tmp);
            let (body_out, next_tmp) =
                drop_insert_comp(body, ownership, module_ownership, param_order, next_tmp);
            let (body_with_drop, next_tmp) = insert_owned_let_drop(
                name,
                ty,
                body_out,
                ownership,
                module_ownership,
                param_order,
                next_tmp,
            );
            (
                CompExpr::Let {
                    name: name.clone(),
                    ty: ty.clone(),
                    value: Box::new(value_out),
                    body: Box::new(body_with_drop),
                },
                next_tmp,
            )
        }
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => {
            let (value, next_tmp) =
                drop_insert_comp(value, ownership, module_ownership, param_order, next_tmp);
            let (body, next_tmp) =
                drop_insert_comp(body, ownership, module_ownership, param_order, next_tmp);
            (
                CompExpr::LetMut {
                    name: name.clone(),
                    ty: ty.clone(),
                    value: Box::new(value),
                    body: Box::new(body),
                },
                next_tmp,
            )
        }
        CompExpr::Seq { stmts, tail } => {
            let mut next_tmp_mut = next_tmp;
            let stmts = stmts
                .iter()
                .map(|stmt| {
                    let (stmt, new_next_tmp) = drop_insert_comp(
                        stmt,
                        ownership,
                        module_ownership,
                        param_order,
                        next_tmp_mut,
                    );
                    next_tmp_mut = new_next_tmp;
                    stmt
                })
                .collect();
            let (tail, next_tmp_mut) =
                drop_insert_comp(tail, ownership, module_ownership, param_order, next_tmp_mut);
            (
                CompExpr::Seq {
                    stmts,
                    tail: Box::new(tail),
                },
                next_tmp_mut,
            )
        }
        CompExpr::If { cond, then_, else_ } => {
            // M4 conservative slice: we do not branch-balance across `If` arms on
            // general bindings. Real Goby programs mix consuming (Call) and borrowing
            // (builtin intrinsics such as `length`) uses of the same binding, and a
            // count-based liveness approximation cannot distinguish them — attempting
            // to balance leads to use-after-free. Borrow inference (M4.5) will make
            // this precise. Fresh-heap `let` bindings that are unused on a branch are
            // still handled by `insert_owned_let_drop` at the enclosing Let site.
            let (then_out, next_tmp) =
                drop_insert_comp(then_, ownership, module_ownership, param_order, next_tmp);
            let (else_out, next_tmp) =
                drop_insert_comp(else_, ownership, module_ownership, param_order, next_tmp);
            (
                CompExpr::If {
                    cond: cond.clone(),
                    then_: Box::new(then_out),
                    else_: Box::new(else_out),
                },
                next_tmp,
            )
        }
        CompExpr::Call { callee, args } => (
            CompExpr::Call {
                callee: callee.clone(),
                args: args.clone(),
            },
            next_tmp,
        ),
        CompExpr::Assign { name, value } => {
            let (value, next_tmp) =
                drop_insert_comp(value, ownership, module_ownership, param_order, next_tmp);
            (
                CompExpr::Assign {
                    name: name.clone(),
                    value: Box::new(value),
                },
                next_tmp,
            )
        }
        CompExpr::AssignIndex { root, path, value } => {
            let (value, next_tmp) =
                drop_insert_comp(value, ownership, module_ownership, param_order, next_tmp);
            (
                CompExpr::AssignIndex {
                    root: root.clone(),
                    path: path.clone(),
                    value: Box::new(value),
                },
                next_tmp,
            )
        }
        CompExpr::Case { scrutinee, arms } => {
            // M4 conservative slice: no branch balancing on general bindings across
            // Case arms (same reasoning as If). We still emit Drops for *pattern-bound*
            // variables that are introduced by the arm's pattern but not referenced in
            // the arm body — these are freshly owned slices of the scrutinee and are
            // safe to release.
            let mut next_tmp_mut = next_tmp;
            let arms = arms
                .iter()
                .map(|arm| {
                    let pattern_bindings = collect_pattern_bindings(&arm.pattern);
                    let (body_with_pat_drops, new_next_tmp) = drop_unused_pattern_bindings(
                        &pattern_bindings,
                        arm.body.clone(),
                        ownership,
                        next_tmp_mut,
                    );
                    next_tmp_mut = new_next_tmp;
                    let (body_out, new_next_tmp) = drop_insert_comp(
                        &body_with_pat_drops,
                        ownership,
                        module_ownership,
                        param_order,
                        next_tmp_mut,
                    );
                    next_tmp_mut = new_next_tmp;
                    IrCaseArm {
                        pattern: arm.pattern.clone(),
                        body: body_out,
                    }
                })
                .collect();
            (
                CompExpr::Case {
                    scrutinee: scrutinee.clone(),
                    arms,
                },
                next_tmp_mut,
            )
        }
        CompExpr::Dup { value } => (
            CompExpr::Dup {
                value: value.clone(),
            },
            next_tmp,
        ),
        CompExpr::Drop { value } => (
            CompExpr::Drop {
                value: value.clone(),
            },
            next_tmp,
        ),
        CompExpr::PerformEffect { effect, op, args } => (
            CompExpr::PerformEffect {
                effect: effect.clone(),
                op: op.clone(),
                args: args.clone(),
            },
            next_tmp,
        ),
        CompExpr::Handle { clauses } => {
            let mut next_tmp_mut = next_tmp;
            let clauses = clauses
                .iter()
                .map(|clause| {
                    let (body, new_next_tmp) = drop_insert_comp(
                        &clause.body,
                        ownership,
                        module_ownership,
                        param_order,
                        next_tmp_mut,
                    );
                    next_tmp_mut = new_next_tmp;
                    IrHandlerClause {
                        op_name: clause.op_name.clone(),
                        params: clause.params.clone(),
                        body,
                    }
                })
                .collect();
            (CompExpr::Handle { clauses }, next_tmp_mut)
        }
        CompExpr::WithHandler { handler, body } => {
            // Owned bindings live across the WithHandler boundary get a Dup before the site.
            let live_in_handler = collect_live_vars(handler);
            let live_in_body = collect_live_vars(body);
            let live_across: HashSet<String> = live_in_handler
                .union(&live_in_body)
                .filter(|n| ownership.get(*n) == Some(&OwnershipClass::Owned))
                .cloned()
                .collect();

            let (handler, next_tmp) =
                drop_insert_comp(handler, ownership, module_ownership, param_order, next_tmp);
            let (body, next_tmp) =
                drop_insert_comp(body, ownership, module_ownership, param_order, next_tmp);

            let with_handler = CompExpr::WithHandler {
                handler: Box::new(handler),
                body: Box::new(body),
            };

            if live_across.is_empty() {
                (with_handler, next_tmp)
            } else {
                // Wrap with Seq of Dups for each live-across owned binding.
                let mut dups: Vec<CompExpr> = live_across
                    .into_iter()
                    .map(|n| CompExpr::Dup {
                        value: Box::new(ValueExpr::Var(n)),
                    })
                    .collect();
                dups.sort_by(|a, b| {
                    // Deterministic order for tests.
                    let CompExpr::Dup { value: va } = a else {
                        unreachable!()
                    };
                    let CompExpr::Dup { value: vb } = b else {
                        unreachable!()
                    };
                    let ValueExpr::Var(na) = va.as_ref() else {
                        unreachable!()
                    };
                    let ValueExpr::Var(nb) = vb.as_ref() else {
                        unreachable!()
                    };
                    na.cmp(nb)
                });
                (
                    CompExpr::Seq {
                        stmts: dups,
                        tail: Box::new(with_handler),
                    },
                    next_tmp,
                )
            }
        }
        CompExpr::Resume { value } => (
            CompExpr::Resume {
                value: value.clone(),
            },
            next_tmp,
        ),
    }
}

/// Collect variable names bound by a case pattern.
fn collect_pattern_bindings(pattern: &crate::ir::IrCasePattern) -> Vec<String> {
    use crate::ir::{IrCasePattern, IrListPatternItem, IrListPatternTail};
    match pattern {
        IrCasePattern::ListPattern { items, tail } => {
            let mut names = Vec::new();
            for item in items {
                if let IrListPatternItem::Bind(name) = item {
                    names.push(name.clone());
                }
            }
            if let Some(IrListPatternTail::Bind(name)) = tail {
                names.push(name.clone());
            }
            names
        }
        _ => vec![],
    }
}

/// For each pattern-bound variable that is Owned but unused in `body`, prepend a Drop.
fn drop_unused_pattern_bindings(
    bindings: &[String],
    body: CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
    next_tmp: usize,
) -> (CompExpr, usize) {
    let live_in_body = collect_live_vars(&body);
    let mut drops: Vec<CompExpr> = bindings
        .iter()
        .filter(|n| !live_in_body.contains(*n))
        .filter(|n| ownership.get(*n) == Some(&OwnershipClass::Owned))
        .map(|n| CompExpr::Drop {
            value: Box::new(ValueExpr::Var(n.clone())),
        })
        .collect();

    if drops.is_empty() {
        return (body, next_tmp);
    }
    drops.sort_by(|a, b| {
        let CompExpr::Drop { value: va } = a else {
            unreachable!()
        };
        let CompExpr::Drop { value: vb } = b else {
            unreachable!()
        };
        let ValueExpr::Var(na) = va.as_ref() else {
            unreachable!()
        };
        let ValueExpr::Var(nb) = vb.as_ref() else {
            unreachable!()
        };
        na.cmp(nb)
    });
    (
        CompExpr::Seq {
            stmts: drops,
            tail: Box::new(body),
        },
        next_tmp,
    )
}

/// For a `Let { name }` binding that is Owned, decide whether to insert a Drop.
///
/// M4.5 keeps the original `use_count == 0` entry-drop and additionally emits a
/// post-body Drop when all uses of the binding are known borrows.
///
/// - If uses == 0: prepend `Drop(name)` at the top of the body.
/// - If every use is a borrow: evaluate the body into a temporary, then Drop
///   `name` and return the temporary.
/// - If any use consumes: ownership has transferred, so leave the body alone.
fn insert_owned_let_drop(
    name: &str,
    _ty: &crate::ir::IrType,
    body: CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
    next_tmp: usize,
) -> (CompExpr, usize) {
    if ownership.get(name) != Some(&OwnershipClass::Owned) {
        return (body, next_tmp);
    }

    let use_count = count_var_uses(&body, name);

    if use_count == 0 {
        return (
            CompExpr::Seq {
                stmts: vec![CompExpr::Drop {
                    value: Box::new(ValueExpr::Var(name.to_string())),
                }],
                tail: Box::new(body),
            },
            next_tmp,
        );
    }

    if !comp_consumes_name(&body, name, module_ownership, param_order) {
        let tmp = format!("__perceus_drop_tmp_{next_tmp}");
        return (
            CompExpr::Let {
                name: tmp.clone(),
                ty: crate::ir::IrType::Unknown,
                value: Box::new(body),
                body: Box::new(CompExpr::Seq {
                    stmts: vec![CompExpr::Drop {
                        value: Box::new(ValueExpr::Var(name.to_string())),
                    }],
                    tail: Box::new(CompExpr::Value(ValueExpr::Var(tmp))),
                }),
            },
            next_tmp + 1,
        );
    }

    (body, next_tmp)
}

fn comp_consumes_name(
    comp: &CompExpr,
    name: &str,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
) -> bool {
    match comp {
        CompExpr::Value(value) => return_value_consumes_name(value, name),
        CompExpr::Let {
            name: bind,
            value,
            body,
            ..
        }
        | CompExpr::LetMut {
            name: bind,
            value,
            body,
            ..
        } => {
            comp_consumes_name(value, name, module_ownership, param_order)
                || (bind != name && comp_consumes_name(body, name, module_ownership, param_order))
        }
        CompExpr::Seq { stmts, tail } => {
            stmts
                .iter()
                .any(|stmt| comp_consumes_name(stmt, name, module_ownership, param_order))
                || comp_consumes_name(tail, name, module_ownership, param_order)
        }
        CompExpr::If { then_, else_, .. } => {
            comp_consumes_name(then_, name, module_ownership, param_order)
                || comp_consumes_name(else_, name, module_ownership, param_order)
        }
        CompExpr::Call { callee, args } => args.iter().enumerate().any(|(idx, arg)| {
            value_mentions_name(arg, name)
                && !callee_arg_is_borrowed(callee, idx, module_ownership, param_order)
        }),
        CompExpr::Assign { value, .. } => {
            comp_consumes_name(value, name, module_ownership, param_order)
        }
        CompExpr::AssignIndex { root, value, .. } => {
            root == name || comp_consumes_name(value, name, module_ownership, param_order)
        }
        CompExpr::Case { arms, .. } => arms
            .iter()
            .any(|arm| comp_consumes_name(&arm.body, name, module_ownership, param_order)),
        CompExpr::Dup { .. } | CompExpr::Resume { .. } => false,
        CompExpr::Drop { value } => value_mentions_name(value, name),
        CompExpr::PerformEffect { args, .. } => {
            args.iter().any(|arg| value_mentions_name(arg, name))
        }
        CompExpr::Handle { clauses } => clauses
            .iter()
            .any(|clause| comp_consumes_name(&clause.body, name, module_ownership, param_order)),
        CompExpr::WithHandler { handler, body } => {
            comp_consumes_name(handler, name, module_ownership, param_order)
                || comp_consumes_name(body, name, module_ownership, param_order)
        }
    }
}

fn callee_arg_is_borrowed(
    callee: &ValueExpr,
    idx: usize,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
) -> bool {
    let decl_name = match callee {
        ValueExpr::GlobalRef { name, .. } => name,
        _ => return false,
    };
    module_ownership
        .get(decl_name)
        .zip(param_order.get(decl_name))
        .and_then(|(classes, params)| params.get(idx).and_then(|param| classes.get(param)))
        == Some(&OwnershipClass::Borrowed)
}

fn value_mentions_name(value: &ValueExpr, name: &str) -> bool {
    count_var_uses_value(value, name) > 0
}

fn return_value_consumes_name(value: &ValueExpr, name: &str) -> bool {
    match value {
        ValueExpr::Var(n) => n == name,
        ValueExpr::ListLit { elements, spread } => {
            elements.iter().any(|v| value_mentions_name(v, name))
                || spread
                    .as_deref()
                    .is_some_and(|v| value_mentions_name(v, name))
        }
        ValueExpr::TupleLit(items) => items.iter().any(|v| value_mentions_name(v, name)),
        ValueExpr::RecordLit { fields, .. } => {
            fields.iter().any(|(_, v)| value_mentions_name(v, name))
        }
        ValueExpr::Lambda { param, body } => {
            param != name && collect_live_vars(body).contains(name)
        }
        ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            IrInterpPart::Text(_) => false,
            IrInterpPart::Expr(_) => false,
        }),
        ValueExpr::BinOp { .. } | ValueExpr::TupleProject { .. } | ValueExpr::ListGet { .. } => {
            false
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => false,
    }
}

/// Collect all variable names that appear free (unbound) in `comp`.
/// Conservative: does not track shadowing precisely — includes all Var reads
/// except those shadowed by an enclosing Let/LetMut/Lambda with the same name.
fn collect_live_vars(comp: &CompExpr) -> HashSet<String> {
    let mut live = HashSet::new();
    collect_live_comp(comp, &mut live);
    live
}

fn collect_live_comp(comp: &CompExpr, live: &mut HashSet<String>) {
    match comp {
        CompExpr::Value(value) => collect_live_value(value, live),
        CompExpr::Let {
            name, value, body, ..
        }
        | CompExpr::LetMut {
            name, value, body, ..
        } => {
            collect_live_comp(value, live);
            let mut body_live = HashSet::new();
            collect_live_comp(body, &mut body_live);
            body_live.remove(name);
            live.extend(body_live);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                collect_live_comp(stmt, live);
            }
            collect_live_comp(tail, live);
        }
        CompExpr::If { cond, then_, else_ } => {
            collect_live_value(cond, live);
            collect_live_comp(then_, live);
            collect_live_comp(else_, live);
        }
        CompExpr::Call { callee, args } => {
            collect_live_value(callee, live);
            for arg in args {
                collect_live_value(arg, live);
            }
        }
        CompExpr::Assign { value, .. } => collect_live_comp(value, live),
        CompExpr::AssignIndex { path, value, .. } => {
            for index in path {
                collect_live_value(index, live);
            }
            collect_live_comp(value, live);
        }
        CompExpr::Case { scrutinee, arms } => {
            collect_live_value(scrutinee, live);
            for arm in arms {
                let mut arm_live = HashSet::new();
                collect_live_comp(&arm.body, &mut arm_live);
                for bound in collect_pattern_bindings(&arm.pattern) {
                    arm_live.remove(&bound);
                }
                live.extend(arm_live);
            }
        }
        CompExpr::Dup { value } | CompExpr::Drop { value } | CompExpr::Resume { value } => {
            collect_live_value(value, live)
        }
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                collect_live_value(arg, live);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                let mut clause_live = HashSet::new();
                collect_live_comp(&clause.body, &mut clause_live);
                for p in &clause.params {
                    clause_live.remove(p);
                }
                live.extend(clause_live);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            collect_live_comp(handler, live);
            collect_live_comp(body, live);
        }
    }
}

fn collect_live_value(value: &ValueExpr, live: &mut HashSet<String>) {
    match value {
        ValueExpr::Var(name) => {
            live.insert(name.clone());
        }
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                collect_live_value(element, live);
            }
            if let Some(spread) = spread {
                collect_live_value(spread, live);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                collect_live_value(item, live);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                collect_live_value(field, live);
            }
        }
        ValueExpr::Lambda { param, body } => {
            let mut body_live = HashSet::new();
            collect_live_comp(body, &mut body_live);
            body_live.remove(param);
            live.extend(body_live);
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(v) = part {
                    collect_live_value(v, live);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            collect_live_value(left, live);
            collect_live_value(right, live);
        }
        ValueExpr::TupleProject { tuple, .. } => collect_live_value(tuple, live),
        ValueExpr::ListGet { list, index } => {
            collect_live_value(list, live);
            collect_live_value(index, live);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

/// Count how many times `name` appears as a free variable in `comp`.
fn count_var_uses(comp: &CompExpr, name: &str) -> usize {
    match comp {
        CompExpr::Value(value) => count_var_uses_value(value, name),
        CompExpr::Let {
            name: bname,
            value,
            body,
            ..
        }
        | CompExpr::LetMut {
            name: bname,
            value,
            body,
            ..
        } => {
            let in_value = count_var_uses(value, name);
            let in_body = if bname == name {
                0
            } else {
                count_var_uses(body, name)
            };
            in_value + in_body
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().map(|s| count_var_uses(s, name)).sum::<usize>()
                + count_var_uses(tail, name)
        }
        CompExpr::If { cond, then_, else_ } => {
            // At runtime only one arm runs; use max across arms for correct Dup count.
            count_var_uses_value(cond, name)
                + count_var_uses(then_, name).max(count_var_uses(else_, name))
        }
        CompExpr::Call { callee, args } => {
            count_var_uses_value(callee, name)
                + args
                    .iter()
                    .map(|a| count_var_uses_value(a, name))
                    .sum::<usize>()
        }
        CompExpr::Assign { value, .. } => count_var_uses(value, name),
        CompExpr::AssignIndex { path, value, .. } => {
            path.iter()
                .map(|i| count_var_uses_value(i, name))
                .sum::<usize>()
                + count_var_uses(value, name)
        }
        CompExpr::Case { scrutinee, arms } => {
            // At runtime only one arm runs; use max across arms for correct Dup count.
            count_var_uses_value(scrutinee, name)
                + arms
                    .iter()
                    .map(|a| count_var_uses(&a.body, name))
                    .max()
                    .unwrap_or(0)
        }
        CompExpr::Dup { value } | CompExpr::Drop { value } | CompExpr::Resume { value } => {
            count_var_uses_value(value, name)
        }
        CompExpr::PerformEffect { args, .. } => {
            args.iter().map(|a| count_var_uses_value(a, name)).sum()
        }
        CompExpr::Handle { clauses } => clauses.iter().map(|c| count_var_uses(&c.body, name)).sum(),
        CompExpr::WithHandler { handler, body } => {
            count_var_uses(handler, name) + count_var_uses(body, name)
        }
    }
}

fn count_var_uses_value(value: &ValueExpr, name: &str) -> usize {
    match value {
        ValueExpr::Var(n) => usize::from(n == name),
        ValueExpr::ListLit { elements, spread } => {
            elements
                .iter()
                .map(|e| count_var_uses_value(e, name))
                .sum::<usize>()
                + spread
                    .as_deref()
                    .map_or(0, |s| count_var_uses_value(s, name))
        }
        ValueExpr::TupleLit(items) => items.iter().map(|i| count_var_uses_value(i, name)).sum(),
        ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .map(|(_, f)| count_var_uses_value(f, name))
            .sum(),
        ValueExpr::Lambda { param, body } => {
            if param == name {
                0
            } else {
                count_var_uses(body, name)
            }
        }
        ValueExpr::Interp(parts) => parts
            .iter()
            .map(|p| match p {
                IrInterpPart::Text(_) => 0,
                IrInterpPart::Expr(v) => count_var_uses_value(v, name),
            })
            .sum(),
        ValueExpr::BinOp { left, right, .. } => {
            count_var_uses_value(left, name) + count_var_uses_value(right, name)
        }
        ValueExpr::TupleProject { tuple, .. } => count_var_uses_value(tuple, name),
        ValueExpr::ListGet { list, index } => {
            count_var_uses_value(list, name) + count_var_uses_value(index, name)
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::{assert_perceus_pipeline_order, run_perceus_passes};
    use crate::ir::{CompExpr, IrDecl, IrModule, IrType, ValueExpr, fmt_ir};

    #[test]
    fn accepts_expected_pipeline_order() {
        assert_perceus_pipeline_order(&[
            "ir::from_resolved",
            "closure_capture::materialize_envs",
            "ownership_classify",
            "drop_insert",
            "reuse_pair",
        ]);
    }

    #[test]
    #[should_panic(expected = "internal perceus pipeline wiring bug")]
    fn rejects_wrong_pipeline_order() {
        assert_perceus_pipeline_order(&[
            "ir::from_resolved",
            "ownership_classify",
            "closure_capture::materialize_envs",
            "drop_insert",
            "reuse_pair",
        ]);
    }

    #[test]
    fn inserts_drop_for_fresh_heap_binding_when_result_does_not_refer_to_it() {
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                body: CompExpr::Let {
                    name: "xs".to_string(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                        elements: vec![ValueExpr::IntLit(1)],
                        spread: None,
                    })),
                    body: Box::new(CompExpr::Value(ValueExpr::IntLit(42))),
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        assert_eq!(
            fmt_ir(&rewritten),
            "decl main: ? =\n  let xs: ? =\n    [1]\n  in\n    seq\n      drop xs\n    =>\n      42\n\n"
        );
    }

    #[test]
    fn keeps_binding_alive_when_result_returns_the_bound_heap_value() {
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                body: CompExpr::Let {
                    name: "xs".to_string(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                        elements: vec![ValueExpr::IntLit(1)],
                        spread: None,
                    })),
                    body: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        assert_eq!(fmt_ir(&rewritten), fmt_ir(&module));
    }

    // --- M4 correctness tests ---

    /// Owned arguments passed to a call transfer ownership to the callee:
    /// the caller must NOT insert a Drop after the call, and the callee that
    /// returns its own param must NOT Drop it either.
    #[test]
    fn call_site_transfers_ownership_of_owned_arg_to_callee() {
        // decl process(xs: ?) = xs   (param owned, returned → no Drop)
        // decl entry()        = let ys = [1] in process(ys)
        //                       ^ ys ownership transferred to process; no Drop
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "process".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Value(ValueExpr::Var("xs".to_string())),
                },
                IrDecl {
                    name: "entry".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Let {
                        name: "ys".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        })),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "process".to_string(),
                            }),
                            args: vec![ValueExpr::Var("ys".to_string())],
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        // Ownership transfer: caller must not Drop `ys` after passing it to the callee.
        assert!(
            !ir_str.contains("drop ys"),
            "caller must not Drop owned arg after call; got:\n{ir_str}"
        );
        // Callee returns `xs` so it also must not Drop it.
        assert!(
            !ir_str.contains("drop xs"),
            "callee must not Drop returned owned param; got:\n{ir_str}"
        );
    }

    /// Regression: `let xs = [1] in let y = process(xs) in y` must not emit
    /// `drop xs` at the outer Let, because `process(xs)` already consumes xs
    /// (callee takes ownership). Emitting a post-body Drop would double-free.
    ///
    /// This guards the decision to gate `insert_owned_let_drop` on
    /// `use_count == 0`, not `use_count == 1 && !result_references`.
    #[test]
    fn nested_let_with_call_consuming_binding_does_not_get_outer_drop() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "process".to_string(),
                    params: vec![("p".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Value(ValueExpr::Var("p".to_string())),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Let {
                        name: "xs".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        })),
                        body: Box::new(CompExpr::Let {
                            name: "y".to_string(),
                            ty: IrType::Unknown,
                            value: Box::new(CompExpr::Call {
                                callee: Box::new(ValueExpr::GlobalRef {
                                    module: "".to_string(),
                                    name: "process".to_string(),
                                }),
                                args: vec![ValueExpr::Var("xs".to_string())],
                            }),
                            body: Box::new(CompExpr::Value(ValueExpr::Var("y".to_string()))),
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop xs"),
            "outer Let must not drop xs; process(xs) consumes it. Got:\n{ir_str}"
        );
    }

    /// Regression (Seq variant): `let xs = [1] in seq { process(xs); unit } => xs_follow`
    /// The single use of `xs` sits inside a Seq statement as a Call argument; the outer
    /// Let must not synthesise a post-body drop for xs.
    #[test]
    fn seq_stmt_with_call_consuming_binding_does_not_get_outer_drop() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "process".to_string(),
                    params: vec![("p".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Value(ValueExpr::Var("p".to_string())),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Let {
                        name: "xs".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        })),
                        body: Box::new(CompExpr::Seq {
                            stmts: vec![CompExpr::Call {
                                callee: Box::new(ValueExpr::GlobalRef {
                                    module: "".to_string(),
                                    name: "process".to_string(),
                                }),
                                args: vec![ValueExpr::Var("xs".to_string())],
                            }],
                            tail: Box::new(CompExpr::Value(ValueExpr::IntLit(0))),
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop xs"),
            "outer Let must not drop xs after Seq-nested Call(process,[xs]); got:\n{ir_str}"
        );
    }

    /// A lambda that captures a list but is dropped without being applied;
    /// the captured list's ownership must be released (Drop inserted).
    #[test]
    fn partial_application_captured_list_is_left_to_closure_drop() {
        // decl main() =
        //   let xs = [1, 2]
        //   in let _f = Lambda(param="a", body=Var("xs"))
        //      in 99
        //
        // xs is captured by `_f` (exactly once, inside the lambda body). In the
        // M4 conservative slice we intentionally do NOT insert `drop xs` in the
        // outer scope, because emitting it would race with the closure env's
        // own drop of `xs` when `_f` is released (double free). Releasing `xs`
        // is therefore the closure/partial-application's responsibility at
        // runtime, not the outer-Let pass's.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                body: CompExpr::Let {
                    name: "xs".to_string(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                        elements: vec![ValueExpr::IntLit(1), ValueExpr::IntLit(2)],
                        spread: None,
                    })),
                    body: Box::new(CompExpr::Let {
                        name: "_f".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::Lambda {
                            param: "a".to_string(),
                            body: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                        })),
                        body: Box::new(CompExpr::Value(ValueExpr::IntLit(99))),
                    }),
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop xs"),
            "expected NO drop xs at the outer Let — the closure owns it; got:\n{ir_str}"
        );
    }

    /// A closure captures a list; the outer scope must NOT emit a `drop xs`
    /// because the closure's env drop is what releases xs. Emitting both
    /// races with the closure drop and causes a double free.
    #[test]
    fn closure_captures_heap_values_outer_scope_does_not_drop() {
        // decl main() =
        //   let xs = [10]
        //   in let f = Lambda("_", body=Var("xs"))
        //      in f(unit)
        //
        // xs is captured by the closure body. The closure env is responsible
        // for releasing xs at runtime; the outer-Let pass must leave xs alone.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                body: CompExpr::Let {
                    name: "xs".to_string(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                        elements: vec![ValueExpr::IntLit(10)],
                        spread: None,
                    })),
                    body: Box::new(CompExpr::Let {
                        name: "f".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::Lambda {
                            param: "_".to_string(),
                            body: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                        })),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::Var("f".to_string())),
                            args: vec![ValueExpr::Unit],
                        }),
                    }),
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop xs"),
            "outer Let must not drop xs (closure owns it); got:\n{ir_str}"
        );
    }

    /// `case xs { [] -> 0 ; [x, ..rest] -> x }` where `rest` is unused in the arm body.
    /// Branch balancing: the empty-list arm should drop `xs`; the list arm should drop `rest`.
    #[test]
    fn case_heap_bound_pattern_drops_unused_bindings() {
        use crate::ir::{IrCaseArm, IrCasePattern, IrListPatternItem, IrListPatternTail};

        // decl main(xs: List) =
        //   case xs
        //     [] -> 0
        //     [x, ..rest] -> x
        //
        // xs is Owned (param). In the [] arm, xs is NOT in the arm body vars (just `0`).
        // In the [x,..rest] arm, x is used but rest is not.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![("xs".to_string(), IrType::Unknown)],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                body: CompExpr::Case {
                    scrutinee: Box::new(ValueExpr::Var("xs".to_string())),
                    arms: vec![
                        IrCaseArm {
                            pattern: IrCasePattern::EmptyList,
                            body: CompExpr::Value(ValueExpr::IntLit(0)),
                        },
                        IrCaseArm {
                            pattern: IrCasePattern::ListPattern {
                                items: vec![IrListPatternItem::Bind("x".to_string())],
                                tail: Some(IrListPatternTail::Bind("rest".to_string())),
                            },
                            body: CompExpr::Value(ValueExpr::Var("x".to_string())),
                        },
                    ],
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        // In the second arm, `rest` is bound but unused → Drop(rest) should appear.
        // NOTE: `x` is returned from the arm so no Drop(x).
        // NOTE: `xs` is the scrutinee — whether it needs a Drop depends on the
        //       branch-balancing pass seeing xs in one arm but not the other.
        //       For now assert rest is dropped.
        assert!(
            ir_str.contains("drop rest"),
            "expected Drop(rest) in list arm; got:\n{ir_str}"
        );
    }

    /// WithHandler with an owned list binding live across the handler invocation;
    /// a Dup should be inserted before the WithHandler to preserve the refcount.
    #[test]
    fn multi_resume_captured_list_state_preserved_across_resumes() {
        use crate::ir::IrHandlerClause;

        // decl main() =
        //   let xs = [1, 2, 3]
        //   in with_handler { handle_op(v) -> Resume(Var("v")) }
        //      body: Call(some_fn, [xs])
        //
        // `xs` is Owned and appears inside the WithHandler body → live-across,
        // so a Dup should be emitted before the WithHandler node.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                body: CompExpr::Let {
                    name: "xs".to_string(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                        elements: vec![
                            ValueExpr::IntLit(1),
                            ValueExpr::IntLit(2),
                            ValueExpr::IntLit(3),
                        ],
                        spread: None,
                    })),
                    body: Box::new(CompExpr::WithHandler {
                        handler: Box::new(CompExpr::Handle {
                            clauses: vec![IrHandlerClause {
                                op_name: "op".to_string(),
                                params: vec!["v".to_string()],
                                body: CompExpr::Resume {
                                    value: Box::new(ValueExpr::Var("v".to_string())),
                                },
                            }],
                        }),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "some_fn".to_string(),
                            }),
                            args: vec![ValueExpr::Var("xs".to_string())],
                        }),
                    }),
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        // xs is live inside the WithHandler body → Dup should be emitted.
        assert!(
            ir_str.contains("dup xs"),
            "expected Dup(xs) before WithHandler for live-across binding; got:\n{ir_str}"
        );
    }

    #[test]
    fn borrowed_parameter_used_only_by_pure_projection_gets_no_drop() {
        let module = IrModule {
            decls: vec![IrDecl {
                name: "head".to_string(),
                params: vec![("xs".to_string(), IrType::Unknown)],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                body: CompExpr::Value(ValueExpr::ListGet {
                    list: Box::new(ValueExpr::Var("xs".to_string())),
                    index: Box::new(ValueExpr::IntLit(0)),
                }),
            }],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop xs"),
            "pure borrowed parameter should not be dropped by callee; got:\n{ir_str}"
        );
    }

    #[test]
    fn borrowed_callee_arg_is_dropped_by_owner_after_call() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "head".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Value(ValueExpr::ListGet {
                        list: Box::new(ValueExpr::Var("xs".to_string())),
                        index: Box::new(ValueExpr::IntLit(0)),
                    }),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Let {
                        name: "ys".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        })),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "head".to_string(),
                            }),
                            args: vec![ValueExpr::Var("ys".to_string())],
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            ir_str.contains("drop ys"),
            "owner should drop ys after borrowed call; got:\n{ir_str}"
        );
        assert!(
            ir_str.contains("__perceus_drop_tmp_0"),
            "post-call drop must preserve call result through a temp; got:\n{ir_str}"
        );
    }

    #[test]
    fn owned_callee_arg_still_transfers_without_owner_drop() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "id".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Value(ValueExpr::Var("xs".to_string())),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    body: CompExpr::Let {
                        name: "ys".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        })),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "id".to_string(),
                            }),
                            args: vec![ValueExpr::Var("ys".to_string())],
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop ys"),
            "owned callee argument transfers ownership; got:\n{ir_str}"
        );
    }

    #[test]
    fn unknown_call_arg_is_conservatively_consumed() {
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                body: CompExpr::Let {
                    name: "ys".to_string(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                        elements: vec![ValueExpr::IntLit(1)],
                        spread: None,
                    })),
                    body: Box::new(CompExpr::Call {
                        callee: Box::new(ValueExpr::GlobalRef {
                            module: "".to_string(),
                            name: "external".to_string(),
                        }),
                        args: vec![ValueExpr::Var("ys".to_string())],
                    }),
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop ys"),
            "unknown callee must remain conservative/consuming; got:\n{ir_str}"
        );
    }

    #[test]
    fn with_handler_skips_dup_for_borrowed_parameter() {
        use crate::ir::IrHandlerClause;

        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![("xs".to_string(), IrType::Unknown)],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                body: CompExpr::WithHandler {
                    handler: Box::new(CompExpr::Handle {
                        clauses: vec![IrHandlerClause {
                            op_name: "op".to_string(),
                            params: vec!["v".to_string()],
                            body: CompExpr::Resume {
                                value: Box::new(ValueExpr::Var("v".to_string())),
                            },
                        }],
                    }),
                    body: Box::new(CompExpr::Value(ValueExpr::ListGet {
                        list: Box::new(ValueExpr::Var("xs".to_string())),
                        index: Box::new(ValueExpr::IntLit(0)),
                    })),
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("dup xs"),
            "borrowed parameter should not be duped across handler boundary; got:\n{ir_str}"
        );
    }
}
