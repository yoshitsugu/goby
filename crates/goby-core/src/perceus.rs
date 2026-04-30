use std::collections::{HashMap, HashSet};

use crate::ir::{CompExpr, IrCaseArm, IrDecl, IrHandlerClause, IrInterpPart, IrModule, ValueExpr};
use crate::perceus_reuse::{insert_reuse, insert_tail_reuse_module};

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
    let (ownership, decl_returns) = ownership_classify_module(module);
    if std::env::var("GOBY_DUMP_PERCEUS_IR").is_ok() {
        for decl in &module.decls {
            if let Some(classes) = ownership.get(&decl.name) {
                eprintln!("=== ownership[{}] ===", decl.name);
                for (param, _) in &decl.params {
                    eprintln!("  param {param:?} => {:?}", classes.get(param));
                }
            }
            if let Some(class) = decl_returns.get(&decl.name) {
                eprintln!("=== return_ownership[{}] = {:?} ===", decl.name, class);
            }
        }
    }
    // `decl_returns` is consumed inside `ownership_classify_module`'s
    // joint fix-point; downstream passes only need `ownership` (the
    // M4.5 param ownership map).
    drop(decl_returns);
    let dropped = drop_insert_module(module, &ownership);
    if std::env::var("GOBY_DUMP_PERCEUS_IR").is_ok() {
        for decl in &dropped.decls {
            eprintln!("=== dropped IR[{}] ===\n{:#?}", decl.name, decl.body);
        }
    }
    // reuse_pair label covers both intra-block and tail-call cross-call reuse
    let intra = IrModule {
        decls: dropped
            .decls
            .into_iter()
            .map(|decl| {
                let owned_params: HashSet<String> = ownership
                    .get(&decl.name)
                    .map(|classes| {
                        decl.params
                            .iter()
                            .filter_map(|(name, _)| {
                                (classes.get(name) == Some(&OwnershipClass::Owned))
                                    .then(|| name.clone())
                            })
                            .collect()
                    })
                    .unwrap_or_default();
                insert_reuse(decl, &owned_params)
            })
            .collect(),
    };
    let mut next_id: usize = 0;
    let reused = insert_tail_reuse_module(intra, &mut next_id);
    if std::env::var("GOBY_DUMP_PERCEUS_IR").is_ok() {
        for decl in &reused.decls {
            eprintln!("=== reused IR[{}] ===\n{:#?}", decl.name, decl.body);
        }
    }
    reused
}

fn ownership_classify_module(
    module: &IrModule,
) -> (
    HashMap<String, HashMap<String, OwnershipClass>>,
    HashMap<String, OwnershipClass>,
) {
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
    // §4.100 Step 1b: threaded into `classify_owned_result` so a `Call`
    // to a decl whose return ownership is `Owned` produces an `Owned`
    // local binding (replacing the M9 `type_is_known_heap` stopgap).
    // Greatest-fixed-point: start every decl `Owned`; descend on
    // structural Borrowed evidence. See `classify_decl_return_ownership`.
    let mut decl_returns: HashMap<String, OwnershipClass> = module
        .decls
        .iter()
        .map(|decl| (decl.name.clone(), OwnershipClass::Owned))
        .collect();
    for name in HEAP_RETURNING_INTRINSICS {
        decl_returns.insert((*name).to_string(), OwnershipClass::Owned);
    }

    loop {
        // Build per-decl param ownership map keyed by param name (the
        // shape `classify_decl_return_ownership` consumes).
        let param_map: HashMap<String, HashMap<String, OwnershipClass>> = module
            .decls
            .iter()
            .map(|decl| {
                let mut m = HashMap::new();
                if let Some(classes) = param_ownership.get(&decl.name) {
                    for ((name, _), class) in decl.params.iter().zip(classes.iter()) {
                        m.insert(name.clone(), *class);
                    }
                }
                (decl.name.clone(), m)
            })
            .collect();

        let classified: HashMap<String, HashMap<String, OwnershipClass>> = module
            .decls
            .iter()
            .map(|decl| {
                (
                    decl.name.clone(),
                    ownership_classify_decl(decl, &param_ownership, &decl_returns),
                )
            })
            .collect();

        let next_returns = classify_decl_return_ownership(module, &param_map);

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
            let entry = decl_returns
                .get_mut(&decl.name)
                .expect("decl return ownership must be seeded");
            let next = next_returns
                .get(&decl.name)
                .copied()
                .unwrap_or(OwnershipClass::Owned);
            if *entry != next {
                *entry = next;
                changed = true;
            }
        }

        if !changed {
            return (classified, decl_returns);
        }
    }
}

fn ownership_classify_decl(
    decl: &IrDecl,
    module_params: &HashMap<String, Vec<OwnershipClass>>,
    decl_returns: &HashMap<String, OwnershipClass>,
) -> HashMap<String, OwnershipClass> {
    let mut classes = HashMap::new();
    let mut params = HashSet::new();
    let mut aliases = HashMap::new();
    let current = module_params
        .get(&decl.name)
        .expect("parameter ownership facts must exist");
    for (idx, (param_name, _)) in decl.params.iter().enumerate() {
        params.insert(param_name.clone());
        classes.insert(param_name.clone(), current[idx]);
    }
    classify_comp(
        &decl.body,
        &mut classes,
        &params,
        module_params,
        decl_returns,
        &mut aliases,
    );
    for scalar_param in collect_scalar_param_evidence(&decl.body, &params) {
        classes.insert(scalar_param, OwnershipClass::Borrowed);
    }
    classes
}

fn collect_scalar_param_evidence(comp: &CompExpr, params: &HashSet<String>) -> HashSet<String> {
    let mut scalar = HashSet::new();
    collect_scalar_param_evidence_comp(comp, params, &mut scalar);
    scalar
}

fn collect_scalar_param_evidence_comp(
    comp: &CompExpr,
    params: &HashSet<String>,
    scalar: &mut HashSet<String>,
) {
    match comp {
        CompExpr::Value(value) => collect_scalar_param_evidence_value(value, params, scalar),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            collect_scalar_param_evidence_comp(value, params, scalar);
            collect_scalar_param_evidence_comp(body, params, scalar);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                collect_scalar_param_evidence_comp(stmt, params, scalar);
            }
            collect_scalar_param_evidence_comp(tail, params, scalar);
        }
        CompExpr::If { cond, then_, else_ } => {
            mark_scalar_param_value(cond, params, scalar);
            collect_scalar_param_evidence_value(cond, params, scalar);
            collect_scalar_param_evidence_comp(then_, params, scalar);
            collect_scalar_param_evidence_comp(else_, params, scalar);
        }
        CompExpr::Call { callee, args, .. } => {
            collect_scalar_param_evidence_value(callee, params, scalar);
            for arg in args {
                collect_scalar_param_evidence_value(arg, params, scalar);
            }
        }
        CompExpr::Assign { value, .. } => {
            collect_scalar_param_evidence_comp(value, params, scalar);
        }
        CompExpr::AssignIndex { path, value, .. } => {
            for index in path {
                mark_scalar_param_value(index, params, scalar);
            }
            collect_scalar_param_evidence_comp(value, params, scalar);
        }
        CompExpr::Case { scrutinee, arms } => {
            collect_scalar_param_evidence_value(scrutinee, params, scalar);
            for arm in arms {
                collect_scalar_param_evidence_comp(&arm.body, params, scalar);
            }
        }
        CompExpr::Dup { value }
        | CompExpr::Drop { value }
        | CompExpr::DropReuse { value, .. }
        | CompExpr::Resume { value } => collect_scalar_param_evidence_value(value, params, scalar),
        CompExpr::AllocReuse { .. } => {}
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                collect_scalar_param_evidence_value(arg, params, scalar);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                collect_scalar_param_evidence_comp(&clause.body, params, scalar);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            collect_scalar_param_evidence_comp(handler, params, scalar);
            collect_scalar_param_evidence_comp(body, params, scalar);
        }
    }
}

fn collect_scalar_param_evidence_value(
    value: &ValueExpr,
    params: &HashSet<String>,
    scalar: &mut HashSet<String>,
) {
    match value {
        ValueExpr::BinOp { left, right, .. } => {
            mark_scalar_param_value(left, params, scalar);
            mark_scalar_param_value(right, params, scalar);
            collect_scalar_param_evidence_value(left, params, scalar);
            collect_scalar_param_evidence_value(right, params, scalar);
        }
        ValueExpr::ListGet { list, index } => {
            mark_scalar_param_value(list, params, scalar);
            collect_scalar_param_evidence_value(list, params, scalar);
            mark_scalar_param_value(index, params, scalar);
            collect_scalar_param_evidence_value(index, params, scalar);
        }
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                collect_scalar_param_evidence_value(element, params, scalar);
            }
            if let Some(spread) = spread {
                collect_scalar_param_evidence_value(spread, params, scalar);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                collect_scalar_param_evidence_value(item, params, scalar);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                collect_scalar_param_evidence_value(field, params, scalar);
            }
        }
        ValueExpr::Lambda { param: _, body } => {
            collect_scalar_param_evidence_comp(body, params, scalar);
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    mark_scalar_param_value(value, params, scalar);
                    collect_scalar_param_evidence_value(value, params, scalar);
                }
            }
        }
        ValueExpr::TupleProject { tuple, .. } => {
            mark_scalar_param_value(tuple, params, scalar);
            collect_scalar_param_evidence_value(tuple, params, scalar);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn mark_scalar_param_value(
    value: &ValueExpr,
    params: &HashSet<String>,
    scalar: &mut HashSet<String>,
) {
    if let ValueExpr::Var(name) = value
        && params.contains(name)
    {
        scalar.insert(name.clone());
    }
}

fn classify_comp(
    comp: &CompExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    module_params: &HashMap<String, Vec<OwnershipClass>>,
    decl_returns: &HashMap<String, OwnershipClass>,
    aliases: &mut HashMap<String, String>,
) {
    match comp {
        CompExpr::Value(value) => classify_return_value(value, classes, params, aliases),
        CompExpr::Let {
            name, value, body, ..
        } => {
            classify_bound_comp(value, classes, params, module_params, decl_returns, aliases);
            classes.insert(
                name.clone(),
                classify_owned_result(value, classes, decl_returns, aliases),
            );
            let previous_alias = bind_alias(name, value, params, aliases);
            classify_comp(body, classes, params, module_params, decl_returns, aliases);
            restore_alias(name, previous_alias, aliases);
        }
        CompExpr::LetMut {
            name, value, body, ..
        } => {
            classify_bound_comp(value, classes, params, module_params, decl_returns, aliases);
            classify_consumed_let_mut_source(value, body, classes, params, aliases);
            classes.insert(name.clone(), OwnershipClass::Borrowed);
            let previous_alias = aliases.remove(name);
            classify_comp(body, classes, params, module_params, decl_returns, aliases);
            restore_alias(name, previous_alias, aliases);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                classify_comp(stmt, classes, params, module_params, decl_returns, aliases);
            }
            classify_comp(tail, classes, params, module_params, decl_returns, aliases);
        }
        CompExpr::If { cond, then_, else_ } => {
            classify_borrowed_value(cond, classes, params, aliases);
            classify_comp(then_, classes, params, module_params, decl_returns, aliases);
            classify_comp(else_, classes, params, module_params, decl_returns, aliases);
        }
        CompExpr::Call { callee, args, .. } => {
            classify_borrowed_value(callee, classes, params, aliases);
            classify_call_args(callee, args, classes, params, module_params, aliases);
        }
        CompExpr::Assign { value, .. } => {
            classify_bound_comp(value, classes, params, module_params, decl_returns, aliases)
        }
        CompExpr::AssignIndex { path, value, .. } => {
            for index in path {
                classify_borrowed_value(index, classes, params, aliases);
            }
            classify_bound_comp(value, classes, params, module_params, decl_returns, aliases);
        }
        CompExpr::Case { scrutinee, arms } => {
            classify_borrowed_value(scrutinee, classes, params, aliases);
            for arm in arms {
                // Pattern-bound variables own the sub-parts of the scrutinee.
                classify_case_pattern_bindings(&arm.pattern, classes);
                let shadowed_aliases =
                    remove_aliases(&collect_pattern_bindings(&arm.pattern), aliases);
                classify_comp(
                    &arm.body,
                    classes,
                    params,
                    module_params,
                    decl_returns,
                    aliases,
                );
                restore_aliases(shadowed_aliases, aliases);
            }
        }
        CompExpr::Dup { value } | CompExpr::Resume { value } => {
            classify_borrowed_value(value, classes, params, aliases)
        }
        CompExpr::Drop { value } | CompExpr::DropReuse { value, .. } => {
            classify_consumed_value(value, classes, params, aliases);
        }
        CompExpr::AllocReuse { .. } => {}
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                classify_consumed_value(arg, classes, params, aliases);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                let shadowed_aliases = remove_aliases(&clause.params, aliases);
                classify_comp(
                    &clause.body,
                    classes,
                    params,
                    module_params,
                    decl_returns,
                    aliases,
                );
                restore_aliases(shadowed_aliases, aliases);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            classify_comp(
                handler,
                classes,
                params,
                module_params,
                decl_returns,
                aliases,
            );
            classify_comp(body, classes, params, module_params, decl_returns, aliases);
        }
    }
}

fn classify_bound_comp(
    comp: &CompExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    module_params: &HashMap<String, Vec<OwnershipClass>>,
    decl_returns: &HashMap<String, OwnershipClass>,
    aliases: &mut HashMap<String, String>,
) {
    match comp {
        CompExpr::Value(value) => classify_bound_value(value, classes, params, aliases),
        _ => classify_comp(comp, classes, params, module_params, decl_returns, aliases),
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

fn demote_owner(
    name: &str,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    aliases: &HashMap<String, String>,
) {
    let owner = aliases.get(name).map_or(name, String::as_str);
    demote_param(owner, classes, params);
}

fn bind_alias(
    name: &str,
    value: &CompExpr,
    params: &HashSet<String>,
    aliases: &mut HashMap<String, String>,
) -> Option<String> {
    let previous = aliases.remove(name);
    if let CompExpr::Value(ValueExpr::Var(source)) = value {
        let owner = aliases.get(source).map_or(source, |root| root);
        if params.contains(owner) {
            aliases.insert(name.to_string(), owner.clone());
        }
    }
    previous
}

fn restore_alias(name: &str, previous: Option<String>, aliases: &mut HashMap<String, String>) {
    if let Some(previous) = previous {
        aliases.insert(name.to_string(), previous);
    } else {
        aliases.remove(name);
    }
}

fn remove_aliases(
    names: &[String],
    aliases: &mut HashMap<String, String>,
) -> Vec<(String, Option<String>)> {
    names
        .iter()
        .map(|name| (name.clone(), aliases.remove(name)))
        .collect()
}

fn restore_aliases(
    shadowed_aliases: Vec<(String, Option<String>)>,
    aliases: &mut HashMap<String, String>,
) {
    for (name, previous) in shadowed_aliases {
        restore_alias(&name, previous, aliases);
    }
}

fn classify_call_args(
    callee: &ValueExpr,
    args: &[ValueExpr],
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    module_params: &HashMap<String, Vec<OwnershipClass>>,
    aliases: &HashMap<String, String>,
) {
    let callee_params = callee_param_classes(callee, module_params);
    for (idx, arg) in args.iter().enumerate() {
        if intrinsic_arg_is_borrowed(callee, idx)
            || callee_params
                .and_then(|params| params.get(idx))
                .is_some_and(|class| *class == OwnershipClass::Borrowed)
        {
            classify_borrowed_value(arg, classes, params, aliases);
        } else {
            classify_consumed_value(arg, classes, params, aliases);
        }
    }
}

fn classify_consumed_let_mut_source(
    value: &CompExpr,
    body: &CompExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    aliases: &HashMap<String, String>,
) {
    let CompExpr::Value(ValueExpr::Var(source)) = value else {
        return;
    };
    let owner = aliases.get(source).map_or(source, |root| root);
    if params.contains(owner) && !collect_live_vars(body).contains(owner) {
        demote_param(owner, classes, params);
    }
}

fn intrinsic_arg_is_borrowed(callee: &ValueExpr, idx: usize) -> bool {
    matches!(
        (callee, idx),
        (
            ValueExpr::GlobalRef { name, .. } | ValueExpr::Var(name),
            0
        ) if matches!(name.as_str(), "__goby_list_length" | "__goby_list_fold")
    )
}

fn callee_param_classes<'a>(
    callee: &ValueExpr,
    module_params: &'a HashMap<String, Vec<OwnershipClass>>,
) -> Option<&'a [OwnershipClass]> {
    match callee {
        ValueExpr::GlobalRef { name, .. } | ValueExpr::Var(name) => {
            module_params.get(name).map(Vec::as_slice)
        }
        _ => None,
    }
}

fn classify_return_value(
    value: &ValueExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    aliases: &HashMap<String, String>,
) {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                classify_consumed_value(element, classes, params, aliases);
            }
            if let Some(spread) = spread {
                classify_consumed_value(spread, classes, params, aliases);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                classify_consumed_value(item, classes, params, aliases);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                classify_consumed_value(field, classes, params, aliases);
            }
        }
        ValueExpr::Lambda { param, body } => {
            classify_lambda_capture(body, param, classes, params, aliases)
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    classify_consumed_value(value, classes, params, aliases);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            classify_borrowed_value(left, classes, params, aliases);
            classify_borrowed_value(right, classes, params, aliases);
        }
        ValueExpr::TupleProject { tuple, .. } => {
            // Projecting from a tuple consumes the tuple (ownership transfers to the projection
            // result), so the source is classified as consumed, not merely borrowed.
            classify_consumed_value(tuple, classes, params, aliases)
        }
        ValueExpr::ListGet { list, index } => {
            classify_borrowed_value(list, classes, params, aliases);
            classify_borrowed_value(index, classes, params, aliases);
        }
        ValueExpr::Var(name) => demote_owner(name, classes, params, aliases),
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
    aliases: &HashMap<String, String>,
) {
    match value {
        ValueExpr::Var(_) => {}
        _ => classify_return_value(value, classes, params, aliases),
    }
}

fn classify_borrowed_value(
    value: &ValueExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    aliases: &HashMap<String, String>,
) {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                classify_consumed_value(element, classes, params, aliases);
            }
            if let Some(spread) = spread {
                classify_consumed_value(spread, classes, params, aliases);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                classify_consumed_value(item, classes, params, aliases);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                classify_consumed_value(field, classes, params, aliases);
            }
        }
        ValueExpr::Lambda { param, body } => {
            classify_lambda_capture(body, param, classes, params, aliases)
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    classify_borrowed_value(value, classes, params, aliases);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            classify_borrowed_value(left, classes, params, aliases);
            classify_borrowed_value(right, classes, params, aliases);
        }
        ValueExpr::TupleProject { tuple, .. } => {
            classify_borrowed_value(tuple, classes, params, aliases)
        }
        ValueExpr::ListGet { list, index } => {
            classify_borrowed_value(list, classes, params, aliases);
            classify_borrowed_value(index, classes, params, aliases);
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
    aliases: &HashMap<String, String>,
) {
    match value {
        ValueExpr::Var(name) => demote_owner(name, classes, params, aliases),
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                classify_consumed_value(element, classes, params, aliases);
            }
            if let Some(spread) = spread {
                classify_consumed_value(spread, classes, params, aliases);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                classify_consumed_value(item, classes, params, aliases);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                classify_consumed_value(field, classes, params, aliases);
            }
        }
        ValueExpr::Lambda { param, body } => {
            classify_lambda_capture(body, param, classes, params, aliases)
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    classify_consumed_value(value, classes, params, aliases);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            classify_consumed_value(left, classes, params, aliases);
            classify_consumed_value(right, classes, params, aliases);
        }
        ValueExpr::TupleProject { tuple, .. } => {
            classify_consumed_value(tuple, classes, params, aliases)
        }
        ValueExpr::ListGet { list, index } => {
            classify_consumed_value(list, classes, params, aliases);
            classify_consumed_value(index, classes, params, aliases);
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
    aliases: &HashMap<String, String>,
) {
    let mut live = collect_live_vars(body);
    live.remove(lambda_param);
    for name in live {
        demote_owner(&name, classes, params, aliases);
    }
}

fn classify_owned_result(
    comp: &CompExpr,
    classes: &HashMap<String, OwnershipClass>,
    decl_returns: &HashMap<String, OwnershipClass>,
    aliases: &HashMap<String, String>,
) -> OwnershipClass {
    match comp {
        CompExpr::Value(value) if value_is_fresh_heap(value) => OwnershipClass::Owned,
        CompExpr::Value(ValueExpr::Var(name)) => {
            let owner = aliases.get(name).map_or(name, |root| root);
            classes
                .get(owner)
                .copied()
                .unwrap_or(OwnershipClass::Borrowed)
        }
        // Tuple projection from a `Var` whose underlying tuple is `Owned`
        // is itself `Owned`. We no longer gate on `type_may_be_heap(ty)`:
        // the §M9 3c rule's intent is purely structural (project from an
        // owned composite). The drop machinery downstream will still
        // ignore non-heap projections at the *use site* via its existing
        // `type_may_be_heap` checks (`drop_instrumentation_ownership`,
        // `insert_owned_let_drop`).
        CompExpr::Value(ValueExpr::TupleProject { tuple, .. }) => {
            if let ValueExpr::Var(name) = tuple.as_ref() {
                let owner = aliases.get(name).map_or(name, |root| root);
                classes
                    .get(owner)
                    .copied()
                    .unwrap_or(OwnershipClass::Borrowed)
            } else {
                OwnershipClass::Borrowed
            }
        }
        // M10 §4.100 Step 1: `Call` result ownership is the callee's
        // declared return ownership, not a heap-shape heuristic. The
        // `type_is_known_heap` stopgap that lived here under M9 is
        // removed; `decl_returns` is computed by
        // `classify_decl_return_ownership` and threaded through the
        // M4.5 fix-point in `ownership_classify_module`.
        CompExpr::Call { callee, args, .. } => {
            classify_call_result_ownership(callee, args, classes, decl_returns)
        }
        _ => OwnershipClass::Borrowed,
    }
}

fn classify_call_result_ownership(
    callee: &ValueExpr,
    args: &[ValueExpr],
    classes: &HashMap<String, OwnershipClass>,
    decl_returns: &HashMap<String, OwnershipClass>,
) -> OwnershipClass {
    if is_list_map_callee(callee)
        && args
            .get(1)
            .is_some_and(|callback| callback_returns_owned(callback, classes, decl_returns))
    {
        return OwnershipClass::Owned;
    }

    callee_decl_return_class(callee, classes, decl_returns)
}

fn is_list_map_callee(callee: &ValueExpr) -> bool {
    matches!(
        callee,
        ValueExpr::GlobalRef { name, .. } | ValueExpr::Var(name)
            if matches!(name.as_str(), "map" | "__goby_list_map")
    )
}

fn callback_returns_owned(
    callback: &ValueExpr,
    classes: &HashMap<String, OwnershipClass>,
    decl_returns: &HashMap<String, OwnershipClass>,
) -> bool {
    match callback {
        ValueExpr::Lambda { param, body } => {
            let mut env = HashMap::from([(param.clone(), OwnershipClass::Borrowed)]);
            return_ownership_comp(body, &mut env, decl_returns) == OwnershipClass::Owned
        }
        ValueExpr::GlobalRef { name, .. } => {
            decl_returns.get(name).copied() == Some(OwnershipClass::Owned)
        }
        ValueExpr::Var(name) if !classes.contains_key(name) => {
            decl_returns.get(name).copied() == Some(OwnershipClass::Owned)
        }
        _ => false,
    }
}

/// Resolve a `Call`'s callee to its decl-level return ownership.
///
/// `Var(name)` is a module decl reference only when `name` is not
/// shadowed by a local binding; otherwise the call is closure / indirect
/// and conservatively `Borrowed`. `GlobalRef` always names a module decl.
fn callee_decl_return_class(
    callee: &ValueExpr,
    classes: &HashMap<String, OwnershipClass>,
    decl_returns: &HashMap<String, OwnershipClass>,
) -> OwnershipClass {
    match callee {
        ValueExpr::Var(name) => {
            // Shadowing check: any local binding for `name` (param, let,
            // case binder) shows up in `classes` for this decl. Module
            // decl names that are never bound locally do not.
            if classes.contains_key(name) {
                OwnershipClass::Borrowed
            } else {
                decl_returns
                    .get(name)
                    .copied()
                    .unwrap_or(OwnershipClass::Borrowed)
            }
        }
        ValueExpr::GlobalRef { name, .. } => decl_returns
            .get(name)
            .copied()
            .unwrap_or(OwnershipClass::Borrowed),
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

/// Heap-returning runtime intrinsic registry. Used by
/// `classify_decl_return_ownership` to seed `decl_returns` with `Owned`
/// for intrinsics whose return is structurally a fresh heap allocation.
/// Anything not in this list defaults to `Borrowed`, matching the
/// pre-M10 behaviour for intrinsics that the analysis cannot reason
/// about (e.g. `__goby_list_fold`, whose accumulator type determines
/// ownership of the result).
const HEAP_RETURNING_INTRINSICS: &[&str] = &[
    // M10 Step 1c deliberately does not seed `__goby_list_map`
    // unconditionally. The outer list is fresh, but dropping it also drops
    // its elements; that is only safe when the callback returns owned values.
    // `classify_call_result_ownership` handles map at the call site where the
    // callback argument is visible.
    // Intentionally omitted (in-place / consume-and-return idioms):
    // - `__goby_list_push_string`: returns the same heap reference as
    //   arg-0 when there is room to push in place.
    // - `__goby_list_join_string`, `__goby_string_concat`,
    //   `__goby_string_graphemes_list`, `__goby_string_split_lines`,
    //   `__goby_env_fetch_env_var`: deferred because graphemes-pipeline
    //   regressions during Step 1b suggest at least one of these
    //   intrinsics consumes its arg or returns an aliased reference.
    //   Conservative Borrowed retains pre-M10 behaviour while the
    //   per-intrinsic ownership semantics are documented and verified
    //   one at a time. Re-add an entry only with a regression test in
    //   the same commit.
];

/// Per-decl ownership of the returned reference (M10 §4.100 Step 1).
///
/// `Owned` means the caller owns the result reference: every tail-position
/// value in the body is provably a fresh heap allocation, a fresh closure,
/// or a call to another `Owned`-returning decl (or projects an `Owned`
/// composite). `Borrowed` is the conservative default — used whenever any
/// branch returns a `Var` whose M4.5 ownership is `Borrowed`, or any other
/// shape that does not produce a fresh owned reference.
///
/// This pass *consumes* the M4.5 parameter ownership map produced by
/// `ownership_classify_module`. Initial Γ binds each parameter to its
/// already-classified ownership; this is what lets a recursive helper's
/// base case (e.g. `build`'s `acc`) propagate to `Owned` even though the
/// body itself does not freshly allocate it.
///
/// **Fix-point direction (greatest fixed point).** Initialise every decl
/// to `Owned`, then iteratively recompute against a snapshot of the
/// previous map. A decl's class only moves `Owned → Borrowed` (monotone
/// in the descending direction), so the loop terminates in at most
/// `|decls|` iterations. This direction matters because `build`-shaped
/// recursive helpers ("base case is `acc`, recursive case is a self-call
/// with a fresh spread") need the recursive call to be *assumed* Owned in
/// iteration 1, then *confirmed* Owned in iteration 2 once both branches
/// of the body classify as Owned.
fn classify_decl_return_ownership(
    module: &IrModule,
    param_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
) -> HashMap<String, OwnershipClass> {
    let mut returns: HashMap<String, OwnershipClass> = module
        .decls
        .iter()
        .map(|decl| (decl.name.clone(), OwnershipClass::Owned))
        .collect();
    // Heap-returning runtime intrinsics. Stdlib decls forward many
    // language operations to these intrinsics (e.g. `list.map xs f =
    // __goby_list_map xs f`), so without seeding the intrinsic's return
    // ownership the stdlib wrapper would classify Borrowed and the
    // caller would never emit a Drop on the freshly allocated result.
    //
    // Conservative subset: only intrinsics whose signature is *known*
    // to return a freshly allocated heap object regardless of inputs.
    // Intrinsics whose return ownership depends on argument shape (e.g.
    // `__goby_list_fold` over a heap accumulator) stay default Borrowed
    // until a future per-intrinsic registry can express the dependency.
    for name in HEAP_RETURNING_INTRINSICS {
        returns.insert((*name).to_string(), OwnershipClass::Owned);
    }

    // Bound iterations strictly above |decls| to defend against an
    // implementation regression that would break monotonicity.
    for _ in 0..=module.decls.len() {
        let snapshot = returns.clone();
        let mut changed = false;
        for decl in &module.decls {
            let mut env: HashMap<String, OwnershipClass> = HashMap::new();
            if let Some(params) = param_ownership.get(&decl.name) {
                for (name, class) in params {
                    env.insert(name.clone(), *class);
                }
            }
            let class = return_ownership_comp(&decl.body, &mut env, &snapshot);
            let entry = returns
                .get_mut(&decl.name)
                .expect("seeded above for every decl");
            // Greatest-fixed-point: only ever flip Owned → Borrowed.
            if class == OwnershipClass::Borrowed && *entry == OwnershipClass::Owned {
                *entry = OwnershipClass::Borrowed;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    returns
}

/// Walk a `CompExpr` tail-position to derive its ownership against `env` and
/// the in-progress per-decl `returns` snapshot. Caller-side `env` mutation
/// (let / case bindings) is balanced by snapshot/restore so the walker is
/// effectively pure on `env` once it returns.
fn return_ownership_comp(
    comp: &CompExpr,
    env: &mut HashMap<String, OwnershipClass>,
    returns: &HashMap<String, OwnershipClass>,
) -> OwnershipClass {
    match comp {
        CompExpr::Value(value) => return_ownership_value(value, env, returns),
        CompExpr::Let {
            name, value, body, ..
        }
        | CompExpr::LetMut {
            name, value, body, ..
        } => {
            let value_class = return_ownership_comp(value, env, returns);
            let previous = env.insert(name.clone(), value_class);
            let result = return_ownership_comp(body, env, returns);
            match previous {
                Some(prev) => {
                    env.insert(name.clone(), prev);
                }
                None => {
                    env.remove(name);
                }
            }
            result
        }
        CompExpr::Seq { tail, .. } => return_ownership_comp(tail, env, returns),
        CompExpr::If { then_, else_, .. } => {
            let t = return_ownership_comp(then_, env, returns);
            if t == OwnershipClass::Borrowed {
                return OwnershipClass::Borrowed;
            }
            return_ownership_comp(else_, env, returns)
        }
        CompExpr::Case { arms, .. } => {
            if arms.is_empty() {
                return OwnershipClass::Borrowed;
            }
            let mut acc = OwnershipClass::Owned;
            for arm in arms {
                // Pattern-bound names own the projected sub-parts.
                let pattern_binders = collect_pattern_bindings(&arm.pattern);
                let mut shadowed: Vec<(String, Option<OwnershipClass>)> = Vec::new();
                for name in &pattern_binders {
                    let prev = env.insert(name.clone(), OwnershipClass::Owned);
                    shadowed.push((name.clone(), prev));
                }
                let arm_class = return_ownership_comp(&arm.body, env, returns);
                for (name, prev) in shadowed.into_iter().rev() {
                    match prev {
                        Some(p) => {
                            env.insert(name, p);
                        }
                        None => {
                            env.remove(&name);
                        }
                    }
                }
                if arm_class == OwnershipClass::Borrowed {
                    acc = OwnershipClass::Borrowed;
                }
            }
            acc
        }
        CompExpr::Call { callee, .. } => match callee.as_ref() {
            // A `Var(name)` callee is a *module decl reference* only when
            // `name` does not currently shadow a local binder. If `env`
            // already binds `name` (param, let, let-mut, case binder),
            // this is a closure / indirect call and we cannot consult the
            // module return map — its decl-name resolution would be wrong.
            ValueExpr::Var(name) => {
                if env.contains_key(name) {
                    OwnershipClass::Borrowed
                } else {
                    returns
                        .get(name)
                        .copied()
                        .unwrap_or(OwnershipClass::Borrowed)
                }
            }
            // `GlobalRef` always names a module decl in callee position.
            ValueExpr::GlobalRef { name, .. } => returns
                .get(name)
                .copied()
                .unwrap_or(OwnershipClass::Borrowed),
            _ => OwnershipClass::Borrowed,
        },
        // Drop / DropReuse / Dup / Assign / AssignIndex / AllocReuse /
        // PerformEffect / Resume / Handle / WithHandler never produce a
        // fresh owned tail-position value; conservative Borrowed.
        CompExpr::Drop { .. }
        | CompExpr::DropReuse { .. }
        | CompExpr::Dup { .. }
        | CompExpr::Assign { .. }
        | CompExpr::AssignIndex { .. }
        | CompExpr::AllocReuse { .. }
        | CompExpr::PerformEffect { .. }
        | CompExpr::Resume { .. }
        | CompExpr::Handle { .. }
        | CompExpr::WithHandler { .. } => OwnershipClass::Borrowed,
    }
}

#[allow(dead_code)]
fn return_ownership_value(
    value: &ValueExpr,
    env: &HashMap<String, OwnershipClass>,
    returns: &HashMap<String, OwnershipClass>,
) -> OwnershipClass {
    match value {
        ValueExpr::Var(name) => env.get(name).copied().unwrap_or(OwnershipClass::Borrowed),
        // `GlobalRef` in *value* position is a *reference to* a module
        // decl (used as a first-class function value). It is **not** the
        // decl's return value; that would require a `Call` form. We must
        // not borrow `returns[name]` here — otherwise returning a
        // function value of an `Owned`-returning decl would spuriously
        // classify as `Owned` and the caller would emit a Drop on a
        // function reference.
        ValueExpr::GlobalRef { .. } => OwnershipClass::Borrowed,
        ValueExpr::ListLit { spread: None, .. } => OwnershipClass::Owned,
        ValueExpr::ListLit {
            spread: Some(s), ..
        } => {
            // A spread of a fresh list keeps the result fresh; a spread of a
            // Borrowed param does not.
            return_ownership_value(s, env, returns)
        }
        ValueExpr::TupleLit(items) => {
            // Mixed-payload tuple is Owned iff at least one item is Owned;
            // the §M9 3c projection rule widens at the use site.
            if items
                .iter()
                .any(|item| return_ownership_value(item, env, returns) == OwnershipClass::Owned)
            {
                OwnershipClass::Owned
            } else {
                OwnershipClass::Borrowed
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            if fields
                .iter()
                .any(|(_, v)| return_ownership_value(v, env, returns) == OwnershipClass::Owned)
            {
                OwnershipClass::Owned
            } else {
                OwnershipClass::Borrowed
            }
        }
        ValueExpr::Interp(_) => OwnershipClass::Owned,
        ValueExpr::Lambda { .. } => OwnershipClass::Owned,
        // Tuple projection: conservatively `Borrowed`. We do not have a
        // per-field type or per-field ownership map at this layer, so we
        // cannot prove the projected field is itself heap-shaped. The
        // §M9 3c rule widens projections only at the *use site* where
        // `type_may_be_heap(field_ty)` is checked; replicating that here
        // would require threading per-field types through the value
        // walker, which is out of scope for §4.100 Step 1.
        ValueExpr::TupleProject { .. } => OwnershipClass::Borrowed,
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Unit
        | ValueExpr::BinOp { .. }
        | ValueExpr::ListGet { .. } => OwnershipClass::Borrowed,
    }
}

fn drop_insert_module(
    module: &IrModule,
    ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
) -> IrModule {
    let drop_ownership = drop_instrumentation_ownership(module, ownership);
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
                let decl_ownership = drop_ownership
                    .get(&decl.name)
                    .expect("ownership facts must exist");

                let (body, next_tmp) =
                    drop_insert_comp(&decl.body, decl_ownership, &drop_ownership, &param_order, 0);

                let (body, _) = drop_insert_params(
                    &decl.params,
                    body,
                    decl_ownership,
                    &drop_ownership,
                    &param_order,
                    next_tmp,
                );

                // 3a: insert Drops for Owned params that go dead at tail-recursive self-calls.
                let owned_self_params: Vec<String> = decl
                    .params
                    .iter()
                    .filter(|(name, _)| decl_ownership.get(name) == Some(&OwnershipClass::Owned))
                    .map(|(name, _)| name.clone())
                    .collect();
                let body = if owned_self_params.is_empty() {
                    body
                } else {
                    insert_self_tail_call_param_drops_comp(
                        body,
                        &decl.name,
                        &owned_self_params,
                        decl_ownership,
                        &drop_ownership,
                        &param_order,
                    )
                };

                IrDecl {
                    name: decl.name.clone(),
                    params: decl.params.clone(),
                    result_ty: decl.result_ty.clone(),
                    residual_effects: decl.residual_effects.clone(),
                    body,
                    reuse_param: decl.reuse_param.clone(),
                }
            })
            .collect(),
    }
}

/// Collect all `Var` names mentioned anywhere in a value expression.
fn collect_value_var_names(value: &ValueExpr, out: &mut HashSet<String>) {
    match value {
        ValueExpr::Var(n) => {
            out.insert(n.clone());
        }
        ValueExpr::ListLit { elements, spread } => {
            for e in elements {
                collect_value_var_names(e, out);
            }
            if let Some(s) = spread {
                collect_value_var_names(s, out);
            }
        }
        ValueExpr::TupleLit(items) => {
            for i in items {
                collect_value_var_names(i, out);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, v) in fields {
                collect_value_var_names(v, out);
            }
        }
        ValueExpr::Lambda { .. } => {}
        ValueExpr::Interp(parts) => {
            for p in parts {
                if let IrInterpPart::Expr(v) = p {
                    collect_value_var_names(v, out);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            collect_value_var_names(left, out);
            collect_value_var_names(right, out);
        }
        ValueExpr::TupleProject { tuple, .. } => collect_value_var_names(tuple, out),
        ValueExpr::ListGet { list, index } => {
            collect_value_var_names(list, out);
            collect_value_var_names(index, out);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

/// For a `Call { callee, args }` that is a direct tail-recursive self-call, return the Owned
/// parameters of the current decl that are NOT mentioned in any argument expression and are
/// therefore dead before the recursive call. Drops for these should be emitted before the call.
fn tail_call_self_dropped_params<'a>(
    self_name: &str,
    callee: &ValueExpr,
    args: &[ValueExpr],
    ownership: &HashMap<String, OwnershipClass>,
    self_params: &'a [String],
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
) -> Vec<&'a str> {
    let calling_self = matches!(
        callee,
        ValueExpr::GlobalRef { name, .. } | ValueExpr::Var(name) if name == self_name
    );
    if !calling_self {
        return Vec::new();
    }

    // Collect all vars mentioned anywhere in the arg expressions.
    let mut mentioned: HashSet<String> = HashSet::new();
    for arg in args {
        collect_value_var_names(arg, &mut mentioned);
    }

    // A param can be dropped before the call iff:
    // 1. It is classified Owned in the current decl's ownership map.
    // 2. It does not appear in any arg expression (textual mention).
    // 3. As a defensive check: the full Call does not consume the param via aliasing
    //    (use comp_consumes_name on the original call).
    let call_comp = CompExpr::Call {
        callee: Box::new(callee.clone()),
        args: args.to_vec(),
        reuse_token: None,
    };

    self_params
        .iter()
        .filter(|p| ownership.get(p.as_str()) == Some(&OwnershipClass::Owned))
        .filter(|p| !mentioned.contains(p.as_str()))
        .filter(|p| !comp_consumes_name(&call_comp, p, module_ownership, param_order))
        .map(|p| p.as_str())
        .collect()
}

/// Walk a comp expression and, at each direct self-tail-call, prepend `Drop` for Owned params
/// that are not forwarded. Recurses into all sub-comps.
fn insert_self_tail_call_param_drops_comp(
    comp: CompExpr,
    self_name: &str,
    owned_params: &[String],
    ownership: &HashMap<String, OwnershipClass>,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
) -> CompExpr {
    let recurse = |c: CompExpr| {
        insert_self_tail_call_param_drops_comp(
            c,
            self_name,
            owned_params,
            ownership,
            module_ownership,
            param_order,
        )
    };
    match comp {
        CompExpr::Call {
            ref callee,
            ref args,
            ..
        } => {
            let drops = tail_call_self_dropped_params(
                self_name,
                callee,
                args,
                ownership,
                owned_params,
                module_ownership,
                param_order,
            );
            if drops.is_empty() {
                return comp;
            }
            let drop_stmts: Vec<CompExpr> = drops
                .into_iter()
                .map(|p| CompExpr::Drop {
                    value: Box::new(ValueExpr::Var(p.to_string())),
                })
                .collect();
            CompExpr::Seq {
                stmts: drop_stmts,
                tail: Box::new(comp),
            }
        }
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => {
            // params consumed by the bound expression are no longer live in the body
            let remaining: Vec<String> = owned_params
                .iter()
                .filter(|p| !comp_consumes_name(&value, p, module_ownership, param_order))
                .cloned()
                .collect();
            CompExpr::Let {
                name,
                ty,
                value: Box::new(recurse(*value)),
                body: Box::new(insert_self_tail_call_param_drops_comp(
                    *body,
                    self_name,
                    &remaining,
                    ownership,
                    module_ownership,
                    param_order,
                )),
            }
        }
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => {
            let remaining: Vec<String> = owned_params
                .iter()
                .filter(|p| !comp_consumes_name(&value, p, module_ownership, param_order))
                .cloned()
                .collect();
            CompExpr::LetMut {
                name,
                ty,
                value: Box::new(recurse(*value)),
                body: Box::new(insert_self_tail_call_param_drops_comp(
                    *body,
                    self_name,
                    &remaining,
                    ownership,
                    module_ownership,
                    param_order,
                )),
            }
        }
        CompExpr::Seq { stmts, tail } => {
            // accumulate consumed params across stmts
            let mut remaining = owned_params.to_vec();
            let new_stmts: Vec<CompExpr> = stmts
                .into_iter()
                .map(|stmt| {
                    let s = insert_self_tail_call_param_drops_comp(
                        stmt.clone(),
                        self_name,
                        &remaining,
                        ownership,
                        module_ownership,
                        param_order,
                    );
                    remaining
                        .retain(|p| !comp_consumes_name(&stmt, p, module_ownership, param_order));
                    s
                })
                .collect();
            CompExpr::Seq {
                stmts: new_stmts,
                tail: Box::new(insert_self_tail_call_param_drops_comp(
                    *tail,
                    self_name,
                    &remaining,
                    ownership,
                    module_ownership,
                    param_order,
                )),
            }
        }
        CompExpr::If { cond, then_, else_ } => CompExpr::If {
            cond,
            then_: Box::new(recurse(*then_)),
            else_: Box::new(recurse(*else_)),
        },
        CompExpr::Case { scrutinee, arms } => CompExpr::Case {
            scrutinee,
            arms: arms
                .into_iter()
                .map(|arm| IrCaseArm {
                    pattern: arm.pattern,
                    body: recurse(arm.body),
                })
                .collect(),
        },
        CompExpr::Value(_)
        | CompExpr::Assign { .. }
        | CompExpr::AssignIndex { .. }
        | CompExpr::Drop { .. }
        | CompExpr::DropReuse { .. }
        | CompExpr::Dup { .. }
        | CompExpr::AllocReuse { .. }
        | CompExpr::PerformEffect { .. }
        | CompExpr::Resume { .. }
        | CompExpr::Handle { .. }
        | CompExpr::WithHandler { .. } => comp,
    }
}

fn drop_instrumentation_ownership(
    module: &IrModule,
    ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
) -> HashMap<String, HashMap<String, OwnershipClass>> {
    let mut drop_ownership = ownership.clone();
    for decl in &module.decls {
        let Some(classes) = drop_ownership.get_mut(&decl.name) else {
            continue;
        };
        for (param_name, ty) in &decl.params {
            if !type_may_be_heap(ty) {
                classes.insert(param_name.clone(), OwnershipClass::Borrowed);
            }
        }
    }
    drop_ownership
}

fn type_may_be_heap(ty: &crate::ir::IrType) -> bool {
    match ty {
        crate::ir::IrType::Int
        | crate::ir::IrType::Bool
        | crate::ir::IrType::Str
        | crate::ir::IrType::Unit => false,
        crate::ir::IrType::Opaque(name)
            if matches!(name.as_str(), "Int" | "Bool" | "String" | "Unit") =>
        {
            false
        }
        crate::ir::IrType::Unknown | crate::ir::IrType::Opaque(_) => true,
    }
}

fn drop_insert_params(
    params: &[(String, crate::ir::IrType)],
    mut body: CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
    next_tmp: usize,
) -> (CompExpr, usize) {
    let mut next_tmp = next_tmp;
    let mut owned_params: Vec<&str> = params
        .iter()
        .map(|(name, _)| name.as_str())
        .filter(|name| ownership.get(*name) == Some(&OwnershipClass::Owned))
        .collect();
    owned_params.sort_unstable();

    for name in owned_params.into_iter().rev() {
        let (body_out, new_next_tmp) =
            insert_owned_binding_drop(name, body, module_ownership, param_order, next_tmp);
        body = body_out;
        next_tmp = new_next_tmp;
    }

    (body, next_tmp)
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
            let value_out = insert_live_across_let_value_dups(
                value_out,
                &body_with_drop,
                ownership,
                module_ownership,
                param_order,
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
            let branch_visible = collect_live_vars(comp);
            let (then_out, next_tmp) =
                drop_insert_comp(then_, ownership, module_ownership, param_order, next_tmp);
            let (else_out, next_tmp) =
                drop_insert_comp(else_, ownership, module_ownership, param_order, next_tmp);
            let (then_out, else_out, next_tmp) = balance_if_branch_drops(
                then_out,
                else_out,
                ownership,
                module_ownership,
                param_order,
                &branch_visible,
                next_tmp,
            );
            (
                CompExpr::If {
                    cond: cond.clone(),
                    then_: Box::new(then_out),
                    else_: Box::new(else_out),
                },
                next_tmp,
            )
        }
        CompExpr::Call {
            callee,
            args,
            reuse_token,
        } => {
            let call = CompExpr::Call {
                callee: callee.clone(),
                args: args.clone(),
                reuse_token: reuse_token.clone(),
            };
            (
                insert_non_last_call_arg_dups(call, ownership, module_ownership, param_order),
                next_tmp,
            )
        }
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
        CompExpr::AssignIndex {
            root,
            path,
            value,
            reuse_token,
        } => {
            let (value, next_tmp) =
                drop_insert_comp(value, ownership, module_ownership, param_order, next_tmp);
            (
                CompExpr::AssignIndex {
                    root: root.clone(),
                    path: path.clone(),
                    value: Box::new(value),
                    reuse_token: reuse_token.clone(),
                },
                next_tmp,
            )
        }
        CompExpr::Case { scrutinee, arms } => {
            let branch_visible = collect_live_vars(comp);
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
            let (arms, next_tmp_mut) = balance_case_branch_drops(
                arms,
                ownership,
                module_ownership,
                param_order,
                &branch_visible,
                next_tmp_mut,
            );
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
        CompExpr::DropReuse { value, bind } => (
            CompExpr::DropReuse {
                value: value.clone(),
                bind: bind.clone(),
            },
            next_tmp,
        ),
        CompExpr::AllocReuse {
            token,
            size_class,
            init,
        } => (
            CompExpr::AllocReuse {
                token: token.clone(),
                size_class: *size_class,
                init: init.clone(),
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

fn owned_binding_names(ownership: &HashMap<String, OwnershipClass>) -> Vec<&str> {
    let mut names: Vec<&str> = ownership
        .iter()
        .filter_map(|(name, class)| (*class == OwnershipClass::Owned).then_some(name.as_str()))
        .collect();
    names.sort_unstable();
    names
}

fn insert_owned_binding_drop(
    name: &str,
    body: CompExpr,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
    next_tmp: usize,
) -> (CompExpr, usize) {
    if count_var_uses(&body, name) == 0 {
        return prepend_drop(name, body, next_tmp);
    }

    if !comp_consumes_name(&body, name, module_ownership, param_order) {
        return append_drop_preserving_result(name, body, next_tmp);
    }

    (body, next_tmp)
}

fn prepend_drop(name: &str, body: CompExpr, next_tmp: usize) -> (CompExpr, usize) {
    (
        CompExpr::Seq {
            stmts: vec![CompExpr::Drop {
                value: Box::new(ValueExpr::Var(name.to_string())),
            }],
            tail: Box::new(body),
        },
        next_tmp,
    )
}

fn append_drop_preserving_result(name: &str, body: CompExpr, next_tmp: usize) -> (CompExpr, usize) {
    // Insert Drop(name) at every tail position in `body` so that tail-calls
    // (which never return to the current frame) still execute the drop.
    insert_drop_at_tail(name, body, next_tmp)
}

/// Insert `Drop(name)` immediately before every "terminal" expression in `body`.
///
/// This ensures the drop runs on every execution path including tail-calls
/// (Wasm `return_call`), which skip the caller frame entirely.
///
/// Rules:
/// - `Value(_)` — prepend Drop.
/// - `Call { .. }` where `name` not in args/callee — prepend Drop.
/// - `Call { .. }` where `name` appears only in args and the callee can lower
///   to `TailDeclCall` — emit `Dup; Drop` before the call so direct tail calls
///   stay in tail position.
/// - `Call { .. }` where `name` appears in callee — wrap:
///   `let tmp = call in Drop; tmp`.
/// - `PerformEffect` — same as Call.
/// - `Let { name: n, .. }` — if `n == name`, stop (shadowed); if `value`
///   projects from `name` and `n` is later live, drop after the whole Let;
///   otherwise recurse into body.
/// - `LetMut` — same as Let.
/// - `Seq { tail, .. }` — recurse into tail.
/// - `If` / `Case` — recurse into every branch.
/// - Other statements — prepend Drop (conservative).
fn insert_drop_at_tail(name: &str, body: CompExpr, next_tmp: usize) -> (CompExpr, usize) {
    let mut next_tmp = next_tmp;
    let recurse = |c, t: &mut usize| {
        let (out, nt) = insert_drop_at_tail(name, c, *t);
        *t = nt;
        out
    };
    let prepend = |c: CompExpr| CompExpr::Seq {
        stmts: vec![CompExpr::Drop {
            value: Box::new(ValueExpr::Var(name.to_string())),
        }],
        tail: Box::new(c),
    };
    let wrap_after_expr = |c: CompExpr, t: &mut usize| {
        let tmp = format!("__perceus_drop_tmp_{}", *t);
        *t += 1;
        CompExpr::Let {
            name: tmp.clone(),
            ty: crate::ir::IrType::Unknown,
            value: Box::new(c),
            body: Box::new(CompExpr::Seq {
                stmts: vec![CompExpr::Drop {
                    value: Box::new(ValueExpr::Var(name.to_string())),
                }],
                tail: Box::new(CompExpr::Value(ValueExpr::Var(tmp))),
            }),
        }
    };
    let result = match body {
        CompExpr::Value(_) => prepend(body),
        CompExpr::Call {
            ref callee,
            ref args,
            ..
        } => {
            let name_in_callee = value_mentions_name(callee, name);
            let name_in_args = args.iter().any(|a| value_mentions_name(a, name));
            if name_in_callee {
                // C3 is not preserved because indirect tail calls are not
                // implemented; this is intentional per PLAN_PERCEUS §4.100
                // Step 2.
                wrap_after_expr(body, &mut next_tmp)
            } else if name_in_args && callee_can_lower_to_tail_decl_call(callee) {
                CompExpr::Seq {
                    stmts: vec![
                        CompExpr::Dup {
                            value: Box::new(ValueExpr::Var(name.to_string())),
                        },
                        CompExpr::Drop {
                            value: Box::new(ValueExpr::Var(name.to_string())),
                        },
                    ],
                    tail: Box::new(body),
                }
            } else if name_in_args {
                wrap_after_expr(body, &mut next_tmp)
            } else {
                prepend(body)
            }
        }
        CompExpr::PerformEffect { ref args, .. } => {
            let name_in_args = args.iter().any(|a| value_mentions_name(a, name));
            if name_in_args {
                wrap_after_expr(body, &mut next_tmp)
            } else {
                prepend(body)
            }
        }
        CompExpr::Let {
            name: n,
            ty,
            value,
            body,
        } => {
            if n == name {
                CompExpr::Let {
                    name: n,
                    ty,
                    value,
                    body,
                }
            } else if comp_projects_from_name(&value, name) && count_var_uses(&body, &n) > 0 {
                wrap_after_expr(
                    CompExpr::Let {
                        name: n,
                        ty,
                        value,
                        body,
                    },
                    &mut next_tmp,
                )
            } else {
                let new_body = recurse(*body, &mut next_tmp);
                CompExpr::Let {
                    name: n,
                    ty,
                    value,
                    body: Box::new(new_body),
                }
            }
        }
        CompExpr::LetMut {
            name: n,
            ty,
            value,
            body,
        } => {
            if n == name {
                CompExpr::LetMut {
                    name: n,
                    ty,
                    value,
                    body,
                }
            } else {
                let new_body = recurse(*body, &mut next_tmp);
                CompExpr::LetMut {
                    name: n,
                    ty,
                    value,
                    body: Box::new(new_body),
                }
            }
        }
        CompExpr::Seq { stmts, tail } => {
            let new_tail = recurse(*tail, &mut next_tmp);
            CompExpr::Seq {
                stmts,
                tail: Box::new(new_tail),
            }
        }
        CompExpr::If { cond, then_, else_ } => {
            let new_then = recurse(*then_, &mut next_tmp);
            let new_else = recurse(*else_, &mut next_tmp);
            CompExpr::If {
                cond,
                then_: Box::new(new_then),
                else_: Box::new(new_else),
            }
        }
        CompExpr::Case { scrutinee, arms } => {
            let new_arms = arms
                .into_iter()
                .map(|arm| IrCaseArm {
                    pattern: arm.pattern,
                    body: recurse(arm.body, &mut next_tmp),
                })
                .collect();
            CompExpr::Case {
                scrutinee,
                arms: new_arms,
            }
        }
        CompExpr::Drop { .. }
        | CompExpr::DropReuse { .. }
        | CompExpr::Dup { .. }
        | CompExpr::Assign { .. }
        | CompExpr::AssignIndex { .. }
        | CompExpr::AllocReuse { .. }
        | CompExpr::Resume { .. }
        | CompExpr::Handle { .. }
        | CompExpr::WithHandler { .. } => prepend(body),
    };
    (result, next_tmp)
}

fn callee_can_lower_to_tail_decl_call(callee: &ValueExpr) -> bool {
    match callee {
        ValueExpr::Var(name) => !name.starts_with("__goby_"),
        ValueExpr::GlobalRef { .. }
        | ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::ListLit { .. }
        | ValueExpr::TupleLit(_)
        | ValueExpr::RecordLit { .. }
        | ValueExpr::Lambda { .. }
        | ValueExpr::Interp(_)
        | ValueExpr::BinOp { .. }
        | ValueExpr::Unit
        | ValueExpr::TupleProject { .. }
        | ValueExpr::ListGet { .. } => false,
    }
}

fn comp_projects_from_name(comp: &CompExpr, name: &str) -> bool {
    match comp {
        CompExpr::Value(value) => value_projects_from_name(value, name),
        _ => false,
    }
}

fn value_projects_from_name(value: &ValueExpr, name: &str) -> bool {
    match value {
        ValueExpr::ListGet { list, .. } => value_mentions_name(list, name),
        _ => false,
    }
}

fn balance_if_branch_drops(
    mut then_out: CompExpr,
    mut else_out: CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
    branch_visible: &HashSet<String>,
    next_tmp: usize,
) -> (CompExpr, CompExpr, usize) {
    let mut next_tmp = next_tmp;
    for name in owned_binding_names(ownership).into_iter().rev() {
        if !branch_visible.contains(name) {
            continue;
        }
        let then_consumes = comp_consumes_name(&then_out, name, module_ownership, param_order);
        let else_consumes = comp_consumes_name(&else_out, name, module_ownership, param_order);
        match (then_consumes, else_consumes) {
            (true, false) => {
                let (balanced, new_next_tmp) = insert_owned_binding_drop(
                    name,
                    else_out,
                    module_ownership,
                    param_order,
                    next_tmp,
                );
                else_out = balanced;
                next_tmp = new_next_tmp;
            }
            (false, true) => {
                let (balanced, new_next_tmp) = insert_owned_binding_drop(
                    name,
                    then_out,
                    module_ownership,
                    param_order,
                    next_tmp,
                );
                then_out = balanced;
                next_tmp = new_next_tmp;
            }
            _ => {}
        }
    }

    (then_out, else_out, next_tmp)
}

fn balance_case_branch_drops(
    mut arms: Vec<IrCaseArm>,
    ownership: &HashMap<String, OwnershipClass>,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
    branch_visible: &HashSet<String>,
    next_tmp: usize,
) -> (Vec<IrCaseArm>, usize) {
    let mut next_tmp = next_tmp;
    for name in owned_binding_names(ownership).into_iter().rev() {
        if !branch_visible.contains(name) {
            continue;
        }
        let arm_consumes: Vec<bool> = arms
            .iter()
            .map(|arm| comp_consumes_name(&arm.body, name, module_ownership, param_order))
            .collect();
        if !arm_consumes.iter().any(|consumes| *consumes) {
            continue;
        }
        if arm_consumes.iter().all(|consumes| *consumes) {
            continue;
        }

        for (arm, consumes) in arms.iter_mut().zip(arm_consumes) {
            if consumes
                || collect_pattern_bindings(&arm.pattern)
                    .iter()
                    .any(|bound| bound == name)
            {
                continue;
            }
            let body = std::mem::replace(&mut arm.body, CompExpr::Value(ValueExpr::Unit));
            let (balanced, new_next_tmp) =
                insert_owned_binding_drop(name, body, module_ownership, param_order, next_tmp);
            arm.body = balanced;
            next_tmp = new_next_tmp;
        }
    }

    (arms, next_tmp)
}

fn insert_non_last_call_arg_dups(
    call: CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
) -> CompExpr {
    let CompExpr::Call {
        callee,
        args,
        reuse_token,
    } = call
    else {
        return call;
    };

    let mut consuming_counts: HashMap<String, usize> = HashMap::new();
    for (idx, arg) in args.iter().enumerate() {
        if callee_arg_is_borrowed(&callee, idx, module_ownership, param_order) {
            continue;
        }
        collect_owned_var_mentions(arg, ownership, &mut consuming_counts);
    }

    let mut dups: Vec<CompExpr> = consuming_counts
        .into_iter()
        .flat_map(|(name, count)| {
            (1..count).map(move |_| CompExpr::Dup {
                value: Box::new(ValueExpr::Var(name.clone())),
            })
        })
        .collect();

    if dups.is_empty() {
        return CompExpr::Call {
            callee,
            args,
            reuse_token,
        };
    }

    dups.sort_by(|a, b| {
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

    CompExpr::Seq {
        stmts: dups,
        tail: Box::new(CompExpr::Call {
            callee,
            args,
            reuse_token,
        }),
    }
}

fn insert_live_across_let_value_dups(
    value: CompExpr,
    body: &CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
    module_ownership: &HashMap<String, HashMap<String, OwnershipClass>>,
    param_order: &HashMap<String, Vec<String>>,
) -> CompExpr {
    let body_live = collect_live_vars(body);
    let mut dups: Vec<CompExpr> = owned_binding_names(ownership)
        .into_iter()
        .filter(|name| body_live.contains(*name))
        .filter(|name| comp_consumes_name(&value, name, module_ownership, param_order))
        .map(|name| CompExpr::Dup {
            value: Box::new(ValueExpr::Var(name.to_string())),
        })
        .collect();

    if dups.is_empty() {
        return value;
    }

    dups.sort_by(|a, b| {
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

    CompExpr::Seq {
        stmts: dups,
        tail: Box::new(value),
    }
}

fn collect_owned_var_mentions(
    value: &ValueExpr,
    ownership: &HashMap<String, OwnershipClass>,
    out: &mut HashMap<String, usize>,
) {
    match value {
        ValueExpr::Var(name) if ownership.get(name) == Some(&OwnershipClass::Owned) => {
            *out.entry(name.clone()).or_insert(0) += 1;
        }
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                collect_owned_var_mentions(element, ownership, out);
            }
            if let Some(spread) = spread {
                collect_owned_var_mentions(spread, ownership, out);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                collect_owned_var_mentions(item, ownership, out);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                collect_owned_var_mentions(field, ownership, out);
            }
        }
        ValueExpr::Lambda { .. } => {}
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    collect_owned_var_mentions(value, ownership, out);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            collect_owned_var_mentions(left, ownership, out);
            collect_owned_var_mentions(right, ownership, out);
        }
        ValueExpr::TupleProject { tuple, .. } => {
            collect_owned_var_mentions(tuple, ownership, out);
        }
        ValueExpr::ListGet { list, index } => {
            collect_owned_var_mentions(list, ownership, out);
            collect_owned_var_mentions(index, ownership, out);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
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
        return prepend_drop(name, body, next_tmp);
    }

    if !comp_consumes_name(&body, name, module_ownership, param_order) {
        return append_drop_preserving_result(name, body, next_tmp);
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
        CompExpr::Call { callee, args, .. } => args.iter().enumerate().any(|(idx, arg)| {
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
        CompExpr::Dup { .. } | CompExpr::Resume { .. } | CompExpr::AllocReuse { .. } => false,
        CompExpr::Drop { value } | CompExpr::DropReuse { value, .. } => {
            value_mentions_name(value, name)
        }
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
    if intrinsic_arg_is_borrowed(callee, idx) {
        return true;
    }
    let decl_name = match callee {
        ValueExpr::GlobalRef { name, .. } | ValueExpr::Var(name) => name,
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
            IrInterpPart::Expr(value) => value_mentions_name(value, name),
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
        CompExpr::Call { callee, args, .. } => {
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
        CompExpr::Dup { value }
        | CompExpr::Drop { value }
        | CompExpr::DropReuse { value, .. }
        | CompExpr::Resume { value } => collect_live_value(value, live),
        CompExpr::AllocReuse { .. } => {}
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
        CompExpr::Call { callee, args, .. } => {
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
        CompExpr::Dup { value }
        | CompExpr::Drop { value }
        | CompExpr::DropReuse { value, .. }
        | CompExpr::Resume { value } => count_var_uses_value(value, name),
        CompExpr::AllocReuse { .. } => 0,
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
    use super::{
        OwnershipClass, assert_perceus_pipeline_order, classify_decl_return_ownership,
        insert_drop_at_tail, ownership_classify_module, run_perceus_passes,
    };
    use crate::ir::{
        CompExpr, IrCaseArm, IrCasePattern, IrDecl, IrModule, IrType, ValueExpr, fmt_ir,
    };

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
                reuse_param: None,
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
                reuse_param: None,
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
                    reuse_param: None,
                    body: CompExpr::Value(ValueExpr::Var("xs".to_string())),
                },
                IrDecl {
                    name: "entry".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
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
                            reuse_token: None,
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
                    reuse_param: None,
                    body: CompExpr::Value(ValueExpr::Var("p".to_string())),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
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
                                reuse_token: None,
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
                    reuse_param: None,
                    body: CompExpr::Value(ValueExpr::Var("p".to_string())),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
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
                                reuse_token: None,
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
                reuse_param: None,
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
                reuse_param: None,
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
                            reuse_token: None,
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
                reuse_param: None,
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
                reuse_param: None,
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
                            reuse_token: None,
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
                reuse_param: None,
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
                    reuse_param: None,
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
                    reuse_param: None,
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
                            reuse_token: None,
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
    fn tail_drop_for_name_in_call_arg_preserves_tail_call_shape() {
        let call = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("walk".to_string())),
            args: vec![ValueExpr::Var("xs".to_string()), ValueExpr::IntLit(1)],
            reuse_token: None,
        };

        let (rewritten, _) = insert_drop_at_tail("xs", call, 0);
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                reuse_param: None,
                body: rewritten,
            }],
        };
        let ir_str = fmt_ir(&module);
        let dup_pos = ir_str.find("dup xs").expect("expected Dup before call");
        let drop_pos = ir_str.find("drop xs").expect("expected Drop before call");
        let call_pos = ir_str.find("call walk(xs, 1)").expect("expected tail call");
        assert!(
            dup_pos < drop_pos && drop_pos < call_pos,
            "name-in-arg tail drop must emit Dup; Drop before the call:\n{ir_str}"
        );
        assert!(
            !ir_str.contains("__perceus_drop_tmp_"),
            "name-in-arg tail drop must not wrap the call in a temp:\n{ir_str}"
        );
    }

    #[test]
    fn tail_drop_for_name_in_callee_keeps_conservative_temp_wrap() {
        let call = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("f".to_string())),
            args: vec![ValueExpr::IntLit(1)],
            reuse_token: None,
        };

        let (rewritten, _) = insert_drop_at_tail("f", call, 0);
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                reuse_param: None,
                body: rewritten,
            }],
        };
        let ir_str = fmt_ir(&module);
        assert!(
            ir_str.contains("__perceus_drop_tmp_0"),
            "name-in-callee tail drop should keep the conservative temp wrap:\n{ir_str}"
        );
        assert!(
            ir_str.contains("drop f"),
            "name-in-callee temp wrap should still drop the callee binding:\n{ir_str}"
        );
    }

    #[test]
    fn tail_drop_for_intrinsic_arg_keeps_temp_wrap() {
        let call = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("__goby_list_each".to_string())),
            args: vec![
                ValueExpr::Var("xs".to_string()),
                ValueExpr::Var("println".to_string()),
            ],
            reuse_token: None,
        };

        let (rewritten, _) = insert_drop_at_tail("xs", call, 0);
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                reuse_param: None,
                body: rewritten,
            }],
        };
        let ir_str = fmt_ir(&module);
        assert!(
            ir_str.contains("__perceus_drop_tmp_0"),
            "intrinsic calls must keep the post-call drop temp wrap:\n{ir_str}"
        );
        assert!(
            !ir_str.contains("dup xs"),
            "intrinsic calls do not lower to TailDeclCall and must not use the C2 Dup; Drop shape:\n{ir_str}"
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
                    reuse_param: None,
                    body: CompExpr::Value(ValueExpr::Var("xs".to_string())),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
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
                            reuse_token: None,
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
                reuse_param: None,
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
                        reuse_token: None,
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
    fn parameter_returned_through_let_alias_is_owned() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "id_alias".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Let {
                        name: "ys".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                        body: Box::new(CompExpr::Value(ValueExpr::Var("ys".to_string()))),
                    },
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Let {
                        name: "zs".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        })),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "id_alias".to_string(),
                            }),
                            args: vec![ValueExpr::Var("zs".to_string())],
                            reuse_token: None,
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop zs"),
            "alias-returning callee owns its param, so caller must transfer zs; got:\n{ir_str}"
        );
    }

    #[test]
    fn parameter_consumed_through_let_alias_is_owned() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "sink_alias".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Let {
                        name: "ys".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "external".to_string(),
                            }),
                            args: vec![ValueExpr::Var("ys".to_string())],
                            reuse_token: None,
                        }),
                    },
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Let {
                        name: "zs".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        })),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "sink_alias".to_string(),
                            }),
                            args: vec![ValueExpr::Var("zs".to_string())],
                            reuse_token: None,
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop zs"),
            "alias-consuming callee owns its param, so caller must transfer zs; got:\n{ir_str}"
        );
    }

    #[test]
    fn if_arm_without_consuming_use_drops_owned_parameter() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "id".to_string(),
                    params: vec![("p".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Value(ValueExpr::Var("p".to_string())),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![
                        ("xs".to_string(), IrType::Unknown),
                        ("flag".to_string(), IrType::Bool),
                    ],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::If {
                        cond: Box::new(ValueExpr::Var("flag".to_string())),
                        then_: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "id".to_string(),
                            }),
                            args: vec![ValueExpr::Var("xs".to_string())],
                            reuse_token: None,
                        }),
                        else_: Box::new(CompExpr::Value(ValueExpr::IntLit(0))),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            ir_str.contains("drop xs"),
            "dead If arm should drop owned xs when the other arm consumes it; got:\n{ir_str}"
        );
    }

    #[test]
    fn if_list_get_arm_keeps_parameter_borrowed_without_balancing_drop() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "id".to_string(),
                    params: vec![("p".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Value(ValueExpr::Var("p".to_string())),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![
                        ("xs".to_string(), IrType::Unknown),
                        ("flag".to_string(), IrType::Bool),
                    ],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::If {
                        cond: Box::new(ValueExpr::Var("flag".to_string())),
                        then_: Box::new(CompExpr::Value(ValueExpr::ListGet {
                            list: Box::new(ValueExpr::Var("xs".to_string())),
                            index: Box::new(ValueExpr::IntLit(0)),
                        })),
                        else_: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "id".to_string(),
                            }),
                            args: vec![ValueExpr::Var("xs".to_string())],
                            reuse_token: None,
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop xs"),
            "ListGet evidence should keep xs borrowed and avoid early branch Drop; got:\n{ir_str}"
        );
    }

    #[test]
    fn projection_borrow_delays_parent_drop_until_child_last_use() {
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: vec![],
                result_ty: IrType::Unit,
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::Let {
                    name: "rolls".to_string(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Call {
                        callee: Box::new(ValueExpr::GlobalRef {
                            module: "".to_string(),
                            name: "__goby_list_map".to_string(),
                        }),
                        args: vec![
                            ValueExpr::ListLit {
                                elements: vec![ValueExpr::ListLit {
                                    elements: vec![ValueExpr::IntLit(1)],
                                    spread: None,
                                }],
                                spread: None,
                            },
                            ValueExpr::Lambda {
                                param: "n".to_string(),
                                body: Box::new(CompExpr::Value(ValueExpr::ListLit {
                                    elements: vec![ValueExpr::Var("n".to_string())],
                                    spread: None,
                                })),
                            },
                        ],
                        reuse_token: None,
                    }),
                    body: Box::new(CompExpr::Let {
                        name: "row2".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListGet {
                            list: Box::new(ValueExpr::Var("rolls".to_string())),
                            index: Box::new(ValueExpr::IntLit(0)),
                        })),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "__goby_list_each".to_string(),
                            }),
                            args: vec![
                                ValueExpr::Var("row2".to_string()),
                                ValueExpr::GlobalRef {
                                    module: "".to_string(),
                                    name: "println".to_string(),
                                },
                            ],
                            reuse_token: None,
                        }),
                    }),
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        let list_get_pos = ir_str
            .find("list.get rolls 0")
            .expect("rewritten IR should still bind row2 from rolls");
        let each_pos = ir_str
            .find("__goby_list_each")
            .expect("rewritten IR should still call list.each intrinsic");
        let drop_pos = ir_str
            .rfind("drop rolls")
            .expect("owned map result should eventually drop rolls");
        assert!(
            list_get_pos < each_pos && each_pos < drop_pos,
            "parent rolls must be dropped after projected child row2 is used:\n{ir_str}"
        );
    }

    #[test]
    fn repeated_owned_call_arg_gets_dup_before_call() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "pair".to_string(),
                    params: vec![
                        ("left".to_string(), IrType::Unknown),
                        ("right".to_string(), IrType::Unknown),
                    ],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Value(ValueExpr::TupleLit(vec![
                        ValueExpr::Var("left".to_string()),
                        ValueExpr::Var("right".to_string()),
                    ])),
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Let {
                        name: "xs".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        })),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "pair".to_string(),
                            }),
                            args: vec![
                                ValueExpr::Var("xs".to_string()),
                                ValueExpr::Var("xs".to_string()),
                            ],
                            reuse_token: None,
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            ir_str.contains("dup xs"),
            "non-last consuming call arg should dup xs before the call; got:\n{ir_str}"
        );
    }

    #[test]
    fn recursive_scc_owned_parameter_demotes_across_call_edges() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "left".to_string(),
                    params: vec![
                        ("xs".to_string(), IrType::Unknown),
                        ("flag".to_string(), IrType::Bool),
                    ],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::If {
                        cond: Box::new(ValueExpr::Var("flag".to_string())),
                        then_: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "right".to_string(),
                            }),
                            args: vec![
                                ValueExpr::Var("xs".to_string()),
                                ValueExpr::Var("flag".to_string()),
                            ],
                            reuse_token: None,
                        }),
                        else_: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                    },
                },
                IrDecl {
                    name: "right".to_string(),
                    params: vec![
                        ("ys".to_string(), IrType::Unknown),
                        ("flag".to_string(), IrType::Bool),
                    ],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Call {
                        callee: Box::new(ValueExpr::GlobalRef {
                            module: "".to_string(),
                            name: "left".to_string(),
                        }),
                        args: vec![
                            ValueExpr::Var("ys".to_string()),
                            ValueExpr::Var("flag".to_string()),
                        ],
                        reuse_token: None,
                    },
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Let {
                        name: "zs".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        })),
                        body: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "right".to_string(),
                            }),
                            args: vec![ValueExpr::Var("zs".to_string()), ValueExpr::BoolLit(true)],
                            reuse_token: None,
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("drop zs"),
            "SCC propagation should make right.ys owned, so main transfers zs; got:\n{ir_str}"
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
                reuse_param: None,
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

    /// M6 Step 7-a benchmark shape: a recursive `step xs i n` whose body is
    /// `if n==0 { xs } else { mut ys = xs; ys[i] := v; step ys (i+1) (n-1) }`.
    /// After the perceus pipeline, the AssignIndex on `ys` must carry a
    /// `@reuse(...)` annotation (Owned-param seed propagating from `xs`).
    #[test]
    fn owned_param_seed_fires_on_step_like_recursion() {
        let module = IrModule {
            decls: vec![IrDecl {
                name: "step".to_string(),
                params: vec![
                    ("xs".to_string(), IrType::Unknown),
                    ("i".to_string(), IrType::Int),
                    ("n".to_string(), IrType::Int),
                ],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::If {
                    cond: Box::new(ValueExpr::BinOp {
                        op: crate::ir::IrBinOp::Eq,
                        left: Box::new(ValueExpr::Var("n".to_string())),
                        right: Box::new(ValueExpr::IntLit(0)),
                    }),
                    then_: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                    else_: Box::new(CompExpr::LetMut {
                        name: "ys".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                        body: Box::new(CompExpr::Seq {
                            stmts: vec![CompExpr::AssignIndex {
                                root: "ys".to_string(),
                                path: vec![ValueExpr::Var("i".to_string())],
                                value: Box::new(CompExpr::Value(ValueExpr::IntLit(7))),
                                reuse_token: None,
                            }],
                            tail: Box::new(CompExpr::Call {
                                callee: Box::new(ValueExpr::GlobalRef {
                                    module: "".to_string(),
                                    name: "step".to_string(),
                                }),
                                args: vec![
                                    ValueExpr::Var("ys".to_string()),
                                    ValueExpr::BinOp {
                                        op: crate::ir::IrBinOp::Add,
                                        left: Box::new(ValueExpr::Var("i".to_string())),
                                        right: Box::new(ValueExpr::IntLit(1)),
                                    },
                                    ValueExpr::BinOp {
                                        op: crate::ir::IrBinOp::Sub,
                                        left: Box::new(ValueExpr::Var("n".to_string())),
                                        right: Box::new(ValueExpr::IntLit(1)),
                                    },
                                ],
                                reuse_token: None,
                            }),
                        }),
                    }),
                },
            }],
        };
        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            ir_str.contains("@reuse("),
            "Owned-param `mut ys = xs` must seed AssignIndex reuse:\n{ir_str}"
        );
    }

    #[test]
    fn list_length_wrapper_keeps_step_param_unique_for_assignindex_reuse() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "length".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Int,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Call {
                        callee: Box::new(ValueExpr::GlobalRef {
                            module: "".to_string(),
                            name: "__goby_list_length".to_string(),
                        }),
                        args: vec![ValueExpr::Var("xs".to_string())],
                        reuse_token: None,
                    },
                },
                IrDecl {
                    name: "step".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::If {
                        cond: Box::new(ValueExpr::BoolLit(false)),
                        then_: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                        else_: Box::new(CompExpr::Let {
                            name: "n".to_string(),
                            ty: IrType::Int,
                            value: Box::new(CompExpr::Call {
                                callee: Box::new(ValueExpr::GlobalRef {
                                    module: "".to_string(),
                                    name: "length".to_string(),
                                }),
                                args: vec![ValueExpr::Var("xs".to_string())],
                                reuse_token: None,
                            }),
                            body: Box::new(CompExpr::LetMut {
                                name: "ys".to_string(),
                                ty: IrType::Unknown,
                                value: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                                body: Box::new(CompExpr::Seq {
                                    stmts: vec![CompExpr::AssignIndex {
                                        root: "ys".to_string(),
                                        path: vec![ValueExpr::Var("n".to_string())],
                                        value: Box::new(CompExpr::Value(ValueExpr::IntLit(7))),
                                        reuse_token: None,
                                    }],
                                    tail: Box::new(CompExpr::Value(ValueExpr::Var(
                                        "ys".to_string(),
                                    ))),
                                }),
                            }),
                        }),
                    },
                },
            ],
        };
        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("dup xs"),
            "borrowed length wrapper must not increase xs refcount before reuse:\n{ir_str}"
        );
        assert!(
            ir_str.contains("@reuse("),
            "length borrow should preserve owned-param AssignIndex reuse:\n{ir_str}"
        );
    }

    #[test]
    fn mut_helper_param_seed_survives_prior_borrow_in_driver() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "count".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Int,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Value(ValueExpr::ListGet {
                        list: Box::new(ValueExpr::Var("xs".to_string())),
                        index: Box::new(ValueExpr::IntLit(0)),
                    }),
                },
                IrDecl {
                    name: "update_xs".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::LetMut {
                        name: "ys".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                        body: Box::new(CompExpr::Seq {
                            stmts: vec![CompExpr::AssignIndex {
                                root: "ys".to_string(),
                                path: vec![ValueExpr::IntLit(0)],
                                value: Box::new(CompExpr::Value(ValueExpr::IntLit(7))),
                                reuse_token: None,
                            }],
                            tail: Box::new(CompExpr::Value(ValueExpr::Var("ys".to_string()))),
                        }),
                    },
                },
                IrDecl {
                    name: "drive".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
                    reuse_param: None,
                    body: CompExpr::Let {
                        name: "c".to_string(),
                        ty: IrType::Int,
                        value: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "count".to_string(),
                            }),
                            args: vec![ValueExpr::Var("xs".to_string())],
                            reuse_token: None,
                        }),
                        body: Box::new(CompExpr::Let {
                            name: "new_xs".to_string(),
                            ty: IrType::Unknown,
                            value: Box::new(CompExpr::Call {
                                callee: Box::new(ValueExpr::GlobalRef {
                                    module: "".to_string(),
                                    name: "update_xs".to_string(),
                                }),
                                args: vec![ValueExpr::Var("xs".to_string())],
                                reuse_token: None,
                            }),
                            body: Box::new(CompExpr::Value(ValueExpr::Var("new_xs".to_string()))),
                        }),
                    },
                },
            ],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            ir_str.contains("@reuse("),
            "mutating helper must seed AssignIndex reuse even when caller borrowed first:\n{ir_str}"
        );
        assert!(
            !ir_str.contains("dup xs"),
            "prior borrowed call must not force a Dup before last-use update_xs(xs):\n{ir_str}"
        );
    }

    #[test]
    fn let_mut_source_used_after_seed_does_not_authorize_reuse() {
        let module = IrModule {
            decls: vec![IrDecl {
                name: "bad".to_string(),
                params: vec![("xs".to_string(), IrType::Unknown)],
                result_ty: IrType::Unknown,
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::LetMut {
                    name: "ys".to_string(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                    body: Box::new(CompExpr::Seq {
                        stmts: vec![CompExpr::AssignIndex {
                            root: "ys".to_string(),
                            path: vec![ValueExpr::IntLit(0)],
                            value: Box::new(CompExpr::Value(ValueExpr::IntLit(7))),
                            reuse_token: None,
                        }],
                        tail: Box::new(CompExpr::Value(ValueExpr::ListGet {
                            list: Box::new(ValueExpr::Var("xs".to_string())),
                            index: Box::new(ValueExpr::IntLit(0)),
                        })),
                    }),
                },
            }],
        };

        let rewritten = run_perceus_passes(&module);
        let ir_str = fmt_ir(&rewritten);
        assert!(
            !ir_str.contains("@reuse("),
            "source binding is still live, so mut ys = xs must not seed reuse:\n{ir_str}"
        );
    }

    // -------------------------------------------------------------------
    // M10 §4.100 Step 1 — `classify_decl_return_ownership` soundness gate
    //
    // The shapes mirror the spec block in PLAN_PERCEUS §4.100 Step 1
    // "Soundness gate". Each test names *why* the expected class is what
    // it is, so future readers can see the rule under examination.
    // -------------------------------------------------------------------

    fn list_int_ty() -> IrType {
        IrType::Opaque("List".to_string())
    }

    fn return_ownership_for(decl_name: &str, module: &IrModule) -> OwnershipClass {
        let (params, _decl_returns) = ownership_classify_module(module);
        let returns = classify_decl_return_ownership(module, &params);
        *returns.get(decl_name).expect("decl must be classified")
    }

    #[test]
    fn return_ownership_passthrough_with_owned_param_is_owned() {
        // (1) passthrough xs = xs
        //     M4.5 marks `xs: Owned` (no demote evidence in the body — `xs`
        //     is simply returned, not borrowed by a callee). The pass picks
        //     `Owned` from Γ, so the call site treats the return as a
        //     transferred reference. This is sound: the arg `xs` is moved
        //     into the call, and the return is the same heap pointer with
        //     ownership now back on the caller — the caller drops it once.
        //     The §4.99 DI-1 "passthrough double-frees" worry was a mis-
        //     reading of M4.5: there is no separate caller-side drop on
        //     the moved arg.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "passthrough".to_string(),
                params: vec![("xs".to_string(), list_int_ty())],
                result_ty: list_int_ty(),
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::Value(ValueExpr::Var("xs".to_string())),
            }],
        };
        assert_eq!(
            return_ownership_for("passthrough", &module),
            OwnershipClass::Owned
        );
    }

    #[test]
    fn return_ownership_branch_with_owned_param_in_both_arms_is_owned() {
        // (2) maybe_pass xs flag = if flag then xs else [0, ..xs]
        //     M4.5 marks `xs: Owned` (the else-branch spread `[0, ..xs]`
        //     consumes it, but on the then-branch the same Var(xs) is
        //     simply returned; both arms produce an Owned reference).
        //     Result: Owned. The Borrowed-collapse shape is exercised
        //     by the mutual-recursion test below where M4.5 itself
        //     already returns Borrowed for the param.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "maybe_pass".to_string(),
                params: vec![
                    ("xs".to_string(), list_int_ty()),
                    ("flag".to_string(), IrType::Bool),
                ],
                result_ty: list_int_ty(),
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::If {
                    cond: Box::new(ValueExpr::Var("flag".to_string())),
                    then_: Box::new(CompExpr::Value(ValueExpr::Var("xs".to_string()))),
                    else_: Box::new(CompExpr::Value(ValueExpr::ListLit {
                        elements: vec![ValueExpr::IntLit(0)],
                        spread: Some(Box::new(ValueExpr::Var("xs".to_string()))),
                    })),
                },
            }],
        };
        assert_eq!(
            return_ownership_for("maybe_pass", &module),
            OwnershipClass::Owned
        );
    }

    #[test]
    fn return_ownership_fresh_list_lit_is_owned() {
        // (3) fresh _ = [1, 2, 3] — pure ListLit (no spread) is fresh-heap.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "fresh".to_string(),
                params: vec![("_".to_string(), IrType::Unit)],
                result_ty: list_int_ty(),
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::Value(ValueExpr::ListLit {
                    elements: vec![
                        ValueExpr::IntLit(1),
                        ValueExpr::IntLit(2),
                        ValueExpr::IntLit(3),
                    ],
                    spread: None,
                }),
            }],
        };
        assert_eq!(
            return_ownership_for("fresh", &module),
            OwnershipClass::Owned
        );
    }

    #[test]
    fn return_ownership_tuple_with_owned_payload_is_owned() {
        // (4) fresh_tuple _ = ([1], 0)
        //     At least one item Owned ⇒ tuple Owned. The §M9 3c rule lets
        //     downstream projection extract the heap part.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "fresh_tuple".to_string(),
                params: vec![("_".to_string(), IrType::Unit)],
                result_ty: IrType::Opaque("Tuple".to_string()),
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::ListLit {
                        elements: vec![ValueExpr::IntLit(1)],
                        spread: None,
                    },
                    ValueExpr::IntLit(0),
                ])),
            }],
        };
        assert_eq!(
            return_ownership_for("fresh_tuple", &module),
            OwnershipClass::Owned
        );
    }

    #[test]
    fn return_ownership_scalar_only_tuple_is_borrowed() {
        // (4-neg) Codex review supplemental: a tuple with no Owned items
        //         must NOT classify as Owned. Otherwise we would emit a
        //         spurious Drop on a scalar pair.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "scalar_tuple".to_string(),
                params: vec![("_".to_string(), IrType::Unit)],
                result_ty: IrType::Opaque("Tuple".to_string()),
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(1),
                    ValueExpr::IntLit(2),
                ])),
            }],
        };
        assert_eq!(
            return_ownership_for("scalar_tuple", &module),
            OwnershipClass::Borrowed
        );
    }

    #[test]
    fn return_ownership_recursive_build_with_owned_acc_is_owned() {
        // (5) build n acc = if n == 0 then acc else build (n-1) [n, ..acc]
        //     M4.5 marks `acc` Owned (consumed by spread in recursive call).
        //     Base case Var(acc) under env[acc=Owned] ⇒ Owned. Recursive
        //     branch is a Call to `build` itself — fix-point resolves it
        //     to Owned in iteration 2.
        use crate::ir::IrBinOp;

        let module = IrModule {
            decls: vec![IrDecl {
                name: "build".to_string(),
                params: vec![
                    ("n".to_string(), IrType::Int),
                    ("acc".to_string(), list_int_ty()),
                ],
                result_ty: list_int_ty(),
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::If {
                    cond: Box::new(ValueExpr::BinOp {
                        op: IrBinOp::Eq,
                        left: Box::new(ValueExpr::Var("n".to_string())),
                        right: Box::new(ValueExpr::IntLit(0)),
                    }),
                    then_: Box::new(CompExpr::Value(ValueExpr::Var("acc".to_string()))),
                    else_: Box::new(CompExpr::Call {
                        callee: Box::new(ValueExpr::Var("build".to_string())),
                        args: vec![
                            ValueExpr::BinOp {
                                op: IrBinOp::Sub,
                                left: Box::new(ValueExpr::Var("n".to_string())),
                                right: Box::new(ValueExpr::IntLit(1)),
                            },
                            ValueExpr::ListLit {
                                elements: vec![ValueExpr::Var("n".to_string())],
                                spread: Some(Box::new(ValueExpr::Var("acc".to_string()))),
                            },
                        ],
                        reuse_token: None,
                    }),
                },
            }],
        };
        assert_eq!(
            return_ownership_for("build", &module),
            OwnershipClass::Owned
        );
    }

    #[test]
    fn return_ownership_fresh_base_recursion_is_owned() {
        // (6) mk n = if n == 0 then [] else [n, ..mk (n-1)]
        //     Both arms produce ListLit (fresh). The spread element is a
        //     recursive call, but the *return* of `mk` is the literal,
        //     not the call. Owned.
        use crate::ir::IrBinOp;

        let module = IrModule {
            decls: vec![IrDecl {
                name: "mk".to_string(),
                params: vec![("n".to_string(), IrType::Int)],
                result_ty: list_int_ty(),
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::If {
                    cond: Box::new(ValueExpr::BinOp {
                        op: IrBinOp::Eq,
                        left: Box::new(ValueExpr::Var("n".to_string())),
                        right: Box::new(ValueExpr::IntLit(0)),
                    }),
                    then_: Box::new(CompExpr::Value(ValueExpr::ListLit {
                        elements: vec![],
                        spread: None,
                    })),
                    else_: Box::new(CompExpr::Let {
                        name: "tail".to_string(),
                        ty: list_int_ty(),
                        value: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::Var("mk".to_string())),
                            args: vec![ValueExpr::BinOp {
                                op: IrBinOp::Sub,
                                left: Box::new(ValueExpr::Var("n".to_string())),
                                right: Box::new(ValueExpr::IntLit(1)),
                            }],
                            reuse_token: None,
                        }),
                        body: Box::new(CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::Var("n".to_string())],
                            spread: Some(Box::new(ValueExpr::Var("tail".to_string()))),
                        })),
                    }),
                },
            }],
        };
        assert_eq!(return_ownership_for("mk", &module), OwnershipClass::Owned);
    }

    #[test]
    fn return_ownership_mutual_recursion_without_base_case_remains_owned() {
        // (7) left xs = right xs ; right xs = left xs
        //
        // Greatest-fixed-point note: a no-base-case mutual cycle is
        // optimistically classified `Owned` because no branch ever
        // structurally returns a Borrowed reference (every tail position
        // is a sibling self-call, whose return is `Owned` by the GFP
        // assumption). At runtime such a cycle never returns, so the
        // classification has no observable safety consequence — drops
        // are never emitted on a value that does not exist.
        //
        // The Borrowed-collapse path is exercised by
        // `return_ownership_call_to_borrowed_returning_decl_is_borrowed`
        // below, where a callee whose body *structurally* returns a
        // Borrowed value forces the cycle to descend.
        let left = IrDecl {
            name: "left".to_string(),
            params: vec![("xs".to_string(), list_int_ty())],
            result_ty: list_int_ty(),
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Call {
                callee: Box::new(ValueExpr::Var("right".to_string())),
                args: vec![ValueExpr::Var("xs".to_string())],
                reuse_token: None,
            },
        };
        let right = IrDecl {
            name: "right".to_string(),
            params: vec![("xs".to_string(), list_int_ty())],
            result_ty: list_int_ty(),
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Call {
                callee: Box::new(ValueExpr::Var("left".to_string())),
                args: vec![ValueExpr::Var("xs".to_string())],
                reuse_token: None,
            },
        };
        let module = IrModule {
            decls: vec![left, right],
        };
        assert_eq!(return_ownership_for("left", &module), OwnershipClass::Owned);
        assert_eq!(
            return_ownership_for("right", &module),
            OwnershipClass::Owned
        );
    }

    #[test]
    fn return_ownership_call_to_borrowed_returning_decl_is_borrowed() {
        // Borrowed-confirmation path: a sibling decl whose body returns a
        // structurally Borrowed value (here `ListGet`, which only ever
        // produces a borrow into a list element) forces every caller in
        // its SCC to descend `Owned → Borrowed` in the gfp loop.
        //
        //   borrowed_helper xs = list_get xs 0     -- Borrowed shape
        //   uses_helper xs = borrowed_helper xs    -- Borrowed (collapsed)
        let helper = IrDecl {
            name: "borrowed_helper".to_string(),
            params: vec![("xs".to_string(), list_int_ty())],
            result_ty: IrType::Int,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Value(ValueExpr::ListGet {
                list: Box::new(ValueExpr::Var("xs".to_string())),
                index: Box::new(ValueExpr::IntLit(0)),
            }),
        };
        let user = IrDecl {
            name: "uses_helper".to_string(),
            params: vec![("xs".to_string(), list_int_ty())],
            result_ty: IrType::Int,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Call {
                callee: Box::new(ValueExpr::Var("borrowed_helper".to_string())),
                args: vec![ValueExpr::Var("xs".to_string())],
                reuse_token: None,
            },
        };
        let module = IrModule {
            decls: vec![helper, user],
        };
        assert_eq!(
            return_ownership_for("borrowed_helper", &module),
            OwnershipClass::Borrowed
        );
        assert_eq!(
            return_ownership_for("uses_helper", &module),
            OwnershipClass::Borrowed
        );
    }

    #[test]
    fn return_ownership_lambda_value_is_owned() {
        // Supplemental: a decl whose body is a Lambda value classifies as
        // Owned (a fresh closure object). Documents the explicit Lambda
        // arm in `return_ownership_value`.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "make_adder".to_string(),
                params: vec![("n".to_string(), IrType::Int)],
                result_ty: IrType::Opaque("Closure".to_string()),
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::Value(ValueExpr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(CompExpr::Value(ValueExpr::IntLit(0))),
                }),
            }],
        };
        assert_eq!(
            return_ownership_for("make_adder", &module),
            OwnershipClass::Owned
        );
    }

    #[test]
    fn return_ownership_call_via_shadowed_var_does_not_consult_module_decl() {
        // Codex pass-1 finding: a `Var(name)` callee that *shadows* a
        // module decl name with a local binding (param / let / case
        // binder) is a closure or indirect call. It must NOT pick up the
        // module decl's return ownership, otherwise an `Owned`-returning
        // decl would lend its Owned-ness to an unrelated indirect call.
        //
        //   build n acc = ...                          -- Owned-returning decl
        //   shadow build = build ()                    -- `build` here is a
        //                                                  param shadowing
        //                                                  the module decl.
        //                                                  Result must be
        //                                                  Borrowed.
        let build = IrDecl {
            name: "build".to_string(),
            params: vec![("acc".to_string(), list_int_ty())],
            result_ty: list_int_ty(),
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Value(ValueExpr::Var("acc".to_string())),
        };
        let shadow = IrDecl {
            name: "shadow".to_string(),
            // `build` is a *parameter* here — shadows the module decl.
            params: vec![("build".to_string(), IrType::Opaque("Closure".to_string()))],
            result_ty: list_int_ty(),
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Call {
                callee: Box::new(ValueExpr::Var("build".to_string())),
                args: vec![ValueExpr::Unit],
                reuse_token: None,
            },
        };
        let module = IrModule {
            decls: vec![build, shadow],
        };
        // `build` (the module decl) classifies Owned.
        assert_eq!(
            return_ownership_for("build", &module),
            OwnershipClass::Owned
        );
        // `shadow`'s body is `Call { callee: Var("build") }` but `build`
        // is the local param, not the module decl. Indirect calls
        // classify Borrowed.
        assert_eq!(
            return_ownership_for("shadow", &module),
            OwnershipClass::Borrowed
        );
    }

    #[test]
    fn return_ownership_global_ref_value_is_borrowed() {
        // Codex pass-1 finding: returning a `GlobalRef` *value* (a first-
        // class function reference) must classify Borrowed, regardless of
        // whether the decl's body would otherwise classify as Owned. The
        // value here is a function pointer / closure handle, not the
        // result of calling that decl.
        let build = IrDecl {
            name: "build".to_string(),
            params: vec![("acc".to_string(), list_int_ty())],
            result_ty: list_int_ty(),
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Value(ValueExpr::Var("acc".to_string())),
        };
        let pick_build = IrDecl {
            name: "pick_build".to_string(),
            params: vec![("_".to_string(), IrType::Unit)],
            result_ty: IrType::Opaque("Closure".to_string()),
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Value(ValueExpr::GlobalRef {
                module: "self".to_string(),
                name: "build".to_string(),
            }),
        };
        let module = IrModule {
            decls: vec![build, pick_build],
        };
        assert_eq!(
            return_ownership_for("build", &module),
            OwnershipClass::Owned
        );
        assert_eq!(
            return_ownership_for("pick_build", &module),
            OwnershipClass::Borrowed
        );
    }

    #[test]
    fn return_ownership_tuple_project_is_borrowed_conservatively() {
        // Codex pass-1 finding: projecting a field from an Owned tuple
        // does not make the projection itself Owned at this layer (we
        // have no per-field type information). The §M9 3c rule widens
        // projections at the *use site*, not in this analysis.
        //
        //   project_first _ = ([1, 2, 3], 0).0   -- conservatively Borrowed
        //
        // Even though the inner tuple is Owned (TupleLit with a fresh
        // ListLit payload), the projection result must classify Borrowed
        // here.
        let module = IrModule {
            decls: vec![IrDecl {
                name: "project_first".to_string(),
                params: vec![("_".to_string(), IrType::Unit)],
                result_ty: list_int_ty(),
                residual_effects: vec![],
                reuse_param: None,
                body: CompExpr::Value(ValueExpr::TupleProject {
                    tuple: Box::new(ValueExpr::TupleLit(vec![
                        ValueExpr::ListLit {
                            elements: vec![
                                ValueExpr::IntLit(1),
                                ValueExpr::IntLit(2),
                                ValueExpr::IntLit(3),
                            ],
                            spread: None,
                        },
                        ValueExpr::IntLit(0),
                    ])),
                    index: 0,
                }),
            }],
        };
        assert_eq!(
            return_ownership_for("project_first", &module),
            OwnershipClass::Borrowed
        );
    }

    #[test]
    fn return_ownership_case_with_borrowed_arm_collapses() {
        // Case-arm Borrowed collapse: one arm returns a structurally
        // Borrowed value (`ListGet`, which always produces a borrow), the
        // other a fresh `[1]`. Any-Borrowed-arm collapses the case.
        //
        //   wrap xs = case xs of
        //     [] => list.get xs 0   -- Borrowed (returns Int but heap-shape
        //                              is irrelevant for the rule; the
        //                              Borrowed-classification happens
        //                              regardless of result type)
        //     _  => [1]
        let wrap = IrDecl {
            name: "wrap".to_string(),
            params: vec![("xs".to_string(), list_int_ty())],
            result_ty: list_int_ty(),
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Case {
                scrutinee: Box::new(ValueExpr::Var("xs".to_string())),
                arms: vec![
                    IrCaseArm {
                        pattern: IrCasePattern::EmptyList,
                        body: CompExpr::Value(ValueExpr::ListGet {
                            list: Box::new(ValueExpr::Var("xs".to_string())),
                            index: Box::new(ValueExpr::IntLit(0)),
                        }),
                    },
                    IrCaseArm {
                        pattern: IrCasePattern::Wildcard,
                        body: CompExpr::Value(ValueExpr::ListLit {
                            elements: vec![ValueExpr::IntLit(1)],
                            spread: None,
                        }),
                    },
                ],
            },
        };
        let module = IrModule { decls: vec![wrap] };
        assert_eq!(
            return_ownership_for("wrap", &module),
            OwnershipClass::Borrowed
        );
    }
}
