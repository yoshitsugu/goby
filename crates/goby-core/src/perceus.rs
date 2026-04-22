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
        CompExpr::Call { callee, args } => {
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
    aliases: &mut HashMap<String, String>,
) {
    match comp {
        CompExpr::Value(value) => classify_return_value(value, classes, params, aliases),
        CompExpr::Let {
            name, value, body, ..
        } => {
            classify_bound_comp(value, classes, params, module_params, aliases);
            classes.insert(name.clone(), classify_owned_result(value, classes, aliases));
            let previous_alias = bind_alias(name, value, params, aliases);
            classify_comp(body, classes, params, module_params, aliases);
            restore_alias(name, previous_alias, aliases);
        }
        CompExpr::LetMut {
            name, value, body, ..
        } => {
            classify_bound_comp(value, classes, params, module_params, aliases);
            classes.insert(name.clone(), OwnershipClass::Borrowed);
            let previous_alias = aliases.remove(name);
            classify_comp(body, classes, params, module_params, aliases);
            restore_alias(name, previous_alias, aliases);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                classify_comp(stmt, classes, params, module_params, aliases);
            }
            classify_comp(tail, classes, params, module_params, aliases);
        }
        CompExpr::If { cond, then_, else_ } => {
            classify_borrowed_value(cond, classes, params, aliases);
            classify_comp(then_, classes, params, module_params, aliases);
            classify_comp(else_, classes, params, module_params, aliases);
        }
        CompExpr::Call { callee, args } => {
            classify_borrowed_value(callee, classes, params, aliases);
            classify_call_args(callee, args, classes, params, module_params, aliases);
        }
        CompExpr::Assign { value, .. } => {
            classify_bound_comp(value, classes, params, module_params, aliases)
        }
        CompExpr::AssignIndex { path, value, .. } => {
            for index in path {
                classify_borrowed_value(index, classes, params, aliases);
            }
            classify_bound_comp(value, classes, params, module_params, aliases);
        }
        CompExpr::Case { scrutinee, arms } => {
            classify_borrowed_value(scrutinee, classes, params, aliases);
            for arm in arms {
                // Pattern-bound variables own the sub-parts of the scrutinee.
                classify_case_pattern_bindings(&arm.pattern, classes);
                let shadowed_aliases =
                    remove_aliases(&collect_pattern_bindings(&arm.pattern), aliases);
                classify_comp(&arm.body, classes, params, module_params, aliases);
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
                classify_comp(&clause.body, classes, params, module_params, aliases);
                restore_aliases(shadowed_aliases, aliases);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            classify_comp(handler, classes, params, module_params, aliases);
            classify_comp(body, classes, params, module_params, aliases);
        }
    }
}

fn classify_bound_comp(
    comp: &CompExpr,
    classes: &mut HashMap<String, OwnershipClass>,
    params: &HashSet<String>,
    module_params: &HashMap<String, Vec<OwnershipClass>>,
    aliases: &mut HashMap<String, String>,
) {
    match comp {
        CompExpr::Value(value) => classify_bound_value(value, classes, params, aliases),
        _ => classify_comp(comp, classes, params, module_params, aliases),
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
        if callee_params
            .and_then(|params| params.get(idx))
            .is_some_and(|class| *class == OwnershipClass::Borrowed)
        {
            classify_borrowed_value(arg, classes, params, aliases);
        } else {
            classify_consumed_value(arg, classes, params, aliases);
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
            classify_borrowed_value(tuple, classes, params, aliases)
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
        CompExpr::Call { callee, args } => {
            let call = CompExpr::Call {
                callee: callee.clone(),
                args: args.clone(),
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
    let tmp = format!("__perceus_drop_tmp_{next_tmp}");
    (
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
    )
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
    let CompExpr::Call { callee, args } = call else {
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
        return CompExpr::Call { callee, args };
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
        tail: Box::new(CompExpr::Call { callee, args }),
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
    fn parameter_returned_through_let_alias_is_owned() {
        let module = IrModule {
            decls: vec![
                IrDecl {
                    name: "id_alias".to_string(),
                    params: vec![("xs".to_string(), IrType::Unknown)],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
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
                        }),
                    },
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
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
                    body: CompExpr::If {
                        cond: Box::new(ValueExpr::Var("flag".to_string())),
                        then_: Box::new(CompExpr::Call {
                            callee: Box::new(ValueExpr::GlobalRef {
                                module: "".to_string(),
                                name: "id".to_string(),
                            }),
                            args: vec![ValueExpr::Var("xs".to_string())],
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
                    body: CompExpr::Call {
                        callee: Box::new(ValueExpr::GlobalRef {
                            module: "".to_string(),
                            name: "left".to_string(),
                        }),
                        args: vec![
                            ValueExpr::Var("ys".to_string()),
                            ValueExpr::Var("flag".to_string()),
                        ],
                    },
                },
                IrDecl {
                    name: "main".to_string(),
                    params: vec![],
                    result_ty: IrType::Unknown,
                    residual_effects: vec![],
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
