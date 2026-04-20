use std::collections::HashMap;

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
    FreshHeap,
    NonOwning,
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
    module
        .decls
        .iter()
        .map(|decl| (decl.name.clone(), ownership_classify_decl(decl)))
        .collect()
}

fn ownership_classify_decl(decl: &IrDecl) -> HashMap<String, OwnershipClass> {
    let mut classes = HashMap::new();
    classify_comp(&decl.body, &mut classes);
    classes
}

fn classify_comp(comp: &CompExpr, classes: &mut HashMap<String, OwnershipClass>) {
    match comp {
        CompExpr::Value(value) => classify_value(value, classes),
        CompExpr::Let {
            name, value, body, ..
        } => {
            classify_comp(value, classes);
            classes.insert(name.clone(), classify_owned_result(value));
            classify_comp(body, classes);
        }
        CompExpr::LetMut {
            name, value, body, ..
        } => {
            classify_comp(value, classes);
            classes.insert(name.clone(), OwnershipClass::NonOwning);
            classify_comp(body, classes);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                classify_comp(stmt, classes);
            }
            classify_comp(tail, classes);
        }
        CompExpr::If { cond, then_, else_ } => {
            classify_value(cond, classes);
            classify_comp(then_, classes);
            classify_comp(else_, classes);
        }
        CompExpr::Call { callee, args } => {
            classify_value(callee, classes);
            for arg in args {
                classify_value(arg, classes);
            }
        }
        CompExpr::Assign { value, .. } => classify_comp(value, classes),
        CompExpr::AssignIndex { path, value, .. } => {
            for index in path {
                classify_value(index, classes);
            }
            classify_comp(value, classes);
        }
        CompExpr::Case { scrutinee, arms } => {
            classify_value(scrutinee, classes);
            for arm in arms {
                classify_comp(&arm.body, classes);
            }
        }
        CompExpr::Dup { value } | CompExpr::Drop { value } | CompExpr::Resume { value } => {
            classify_value(value, classes)
        }
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                classify_value(arg, classes);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                classify_comp(&clause.body, classes);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            classify_comp(handler, classes);
            classify_comp(body, classes);
        }
    }
}

fn classify_value(value: &ValueExpr, classes: &mut HashMap<String, OwnershipClass>) {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                classify_value(element, classes);
            }
            if let Some(spread) = spread {
                classify_value(spread, classes);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                classify_value(item, classes);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                classify_value(field, classes);
            }
        }
        ValueExpr::Lambda { body, .. } => classify_comp(body, classes),
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    classify_value(value, classes);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            classify_value(left, classes);
            classify_value(right, classes);
        }
        ValueExpr::TupleProject { tuple, .. } => classify_value(tuple, classes),
        ValueExpr::ListGet { list, index } => {
            classify_value(list, classes);
            classify_value(index, classes);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn classify_owned_result(comp: &CompExpr) -> OwnershipClass {
    match comp {
        CompExpr::Value(value) if value_is_fresh_heap(value) => OwnershipClass::FreshHeap,
        _ => OwnershipClass::NonOwning,
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
    IrModule {
        decls: module
            .decls
            .iter()
            .map(|decl| IrDecl {
                name: decl.name.clone(),
                params: decl.params.clone(),
                result_ty: decl.result_ty.clone(),
                residual_effects: decl.residual_effects.clone(),
                body: drop_insert_comp(
                    &decl.body,
                    ownership
                        .get(&decl.name)
                        .expect("ownership facts must exist"),
                    0,
                )
                .0,
            })
            .collect(),
    }
}

fn drop_insert_comp(
    comp: &CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
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
            let (value, next_tmp) = drop_insert_comp(value, ownership, next_tmp);
            let (body, next_tmp) = drop_insert_comp(body, ownership, next_tmp);
            let body = insert_drop_after_body(name, body, ownership, next_tmp);
            (
                CompExpr::Let {
                    name: name.clone(),
                    ty: ty.clone(),
                    value: Box::new(value),
                    body: Box::new(body.0),
                },
                body.1,
            )
        }
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => {
            let (value, next_tmp) = drop_insert_comp(value, ownership, next_tmp);
            let (body, next_tmp) = drop_insert_comp(body, ownership, next_tmp);
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
                    let (stmt, new_next_tmp) = drop_insert_comp(stmt, ownership, next_tmp_mut);
                    next_tmp_mut = new_next_tmp;
                    stmt
                })
                .collect();
            let (tail, next_tmp_mut) = drop_insert_comp(tail, ownership, next_tmp_mut);
            (
                CompExpr::Seq {
                    stmts,
                    tail: Box::new(tail),
                },
                next_tmp_mut,
            )
        }
        CompExpr::If { cond, then_, else_ } => {
            let (then_, next_tmp) = drop_insert_comp(then_, ownership, next_tmp);
            let (else_, next_tmp) = drop_insert_comp(else_, ownership, next_tmp);
            (
                CompExpr::If {
                    cond: cond.clone(),
                    then_: Box::new(then_),
                    else_: Box::new(else_),
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
            let (value, next_tmp) = drop_insert_comp(value, ownership, next_tmp);
            (
                CompExpr::Assign {
                    name: name.clone(),
                    value: Box::new(value),
                },
                next_tmp,
            )
        }
        CompExpr::AssignIndex { root, path, value } => {
            let (value, next_tmp) = drop_insert_comp(value, ownership, next_tmp);
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
            let mut next_tmp_mut = next_tmp;
            let arms = arms
                .iter()
                .map(|arm| {
                    let (body, new_next_tmp) = drop_insert_comp(&arm.body, ownership, next_tmp_mut);
                    next_tmp_mut = new_next_tmp;
                    IrCaseArm {
                        pattern: arm.pattern.clone(),
                        body,
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
                    let (body, new_next_tmp) =
                        drop_insert_comp(&clause.body, ownership, next_tmp_mut);
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
            let (handler, next_tmp) = drop_insert_comp(handler, ownership, next_tmp);
            let (body, next_tmp) = drop_insert_comp(body, ownership, next_tmp);
            (
                CompExpr::WithHandler {
                    handler: Box::new(handler),
                    body: Box::new(body),
                },
                next_tmp,
            )
        }
        CompExpr::Resume { value } => (
            CompExpr::Resume {
                value: value.clone(),
            },
            next_tmp,
        ),
    }
}

fn insert_drop_after_body(
    name: &str,
    body: CompExpr,
    ownership: &HashMap<String, OwnershipClass>,
    next_tmp: usize,
) -> (CompExpr, usize) {
    if ownership.get(name) != Some(&OwnershipClass::FreshHeap)
        || comp_result_may_reference_name(&body, name)
    {
        return (body, next_tmp);
    }

    let tmp_name = format!("__goby_perceus_ret_{next_tmp}");
    (
        CompExpr::Let {
            name: tmp_name.clone(),
            ty: infer_result_type(&body),
            value: Box::new(body),
            body: Box::new(CompExpr::Seq {
                stmts: vec![CompExpr::Drop {
                    value: Box::new(ValueExpr::Var(name.to_string())),
                }],
                tail: Box::new(CompExpr::Value(ValueExpr::Var(tmp_name))),
            }),
        },
        next_tmp + 1,
    )
}

fn infer_result_type(comp: &CompExpr) -> crate::ir::IrType {
    match comp {
        CompExpr::Assign { .. }
        | CompExpr::AssignIndex { .. }
        | CompExpr::Dup { .. }
        | CompExpr::Drop { .. } => crate::ir::IrType::Unit,
        _ => crate::ir::IrType::Unknown,
    }
}

fn comp_result_may_reference_name(comp: &CompExpr, target: &str) -> bool {
    match comp {
        CompExpr::Value(value) => value_mentions_name(value, target),
        CompExpr::Let { name, body, .. } | CompExpr::LetMut { name, body, .. } => {
            if name == target {
                false
            } else {
                comp_result_may_reference_name(body, target)
            }
        }
        CompExpr::Seq { tail, .. } => comp_result_may_reference_name(tail, target),
        CompExpr::If { cond, then_, else_ } => {
            value_mentions_name(cond, target)
                || comp_result_may_reference_name(then_, target)
                || comp_result_may_reference_name(else_, target)
        }
        CompExpr::Call { callee, args } => {
            value_mentions_name(callee, target)
                || args.iter().any(|arg| value_mentions_name(arg, target))
        }
        CompExpr::Assign { .. }
        | CompExpr::AssignIndex { .. }
        | CompExpr::Dup { .. }
        | CompExpr::Drop { .. } => false,
        CompExpr::Case { scrutinee, arms } => {
            value_mentions_name(scrutinee, target)
                || arms
                    .iter()
                    .any(|arm| comp_result_may_reference_name(&arm.body, target))
        }
        CompExpr::PerformEffect { args, .. } => {
            args.iter().any(|arg| value_mentions_name(arg, target))
        }
        CompExpr::Handle { clauses } => clauses
            .iter()
            .any(|clause| comp_result_may_reference_name(&clause.body, target)),
        CompExpr::WithHandler { handler, body } => {
            comp_result_may_reference_name(handler, target)
                || comp_result_may_reference_name(body, target)
        }
        CompExpr::Resume { value } => value_mentions_name(value, target),
    }
}

fn value_mentions_name(value: &ValueExpr, target: &str) -> bool {
    match value {
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => false,
        ValueExpr::Var(name) => name == target,
        ValueExpr::ListLit { elements, spread } => {
            elements
                .iter()
                .any(|element| value_mentions_name(element, target))
                || spread
                    .as_deref()
                    .is_some_and(|spread| value_mentions_name(spread, target))
        }
        ValueExpr::TupleLit(items) => items.iter().any(|item| value_mentions_name(item, target)),
        ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, field)| value_mentions_name(field, target)),
        ValueExpr::Lambda { param, body } => {
            if param == target {
                false
            } else {
                comp_result_may_reference_name(body, target)
            }
        }
        ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            IrInterpPart::Text(_) => false,
            IrInterpPart::Expr(value) => value_mentions_name(value, target),
        }),
        ValueExpr::BinOp { left, right, .. } => {
            value_mentions_name(left, target) || value_mentions_name(right, target)
        }
        ValueExpr::TupleProject { tuple, .. } => value_mentions_name(tuple, target),
        ValueExpr::ListGet { list, index } => {
            value_mentions_name(list, target) || value_mentions_name(index, target)
        }
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
            "decl main: ? =\n  let xs: ? =\n    [1]\n  in\n    let __goby_perceus_ret_0: ? =\n      42\n    in\n      seq\n        drop xs\n      =>\n        __goby_perceus_ret_0\n\n"
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
}
