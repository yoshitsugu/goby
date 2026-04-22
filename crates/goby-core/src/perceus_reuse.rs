use std::collections::HashMap;

use crate::ir::{AllocInit, CompExpr, IrDecl, IrType, ValueExpr};
use crate::size_class::SizeClass;

type SizeEnv = HashMap<String, SizeClass>;

pub fn insert_reuse(decl: IrDecl) -> IrDecl {
    IrDecl {
        name: decl.name,
        params: decl.params,
        result_ty: decl.result_ty,
        residual_effects: decl.residual_effects,
        body: insert_reuse_comp(decl.body, &SizeEnv::new(), 0).0,
    }
}

fn insert_reuse_comp(comp: CompExpr, sizes: &SizeEnv, next_token: usize) -> (CompExpr, usize) {
    match comp {
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => {
            let bound_size = comp_alloc_class(&value);
            let (value, next_token) = insert_reuse_comp(*value, sizes, next_token);
            let mut body_sizes = sizes.clone();
            if let Some(class) = bound_size {
                body_sizes.insert(name.clone(), class);
            } else {
                body_sizes.remove(&name);
            }
            let (body, next_token) = insert_reuse_comp(*body, &body_sizes, next_token);
            (
                CompExpr::Let {
                    name,
                    ty,
                    value: Box::new(value),
                    body: Box::new(body),
                },
                next_token,
            )
        }
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => {
            let (value, next_token) = insert_reuse_comp(*value, sizes, next_token);
            let mut body_sizes = sizes.clone();
            body_sizes.remove(&name);
            let (body, next_token) = insert_reuse_comp(*body, &body_sizes, next_token);
            (
                CompExpr::LetMut {
                    name,
                    ty,
                    value: Box::new(value),
                    body: Box::new(body),
                },
                next_token,
            )
        }
        CompExpr::Seq { stmts, tail } => insert_reuse_seq(stmts, *tail, sizes, next_token),
        CompExpr::If { cond, then_, else_ } => {
            let (then_, next_token) = insert_reuse_comp(*then_, sizes, next_token);
            let (else_, next_token) = insert_reuse_comp(*else_, sizes, next_token);
            (
                CompExpr::If {
                    cond,
                    then_: Box::new(then_),
                    else_: Box::new(else_),
                },
                next_token,
            )
        }
        CompExpr::Assign { name, value } => {
            let (value, next_token) = insert_reuse_comp(*value, sizes, next_token);
            (
                CompExpr::Assign {
                    name,
                    value: Box::new(value),
                },
                next_token,
            )
        }
        CompExpr::AssignIndex { root, path, value } => {
            let (value, next_token) = insert_reuse_comp(*value, sizes, next_token);
            (
                CompExpr::AssignIndex {
                    root,
                    path,
                    value: Box::new(value),
                },
                next_token,
            )
        }
        CompExpr::Case { scrutinee, arms } => {
            let mut next = next_token;
            let arms = arms
                .into_iter()
                .map(|arm| {
                    let (body, updated) = insert_reuse_comp(arm.body, sizes, next);
                    next = updated;
                    crate::ir::IrCaseArm {
                        pattern: arm.pattern,
                        body,
                    }
                })
                .collect();
            (CompExpr::Case { scrutinee, arms }, next)
        }
        CompExpr::Handle { clauses } => {
            let mut next = next_token;
            let clauses = clauses
                .into_iter()
                .map(|clause| {
                    let (body, updated) = insert_reuse_comp(clause.body, sizes, next);
                    next = updated;
                    crate::ir::IrHandlerClause {
                        op_name: clause.op_name,
                        params: clause.params,
                        body,
                    }
                })
                .collect();
            (CompExpr::Handle { clauses }, next)
        }
        CompExpr::WithHandler { handler, body } => {
            let (handler, next_token) = insert_reuse_comp(*handler, sizes, next_token);
            let (body, next_token) = insert_reuse_comp(*body, sizes, next_token);
            (
                CompExpr::WithHandler {
                    handler: Box::new(handler),
                    body: Box::new(body),
                },
                next_token,
            )
        }
        CompExpr::Value(_)
        | CompExpr::Call { .. }
        | CompExpr::Dup { .. }
        | CompExpr::Drop { .. }
        | CompExpr::DropReuse { .. }
        | CompExpr::AllocReuse { .. }
        | CompExpr::PerformEffect { .. }
        | CompExpr::Resume { .. } => (comp, next_token),
    }
}

fn insert_reuse_seq(
    stmts: Vec<CompExpr>,
    tail: CompExpr,
    sizes: &SizeEnv,
    next_token: usize,
) -> (CompExpr, usize) {
    let mut next = next_token;
    let mut rewritten = Vec::with_capacity(stmts.len());
    let mut pending_drop: Option<(ValueExpr, SizeClass, String)> = None;

    for stmt in stmts {
        match stmt {
            CompExpr::Drop { value } if pending_drop.is_none() => {
                let Some(class) = drop_value_class(&value, sizes) else {
                    rewritten.push(CompExpr::Drop { value });
                    continue;
                };
                let token = format!("__perceus_reuse_token_{next}");
                next += 1;
                pending_drop = Some((*value, class, token));
            }
            other => {
                if let Some((value, _, token)) = pending_drop.take() {
                    let _ = token;
                    rewritten.push(CompExpr::Drop {
                        value: Box::new(value),
                    });
                }
                let (other, updated) = insert_reuse_comp(other, sizes, next);
                next = updated;
                rewritten.push(other);
            }
        }
    }

    let (tail, next) = if let Some((value, drop_class, token)) = pending_drop.take() {
        if let Some((alloc_class, init, tail)) = rewrite_first_alloc(tail.clone(), &token)
            && drop_class == alloc_class
        {
            rewritten.push(CompExpr::DropReuse {
                value: Box::new(value),
                bind: token,
            });
            (tail_with_alloc_reuse(tail, alloc_class, init), next)
        } else {
            rewritten.push(CompExpr::Drop {
                value: Box::new(value),
            });
            insert_reuse_comp(tail, sizes, next)
        }
    } else {
        insert_reuse_comp(tail, sizes, next)
    };

    (
        CompExpr::Seq {
            stmts: rewritten,
            tail: Box::new(tail),
        },
        next,
    )
}

fn comp_alloc_class(comp: &CompExpr) -> Option<SizeClass> {
    let CompExpr::Value(value) = comp else {
        return None;
    };
    allocation_class(value)
}

fn drop_value_class(value: &ValueExpr, sizes: &SizeEnv) -> Option<SizeClass> {
    match value {
        ValueExpr::Var(name) => sizes.get(name).copied(),
        _ => allocation_class(value),
    }
}

fn allocation_class(value: &ValueExpr) -> Option<SizeClass> {
    allocation_init(value.clone()).map(|(class, _)| class)
}

fn rewrite_first_alloc(
    comp: CompExpr,
    token: &str,
) -> Option<(SizeClass, AllocInit, PendingAllocTail)> {
    match comp {
        CompExpr::Value(value) => allocation_init(value)
            .map(|(class, init)| (class, init, PendingAllocTail::Value(token.to_string()))),
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => {
            let CompExpr::Value(value) = *value else {
                return None;
            };
            allocation_init(value).map(|(class, init)| {
                (
                    class,
                    init,
                    PendingAllocTail::Let {
                        token: token.to_string(),
                        name,
                        ty,
                        body,
                    },
                )
            })
        }
        _ => None,
    }
}

enum PendingAllocTail {
    Value(String),
    Let {
        token: String,
        name: String,
        ty: IrType,
        body: Box<CompExpr>,
    },
}

fn tail_with_alloc_reuse(
    tail: PendingAllocTail,
    size_class: SizeClass,
    init: AllocInit,
) -> CompExpr {
    match tail {
        PendingAllocTail::Value(token) => CompExpr::AllocReuse {
            token,
            size_class,
            init,
        },
        PendingAllocTail::Let {
            token,
            name,
            ty,
            body,
        } => CompExpr::Let {
            name,
            ty,
            value: Box::new(CompExpr::AllocReuse {
                token,
                size_class,
                init,
            }),
            body,
        },
    }
}

fn allocation_init(value: ValueExpr) -> Option<(SizeClass, AllocInit)> {
    match value {
        ValueExpr::TupleLit(items) => {
            let class = SizeClass::for_tuple(items.len() as u32);
            class
                .is_reusable()
                .then_some((class, AllocInit::TupleLit(items)))
        }
        ValueExpr::RecordLit {
            constructor,
            fields,
        } => {
            let class = SizeClass::for_record(fields.len() as u32);
            class.is_reusable().then_some((
                class,
                AllocInit::RecordLit {
                    constructor,
                    fields,
                },
            ))
        }
        ValueExpr::ListLit { elements, spread } if spread.is_none() => {
            let class = SizeClass::for_list_header(1);
            class
                .is_reusable()
                .then_some((class, AllocInit::ListLit { elements, spread }))
        }
        ValueExpr::Interp(parts) => Some((SizeClass::String(512), AllocInit::Interp(parts))),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::insert_reuse;
    use crate::ir::{CompExpr, IrDecl, IrModule, IrType, ValueExpr, fmt_ir, validate_ir};

    #[test]
    fn pairs_drop_with_following_tuple_allocation_in_same_seq_tail() {
        let decl = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            body: CompExpr::Let {
                name: "old".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(9),
                    ValueExpr::IntLit(10),
                ]))),
                body: Box::new(CompExpr::Seq {
                    stmts: vec![CompExpr::Drop {
                        value: Box::new(ValueExpr::Var("old".to_string())),
                    }],
                    tail: Box::new(CompExpr::Let {
                        name: "next".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                            ValueExpr::IntLit(1),
                            ValueExpr::IntLit(2),
                        ]))),
                        body: Box::new(CompExpr::Value(ValueExpr::Var("next".to_string()))),
                    }),
                }),
            },
        };

        let rewritten = insert_reuse(decl);
        let module = IrModule {
            decls: vec![rewritten],
        };
        validate_ir(&module).expect("reuse IR should validate");
        assert_eq!(
            fmt_ir(&module),
            "decl main: ? =\n  let old: ? =\n    (9, 10)\n  in\n    seq\n      drop_reuse old as __perceus_reuse_token_0\n    =>\n      let next: ? =\n        alloc_reuse __perceus_reuse_token_0 Tuple(2) = (1, 2)\n      in\n        next\n\n"
        );
    }

    #[test]
    fn does_not_pair_when_size_classes_differ() {
        let decl = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            body: CompExpr::Let {
                name: "old".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(9),
                    ValueExpr::IntLit(10),
                ]))),
                body: Box::new(CompExpr::Seq {
                    stmts: vec![CompExpr::Drop {
                        value: Box::new(ValueExpr::Var("old".to_string())),
                    }],
                    tail: Box::new(CompExpr::Let {
                        name: "next".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                            ValueExpr::IntLit(1),
                        ]))),
                        body: Box::new(CompExpr::Value(ValueExpr::Var("next".to_string()))),
                    }),
                }),
            },
        };

        let rewritten = insert_reuse(decl);
        let module = IrModule {
            decls: vec![rewritten],
        };
        validate_ir(&module).expect("non-reuse IR should validate");
        let ir = fmt_ir(&module);
        assert!(
            ir.contains("drop old"),
            "drop should remain ordinary:\n{ir}"
        );
        assert!(
            !ir.contains("alloc_reuse"),
            "mismatched size classes must not pair:\n{ir}"
        );
    }

    #[test]
    fn does_not_pair_across_effect_boundary_statement() {
        let decl = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            body: CompExpr::Let {
                name: "old".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(1),
                ]))),
                body: Box::new(CompExpr::Seq {
                    stmts: vec![
                        CompExpr::Drop {
                            value: Box::new(ValueExpr::Var("old".to_string())),
                        },
                        CompExpr::PerformEffect {
                            effect: "Print".to_string(),
                            op: "println".to_string(),
                            args: vec![ValueExpr::StrLit("x".to_string())],
                        },
                    ],
                    tail: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                        ValueExpr::IntLit(2),
                    ]))),
                }),
            },
        };

        let rewritten = insert_reuse(decl);
        let module = IrModule {
            decls: vec![rewritten],
        };
        validate_ir(&module).expect("non-reuse IR should validate");
        let ir = fmt_ir(&module);
        assert!(
            ir.contains("drop old"),
            "drop should remain ordinary:\n{ir}"
        );
        assert!(
            !ir.contains("alloc_reuse"),
            "effect boundary must block reuse pairing:\n{ir}"
        );
    }
}
