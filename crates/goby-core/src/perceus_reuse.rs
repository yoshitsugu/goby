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
        reuse_param: decl.reuse_param,
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

// ---------------------------------------------------------------------------
// Tail-call cross-call reuse (Step 8+)
// ---------------------------------------------------------------------------

/// Analyse `decl.body` and return the `SizeClass` of its first reachable
/// allocation, or `None` if the analysis cannot guarantee one.
///
/// Conservative abort triggers: any `If`, `Case`, `PerformEffect`,
/// `WithHandler`, `Resume`, or `Call` found before an alloc.
pub fn first_alloc_class(decl: &IrDecl) -> Option<SizeClass> {
    first_alloc_class_comp(&decl.body)
}

fn first_alloc_class_comp(comp: &CompExpr) -> Option<SizeClass> {
    match comp {
        // Straight allocation — success.
        CompExpr::Value(v) => allocation_class(v),

        // Let / LetMut: check value side only if it is a pure alloc-free expr,
        // otherwise fall through to body.
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            match comp_alloc_class(value) {
                // The let-value itself allocates → that is the first alloc.
                Some(c) => Some(c),
                // The let-value is non-alloc but also non-pure-abort → descend into body.
                None if is_conservative_abort_comp(value) => None,
                None => first_alloc_class_comp(body),
            }
        }

        // Seq: scan stmts for abort signals; if none, check tail.
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                if is_conservative_abort_comp(stmt) {
                    return None;
                }
                // A Dup/Drop/DropReuse stmt is fine to skip.
            }
            first_alloc_class_comp(tail)
        }

        // Branches, effects, calls — abort conservatively.
        CompExpr::If { .. }
        | CompExpr::Case { .. }
        | CompExpr::PerformEffect { .. }
        | CompExpr::WithHandler { .. }
        | CompExpr::Resume { .. }
        | CompExpr::Call { .. } => None,

        // Side-effecting stmts that produce Unit — not allocations.
        CompExpr::Assign { .. }
        | CompExpr::AssignIndex { .. }
        | CompExpr::Dup { .. }
        | CompExpr::Drop { .. }
        | CompExpr::DropReuse { .. } => None,

        CompExpr::AllocReuse { size_class, .. } => Some(*size_class),
        CompExpr::Handle { .. } => None,
    }
}

/// Returns `true` when `comp` should abort the first-alloc analysis.
fn is_conservative_abort_comp(comp: &CompExpr) -> bool {
    matches!(
        comp,
        CompExpr::If { .. }
            | CompExpr::Case { .. }
            | CompExpr::PerformEffect { .. }
            | CompExpr::WithHandler { .. }
            | CompExpr::Resume { .. }
            | CompExpr::Call { .. }
            | CompExpr::Handle { .. }
    )
}

/// Rewrite the first allocation in `decl.body` to `AllocReuse { token }`.
/// Requires that `first_alloc_class(decl)` returns `Some(_)` — panics otherwise.
pub fn rewrite_callee_first_alloc(mut decl: IrDecl, token: &str) -> IrDecl {
    decl.body = rewrite_first_alloc_comp(decl.body, token)
        .expect("rewrite_callee_first_alloc: no alloc found");
    decl
}

fn rewrite_first_alloc_comp(comp: CompExpr, token: &str) -> Option<CompExpr> {
    match comp {
        CompExpr::Value(v) => {
            let (class, init) = allocation_init(v)?;
            Some(CompExpr::AllocReuse {
                token: token.to_string(),
                size_class: class,
                init,
            })
        }
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => {
            if let Some(class) = comp_alloc_class(&value) {
                let init = match *value {
                    CompExpr::Value(v) => allocation_init(v).map(|(_, i)| i)?,
                    _ => return None,
                };
                Some(CompExpr::Let {
                    name,
                    ty,
                    value: Box::new(CompExpr::AllocReuse {
                        token: token.to_string(),
                        size_class: class,
                        init,
                    }),
                    body,
                })
            } else if !is_conservative_abort_comp(&value) {
                let new_body = rewrite_first_alloc_comp(*body, token)?;
                Some(CompExpr::Let {
                    name,
                    ty,
                    value,
                    body: Box::new(new_body),
                })
            } else {
                None
            }
        }
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => {
            if let Some(class) = comp_alloc_class(&value) {
                let init = match *value {
                    CompExpr::Value(v) => allocation_init(v).map(|(_, i)| i)?,
                    _ => return None,
                };
                Some(CompExpr::LetMut {
                    name,
                    ty,
                    value: Box::new(CompExpr::AllocReuse {
                        token: token.to_string(),
                        size_class: class,
                        init,
                    }),
                    body,
                })
            } else if !is_conservative_abort_comp(&value) {
                let new_body = rewrite_first_alloc_comp(*body, token)?;
                Some(CompExpr::LetMut {
                    name,
                    ty,
                    value,
                    body: Box::new(new_body),
                })
            } else {
                None
            }
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in &stmts {
                if is_conservative_abort_comp(stmt) {
                    return None;
                }
            }
            let new_tail = rewrite_first_alloc_comp(*tail, token)?;
            Some(CompExpr::Seq {
                stmts,
                tail: Box::new(new_tail),
            })
        }
        _ => None,
    }
}

/// Apply tail-call cross-call reuse across an entire `IrModule`:
/// 1. Build a map `decl_name → SizeClass` of each decl's first alloc.
/// 2. For each decl, scan for `Seq { stmts: [..., Drop x], tail: Call f }` where
///    `f`'s size class matches `x`'s size class.
/// 3. Rewrite caller: `Drop x` → `DropReuse x as tok`, `Call f` → `Call f[reuse_token=tok]`.
/// 4. Rewrite callee: add `reuse_param=tok` and replace its first alloc with `AllocReuse`.
pub fn insert_tail_reuse_module(
    module: crate::ir::IrModule,
    next_token: &mut usize,
) -> crate::ir::IrModule {
    use crate::ir::IrModule;
    use crate::size_class::SizeClass;
    use std::collections::HashMap;

    // Build callee first-alloc map.
    let first_alloc_map: HashMap<String, SizeClass> = module
        .decls
        .iter()
        .filter_map(|d| first_alloc_class(d).map(|c| (d.name.clone(), c)))
        .collect();

    // Track which callees have been paired so we assign them a reuse_param.
    // Map: callee_name → token_name
    let mut callee_token: HashMap<String, String> = HashMap::new();

    // Rewrite each decl's body (caller side).
    let mut decls: Vec<IrDecl> = module
        .decls
        .into_iter()
        .map(|decl| {
            let body = rewrite_tail_reuse_comp(
                decl.body,
                &SizeEnv::new(),
                &first_alloc_map,
                &mut callee_token,
                next_token,
            );
            IrDecl { body, ..decl }
        })
        .collect();

    // Apply callee-side rewrite for each paired callee.
    for decl in &mut decls {
        if let Some(tok) = callee_token.get(&decl.name) {
            let new_decl = rewrite_callee_first_alloc(decl.clone(), tok);
            decl.body = new_decl.body;
            decl.reuse_param = Some(tok.clone());
        }
    }

    IrModule { decls }
}

fn rewrite_tail_reuse_comp(
    comp: CompExpr,
    sizes: &SizeEnv,
    first_alloc_map: &HashMap<String, SizeClass>,
    callee_token: &mut HashMap<String, String>,
    next_token: &mut usize,
) -> CompExpr {
    match comp {
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => {
            let bound_size = comp_alloc_class(&value);
            let value =
                rewrite_tail_reuse_comp(*value, sizes, first_alloc_map, callee_token, next_token);
            let mut body_sizes = sizes.clone();
            if let Some(class) = bound_size {
                body_sizes.insert(name.clone(), class);
            } else {
                body_sizes.remove(&name);
            }
            let body = rewrite_tail_reuse_comp(
                *body,
                &body_sizes,
                first_alloc_map,
                callee_token,
                next_token,
            );
            CompExpr::Let {
                name,
                ty,
                value: Box::new(value),
                body: Box::new(body),
            }
        }
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => {
            let value =
                rewrite_tail_reuse_comp(*value, sizes, first_alloc_map, callee_token, next_token);
            let mut body_sizes = sizes.clone();
            body_sizes.remove(&name);
            let body = rewrite_tail_reuse_comp(
                *body,
                &body_sizes,
                first_alloc_map,
                callee_token,
                next_token,
            );
            CompExpr::LetMut {
                name,
                ty,
                value: Box::new(value),
                body: Box::new(body),
            }
        }
        CompExpr::If { cond, then_, else_ } => CompExpr::If {
            cond,
            then_: Box::new(rewrite_tail_reuse_comp(
                *then_,
                sizes,
                first_alloc_map,
                callee_token,
                next_token,
            )),
            else_: Box::new(rewrite_tail_reuse_comp(
                *else_,
                sizes,
                first_alloc_map,
                callee_token,
                next_token,
            )),
        },
        CompExpr::Case { scrutinee, arms } => {
            let arms = arms
                .into_iter()
                .map(|arm| crate::ir::IrCaseArm {
                    body: rewrite_tail_reuse_comp(
                        arm.body,
                        sizes,
                        first_alloc_map,
                        callee_token,
                        next_token,
                    ),
                    ..arm
                })
                .collect();
            CompExpr::Case { scrutinee, arms }
        }
        CompExpr::Seq { stmts, tail } => rewrite_tail_reuse_seq(
            stmts,
            *tail,
            sizes,
            first_alloc_map,
            callee_token,
            next_token,
        ),
        CompExpr::WithHandler { handler, body } => CompExpr::WithHandler {
            handler: Box::new(rewrite_tail_reuse_comp(
                *handler,
                sizes,
                first_alloc_map,
                callee_token,
                next_token,
            )),
            body: Box::new(rewrite_tail_reuse_comp(
                *body,
                sizes,
                first_alloc_map,
                callee_token,
                next_token,
            )),
        },
        other => other,
    }
}

fn rewrite_tail_reuse_seq(
    mut stmts: Vec<CompExpr>,
    tail: CompExpr,
    sizes: &SizeEnv,
    first_alloc_map: &HashMap<String, SizeClass>,
    callee_token: &mut HashMap<String, String>,
    next_token: &mut usize,
) -> CompExpr {
    // Check: last stmt is `Drop x` and tail is bare `Call f(...)` with reuse_token: None.
    let callee_name = match &tail {
        CompExpr::Call {
            callee,
            reuse_token: None,
            ..
        } => match callee.as_ref() {
            ValueExpr::Var(n) => Some(n.clone()),
            ValueExpr::GlobalRef { name, .. } => Some(name.clone()),
            _ => None,
        },
        _ => None,
    };

    if let (Some(callee_name), Some(drop_x)) = (
        callee_name,
        stmts.last().and_then(|s| match s {
            CompExpr::Drop { value } => Some(*value.clone()),
            _ => None,
        }),
    ) {
        // Determine size class of the dropped value using the current SizeEnv.
        let drop_class = drop_value_class(&drop_x, sizes);
        // Look up callee's first alloc class.
        let callee_class = first_alloc_map.get(&callee_name).copied();

        if let (Some(dc), Some(cc)) = (drop_class, callee_class) {
            if dc == cc {
                let tok = format!("__perceus_tail_reuse_token_{}", next_token);
                *next_token += 1;

                // Replace last stmt Drop x → DropReuse x as tok.
                *stmts.last_mut().unwrap() = CompExpr::DropReuse {
                    value: Box::new(drop_x),
                    bind: tok.clone(),
                };

                // Rewrite tail Call to add reuse_token.
                let new_tail = match tail {
                    CompExpr::Call { callee, args, .. } => CompExpr::Call {
                        callee,
                        args,
                        reuse_token: Some(tok.clone()),
                    },
                    other => other,
                };

                // Record callee pairing (first caller wins).
                callee_token.entry(callee_name).or_insert(tok);

                return CompExpr::Seq {
                    stmts,
                    tail: Box::new(new_tail),
                };
            }
        }
    }

    // No rewrite — recurse into tail.
    let new_tail = rewrite_tail_reuse_comp(tail, sizes, first_alloc_map, callee_token, next_token);
    CompExpr::Seq {
        stmts,
        tail: Box::new(new_tail),
    }
}

#[cfg(test)]
mod tests {
    use super::{
        first_alloc_class, insert_reuse, insert_tail_reuse_module, rewrite_callee_first_alloc,
    };
    use crate::ir::{CompExpr, IrDecl, IrModule, IrType, ValueExpr, fmt_ir, validate_ir};
    use crate::size_class::SizeClass;

    #[test]
    fn pairs_drop_with_following_tuple_allocation_in_same_seq_tail() {
        let decl = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
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
            reuse_param: None,
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
            reuse_param: None,
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

    /// PLAN_PERCEUS §M5 correctness checklist: reuse must not cross a
    /// `PerformEffect` block terminator. The dropped `old` lives across an
    /// effect call; the following allocation must remain a plain alloc.
    #[test]
    fn reuse_not_across_perform_effect() {
        let decl = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Let {
                name: "old".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(1),
                    ValueExpr::IntLit(2),
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
                    tail: Box::new(CompExpr::Let {
                        name: "next".to_string(),
                        ty: IrType::Unknown,
                        value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                            ValueExpr::IntLit(3),
                            ValueExpr::IntLit(4),
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
            "drop should remain ordinary across PerformEffect:\n{ir}"
        );
        assert!(
            !ir.contains("alloc_reuse"),
            "PerformEffect terminator must block reuse pairing:\n{ir}"
        );
    }

    /// PLAN_PERCEUS §M5 correctness checklist: reuse must not cross a
    /// `WithHandler` boundary. A `Drop` followed by a `WithHandler`-wrapped
    /// allocation must remain a plain alloc — the handler installation is a
    /// block terminator under §3.7.
    #[test]
    fn reuse_not_across_with_handler() {
        let decl = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Let {
                name: "old".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(1),
                    ValueExpr::IntLit(2),
                ]))),
                body: Box::new(CompExpr::Seq {
                    stmts: vec![CompExpr::Drop {
                        value: Box::new(ValueExpr::Var("old".to_string())),
                    }],
                    tail: Box::new(CompExpr::WithHandler {
                        handler: Box::new(CompExpr::Value(ValueExpr::Var("h".to_string()))),
                        body: Box::new(CompExpr::Let {
                            name: "next".to_string(),
                            ty: IrType::Unknown,
                            value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                                ValueExpr::IntLit(3),
                                ValueExpr::IntLit(4),
                            ]))),
                            body: Box::new(CompExpr::Value(ValueExpr::Var("next".to_string()))),
                        }),
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
            "drop should remain ordinary across WithHandler:\n{ir}"
        );
        assert!(
            !ir.contains("alloc_reuse"),
            "WithHandler boundary must block reuse pairing:\n{ir}"
        );
    }

    // -----------------------------------------------------------------------
    // first_alloc_class / rewrite_callee_first_alloc tests (Step 8)
    // -----------------------------------------------------------------------

    fn simple_step_decl() -> IrDecl {
        // fn step(acc) = let next = (acc.0 + 1, acc.1); next
        IrDecl {
            name: "step".to_string(),
            params: vec![("acc".to_string(), IrType::Unknown)],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Let {
                name: "next".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(1),
                    ValueExpr::IntLit(2),
                ]))),
                body: Box::new(CompExpr::Value(ValueExpr::Var("next".to_string()))),
            },
        }
    }

    #[test]
    fn first_alloc_class_returns_tuple_class_for_simple_step() {
        let decl = simple_step_decl();
        let class = first_alloc_class(&decl);
        assert_eq!(class, Some(SizeClass::for_tuple(2)));
    }

    #[test]
    fn first_alloc_class_returns_none_when_size_mismatch_not_applicable() {
        // body = Tuple(3) - should return Tuple(3)
        let decl = IrDecl {
            name: "f".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Value(ValueExpr::TupleLit(vec![
                ValueExpr::IntLit(1),
                ValueExpr::IntLit(2),
                ValueExpr::IntLit(3),
            ])),
        };
        assert_eq!(first_alloc_class(&decl), Some(SizeClass::for_tuple(3)));
    }

    #[test]
    fn first_alloc_class_returns_none_across_effect() {
        // body = PerformEffect(...); (1, 2)  → abort before alloc
        let decl = IrDecl {
            name: "effectful".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Seq {
                stmts: vec![CompExpr::PerformEffect {
                    effect: "Print".to_string(),
                    op: "print".to_string(),
                    args: vec![ValueExpr::StrLit("x".to_string())],
                }],
                tail: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(1),
                    ValueExpr::IntLit(2),
                ]))),
            },
        };
        assert_eq!(first_alloc_class(&decl), None);
    }

    #[test]
    fn rewrite_callee_first_alloc_rewrites_let_value_alloc() {
        let decl = simple_step_decl();
        let rewritten = rewrite_callee_first_alloc(decl, "__tok");
        let module = IrModule {
            decls: vec![rewritten],
        };
        validate_ir(&module).expect("rewritten IR should validate");
        let ir = fmt_ir(&module);
        assert!(
            ir.contains("alloc_reuse __tok"),
            "should contain alloc_reuse with token:\n{ir}"
        );
        assert!(
            !ir.contains("\n    (1, 2)"),
            "plain alloc should be replaced by alloc_reuse:\n{ir}"
        );
    }

    // -----------------------------------------------------------------------
    // insert_tail_reuse_module tests (Step 8)
    // -----------------------------------------------------------------------

    fn make_tail_reuse_module() -> IrModule {
        // decl step(acc: ?) : ? =
        //   let next = (1, 2) in
        //     next
        //
        // decl main() : ? =
        //   let acc = (0, 0) in
        //     seq
        //       drop acc
        //     =>
        //       call step(acc)  -- tail call, Drop immediately before
        let step_decl = IrDecl {
            name: "step".to_string(),
            params: vec![("acc".to_string(), IrType::Unknown)],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Let {
                name: "next".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(1),
                    ValueExpr::IntLit(2),
                ]))),
                body: Box::new(CompExpr::Value(ValueExpr::Var("next".to_string()))),
            },
        };
        let main_decl = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Let {
                name: "acc".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(0),
                    ValueExpr::IntLit(0),
                ]))),
                body: Box::new(CompExpr::Seq {
                    stmts: vec![CompExpr::Drop {
                        value: Box::new(ValueExpr::Var("acc".to_string())),
                    }],
                    tail: Box::new(CompExpr::Call {
                        callee: Box::new(ValueExpr::Var("step".to_string())),
                        args: vec![ValueExpr::Var("acc".to_string())],
                        reuse_token: None,
                    }),
                }),
            },
        };
        IrModule {
            decls: vec![step_decl, main_decl],
        }
    }

    #[test]
    fn tail_call_drop_reuse_rewrites_drop_and_callee_first_alloc() {
        let module = make_tail_reuse_module();
        let mut next = 0;
        let rewritten = insert_tail_reuse_module(module, &mut next);
        validate_ir(&rewritten).expect("rewritten module should validate");
        let ir = fmt_ir(&rewritten);

        // Caller side: drop should become drop_reuse.
        assert!(
            ir.contains("drop_reuse acc as __perceus_tail_reuse_token_0"),
            "caller drop should be drop_reuse:\n{ir}"
        );
        // Caller side: call should carry reuse= annotation.
        assert!(
            ir.contains("reuse=__perceus_tail_reuse_token_0"),
            "call should carry reuse token:\n{ir}"
        );
        // Callee side: first alloc should become alloc_reuse.
        assert!(
            ir.contains("alloc_reuse __perceus_tail_reuse_token_0"),
            "callee first alloc should be alloc_reuse:\n{ir}"
        );
        // Callee decl should have reuse_param annotation.
        assert!(
            ir.contains("reuse_param=__perceus_tail_reuse_token_0"),
            "callee decl should have reuse_param:\n{ir}"
        );
    }

    #[test]
    fn tail_call_no_rewrite_when_size_class_mismatch() {
        // drop Tuple(2); call f  but f's first alloc is Tuple(1)
        let f_decl = IrDecl {
            name: "f".to_string(),
            params: vec![("x".to_string(), IrType::Unknown)],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Value(ValueExpr::TupleLit(vec![ValueExpr::IntLit(1)])),
        };
        let caller = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Let {
                name: "pair".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(0),
                    ValueExpr::IntLit(0),
                ]))),
                body: Box::new(CompExpr::Seq {
                    stmts: vec![CompExpr::Drop {
                        value: Box::new(ValueExpr::Var("pair".to_string())),
                    }],
                    tail: Box::new(CompExpr::Call {
                        callee: Box::new(ValueExpr::Var("f".to_string())),
                        args: vec![ValueExpr::IntLit(1)],
                        reuse_token: None,
                    }),
                }),
            },
        };
        let module = IrModule {
            decls: vec![f_decl, caller],
        };
        let mut next = 0;
        let rewritten = insert_tail_reuse_module(module, &mut next);
        let ir = fmt_ir(&rewritten);
        assert!(
            !ir.contains("drop_reuse"),
            "size class mismatch should not rewrite:\n{ir}"
        );
        assert!(
            !ir.contains("reuse="),
            "no reuse token should be emitted:\n{ir}"
        );
    }

    #[test]
    fn tail_call_no_rewrite_when_not_tail_position() {
        // The Call is wrapped in a Let body — not a bare Seq tail.
        let f_decl = IrDecl {
            name: "f".to_string(),
            params: vec![("x".to_string(), IrType::Unknown)],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Value(ValueExpr::TupleLit(vec![ValueExpr::IntLit(1)])),
        };
        let caller = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            // Let wraps the Call — not a Seq{ stmts=[Drop], tail=Call }
            body: CompExpr::Let {
                name: "pair".to_string(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(0),
                ]))),
                body: Box::new(CompExpr::Let {
                    name: "r".to_string(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Call {
                        callee: Box::new(ValueExpr::Var("f".to_string())),
                        args: vec![ValueExpr::IntLit(1)],
                        reuse_token: None,
                    }),
                    body: Box::new(CompExpr::Value(ValueExpr::Var("r".to_string()))),
                }),
            },
        };
        let module = IrModule {
            decls: vec![f_decl, caller],
        };
        let mut next = 0;
        let rewritten = insert_tail_reuse_module(module, &mut next);
        let ir = fmt_ir(&rewritten);
        assert!(
            !ir.contains("drop_reuse"),
            "non-tail Call should not trigger rewrite:\n{ir}"
        );
    }

    #[test]
    fn module_validator_rejects_reuse_token_without_reuse_param() {
        // Call with reuse_token but callee decl has reuse_param=None
        let callee = IrDecl {
            name: "f".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Value(ValueExpr::TupleLit(vec![ValueExpr::IntLit(1)])),
        };
        let caller = IrDecl {
            name: "main".to_string(),
            params: vec![],
            result_ty: IrType::Unknown,
            residual_effects: vec![],
            reuse_param: None,
            body: CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "".to_string(),
                    name: "f".to_string(),
                }),
                args: vec![],
                reuse_token: Some("tok".to_string()),
            },
        };
        let module = IrModule {
            decls: vec![callee, caller],
        };
        let result = validate_ir(&module);
        assert!(
            result.is_err(),
            "validator should reject reuse_token without matching reuse_param"
        );
    }
}
