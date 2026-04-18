// Suppress dead_code warnings while the general lowering surface continues to expand.
#![allow(dead_code)]

//! General Wasm lowering for Wasm-owned runtime programs.
//!
//! This module implements the shared lowering pipeline for the Wasm-owned execution
//! subset, including runtime-`Read` programs and the current safe handler subset:
//!
//! ```text
//! Goby IR (CompExpr / ValueExpr)
//!   ↓  gen_lower/lower.rs
//! Backend IR (WasmBackendInstr)
//!   ↓  gen_lower/emit.rs
//! wasm_encoder calls
//! ```
//!
//! # Module ownership
//! - `value`: `RtValue` tagged-i64 representation and encode/decode helpers.
//! - `backend_ir`: `WasmBackendInstr` flat instruction set.
//! - `lower`: Goby IR → backend IR lowering, including fused split patterns.
//! - `emit`: backend IR → Wasm emission, including WASI-backed `Read`/`Print`.
//!
//! # Import rules
//! This module and its submodules must NOT import from `runtime_io_plan.rs`.
//! They may import from `backend.rs`, `layout.rs`, `planning.rs`, and `goby-core/ir.rs`.

pub(crate) mod backend_ir;
pub(crate) mod closure_env;
pub(crate) mod emit;
pub(crate) mod lower;
pub(crate) mod ptr;
pub(crate) mod value;

use std::collections::{HashMap, HashSet};

use goby_core::Module;
use goby_core::ir::{CompExpr, IrCaseArm, IrDecl, IrHandlerClause, IrInterpPart, ValueExpr};
use goby_core::stdlib::StdlibResolver;
use goby_core::types::{TypeExpr, parse_function_type, parse_type_expr};

use crate::CodegenError;
use crate::effect_handler_legality::analyze_module_handler_legality;
use crate::effect_handler_lowering::lower_safe_handlers_in_comp;
use crate::gen_lower::backend_ir::{BackendEffectOp, BackendReadOp};
use crate::gen_lower::emit::AuxDecl;
use crate::gen_lower::lower::LambdaAuxDecl;
use crate::layout::MemoryLayout;
use crate::runtime_env::effective_runtime_imports;
use crate::wasm_exec_plan::decl_exec_plan;

fn decl_annotation_returns_int(annotation: Option<&str>) -> bool {
    parse_function_type(annotation.unwrap_or_default())
        .is_some_and(|function_ty| function_ty.result.trim() == "Int")
}

fn decl_annotation_returns_list(annotation: Option<&str>) -> bool {
    let Some(function_ty) = parse_function_type(annotation.unwrap_or_default()) else {
        return false;
    };
    matches!(
        parse_type_expr(&function_ty.result),
        Some(TypeExpr::Apply { head, .. })
            if matches!(head.as_ref(), TypeExpr::Name(name) if name == "List")
    )
}

#[derive(Debug, Clone)]
struct FoldPrependCallbackShape {
    acc_param: String,
    item_param: String,
    body: CompExpr,
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
                .any(|elem| value_mentions_name(elem, target))
                || spread
                    .as_deref()
                    .is_some_and(|tail| value_mentions_name(tail, target))
        }
        ValueExpr::TupleLit(items) => items.iter().any(|item| value_mentions_name(item, target)),
        ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, field)| value_mentions_name(field, target)),
        ValueExpr::Lambda { param, body } => {
            if param == target {
                false
            } else {
                comp_mentions_name(body, target)
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

fn comp_mentions_name(comp: &CompExpr, target: &str) -> bool {
    match comp {
        CompExpr::Value(value) => value_mentions_name(value, target),
        CompExpr::Let {
            name, value, body, ..
        }
        | CompExpr::LetMut {
            name, value, body, ..
        } => {
            comp_mentions_name(value, target)
                || if name == target {
                    false
                } else {
                    comp_mentions_name(body, target)
                }
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(|stmt| comp_mentions_name(stmt, target))
                || comp_mentions_name(tail, target)
        }
        CompExpr::If { cond, then_, else_ } => {
            value_mentions_name(cond, target)
                || comp_mentions_name(then_, target)
                || comp_mentions_name(else_, target)
        }
        CompExpr::Call { callee, args } => {
            value_mentions_name(callee, target)
                || args.iter().any(|arg| value_mentions_name(arg, target))
        }
        CompExpr::Assign { name, value } => name == target || comp_mentions_name(value, target),
        CompExpr::AssignIndex { root, path, value } => {
            root == target
                || path.iter().any(|index| value_mentions_name(index, target))
                || comp_mentions_name(value, target)
        }
        CompExpr::Case { scrutinee, arms } => {
            value_mentions_name(scrutinee, target)
                || arms.iter().any(|arm| comp_mentions_name(&arm.body, target))
        }
        CompExpr::PerformEffect { args, .. } => {
            args.iter().any(|arg| value_mentions_name(arg, target))
        }
        CompExpr::Handle { clauses } => clauses
            .iter()
            .any(|clause| comp_mentions_name(&clause.body, target)),
        CompExpr::WithHandler { handler, body } => {
            comp_mentions_name(handler, target) || comp_mentions_name(body, target)
        }
        CompExpr::Resume { value } => value_mentions_name(value, target),
    }
}

fn fold_prepend_callback_shape(decl: &IrDecl) -> Option<FoldPrependCallbackShape> {
    if decl.params.len() != 2 {
        return None;
    }
    let acc_param = decl.params[0].0.clone();
    let item_param = decl.params[1].0.clone();
    let CompExpr::Value(ValueExpr::ListLit { elements, spread }) = &decl.body else {
        return None;
    };
    let spread = spread.as_deref()?;
    let ValueExpr::Var(spread_name) = spread else {
        return None;
    };
    if spread_name != &acc_param
        || elements
            .iter()
            .any(|elem| value_mentions_name(elem, &acc_param))
    {
        return None;
    }
    Some(FoldPrependCallbackShape {
        acc_param,
        item_param,
        body: decl.body.clone(),
    })
}

fn rewrite_value_fold_prepend_callbacks(
    value: &ValueExpr,
    callback_shapes: &HashMap<String, FoldPrependCallbackShape>,
    aliases: &HashMap<String, String>,
) -> ValueExpr {
    match value {
        ValueExpr::ListLit { elements, spread } => ValueExpr::ListLit {
            elements: elements
                .iter()
                .map(|elem| rewrite_value_fold_prepend_callbacks(elem, callback_shapes, aliases))
                .collect(),
            spread: spread.as_deref().map(|tail| {
                Box::new(rewrite_value_fold_prepend_callbacks(
                    tail,
                    callback_shapes,
                    aliases,
                ))
            }),
        },
        ValueExpr::TupleLit(items) => ValueExpr::TupleLit(
            items
                .iter()
                .map(|item| rewrite_value_fold_prepend_callbacks(item, callback_shapes, aliases))
                .collect(),
        ),
        ValueExpr::RecordLit {
            constructor,
            fields,
        } => ValueExpr::RecordLit {
            constructor: constructor.clone(),
            fields: fields
                .iter()
                .map(|(name, field)| {
                    (
                        name.clone(),
                        rewrite_value_fold_prepend_callbacks(field, callback_shapes, aliases),
                    )
                })
                .collect(),
        },
        ValueExpr::Lambda { param, body } => {
            let mut lambda_aliases = aliases.clone();
            lambda_aliases.remove(param);
            ValueExpr::Lambda {
                param: param.clone(),
                body: Box::new(rewrite_comp_fold_prepend_callbacks(
                    body,
                    callback_shapes,
                    &lambda_aliases,
                )),
            }
        }
        ValueExpr::Interp(parts) => ValueExpr::Interp(
            parts
                .iter()
                .map(|part| match part {
                    IrInterpPart::Text(text) => IrInterpPart::Text(text.clone()),
                    IrInterpPart::Expr(value) => IrInterpPart::Expr(
                        rewrite_value_fold_prepend_callbacks(value, callback_shapes, aliases),
                    ),
                })
                .collect(),
        ),
        ValueExpr::BinOp { op, left, right } => ValueExpr::BinOp {
            op: op.clone(),
            left: Box::new(rewrite_value_fold_prepend_callbacks(
                left,
                callback_shapes,
                aliases,
            )),
            right: Box::new(rewrite_value_fold_prepend_callbacks(
                right,
                callback_shapes,
                aliases,
            )),
        },
        ValueExpr::TupleProject { tuple, index } => ValueExpr::TupleProject {
            tuple: Box::new(rewrite_value_fold_prepend_callbacks(
                tuple,
                callback_shapes,
                aliases,
            )),
            index: *index,
        },
        ValueExpr::ListGet { list, index } => ValueExpr::ListGet {
            list: Box::new(rewrite_value_fold_prepend_callbacks(
                list,
                callback_shapes,
                aliases,
            )),
            index: Box::new(rewrite_value_fold_prepend_callbacks(
                index,
                callback_shapes,
                aliases,
            )),
        },
        _ => value.clone(),
    }
}

fn resolve_callback_shape_name<'a>(
    callback: &'a ValueExpr,
    callback_shapes: &'a HashMap<String, FoldPrependCallbackShape>,
    aliases: &'a HashMap<String, String>,
) -> Option<&'a str> {
    match callback {
        ValueExpr::Var(name) => {
            let resolved = aliases.get(name).map_or(name.as_str(), String::as_str);
            callback_shapes.contains_key(resolved).then_some(resolved)
        }
        _ => None,
    }
}

fn inline_fold_prepend_callback(shape: &FoldPrependCallbackShape) -> ValueExpr {
    ValueExpr::Lambda {
        param: shape.acc_param.clone(),
        body: Box::new(CompExpr::Value(ValueExpr::Lambda {
            param: shape.item_param.clone(),
            body: Box::new(shape.body.clone()),
        })),
    }
}

fn rewrite_comp_fold_prepend_callbacks(
    comp: &CompExpr,
    callback_shapes: &HashMap<String, FoldPrependCallbackShape>,
    aliases: &HashMap<String, String>,
) -> CompExpr {
    match comp {
        CompExpr::Value(value) => CompExpr::Value(rewrite_value_fold_prepend_callbacks(
            value,
            callback_shapes,
            aliases,
        )),
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => {
            let rewritten_value =
                rewrite_comp_fold_prepend_callbacks(value, callback_shapes, aliases);
            let mut next_aliases = aliases.clone();
            if let CompExpr::Value(ValueExpr::Var(target)) = &rewritten_value {
                let resolved = aliases
                    .get(target)
                    .cloned()
                    .unwrap_or_else(|| target.clone());
                if callback_shapes.contains_key(&resolved) {
                    next_aliases.insert(name.clone(), resolved);
                } else {
                    next_aliases.remove(name);
                }
            } else {
                next_aliases.remove(name);
            }
            CompExpr::Let {
                name: name.clone(),
                ty: ty.clone(),
                value: Box::new(rewritten_value),
                body: Box::new(rewrite_comp_fold_prepend_callbacks(
                    body,
                    callback_shapes,
                    &next_aliases,
                )),
            }
        }
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => {
            let rewritten_value =
                rewrite_comp_fold_prepend_callbacks(value, callback_shapes, aliases);
            let mut next_aliases = aliases.clone();
            next_aliases.remove(name);
            CompExpr::LetMut {
                name: name.clone(),
                ty: ty.clone(),
                value: Box::new(rewritten_value),
                body: Box::new(rewrite_comp_fold_prepend_callbacks(
                    body,
                    callback_shapes,
                    &next_aliases,
                )),
            }
        }
        CompExpr::Seq { stmts, tail } => {
            let mut seq_aliases = aliases.clone();
            let mut rewritten_stmts = Vec::with_capacity(stmts.len());
            for stmt in stmts {
                let rewritten =
                    rewrite_comp_fold_prepend_callbacks(stmt, callback_shapes, &seq_aliases);
                match &rewritten {
                    CompExpr::Let { name, value, .. } => {
                        if let CompExpr::Value(ValueExpr::Var(target)) = value.as_ref() {
                            let resolved = seq_aliases
                                .get(target)
                                .cloned()
                                .unwrap_or_else(|| target.clone());
                            if callback_shapes.contains_key(&resolved) {
                                seq_aliases.insert(name.clone(), resolved);
                            } else {
                                seq_aliases.remove(name);
                            }
                        } else {
                            seq_aliases.remove(name);
                        }
                    }
                    CompExpr::LetMut { name, .. } => {
                        seq_aliases.remove(name);
                    }
                    _ => {}
                }
                rewritten_stmts.push(rewritten);
            }
            CompExpr::Seq {
                stmts: rewritten_stmts,
                tail: Box::new(rewrite_comp_fold_prepend_callbacks(
                    tail,
                    callback_shapes,
                    &seq_aliases,
                )),
            }
        }
        CompExpr::If { cond, then_, else_ } => CompExpr::If {
            cond: Box::new(rewrite_value_fold_prepend_callbacks(
                cond,
                callback_shapes,
                aliases,
            )),
            then_: Box::new(rewrite_comp_fold_prepend_callbacks(
                then_,
                callback_shapes,
                aliases,
            )),
            else_: Box::new(rewrite_comp_fold_prepend_callbacks(
                else_,
                callback_shapes,
                aliases,
            )),
        },
        CompExpr::Call { callee, args } => {
            let rewritten_callee =
                rewrite_value_fold_prepend_callbacks(callee, callback_shapes, aliases);
            let mut rewritten_args: Vec<ValueExpr> = args
                .iter()
                .map(|arg| rewrite_value_fold_prepend_callbacks(arg, callback_shapes, aliases))
                .collect();
            let is_fold = matches!(
                &rewritten_callee,
                ValueExpr::GlobalRef { module, name } if module == "list" && name == "fold"
            ) || matches!(&rewritten_callee, ValueExpr::Var(name) if name == "fold" || name == "__goby_list_fold");
            if is_fold
                && rewritten_args.len() == 3
                && let Some(callback_name) =
                    resolve_callback_shape_name(&rewritten_args[2], callback_shapes, aliases)
                && let Some(shape) = callback_shapes.get(callback_name)
            {
                rewritten_args[2] = inline_fold_prepend_callback(shape);
            }
            CompExpr::Call {
                callee: Box::new(rewritten_callee),
                args: rewritten_args,
            }
        }
        CompExpr::Assign { name, value } => CompExpr::Assign {
            name: name.clone(),
            value: Box::new(rewrite_comp_fold_prepend_callbacks(
                value,
                callback_shapes,
                aliases,
            )),
        },
        CompExpr::AssignIndex { root, path, value } => CompExpr::AssignIndex {
            root: root.clone(),
            path: path
                .iter()
                .map(|index| rewrite_value_fold_prepend_callbacks(index, callback_shapes, aliases))
                .collect(),
            value: Box::new(rewrite_comp_fold_prepend_callbacks(
                value,
                callback_shapes,
                aliases,
            )),
        },
        CompExpr::Case { scrutinee, arms } => CompExpr::Case {
            scrutinee: Box::new(rewrite_value_fold_prepend_callbacks(
                scrutinee,
                callback_shapes,
                aliases,
            )),
            arms: arms
                .iter()
                .map(|arm| IrCaseArm {
                    pattern: arm.pattern.clone(),
                    body: rewrite_comp_fold_prepend_callbacks(&arm.body, callback_shapes, aliases),
                })
                .collect(),
        },
        CompExpr::PerformEffect { effect, op, args } => CompExpr::PerformEffect {
            effect: effect.clone(),
            op: op.clone(),
            args: args
                .iter()
                .map(|arg| rewrite_value_fold_prepend_callbacks(arg, callback_shapes, aliases))
                .collect(),
        },
        CompExpr::Handle { clauses } => CompExpr::Handle {
            clauses: clauses
                .iter()
                .map(|clause| IrHandlerClause {
                    op_name: clause.op_name.clone(),
                    params: clause.params.clone(),
                    body: rewrite_comp_fold_prepend_callbacks(
                        &clause.body,
                        callback_shapes,
                        aliases,
                    ),
                })
                .collect(),
        },
        CompExpr::WithHandler { handler, body } => CompExpr::WithHandler {
            handler: Box::new(rewrite_comp_fold_prepend_callbacks(
                handler,
                callback_shapes,
                aliases,
            )),
            body: Box::new(rewrite_comp_fold_prepend_callbacks(
                body,
                callback_shapes,
                aliases,
            )),
        },
        CompExpr::Resume { value } => CompExpr::Resume {
            value: Box::new(rewrite_value_fold_prepend_callbacks(
                value,
                callback_shapes,
                aliases,
            )),
        },
    }
}

/// Reason why a program cannot be lowered via the general-lowering path.
///
/// Returned by [`supports_general_lower_module`] when it returns `Some(reason)`.
/// `None` means the module is fully supported by general lowering.
///
/// This type enables callers (e.g. `runtime_io_plan.rs`, future finer-grained diagnostics) to
/// surface a precise message rather than a generic "unsupported" fallback.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum GeneralLowerUnsupportedReason {
    /// Module has no main execution plan (no `main` declaration or no exec plan built).
    NoMainExecPlan,
    /// Main execution plan exists but has no IR decl available.
    NoIrDecl,
    /// Safe-handler rewrite returned `None` (handler body not rewritable).
    HandlerRewriteFailed,
    /// Main is a handler-only candidate but has conflicting non-main decls, future handler
    /// intrinsics, or effectful non-main declarations.
    /// TODO: split into sub-reasons for more precise diagnostics.
    HandlerOnlyConflict,
    /// Program does not require runtime capabilities (no Read, handler, lambda, or tuple
    /// projection). Pure-Print programs stay on simpler paths.
    NotRequiringRuntimeCapability,
    /// The IR lowering pass encountered a construct it does not yet support.
    /// `node` is the `Display` representation of the unsupported IR node.
    UnsupportedIrForm { node: String },
    /// Read-line instructions could not be built for this module shape.
    ReadLineInstructionsNotSupported,
    /// An auxiliary declaration (user-defined non-main helper) could not be lowered to backend IR
    /// or produced instructions that `lower_aux_decl` could not handle.
    AuxDeclNotSupported,
    /// A user-defined non-main declaration failed IR lowering (`ir_lower::lower_declaration`).
    UserDeclIrLoweringFailed,
    /// A stdlib declaration (imported from the standard library) failed IR lowering.
    StdlibDeclIrLoweringFailed,
    // Note: EmitNotSupported is intentionally absent — emit::supports_instrs currently
    // returns true for all instructions, so that rejection path is unreachable.
}

impl std::fmt::Display for GeneralLowerUnsupportedReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoMainExecPlan => write!(f, "no main execution plan"),
            Self::NoIrDecl => write!(f, "no IR decl available for main"),
            Self::HandlerRewriteFailed => write!(f, "safe-handler rewrite returned None"),
            Self::HandlerOnlyConflict => {
                write!(
                    f,
                    "handler-only candidate conflicts with non-main decls or future intrinsics"
                )
            }
            Self::NotRequiringRuntimeCapability => {
                write!(
                    f,
                    "program does not require general-lowering runtime capabilities"
                )
            }
            Self::UnsupportedIrForm { node } => {
                write!(f, "unsupported IR form in general lowering path: {node}")
            }
            Self::ReadLineInstructionsNotSupported => {
                write!(
                    f,
                    "read-line instructions not supported for this module shape"
                )
            }
            Self::AuxDeclNotSupported => {
                write!(f, "auxiliary declaration produced unsupported instructions")
            }
            Self::UserDeclIrLoweringFailed => {
                write!(
                    f,
                    "user-defined declaration could not be lowered to backend IR"
                )
            }
            Self::StdlibDeclIrLoweringFailed => {
                write!(f, "stdlib declaration could not be lowered to backend IR")
            }
        }
    }
}

fn type_expr_returns_wasm_heap(expr: &TypeExpr) -> bool {
    match expr {
        TypeExpr::Name(name) => !matches!(name.as_str(), "Int" | "Bool" | "String" | "Unit"),
        TypeExpr::Tuple(_) | TypeExpr::Function { .. } => true,
        TypeExpr::Apply { head, .. } => match head.as_ref() {
            TypeExpr::Name(name) => !matches!(name.as_str(), "String"),
            _ => true,
        },
    }
}

fn decl_return_type_uses_wasm_heap(annotation: Option<&str>) -> bool {
    let Some(annotation) = annotation else {
        return true;
    };
    let Some(function_ty) = parse_function_type(annotation) else {
        return true;
    };
    let Some(result_ty) = parse_type_expr(&function_ty.result) else {
        return true;
    };
    type_expr_returns_wasm_heap(&result_ty)
}

fn resolve_stdlib_root() -> std::path::PathBuf {
    std::env::var_os("GOBY_STDLIB_ROOT")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| {
            std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../..")
                .join("stdlib")
        })
}

fn has_runtime_read_effect(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::PerformEffect { effect, .. } => effect == "Read",
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            has_runtime_read_effect(value) || has_runtime_read_effect(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(has_runtime_read_effect) || has_runtime_read_effect(tail)
        }
        CompExpr::If { then_, else_, .. } => {
            has_runtime_read_effect(then_) || has_runtime_read_effect(else_)
        }
        CompExpr::Call { .. } | CompExpr::Value(_) => false,
        CompExpr::Assign { value, .. } => has_runtime_read_effect(value),
        CompExpr::Case { arms, .. } => arms.iter().any(|arm| has_runtime_read_effect(&arm.body)),
        CompExpr::Handle { .. } | CompExpr::WithHandler { .. } | CompExpr::Resume { .. } => false,
        CompExpr::AssignIndex { path, value, .. } => {
            let _ = path;
            has_runtime_read_effect(value)
        }
    }
}

fn has_handler_constructs(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::Handle { .. } | CompExpr::WithHandler { .. } | CompExpr::Resume { .. } => true,
        CompExpr::AssignIndex { path, value, .. } => {
            path.iter().any(value_has_handler_constructs) || has_handler_constructs(value)
        }
        CompExpr::Value(value) => value_has_handler_constructs(value),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            has_handler_constructs(value) || has_handler_constructs(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(has_handler_constructs) || has_handler_constructs(tail)
        }
        CompExpr::If { then_, else_, .. } => {
            has_handler_constructs(then_) || has_handler_constructs(else_)
        }
        CompExpr::Call { .. } => false,
        CompExpr::Assign { value, .. } => has_handler_constructs(value),
        CompExpr::Case { arms, .. } => arms.iter().any(|arm| has_handler_constructs(&arm.body)),
        CompExpr::PerformEffect { .. } => false,
    }
}

fn has_handler_rewrite_entrypoints(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::WithHandler { .. } | CompExpr::Resume { .. } => true,
        CompExpr::Handle { clauses } => clauses
            .iter()
            .any(|clause| has_handler_rewrite_entrypoints(&clause.body)),
        CompExpr::Value(value) => value_has_handler_rewrite_entrypoints(value),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            has_handler_rewrite_entrypoints(value) || has_handler_rewrite_entrypoints(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(has_handler_rewrite_entrypoints)
                || has_handler_rewrite_entrypoints(tail)
        }
        CompExpr::If { then_, else_, .. } => {
            has_handler_rewrite_entrypoints(then_) || has_handler_rewrite_entrypoints(else_)
        }
        CompExpr::Call { .. } => false,
        CompExpr::Assign { value, .. } => has_handler_rewrite_entrypoints(value),
        CompExpr::Case { arms, .. } => arms
            .iter()
            .any(|arm| has_handler_rewrite_entrypoints(&arm.body)),
        CompExpr::PerformEffect { .. } => false,
        CompExpr::AssignIndex { value, .. } => has_handler_rewrite_entrypoints(value),
    }
}

fn has_rooted_list_update(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::AssignIndex { .. } => true,
        CompExpr::Value(value) => value_has_rooted_list_update(value),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            has_rooted_list_update(value) || has_rooted_list_update(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(has_rooted_list_update) || has_rooted_list_update(tail)
        }
        CompExpr::If { then_, else_, .. } => {
            has_rooted_list_update(then_) || has_rooted_list_update(else_)
        }
        CompExpr::Call { .. } | CompExpr::PerformEffect { .. } | CompExpr::Resume { .. } => false,
        CompExpr::Assign { value, .. } => has_rooted_list_update(value),
        CompExpr::Case { arms, .. } => arms.iter().any(|arm| has_rooted_list_update(&arm.body)),
        CompExpr::Handle { clauses } => clauses
            .iter()
            .any(|clause| has_rooted_list_update(&clause.body)),
        CompExpr::WithHandler { handler, body } => {
            has_rooted_list_update(handler) || has_rooted_list_update(body)
        }
    }
}

fn value_has_handler_constructs(value: &goby_core::ir::ValueExpr) -> bool {
    match value {
        goby_core::ir::ValueExpr::ListLit { elements, spread } => {
            elements.iter().any(value_has_handler_constructs)
                || spread.as_deref().is_some_and(value_has_handler_constructs)
        }
        goby_core::ir::ValueExpr::TupleLit(items) => items.iter().any(value_has_handler_constructs),
        goby_core::ir::ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, value)| value_has_handler_constructs(value)),
        goby_core::ir::ValueExpr::Lambda { body, .. } => has_handler_constructs(body),
        goby_core::ir::ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            goby_core::ir::IrInterpPart::Text(_) => false,
            goby_core::ir::IrInterpPart::Expr(value) => value_has_handler_constructs(value),
        }),
        goby_core::ir::ValueExpr::BinOp { left, right, .. } => {
            value_has_handler_constructs(left) || value_has_handler_constructs(right)
        }
        goby_core::ir::ValueExpr::TupleProject { tuple, .. } => value_has_handler_constructs(tuple),
        goby_core::ir::ValueExpr::ListGet { list, index } => {
            value_has_handler_constructs(list) || value_has_handler_constructs(index)
        }
        goby_core::ir::ValueExpr::IntLit(_)
        | goby_core::ir::ValueExpr::BoolLit(_)
        | goby_core::ir::ValueExpr::StrLit(_)
        | goby_core::ir::ValueExpr::Var(_)
        | goby_core::ir::ValueExpr::GlobalRef { .. }
        | goby_core::ir::ValueExpr::Unit => false,
    }
}

fn value_has_rooted_list_update(value: &goby_core::ir::ValueExpr) -> bool {
    match value {
        goby_core::ir::ValueExpr::ListLit { elements, spread } => {
            elements.iter().any(value_has_rooted_list_update)
                || spread.as_deref().is_some_and(value_has_rooted_list_update)
        }
        goby_core::ir::ValueExpr::TupleLit(items) => items.iter().any(value_has_rooted_list_update),
        goby_core::ir::ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, value)| value_has_rooted_list_update(value)),
        goby_core::ir::ValueExpr::Lambda { body, .. } => has_rooted_list_update(body),
        goby_core::ir::ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            goby_core::ir::IrInterpPart::Text(_) => false,
            goby_core::ir::IrInterpPart::Expr(value) => value_has_rooted_list_update(value),
        }),
        goby_core::ir::ValueExpr::BinOp { left, right, .. } => {
            value_has_rooted_list_update(left) || value_has_rooted_list_update(right)
        }
        goby_core::ir::ValueExpr::TupleProject { tuple, .. } => value_has_rooted_list_update(tuple),
        goby_core::ir::ValueExpr::ListGet { list, index } => {
            value_has_rooted_list_update(list) || value_has_rooted_list_update(index)
        }
        goby_core::ir::ValueExpr::IntLit(_)
        | goby_core::ir::ValueExpr::BoolLit(_)
        | goby_core::ir::ValueExpr::StrLit(_)
        | goby_core::ir::ValueExpr::Var(_)
        | goby_core::ir::ValueExpr::GlobalRef { .. }
        | goby_core::ir::ValueExpr::Unit => false,
    }
}

fn value_has_handler_rewrite_entrypoints(value: &goby_core::ir::ValueExpr) -> bool {
    match value {
        goby_core::ir::ValueExpr::ListLit { elements, spread } => {
            elements.iter().any(value_has_handler_rewrite_entrypoints)
                || spread
                    .as_deref()
                    .is_some_and(value_has_handler_rewrite_entrypoints)
        }
        goby_core::ir::ValueExpr::TupleLit(items) => {
            items.iter().any(value_has_handler_rewrite_entrypoints)
        }
        goby_core::ir::ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, value)| value_has_handler_rewrite_entrypoints(value)),
        goby_core::ir::ValueExpr::Lambda { body, .. } => has_handler_rewrite_entrypoints(body),
        goby_core::ir::ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            goby_core::ir::IrInterpPart::Text(_) => false,
            goby_core::ir::IrInterpPart::Expr(value) => {
                value_has_handler_rewrite_entrypoints(value)
            }
        }),
        goby_core::ir::ValueExpr::BinOp { left, right, .. } => {
            value_has_handler_rewrite_entrypoints(left)
                || value_has_handler_rewrite_entrypoints(right)
        }
        goby_core::ir::ValueExpr::TupleProject { tuple, .. } => {
            value_has_handler_rewrite_entrypoints(tuple)
        }
        goby_core::ir::ValueExpr::ListGet { list, index } => {
            value_has_handler_rewrite_entrypoints(list)
                || value_has_handler_rewrite_entrypoints(index)
        }
        goby_core::ir::ValueExpr::IntLit(_)
        | goby_core::ir::ValueExpr::BoolLit(_)
        | goby_core::ir::ValueExpr::StrLit(_)
        | goby_core::ir::ValueExpr::Var(_)
        | goby_core::ir::ValueExpr::GlobalRef { .. }
        | goby_core::ir::ValueExpr::Unit => false,
    }
}

fn contains_future_handler_intrinsics(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::Call { callee, .. } => matches!(
            callee.as_ref(),
            goby_core::ir::ValueExpr::Var(name) if name == "__goby_string_each_grapheme"
        ),
        CompExpr::Value(value) => value_contains_future_handler_intrinsics(value),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            contains_future_handler_intrinsics(value) || contains_future_handler_intrinsics(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(contains_future_handler_intrinsics)
                || contains_future_handler_intrinsics(tail)
        }
        CompExpr::If { then_, else_, .. } => {
            contains_future_handler_intrinsics(then_) || contains_future_handler_intrinsics(else_)
        }
        CompExpr::Assign { value, .. } => contains_future_handler_intrinsics(value),
        CompExpr::Case { arms, .. } => arms
            .iter()
            .any(|arm| contains_future_handler_intrinsics(&arm.body)),
        CompExpr::PerformEffect { .. }
        | CompExpr::Handle { .. }
        | CompExpr::WithHandler { .. }
        | CompExpr::Resume { .. } => false,
        CompExpr::AssignIndex { value, .. } => contains_future_handler_intrinsics(value),
    }
}

fn value_contains_future_handler_intrinsics(value: &goby_core::ir::ValueExpr) -> bool {
    match value {
        goby_core::ir::ValueExpr::ListLit { elements, spread } => {
            elements
                .iter()
                .any(value_contains_future_handler_intrinsics)
                || spread
                    .as_deref()
                    .is_some_and(value_contains_future_handler_intrinsics)
        }
        goby_core::ir::ValueExpr::TupleLit(items) => {
            items.iter().any(value_contains_future_handler_intrinsics)
        }
        goby_core::ir::ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, value)| value_contains_future_handler_intrinsics(value)),
        goby_core::ir::ValueExpr::Lambda { body, .. } => contains_future_handler_intrinsics(body),
        goby_core::ir::ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            goby_core::ir::IrInterpPart::Text(_) => false,
            goby_core::ir::IrInterpPart::Expr(value) => {
                value_contains_future_handler_intrinsics(value)
            }
        }),
        goby_core::ir::ValueExpr::BinOp { left, right, .. } => {
            value_contains_future_handler_intrinsics(left)
                || value_contains_future_handler_intrinsics(right)
        }
        goby_core::ir::ValueExpr::TupleProject { tuple, .. } => {
            value_contains_future_handler_intrinsics(tuple)
        }
        goby_core::ir::ValueExpr::ListGet { list, index } => {
            value_contains_future_handler_intrinsics(list)
                || value_contains_future_handler_intrinsics(index)
        }
        goby_core::ir::ValueExpr::IntLit(_)
        | goby_core::ir::ValueExpr::BoolLit(_)
        | goby_core::ir::ValueExpr::StrLit(_)
        | goby_core::ir::ValueExpr::Var(_)
        | goby_core::ir::ValueExpr::GlobalRef { .. }
        | goby_core::ir::ValueExpr::Unit => false,
    }
}

fn comp_has_effect_boundary_activity(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::PerformEffect { .. }
        | CompExpr::Handle { .. }
        | CompExpr::WithHandler { .. }
        | CompExpr::Resume { .. } => true,
        CompExpr::Value(value) => value_has_effect_boundary_activity(value),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            comp_has_effect_boundary_activity(value) || comp_has_effect_boundary_activity(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(comp_has_effect_boundary_activity)
                || comp_has_effect_boundary_activity(tail)
        }
        CompExpr::If { then_, else_, .. } => {
            comp_has_effect_boundary_activity(then_) || comp_has_effect_boundary_activity(else_)
        }
        CompExpr::Call { .. } => false,
        CompExpr::Assign { value, .. } => comp_has_effect_boundary_activity(value),
        CompExpr::Case { arms, .. } => arms
            .iter()
            .any(|arm| comp_has_effect_boundary_activity(&arm.body)),
        CompExpr::AssignIndex { value, .. } => comp_has_effect_boundary_activity(value),
    }
}

fn value_has_effect_boundary_activity(value: &goby_core::ir::ValueExpr) -> bool {
    match value {
        goby_core::ir::ValueExpr::ListLit { elements, spread } => {
            elements.iter().any(value_has_effect_boundary_activity)
                || spread
                    .as_deref()
                    .is_some_and(value_has_effect_boundary_activity)
        }
        goby_core::ir::ValueExpr::TupleLit(items) => {
            items.iter().any(value_has_effect_boundary_activity)
        }
        goby_core::ir::ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, value)| value_has_effect_boundary_activity(value)),
        goby_core::ir::ValueExpr::Lambda { body, .. } => comp_has_effect_boundary_activity(body),
        goby_core::ir::ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            goby_core::ir::IrInterpPart::Text(_) => false,
            goby_core::ir::IrInterpPart::Expr(value) => value_has_effect_boundary_activity(value),
        }),
        goby_core::ir::ValueExpr::BinOp { left, right, .. } => {
            value_has_effect_boundary_activity(left) || value_has_effect_boundary_activity(right)
        }
        goby_core::ir::ValueExpr::TupleProject { tuple, .. } => {
            value_has_effect_boundary_activity(tuple)
        }
        goby_core::ir::ValueExpr::ListGet { list, index } => {
            value_has_effect_boundary_activity(list) || value_has_effect_boundary_activity(index)
        }
        goby_core::ir::ValueExpr::IntLit(_)
        | goby_core::ir::ValueExpr::BoolLit(_)
        | goby_core::ir::ValueExpr::StrLit(_)
        | goby_core::ir::ValueExpr::Var(_)
        | goby_core::ir::ValueExpr::GlobalRef { .. }
        | goby_core::ir::ValueExpr::Unit => false,
    }
}

/// Returns true if the computation contains a `ValueExpr::Lambda` anywhere in its tree.
///
/// Used to extend the GeneralLower routing gate: programs that use Lambda expressions
/// (but have no Read effect or handler constructs) should still enter the GeneralLowered path
/// once lambda lowering is in place.
fn has_lambda_in_comp(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::Value(v) => has_lambda_in_value(v),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            has_lambda_in_comp(value) || has_lambda_in_comp(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(has_lambda_in_comp) || has_lambda_in_comp(tail)
        }
        CompExpr::PerformEffect { args, .. } => args.iter().any(has_lambda_in_value),
        CompExpr::Call { callee, args } => {
            has_lambda_in_value(callee) || args.iter().any(has_lambda_in_value)
        }
        CompExpr::If { cond, then_, else_ } => {
            has_lambda_in_value(cond) || has_lambda_in_comp(then_) || has_lambda_in_comp(else_)
        }
        CompExpr::Assign { value, .. } => has_lambda_in_comp(value),
        CompExpr::Case { scrutinee, arms } => {
            has_lambda_in_value(scrutinee) || arms.iter().any(|arm| has_lambda_in_comp(&arm.body))
        }
        CompExpr::Handle { .. } | CompExpr::WithHandler { .. } | CompExpr::Resume { .. } => false,
        CompExpr::AssignIndex { path, value, .. } => {
            path.iter().any(has_lambda_in_value) || has_lambda_in_comp(value)
        }
    }
}

fn has_lambda_in_value(v: &goby_core::ir::ValueExpr) -> bool {
    match v {
        goby_core::ir::ValueExpr::Lambda { .. } => true,
        goby_core::ir::ValueExpr::BinOp { left, right, .. } => {
            has_lambda_in_value(left) || has_lambda_in_value(right)
        }
        goby_core::ir::ValueExpr::ListLit { elements, .. } => {
            elements.iter().any(has_lambda_in_value)
        }
        goby_core::ir::ValueExpr::TupleLit(items) => items.iter().any(has_lambda_in_value),
        goby_core::ir::ValueExpr::RecordLit { fields, .. } => {
            fields.iter().any(|(_, v)| has_lambda_in_value(v))
        }
        goby_core::ir::ValueExpr::Interp(parts) => parts.iter().any(|p| match p {
            goby_core::ir::IrInterpPart::Expr(e) => has_lambda_in_value(e),
            goby_core::ir::IrInterpPart::Text(_) => false,
        }),
        _ => false,
    }
}

/// Returns true if the computation contains a `ValueExpr::TupleProject` anywhere in its tree.
///
/// Used to extend the GeneralLower routing gate: programs that use tuple member access
/// but have no Read effect, handler constructs, or Lambda must still enter the GeneralLowered
/// path, because the native fallback evaluator does not support tuple projection.
fn has_tuple_project_in_comp(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::Value(v) => has_tuple_project_in_value(v),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            has_tuple_project_in_comp(value) || has_tuple_project_in_comp(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(has_tuple_project_in_comp) || has_tuple_project_in_comp(tail)
        }
        CompExpr::PerformEffect { args, .. } => args.iter().any(has_tuple_project_in_value),
        CompExpr::Call { callee, args } => {
            has_tuple_project_in_value(callee) || args.iter().any(has_tuple_project_in_value)
        }
        CompExpr::If { cond, then_, else_ } => {
            has_tuple_project_in_value(cond)
                || has_tuple_project_in_comp(then_)
                || has_tuple_project_in_comp(else_)
        }
        CompExpr::Assign { value, .. } => has_tuple_project_in_comp(value),
        CompExpr::Case { scrutinee, arms } => {
            has_tuple_project_in_value(scrutinee)
                || arms.iter().any(|arm| has_tuple_project_in_comp(&arm.body))
        }
        CompExpr::Handle { .. } | CompExpr::WithHandler { .. } | CompExpr::Resume { .. } => false,
        CompExpr::AssignIndex { path, value, .. } => {
            path.iter().any(has_tuple_project_in_value) || has_tuple_project_in_comp(value)
        }
    }
}

fn has_tuple_project_in_value(v: &goby_core::ir::ValueExpr) -> bool {
    match v {
        goby_core::ir::ValueExpr::TupleProject { .. } => true,
        goby_core::ir::ValueExpr::BinOp { left, right, .. } => {
            has_tuple_project_in_value(left) || has_tuple_project_in_value(right)
        }
        goby_core::ir::ValueExpr::ListLit { elements, .. } => {
            elements.iter().any(has_tuple_project_in_value)
        }
        goby_core::ir::ValueExpr::TupleLit(items) => items.iter().any(has_tuple_project_in_value),
        goby_core::ir::ValueExpr::RecordLit { fields, .. } => {
            fields.iter().any(|(_, v)| has_tuple_project_in_value(v))
        }
        goby_core::ir::ValueExpr::Interp(parts) => parts.iter().any(|p| match p {
            goby_core::ir::IrInterpPart::Expr(e) => has_tuple_project_in_value(e),
            goby_core::ir::IrInterpPart::Text(_) => false,
        }),
        _ => false,
    }
}

fn has_effectful_non_main_decl(module: &Module) -> bool {
    module
        .declarations
        .iter()
        .filter(|decl| decl.name != "main")
        .filter_map(|decl| decl_exec_plan(decl).ir_decl)
        .any(|decl| comp_has_effect_boundary_activity(&decl.body))
}

fn rewrite_safe_handlers_if_present(
    body: &CompExpr,
    allow_safe_handler_lowering: bool,
) -> Result<Option<CompExpr>, CodegenError> {
    if !has_handler_rewrite_entrypoints(body) {
        return Ok(Some(body.clone()));
    }
    if !allow_safe_handler_lowering {
        return Ok(None);
    }
    Ok(Some(lower_safe_handlers_in_comp(body)?))
}

fn read_line_instrs_are_supported(instrs: &[backend_ir::WasmBackendInstr]) -> bool {
    // Collect all instructions recursively (If branches may contain EffectOp).
    fn collect_all<'a>(
        instrs: &'a [backend_ir::WasmBackendInstr],
        out: &mut Vec<&'a backend_ir::WasmBackendInstr>,
    ) {
        for instr in instrs {
            out.push(instr);
            if let backend_ir::WasmBackendInstr::If {
                then_instrs,
                else_instrs,
            } = instr
            {
                collect_all(then_instrs, out);
                collect_all(else_instrs, out);
            }
        }
    }
    let mut all = Vec::new();
    collect_all(instrs, &mut all);

    let read_line_count = all
        .iter()
        .filter(|instr| {
            matches!(
                instr,
                backend_ir::WasmBackendInstr::EffectOp {
                    op: BackendEffectOp::Read(BackendReadOp::ReadLine)
                }
            )
        })
        .count();
    if read_line_count == 0 {
        return true;
    }
    if read_line_count != 1 {
        return false;
    }

    !all.iter().any(|instr| {
        matches!(
            instr,
            backend_ir::WasmBackendInstr::EffectOp {
                op: BackendEffectOp::Read(BackendReadOp::Read)
            }
        ) || matches!(
            instr,
            backend_ir::WasmBackendInstr::Intrinsic { .. }
                | backend_ir::WasmBackendInstr::SplitEachPrint { .. }
                | backend_ir::WasmBackendInstr::SplitGetPrint { .. }
        )
    })
}

/// Lower a single auxiliary (non-main) declaration body.
///
/// Unlike `main`, aux decls do not require a Read effect — they are helper functions
/// called by `main`. Unsupported forms are returned as precise
/// `GeneralLowerUnsupportedReason`s so callers can preserve the real blocker.
/// Lambda expressions encountered during lowering are appended to `lambda_decls`.
fn lower_aux_decl(
    name: &str,
    param_names: Vec<String>,
    type_annotation: Option<&str>,
    body: &CompExpr,
    known_decls: &HashSet<String>,
    allow_safe_handler_lowering: bool,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Result<AuxDecl, GeneralLowerUnsupportedReason>, CodegenError> {
    let Some(body) = rewrite_safe_handlers_if_present(body, allow_safe_handler_lowering)? else {
        return Ok(Err(GeneralLowerUnsupportedReason::HandlerRewriteFailed));
    };
    let lowered = if decl_annotation_returns_int(type_annotation) {
        match lower::lower_supported_self_recursive_int_scan(
            name,
            &body,
            &param_names,
            known_decls,
            lambda_decls,
        ) {
            Ok(Some(instrs)) => Ok(instrs),
            Ok(None) => lower::lower_comp_collecting_lambdas_with_params(
                &body,
                &param_names,
                known_decls,
                lambda_decls,
            ),
            Err(err) => Err(err),
        }
    } else if decl_annotation_returns_list(type_annotation) {
        match lower::lower_supported_self_recursive_list_spread_builder(
            name,
            &body,
            &param_names,
            known_decls,
            lambda_decls,
        ) {
            Ok(Some(instrs)) => Ok(instrs),
            Ok(None) => lower::lower_comp_collecting_lambdas_with_params(
                &body,
                &param_names,
                known_decls,
                lambda_decls,
            ),
            Err(err) => Err(err),
        }
    } else {
        lower::lower_comp_collecting_lambdas_with_params(
            &body,
            &param_names,
            known_decls,
            lambda_decls,
        )
    };
    let instrs = match lowered {
        Ok(i) => i,
        Err(lower::LowerError::UnsupportedForm { node }) => {
            return Ok(Err(GeneralLowerUnsupportedReason::UnsupportedIrForm {
                node,
            }));
        }
        Err(lower::LowerError::IntOutOfRange(n)) => {
            return Err(CodegenError {
                message: format!("integer {n} is outside the 60-bit representable range"),
            });
        }
    };
    Ok(Ok(AuxDecl {
        decl_name: name.to_string(),
        param_names,
        returns_wasm_heap: true,
        instrs,
    }))
}

fn first_non_main_lowering_issue(
    module: &Module,
    allow_safe_handler_lowering: bool,
) -> Result<Option<GeneralLowerUnsupportedReason>, CodegenError> {
    let stdlib_resolver = StdlibResolver::new(resolve_stdlib_root());
    let stdlib_export_map = build_stdlib_export_map(module, &stdlib_resolver);

    let mut known_decls: HashSet<String> = module
        .declarations
        .iter()
        .filter(|d| d.name != "main")
        .map(|d| d.name.clone())
        .collect();
    known_decls.extend(stdlib_export_map.keys().cloned());

    let mut lambda_decls: Vec<LambdaAuxDecl> = Vec::new();
    for goby_decl in module.declarations.iter().filter(|d| d.name != "main") {
        let Ok(aux_ir_decl) = goby_core::ir_lower::lower_declaration(goby_decl) else {
            return Ok(Some(
                GeneralLowerUnsupportedReason::UserDeclIrLoweringFailed,
            ));
        };
        let param_names: Vec<String> = aux_ir_decl.params.iter().map(|(n, _)| n.clone()).collect();
        match lower_aux_decl(
            &aux_ir_decl.name,
            param_names,
            goby_decl.type_annotation.as_deref(),
            &aux_ir_decl.body,
            &known_decls,
            allow_safe_handler_lowering,
            &mut lambda_decls,
        )? {
            Ok(_) => {}
            Err(reason) => return Ok(Some(reason)),
        }
    }

    Ok(None)
}

fn main_lowering_issue(module: &Module) -> Option<GeneralLowerUnsupportedReason> {
    let main_decl = module
        .declarations
        .iter()
        .find(|decl| decl.name == "main")?;
    match goby_core::ir_lower::lower_declaration(main_decl) {
        Ok(_) => None,
        Err(err) => Some(GeneralLowerUnsupportedReason::UnsupportedIrForm { node: err.message }),
    }
}

/// Recursively collect all `DeclCall` and `PushFuncHandle` target names from a flat
/// backend-IR instruction list, traversing nested instruction vecs.
fn collect_decl_call_names(instrs: &[backend_ir::WasmBackendInstr], out: &mut HashSet<String>) {
    for instr in instrs {
        match instr {
            backend_ir::WasmBackendInstr::DeclCall { decl_name }
            | backend_ir::WasmBackendInstr::TailDeclCall { decl_name }
            | backend_ir::WasmBackendInstr::PushFuncHandle { decl_name } => {
                out.insert(decl_name.clone());
            }
            backend_ir::WasmBackendInstr::If {
                then_instrs,
                else_instrs,
            } => {
                collect_decl_call_names(then_instrs, out);
                collect_decl_call_names(else_instrs, out);
            }
            backend_ir::WasmBackendInstr::Loop { body_instrs } => {
                collect_decl_call_names(body_instrs, out);
            }
            backend_ir::WasmBackendInstr::CaseMatch { arms, .. } => {
                for arm in arms {
                    collect_decl_call_names(&arm.body_instrs, out);
                }
            }
            backend_ir::WasmBackendInstr::ListLit { element_instrs }
            | backend_ir::WasmBackendInstr::TupleLit { element_instrs } => {
                for elem in element_instrs {
                    collect_decl_call_names(elem, out);
                }
            }
            backend_ir::WasmBackendInstr::RecordLit { field_instrs, .. } => {
                for field in field_instrs {
                    collect_decl_call_names(field, out);
                }
            }
            backend_ir::WasmBackendInstr::ListReverseFoldPrepend {
                list_instrs,
                prefix_element_instrs,
                ..
            } => {
                collect_decl_call_names(list_instrs, out);
                for elem in prefix_element_instrs {
                    collect_decl_call_names(elem, out);
                }
            }
            _ => {}
        }
    }
}

/// Build a map from exported name → (module_path, `Declaration`) for all stdlib modules
/// imported by `module`. This covers transitive imports of stdlib modules that
/// transitively import other stdlib modules.
fn build_stdlib_export_map(
    module: &Module,
    resolver: &StdlibResolver,
) -> HashMap<String, (String, goby_core::ast::Declaration)> {
    let mut export_map: HashMap<String, (String, goby_core::ast::Declaration)> = HashMap::new();
    let mut visited: HashSet<String> = HashSet::new();

    // Process the user's directly-imported stdlib modules first (in declaration order) so that
    // name conflicts between multiple stdlib modules resolve in favour of the module the user
    // explicitly imported.  Transitive imports are collected in a separate pending list and
    // processed afterwards; `or_insert_with` ensures they do not override direct imports.
    let direct_paths: Vec<String> = effective_runtime_imports(module)
        .into_iter()
        .map(|imp| imp.module_path)
        .collect();
    let mut transitive_pending: Vec<String> = Vec::new();

    for path in &direct_paths {
        if visited.contains(path) {
            continue;
        }
        visited.insert(path.clone());
        let Ok(resolved) = resolver.resolve_module(path) else {
            continue;
        };
        for imp in &resolved.module.imports {
            if !visited.contains(&imp.module_path) {
                transitive_pending.push(imp.module_path.clone());
            }
        }
        for decl in resolved.module.declarations {
            export_map
                .entry(decl.name.clone())
                .or_insert_with(|| (path.clone(), decl));
        }
    }

    // Now process transitive imports (BFS).
    while let Some(path) = transitive_pending.pop() {
        if visited.contains(&path) {
            continue;
        }
        visited.insert(path.clone());
        let Ok(resolved) = resolver.resolve_module(&path) else {
            continue;
        };
        for imp in &resolved.module.imports {
            if !visited.contains(&imp.module_path) {
                transitive_pending.push(imp.module_path.clone());
            }
        }
        for decl in resolved.module.declarations {
            // Transitive imports must not override names already registered from direct imports.
            export_map
                .entry(decl.name.clone())
                .or_insert_with(|| (path.clone(), decl));
        }
    }

    export_map
}

/// Alias for the inner result of [`lower_module_to_instrs`].
///
/// `Ok((instrs, aux))` = lowering succeeded.
/// `Err(reason)` = lowering is not applicable or encountered an unsupported form.
type LowerModuleResult =
    Result<(Vec<backend_ir::WasmBackendInstr>, Vec<AuxDecl>), GeneralLowerUnsupportedReason>;

/// Attempt to lower a module's `main` body through the general lowering path.
///
/// Returns `Ok(Ok((main_instrs, aux_decls)))` when the general path succeeds,
/// `Ok(Err(reason))` when the IR contains unsupported forms (fall through to next path),
/// or `Err(CodegenError)` on hard codegen failures.
pub(crate) fn lower_module_to_instrs(module: &Module) -> Result<LowerModuleResult, CodegenError> {
    let handler_legality = analyze_module_handler_legality(module)?;
    let allow_safe_handler_lowering = handler_legality.all_one_shot_tail_resumptive();
    let ir_module = match goby_core::ir_lower::lower_module(module) {
        Ok(ir_module) => ir_module,
        Err(_) => {
            if let Some(reason) =
                first_non_main_lowering_issue(module, allow_safe_handler_lowering)?
            {
                return Ok(Err(reason));
            }
            if let Some(reason) = main_lowering_issue(module) {
                return Ok(Err(reason));
            }
            return Ok(Err(GeneralLowerUnsupportedReason::NoIrDecl));
        }
    };
    let Some(ir_decl) = ir_module.decls.iter().find(|decl| decl.name == "main") else {
        return Ok(Err(GeneralLowerUnsupportedReason::NoMainExecPlan));
    };
    let mut fold_prepend_callback_shapes: HashMap<String, FoldPrependCallbackShape> = ir_module
        .decls
        .iter()
        .filter_map(|decl| {
            fold_prepend_callback_shape(decl).map(|shape| (decl.name.clone(), shape))
        })
        .collect();
    let Some(main_body) =
        rewrite_safe_handlers_if_present(&ir_decl.body, allow_safe_handler_lowering)?
    else {
        return Ok(Err(GeneralLowerUnsupportedReason::HandlerRewriteFailed));
    };
    let main_body = rewrite_comp_fold_prepend_callbacks(
        &main_body,
        &fold_prepend_callback_shapes,
        &HashMap::new(),
    );

    let main_is_handler_only_candidate =
        !has_runtime_read_effect(&ir_decl.body) && has_handler_rewrite_entrypoints(&ir_decl.body);
    if main_is_handler_only_candidate
        && (contains_future_handler_intrinsics(&ir_decl.body)
            || has_effectful_non_main_decl(module)
            || module.declarations.iter().any(|decl| decl.name != "main"))
    {
        return Ok(Err(GeneralLowerUnsupportedReason::HandlerOnlyConflict));
    }

    // Gate: only enter GeneralLowered for programs that require runtime capabilities.
    // A program qualifies when it has:
    //   - a runtime Read effect, OR
    //   - safe handler constructs, OR
    //   - a Lambda expression, OR
    //   - a tuple member projection (TupleProject; native evaluator does not support it), OR
    //   - a rooted list update (`AssignIndex`), which the fallback evaluator does not support.
    //   - non-main user-defined declarations (helpers may use constructs the fallback cannot run).
    // Pure-Print programs without any of the above can stay on the simpler paths.
    let has_non_main_user_decls = ir_module.decls.iter().any(|d| d.name != "main");
    if !has_runtime_read_effect(&ir_decl.body)
        && !has_handler_rewrite_entrypoints(&ir_decl.body)
        && !has_lambda_in_comp(&ir_decl.body)
        && !has_rooted_list_update(&ir_decl.body)
        && !has_tuple_project_in_comp(&ir_decl.body)
        && !has_non_main_user_decls
    {
        return Ok(Err(
            GeneralLowerUnsupportedReason::NotRequiringRuntimeCapability,
        ));
    }

    // Resolve stdlib exports once; reused for both known_decls population and the
    // transitive-closure loop below.
    let stdlib_resolver = StdlibResolver::new(resolve_stdlib_root());
    let stdlib_export_map = build_stdlib_export_map(module, &stdlib_resolver);

    // Collect names of all non-main top-level declarations so callee `Var(name)` can be
    // recognised as a DeclCall during lowering.
    let mut known_decls: std::collections::HashSet<String> = module
        .declarations
        .iter()
        .filter(|d| d.name != "main")
        .map(|d| d.name.clone())
        .collect();
    // Add stdlib-exported names to known_decls so user-written helpers that call stdlib
    // functions via bare names are lowered correctly. Intrinsic-owned stdlib calls are
    // resolved earlier in lowering, so they no longer need to be excluded here.
    known_decls.extend(stdlib_export_map.keys().cloned());
    // Lambda auxiliary declarations collected during main and aux-decl lowering.
    // They are appended AFTER user aux_decls so existing table slot indices remain stable.
    let mut lambda_decls: Vec<LambdaAuxDecl> = Vec::new();

    let mut main_instrs =
        match lower::lower_comp_collecting_lambdas(&main_body, &known_decls, &mut lambda_decls) {
            Ok(i) => i,
            Err(lower::LowerError::UnsupportedForm { node }) => {
                return Ok(Err(GeneralLowerUnsupportedReason::UnsupportedIrForm {
                    node,
                }));
            }
            Err(lower::LowerError::IntOutOfRange(n)) => {
                return Err(CodegenError {
                    message: format!("integer {n} is outside the 60-bit representable range"),
                });
            }
        };
    // `main` is emitted as Wasm `_start` with result type `() -> ()`.
    // The shared IR/body still yields a final Goby value, so the general lowering
    // path must explicitly discard it before emission.
    main_instrs.push(backend_ir::WasmBackendInstr::Drop);
    if !read_line_instrs_are_supported(&main_instrs) {
        return Ok(Err(
            GeneralLowerUnsupportedReason::ReadLineInstructionsNotSupported,
        ));
    }

    // Lower auxiliary declarations (non-main top-level functions).
    // These do not need a Read effect; they are helpers called by main.
    let mut aux_decls = Vec::new();
    for aux_ir_decl in ir_module.decls.iter().filter(|decl| decl.name != "main") {
        let mut param_names: Vec<String> =
            aux_ir_decl.params.iter().map(|(n, _)| n.clone()).collect();
        let source_decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == aux_ir_decl.name)
            .expect("source decl for lowered user aux must exist");
        // A declaration with no explicit params is still called with a synthetic Unit arg in the IR
        // (e.g. `helper()` lowers to `Call { args: [Unit] }`).  The Wasm function must accept that
        // i64 so the caller-pushed Unit does not leak on the operand stack.
        if param_names.is_empty() {
            param_names.push("_unit".to_string());
        }
        let rewritten_body = rewrite_comp_fold_prepend_callbacks(
            &aux_ir_decl.body,
            &fold_prepend_callback_shapes,
            &HashMap::new(),
        );
        match lower_aux_decl(
            &aux_ir_decl.name,
            param_names,
            source_decl.type_annotation.as_deref(),
            &rewritten_body,
            &known_decls,
            allow_safe_handler_lowering,
            &mut lambda_decls,
        )? {
            Ok(mut aux) => {
                aux.returns_wasm_heap =
                    decl_return_type_uses_wasm_heap(source_decl.type_annotation.as_deref());
                aux_decls.push(aux);
            }
            Err(reason) => return Ok(Err(reason)),
        }
    }

    // Resolve stdlib functions referenced by DeclCall/PushFuncHandle in main and user aux_decls.
    // Imported stdlib functions are not in module.declarations, so we must load them here.
    // We iterate to fixpoint: each new stdlib decl may itself call other stdlib decls.
    if !stdlib_export_map.is_empty() {
        let export_map = &stdlib_export_map;

        // Track names whose AuxDecl has been added (not just "known as a potential callee").
        // User-declared non-main functions are already represented in aux_decls.
        let mut aux_added: HashSet<String> = module
            .declarations
            .iter()
            .filter(|d| d.name != "main")
            .map(|d| d.name.clone())
            .collect();

        loop {
            // Collect all DeclCall/PushFuncHandle names across main + current aux_decls +
            // lambda_decls (lambdas captured during lowering may themselves call stdlib fns).
            let mut referenced: HashSet<String> = HashSet::new();
            collect_decl_call_names(&main_instrs, &mut referenced);
            for aux in &aux_decls {
                collect_decl_call_names(&aux.instrs, &mut referenced);
            }
            for lam in &lambda_decls {
                collect_decl_call_names(&lam.instrs, &mut referenced);
            }
            // Find stdlib-exported names referenced but not yet added to aux_decls.
            let unresolved: Vec<String> = referenced
                .into_iter()
                .filter(|name| !aux_added.contains(name) && export_map.contains_key(name))
                .collect();
            if unresolved.is_empty() {
                break;
            }

            let mut added_any = false;
            for name in &unresolved {
                let Some((_, goby_decl)) = export_map.get(name) else {
                    aux_added.insert(name.clone());
                    continue;
                };
                let aux_ir_decl = match goby_core::ir_lower::lower_declaration(goby_decl) {
                    Ok(d) => d,
                    Err(_) => {
                        return Ok(Err(
                            GeneralLowerUnsupportedReason::StdlibDeclIrLoweringFailed,
                        ));
                    }
                };
                if let Some(shape) = fold_prepend_callback_shape(&aux_ir_decl) {
                    fold_prepend_callback_shapes.insert(aux_ir_decl.name.clone(), shape);
                }
                let param_names: Vec<String> =
                    aux_ir_decl.params.iter().map(|(n, _)| n.clone()).collect();
                let rewritten_body = rewrite_comp_fold_prepend_callbacks(
                    &aux_ir_decl.body,
                    &fold_prepend_callback_shapes,
                    &HashMap::new(),
                );
                // Stdlib functions use safe (one-shot tail-resumptive) handlers; always
                // allow handler lowering for stdlib aux decls regardless of whether the
                // user's main body has handlers.
                match lower_aux_decl(
                    &aux_ir_decl.name,
                    param_names,
                    goby_decl.type_annotation.as_deref(),
                    &rewritten_body,
                    &known_decls,
                    true, // stdlib handlers are one-shot tail-resumptive by construction
                    &mut lambda_decls,
                )? {
                    Ok(mut aux) => {
                        aux.returns_wasm_heap =
                            decl_return_type_uses_wasm_heap(goby_decl.type_annotation.as_deref());
                        aux_decls.push(aux);
                        aux_added.insert(name.clone());
                        added_any = true;
                    }
                    Err(reason) => return Ok(Err(reason)),
                }
            }
            if !added_any {
                // All unresolved names were non-stdlib or couldn't be lowered.
                break;
            }
        }
    }

    // Convert lambda decls to AuxDecl and append AFTER user aux_decls
    // so that user-decl funcref table slot indices remain stable.
    for lam in lambda_decls {
        aux_decls.push(AuxDecl {
            decl_name: lam.decl_name,
            param_names: lam.param_names,
            returns_wasm_heap: true,
            instrs: lam.instrs,
        });
    }

    Ok(Ok((main_instrs, aux_decls)))
}

pub(crate) fn supports_general_lower_module(
    module: &Module,
) -> Result<Option<GeneralLowerUnsupportedReason>, CodegenError> {
    let (instrs, aux_decls) = match lower_module_to_instrs(module)? {
        Ok(pair) => pair,
        Err(reason) => return Ok(Some(reason)),
    };
    let aux_supported = aux_decls.iter().all(|d| emit::supports_instrs(&d.instrs));
    if emit::supports_instrs(&instrs) && aux_supported {
        Ok(None)
    } else {
        // emit::supports_instrs currently always returns true, so this branch is unreachable
        // in practice. It exists as a forward-compatibility guard for when supports_instrs
        // gains real instruction filtering.
        Ok(Some(GeneralLowerUnsupportedReason::AuxDeclNotSupported))
    }
}

pub(crate) fn try_general_lower_module(module: &Module) -> Result<Option<Vec<u8>>, CodegenError> {
    try_general_lower_module_with_options(module, emit::EmitOptions::default())
}

fn try_general_lower_module_with_options(
    module: &Module,
    options: emit::EmitOptions,
) -> Result<Option<Vec<u8>>, CodegenError> {
    let (instrs, aux_decls) = match lower_module_to_instrs(module)? {
        Ok(pair) => pair,
        Err(_) => return Ok(None), // reason is discarded; try_ callers only care about success
    };
    let aux_supported = aux_decls.iter().all(|d| emit::supports_instrs(&d.instrs));
    if !emit::supports_instrs(&instrs) || !aux_supported {
        return Ok(None);
    }
    let layout = MemoryLayout::default();
    let wasm =
        emit::emit_general_module_with_aux_and_options(&instrs, &aux_decls, &layout, options)?;
    Ok(Some(wasm))
}

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::*;
    use crate::gen_lower::backend_ir::WasmBackendInstr;
    use crate::gen_lower::emit::{EffectEmitStrategy, EmitOptions};
    use crate::memory_config::{RUNTIME_MEMORY_CONFIG, TEST_MEMORY_CONFIG};

    fn assert_strategy_parity(module: &goby_core::Module) {
        let direct = try_general_lower_module_with_options(
            module,
            EmitOptions {
                effect_emit_strategy: EffectEmitStrategy::Wb3DirectCall,
                memory_config: TEST_MEMORY_CONFIG,
            },
        )
        .expect("direct-call lowering should not error")
        .expect("direct-call general lowering should apply");
        let wasmfx = try_general_lower_module_with_options(
            module,
            EmitOptions {
                effect_emit_strategy: EffectEmitStrategy::Wb3BWasmFxExperimental,
                memory_config: TEST_MEMORY_CONFIG,
            },
        )
        .expect("future handler-path lowering should not error")
        .expect("future handler-path general lowering should apply");
        assert_eq!(
            wasmfx, direct,
            "general lowering should stay byte-identical across emit strategies until WasmFX opcode support lands"
        );
    }

    fn assert_default_strategy_matches_selected_strategy(module: &goby_core::Module) {
        let expected_strategy = if cfg!(feature = "wasmfx-experimental") {
            EffectEmitStrategy::Wb3BWasmFxExperimental
        } else {
            EffectEmitStrategy::Wb3DirectCall
        };
        let default = try_general_lower_module(module)
            .expect("default lowering should not error")
            .expect("default general lowering should apply");
        let explicit = try_general_lower_module_with_options(
            module,
            EmitOptions {
                effect_emit_strategy: expected_strategy,
                memory_config: RUNTIME_MEMORY_CONFIG,
            },
        )
        .expect("explicit lowering should not error")
        .expect("explicit general lowering should apply");
        assert_eq!(
            default, explicit,
            "default general-lowering path should match the feature-selected explicit strategy"
        );
    }

    #[test]
    fn general_lowered_main_discards_final_goby_value() {
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  println (read())
"#,
        )
        .expect("source should parse");
        let (instrs, _aux) = lower_module_to_instrs(&module)
            .expect("lowering should not error")
            .expect("general lowering should apply (reason should be Ok)");
        assert!(
            matches!(instrs.last(), Some(WasmBackendInstr::Drop)),
            "general-lowered main must end with Drop so `_start` leaves no stack value: {:?}",
            instrs
        );
    }

    #[test]
    fn safe_handler_only_main_is_a_general_lower_candidate() {
        let module = parse_module(
            r#"
effect Tick
  tick: Unit -> Unit

main : Unit -> Unit can Tick
main =
  with
    tick _ ->
      resume ()
  in
    tick ()
"#,
        )
        .expect("source should parse");

        assert!(
            lower_module_to_instrs(&module)
                .expect("lowering should not error")
                .is_ok(),
            "safe handler-only main should enter general lowering"
        );
    }

    #[test]
    fn safe_handler_only_module_has_emit_strategy_parity() {
        let module = parse_module(
            r#"
effect Tick
  tick: String -> Unit

main : Unit -> Unit can Tick, Print
main =
  with
    tick value ->
      print value
      resume ()
  in
    tick "a"
"#,
        )
        .expect("source should parse");

        assert_strategy_parity(&module);
    }

    #[test]
    fn safe_handler_only_module_default_strategy_matches_feature_selection() {
        let module = parse_module(
            r#"
effect Tick
  tick: String -> Unit

main : Unit -> Unit can Tick, Print
main =
  with
    tick value ->
      print value
      resume ()
  in
    tick "a"
"#,
        )
        .expect("source should parse");

        assert_default_strategy_matches_selected_strategy(&module);
    }

    #[test]
    fn helper_decl_read_module_has_emit_strategy_parity() {
        let module = parse_module(
            r#"
greet : String -> Unit can Print
greet name =
  println "hello ${name}"

main : Unit -> Unit can Print, Read
main =
  input = read()
  greet input
"#,
        )
        .expect("source should parse");

        assert_strategy_parity(&module);
    }

    #[test]
    fn helper_decl_read_module_default_strategy_matches_feature_selection() {
        let module = parse_module(
            r#"
greet : String -> Unit can Print
greet name =
  println "hello ${name}"

main : Unit -> Unit can Print, Read
main =
  input = read()
  greet input
"#,
        )
        .expect("source should parse");

        assert_default_strategy_matches_selected_strategy(&module);
    }

    // supports_general_lower_module reason plumbing tests

    #[test]
    fn supports_general_lower_module_returns_none_for_supported_read_tuple_program() {
        // A Read+tuple program is fully supported; expects None (no unsupported reason).
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  pair = (1, 2)
  println "${pair.0}"
"#,
        )
        .expect("source should parse");
        let reason =
            supports_general_lower_module(&module).expect("classification should not error");
        assert!(
            reason.is_none(),
            "Read+tuple program must be fully supported (got: {:?})",
            reason
        );
    }

    #[test]
    fn supports_general_lower_module_returns_reason_for_pure_print_program() {
        // Pure-Print programs don't need general lowering; expects NotRequiringRuntimeCapability.
        let module = parse_module(
            r#"
main : Unit -> Unit
main =
  print "hello"
"#,
        )
        .expect("source should parse");
        let reason =
            supports_general_lower_module(&module).expect("classification should not error");
        assert_eq!(
            reason,
            Some(GeneralLowerUnsupportedReason::NotRequiringRuntimeCapability),
            "pure-Print program must return NotRequiringRuntimeCapability"
        );
    }

    #[test]
    fn supports_general_lower_module_accepts_by_value_capturing_lambda() {
        // A lambda that captures an outer variable ByValue is supported at the
        // IR lowering level. The module should be classified as supported (None).
        let module = parse_module(
            r#"
import goby/list ( map )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  prefix = "hello "
  result = map ["world"] (fn s -> "${prefix}${s}")
  println result[0]
"#,
        )
        .expect("source should parse");
        let reason =
            supports_general_lower_module(&module).expect("classification should not error");
        assert!(
            reason.is_none(),
            "ByValue-capturing lambda should be supported, got: {:?}",
            reason
        );
    }

    #[test]
    fn supports_general_lower_module_accepts_rooted_list_update_without_read() {
        let module = parse_module(
            r#"
main : Unit -> Unit can Print
main =
  mut xs = [[1, 2], [3, 4]]
  xs[0][1] := 99
  println "${xs[0][1]}"
"#,
        )
        .expect("source should parse");
        let reason =
            supports_general_lower_module(&module).expect("classification should not error");
        assert!(
            reason.is_none(),
            "rooted list update should require GeneralLowered support, got: {:?}",
            reason
        );
    }

    #[test]
    fn supports_general_lower_module_reports_specific_main_lowering_reason() {
        let module = parse_module(
            r#"
add_one : Int -> Int
add_one x = x + 1

main : Unit -> Unit can Print, Read
main =
  _ = read()
  println "${add_one 5}"
"#,
        )
        .expect("source should parse");
        let reason =
            supports_general_lower_module(&module).expect("classification should not error");
        assert_eq!(
            reason,
            Some(GeneralLowerUnsupportedReason::UnsupportedIrForm {
                node: "interpolated string contains a call expression; bind the result to a local before interpolation".to_string(),
            })
        );
    }
}
