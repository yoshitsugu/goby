//! Goby IR → `WasmBackendInstr` lowering for the general Wasm path.
//!
//! The IR → backend-IR mapping is documented in `backend_ir.rs`.

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU32, Ordering as AtomicOrdering};

use goby_core::closure_capture::{
    BindingRepr, CallableEnv, CallableEnvSlotKind, ClosureBindingEnv,
    analyze_lambda_callable_env_for_params, binding_repr_for_let_mut,
};
use goby_core::ir::{CompExpr, IrBinOp, IrInterpPart, ValueExpr};

use crate::gen_lower::backend_ir::{
    BackendEffectOp, BackendIntrinsic, BackendPrintOp, BackendReadOp, WasmBackendInstr,
};
use crate::gen_lower::value::{ValueError, encode_bool, encode_int, encode_unit};

/// Counter for generating unique lambda auxiliary declaration names (`__lambda_N`).
static LAMBDA_COUNTER: AtomicU32 = AtomicU32::new(0);
static RR4_FOLD_PREPEND_COUNTER: AtomicU32 = AtomicU32::new(0);

fn flatten_lambda_chain<'a>(param: &'a str, body: &'a CompExpr) -> (Vec<String>, &'a CompExpr) {
    let mut params = vec![param.to_string()];
    let mut current = body;
    while let CompExpr::Value(ValueExpr::Lambda { param, body }) = current {
        params.push(param.clone());
        current = body;
    }
    (params, current)
}

/// An auxiliary Wasm function produced by lifting a `ValueExpr::Lambda` out of an expression.
///
/// Lambda bodies are emitted as named top-level Wasm functions and referenced via
/// `PushFuncHandle { decl_name }` or `CreateClosure` at the use site.
///
/// # Parameter conventions
/// - Zero-capture lambda: `param_names = [original_param]` → Wasm sig `(i64) -> i64`.
/// - Capturing lambda:    `param_names = ["__clo", original_param]` → Wasm sig `(i64, i64) -> i64`.
///   `"__clo"` is Wasm local 0 and carries the TAG_CLOSURE-tagged closure record pointer.
#[derive(Debug, Clone)]
pub(crate) struct LambdaAuxDecl {
    /// Unique name for this lambda function (e.g. `__lambda_0`).
    pub(crate) decl_name: String,
    /// Ordered parameter names.  Zero-capture: `[param]`.  Capturing: `["__clo", param]`.
    pub(crate) param_names: Vec<String>,
    /// Closure capture environment.  Empty for zero-capture lambdas.
    pub(crate) env: CallableEnv,
    /// Lowered body instructions.
    pub(crate) instrs: Vec<WasmBackendInstr>,
}

/// Error produced when the general lowering path encounters an unsupported IR node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum LowerError {
    /// IR node that the general lowering path does not yet support.
    UnsupportedForm { node: String },
    /// Integer literal outside the 60-bit representable range.
    IntOutOfRange(i64),
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LowerError::UnsupportedForm { node } => {
                write!(f, "unsupported IR form in general lowering path: {node}")
            }
            LowerError::IntOutOfRange(n) => {
                write!(f, "integer {n} is outside the 60-bit representable range")
            }
        }
    }
}

impl From<ValueError> for LowerError {
    fn from(e: ValueError) -> Self {
        match e {
            ValueError::IntOutOfRange(n) => LowerError::IntOutOfRange(n),
        }
    }
}

/// Lower a `CompExpr` to a flat sequence of `WasmBackendInstr`.
///
/// Supported IR nodes: `Value`, `Let`, `Seq`, `PerformEffect`, `Call` (GlobalRef callee only).
/// All other nodes produce `Err(LowerError::UnsupportedForm)`.
///
/// **Note:** This function does not return lifted `LambdaAuxDecl`s; if the IR contains a Lambda
/// the call will produce a `PushFuncHandle` for an unregistered name, which would cause an emit
/// error.  Use `lower_comp_collecting_lambdas` when Lambda expressions may be present.
pub(crate) fn lower_comp(comp: &CompExpr) -> Result<Vec<WasmBackendInstr>, LowerError> {
    let mut lambda_decls = Vec::new();
    let bindings = ClosureBindingEnv::default();
    let result = lower_comp_inner(
        comp,
        true,
        &HashMap::new(),
        &bindings,
        &HashSet::new(),
        &mut lambda_decls,
    )?;
    debug_assert!(
        lambda_decls.is_empty(),
        "lower_comp: Lambda encountered but LambdaAuxDecls were not collected — use \
         lower_comp_collecting_lambdas instead"
    );
    Ok(result)
}

/// Like `lower_comp` but with a set of known top-level declaration names.
///
/// A `Call { callee: Var(name), .. }` where `name` is in `known_decls` is lowered
/// as `DeclCall { decl_name: name }` rather than `UnsupportedForm`.
///
/// **Note:** Lambda expressions in `comp` will produce `PushFuncHandle` references for
/// unregistered names.  Use `lower_comp_collecting_lambdas` if Lambda may be present.
pub(crate) fn lower_comp_with_decls(
    comp: &CompExpr,
    known_decls: &HashSet<String>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    let mut lambda_decls = Vec::new();
    let bindings = ClosureBindingEnv::default();
    let result = lower_comp_inner(
        comp,
        true,
        &HashMap::new(),
        &bindings,
        known_decls,
        &mut lambda_decls,
    )?;
    debug_assert!(
        lambda_decls.is_empty(),
        "lower_comp_with_decls: Lambda encountered but LambdaAuxDecls were not collected — use \
         lower_comp_collecting_lambdas instead"
    );
    Ok(result)
}

/// Like `lower_comp_with_decls` but also collects lambda auxiliary declarations.
///
/// Lambda expressions encountered during lowering are lifted out as `LambdaAuxDecl` entries
/// and appended to `lambda_decls`. The caller is responsible for registering these as
/// Wasm functions in the output module.
pub(crate) fn lower_comp_collecting_lambdas(
    comp: &CompExpr,
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    let bindings = ClosureBindingEnv::default();
    lower_comp_inner(
        comp,
        true,
        &HashMap::new(),
        &bindings,
        known_decls,
        lambda_decls,
    )
}

pub(crate) fn lower_comp_collecting_lambdas_with_params(
    comp: &CompExpr,
    decl_params: &[String],
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    let bindings = ClosureBindingEnv::with_decl_params(decl_params.iter().map(String::as_str));
    lower_comp_inner(
        comp,
        true,
        &HashMap::new(),
        &bindings,
        known_decls,
        lambda_decls,
    )
}

pub(crate) fn lower_supported_self_recursive_int_scan(
    decl_name: &str,
    comp: &CompExpr,
    decl_params: &[String],
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Option<Vec<WasmBackendInstr>>, LowerError> {
    let bindings = ClosureBindingEnv::with_decl_params(decl_params.iter().map(String::as_str));
    let aliases = HashMap::new();
    let mut decls = HashSet::new();
    let mut recur_temp_counter = 0usize;
    let acc_local = "__rr3_scan_acc".to_string();
    decls.insert(acc_local.clone());

    let Some(body_instrs) = lower_self_recursive_int_scan_case(
        decl_name,
        comp,
        decl_params,
        &acc_local,
        0,
        &aliases,
        &bindings,
        known_decls,
        lambda_decls,
        &mut decls,
        &mut recur_temp_counter,
    )?
    else {
        return Ok(None);
    };

    let mut decl_instrs: Vec<WasmBackendInstr> = decls
        .into_iter()
        .map(|name| WasmBackendInstr::DeclareLocal { name })
        .collect();
    decl_instrs.sort_by(|left, right| match (left, right) {
        (
            WasmBackendInstr::DeclareLocal { name: left },
            WasmBackendInstr::DeclareLocal { name: right },
        ) => left.cmp(right),
        _ => std::cmp::Ordering::Equal,
    });
    decl_instrs.push(WasmBackendInstr::I64Const(encode_int(0)?));
    decl_instrs.push(WasmBackendInstr::StoreLocal {
        name: acc_local.clone(),
    });
    decl_instrs.push(WasmBackendInstr::Loop { body_instrs });
    Ok(Some(decl_instrs))
}

pub(crate) fn lower_supported_self_recursive_list_spread_builder(
    decl_name: &str,
    comp: &CompExpr,
    decl_params: &[String],
    known_decls: &HashSet<String>,
    _lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Option<Vec<WasmBackendInstr>>, LowerError> {
    let CompExpr::If { cond, then_, else_ } = comp else {
        return Ok(None);
    };
    if !matches_empty_list_comp(then_) {
        return Ok(None);
    }
    let CompExpr::Let {
        name, value, body, ..
    } = else_.as_ref()
    else {
        return Ok(None);
    };
    let CompExpr::Call { callee, args } = value.as_ref() else {
        return Ok(None);
    };
    if !is_self_decl_callee(callee, decl_name) || args.len() != decl_params.len() {
        return Ok(None);
    }
    let CompExpr::Value(ValueExpr::ListLit { elements, spread }) = body.as_ref() else {
        return Ok(None);
    };
    let Some(spread) = spread.as_ref() else {
        return Ok(None);
    };
    let ValueExpr::Var(spread_name) = spread.as_ref() else {
        return Ok(None);
    };
    if spread_name != name {
        return Ok(None);
    }

    let bindings = ClosureBindingEnv::with_decl_params(decl_params.iter().map(String::as_str));
    let mut decls: HashSet<String> = [
        "__rr4_list_header_ptr".to_string(),
        "__rr4_list_header_cap".to_string(),
        "__rr4_list_n_chunks".to_string(),
        "__rr4_list_chunk_ptr".to_string(),
        "__rr4_list_total_len".to_string(),
    ]
    .into();
    let mut recur_temp_counter = 0usize;
    let body_instrs = lower_self_recursive_list_spread_loop_body(
        cond,
        elements,
        decl_params,
        args,
        &bindings,
        known_decls,
        &mut decls,
        &mut recur_temp_counter,
    )?;

    let mut decl_instrs: Vec<WasmBackendInstr> = decls
        .into_iter()
        .map(|name| WasmBackendInstr::DeclareLocal { name })
        .collect();
    decl_instrs.sort_by(|left, right| match (left, right) {
        (
            WasmBackendInstr::DeclareLocal { name: left },
            WasmBackendInstr::DeclareLocal { name: right },
        ) => left.cmp(right),
        _ => std::cmp::Ordering::Equal,
    });
    decl_instrs.push(WasmBackendInstr::ListBuilderNew {
        header_ptr_local: "__rr4_list_header_ptr".to_string(),
        header_cap_local: "__rr4_list_header_cap".to_string(),
        n_chunks_local: "__rr4_list_n_chunks".to_string(),
        chunk_ptr_local: "__rr4_list_chunk_ptr".to_string(),
        total_len_local: "__rr4_list_total_len".to_string(),
        initial_capacity: 4,
    });
    decl_instrs.push(WasmBackendInstr::Loop { body_instrs });
    Ok(Some(decl_instrs))
}

fn matches_empty_list_value(value: &ValueExpr) -> bool {
    matches!(
        value,
        ValueExpr::ListLit {
            elements,
            spread: None
        } if elements.is_empty()
    )
}

fn value_expr_mentions_var(value: &ValueExpr, target: &str) -> bool {
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
                .any(|elem| value_expr_mentions_var(elem, target))
                || spread
                    .as_deref()
                    .is_some_and(|tail| value_expr_mentions_var(tail, target))
        }
        ValueExpr::TupleLit(items) => items
            .iter()
            .any(|item| value_expr_mentions_var(item, target)),
        ValueExpr::RecordLit { fields, .. } => fields
            .iter()
            .any(|(_, value)| value_expr_mentions_var(value, target)),
        ValueExpr::Lambda { param, body } => {
            if param == target {
                false
            } else {
                comp_expr_mentions_var(body, target)
            }
        }
        ValueExpr::Interp(parts) => parts.iter().any(|part| match part {
            IrInterpPart::Text(_) => false,
            IrInterpPart::Expr(value) => value_expr_mentions_var(value, target),
        }),
        ValueExpr::BinOp { left, right, .. } => {
            value_expr_mentions_var(left, target) || value_expr_mentions_var(right, target)
        }
        ValueExpr::TupleProject { tuple, .. } => value_expr_mentions_var(tuple, target),
        ValueExpr::ListGet { list, index } => {
            value_expr_mentions_var(list, target) || value_expr_mentions_var(index, target)
        }
    }
}

fn comp_expr_mentions_var(comp: &CompExpr, target: &str) -> bool {
    match comp {
        CompExpr::Value(value) => value_expr_mentions_var(value, target),
        CompExpr::Let {
            name, value, body, ..
        }
        | CompExpr::LetMut {
            name, value, body, ..
        } => {
            comp_expr_mentions_var(value, target)
                || if name == target {
                    false
                } else {
                    comp_expr_mentions_var(body, target)
                }
        }
        CompExpr::Seq { stmts, tail } => {
            stmts
                .iter()
                .any(|stmt| comp_expr_mentions_var(stmt, target))
                || comp_expr_mentions_var(tail, target)
        }
        CompExpr::If { cond, then_, else_ } => {
            value_expr_mentions_var(cond, target)
                || comp_expr_mentions_var(then_, target)
                || comp_expr_mentions_var(else_, target)
        }
        CompExpr::Call { callee, args } => {
            value_expr_mentions_var(callee, target)
                || args.iter().any(|arg| value_expr_mentions_var(arg, target))
        }
        CompExpr::Assign { name, value } => {
            (name == target) || comp_expr_mentions_var(value, target)
        }
        CompExpr::AssignIndex { root, path, value } => {
            root == target
                || path
                    .iter()
                    .any(|item| value_expr_mentions_var(item, target))
                || comp_expr_mentions_var(value, target)
        }
        CompExpr::Case { scrutinee, arms } => {
            value_expr_mentions_var(scrutinee, target)
                || arms
                    .iter()
                    .any(|arm| comp_expr_mentions_var(&arm.body, target))
        }
        CompExpr::PerformEffect { args, .. } => {
            args.iter().any(|arg| value_expr_mentions_var(arg, target))
        }
        CompExpr::Handle { .. } | CompExpr::WithHandler { .. } | CompExpr::Resume { .. } => true,
    }
}

fn rename_value_var(value: &ValueExpr, from: &str, to: &str) -> ValueExpr {
    match value {
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => value.clone(),
        ValueExpr::Var(name) => {
            if name == from {
                ValueExpr::Var(to.to_string())
            } else {
                value.clone()
            }
        }
        ValueExpr::ListLit { elements, spread } => ValueExpr::ListLit {
            elements: elements
                .iter()
                .map(|elem| rename_value_var(elem, from, to))
                .collect(),
            spread: spread
                .as_deref()
                .map(|tail| Box::new(rename_value_var(tail, from, to))),
        },
        ValueExpr::TupleLit(items) => ValueExpr::TupleLit(
            items
                .iter()
                .map(|item| rename_value_var(item, from, to))
                .collect(),
        ),
        ValueExpr::RecordLit {
            constructor,
            fields,
        } => ValueExpr::RecordLit {
            constructor: constructor.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| (name.clone(), rename_value_var(value, from, to)))
                .collect(),
        },
        ValueExpr::Lambda { param, body } => ValueExpr::Lambda {
            param: param.clone(),
            body: if param == from {
                body.clone()
            } else {
                Box::new(rename_comp_var(body, from, to))
            },
        },
        ValueExpr::Interp(parts) => ValueExpr::Interp(
            parts
                .iter()
                .map(|part| match part {
                    IrInterpPart::Text(text) => IrInterpPart::Text(text.clone()),
                    IrInterpPart::Expr(value) => {
                        IrInterpPart::Expr(rename_value_var(value, from, to))
                    }
                })
                .collect(),
        ),
        ValueExpr::BinOp { op, left, right } => ValueExpr::BinOp {
            op: op.clone(),
            left: Box::new(rename_value_var(left, from, to)),
            right: Box::new(rename_value_var(right, from, to)),
        },
        ValueExpr::TupleProject { tuple, index } => ValueExpr::TupleProject {
            tuple: Box::new(rename_value_var(tuple, from, to)),
            index: *index,
        },
        ValueExpr::ListGet { list, index } => ValueExpr::ListGet {
            list: Box::new(rename_value_var(list, from, to)),
            index: Box::new(rename_value_var(index, from, to)),
        },
    }
}

fn rename_comp_var(comp: &CompExpr, from: &str, to: &str) -> CompExpr {
    match comp {
        CompExpr::Value(value) => CompExpr::Value(rename_value_var(value, from, to)),
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => CompExpr::Let {
            name: name.clone(),
            ty: ty.clone(),
            value: Box::new(rename_comp_var(value, from, to)),
            body: if name == from {
                body.clone()
            } else {
                Box::new(rename_comp_var(body, from, to))
            },
        },
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => CompExpr::LetMut {
            name: name.clone(),
            ty: ty.clone(),
            value: Box::new(rename_comp_var(value, from, to)),
            body: if name == from {
                body.clone()
            } else {
                Box::new(rename_comp_var(body, from, to))
            },
        },
        CompExpr::Seq { stmts, tail } => CompExpr::Seq {
            stmts: stmts
                .iter()
                .map(|stmt| rename_comp_var(stmt, from, to))
                .collect(),
            tail: Box::new(rename_comp_var(tail, from, to)),
        },
        CompExpr::If { cond, then_, else_ } => CompExpr::If {
            cond: Box::new(rename_value_var(cond, from, to)),
            then_: Box::new(rename_comp_var(then_, from, to)),
            else_: Box::new(rename_comp_var(else_, from, to)),
        },
        CompExpr::Call { callee, args } => CompExpr::Call {
            callee: Box::new(rename_value_var(callee, from, to)),
            args: args
                .iter()
                .map(|arg| rename_value_var(arg, from, to))
                .collect(),
        },
        CompExpr::Assign { name, value } => CompExpr::Assign {
            name: name.clone(),
            value: Box::new(rename_comp_var(value, from, to)),
        },
        CompExpr::AssignIndex { root, path, value } => CompExpr::AssignIndex {
            root: root.clone(),
            path: path
                .iter()
                .map(|item| rename_value_var(item, from, to))
                .collect(),
            value: Box::new(rename_comp_var(value, from, to)),
        },
        CompExpr::Case { scrutinee, arms } => CompExpr::Case {
            scrutinee: Box::new(rename_value_var(scrutinee, from, to)),
            arms: arms
                .iter()
                .map(|arm| goby_core::ir::IrCaseArm {
                    pattern: arm.pattern.clone(),
                    body: rename_comp_var(&arm.body, from, to),
                })
                .collect(),
        },
        CompExpr::PerformEffect { effect, op, args } => CompExpr::PerformEffect {
            effect: effect.clone(),
            op: op.clone(),
            args: args
                .iter()
                .map(|arg| rename_value_var(arg, from, to))
                .collect(),
        },
        CompExpr::Handle { clauses } => CompExpr::Handle {
            clauses: clauses
                .iter()
                .map(|clause| goby_core::ir::IrHandlerClause {
                    op_name: clause.op_name.clone(),
                    params: clause.params.clone(),
                    body: rename_comp_var(&clause.body, from, to),
                })
                .collect(),
        },
        CompExpr::WithHandler { handler, body } => CompExpr::WithHandler {
            handler: handler.clone(),
            body: Box::new(rename_comp_var(body, from, to)),
        },
        CompExpr::Resume { value } => CompExpr::Resume {
            value: Box::new(rename_value_var(value, from, to)),
        },
    }
}

fn lower_supported_inline_list_fold_prepend_builder(
    args: &[ValueExpr],
    aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Option<Vec<WasmBackendInstr>>, LowerError> {
    if args.len() != 3 || !matches_empty_list_value(&args[1]) {
        return Ok(None);
    }
    let ValueExpr::Lambda {
        param: acc_param,
        body,
    } = &args[2]
    else {
        return Ok(None);
    };
    let CompExpr::Value(ValueExpr::Lambda {
        param: item_param,
        body,
    }) = body.as_ref()
    else {
        return Ok(None);
    };
    let CompExpr::Value(ValueExpr::ListLit { elements, spread }) = body.as_ref() else {
        return Ok(None);
    };
    let Some(spread) = spread.as_deref() else {
        return Ok(None);
    };
    let ValueExpr::Var(spread_name) = spread else {
        return Ok(None);
    };
    if spread_name != acc_param {
        return Ok(None);
    }
    if elements
        .iter()
        .any(|elem| value_expr_mentions_var(elem, acc_param))
    {
        return Ok(None);
    }

    let item_local = format!(
        "__rr4_fold_item_{}",
        RR4_FOLD_PREPEND_COUNTER.fetch_add(1, AtomicOrdering::Relaxed)
    );
    let list_instrs = lower_value_as_arg(&args[0], aliases, bindings, known_decls, lambda_decls)?;
    let prefix_element_instrs = elements
        .iter()
        .map(|elem| {
            let renamed = rename_value_var(elem, item_param, &item_local);
            lower_value_ctx(&renamed, aliases, bindings, known_decls)
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Some(vec![
        WasmBackendInstr::DeclareLocal {
            name: item_local.clone(),
        },
        WasmBackendInstr::ListReverseFoldPrepend {
            list_instrs,
            item_local,
            prefix_element_instrs,
        },
    ]))
}

#[allow(clippy::too_many_arguments)]
fn lower_self_recursive_list_spread_loop_body(
    cond: &ValueExpr,
    elements: &[ValueExpr],
    decl_params: &[String],
    next_args: &[ValueExpr],
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    decls: &mut HashSet<String>,
    recur_temp_counter: &mut usize,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    let mut then_instrs = vec![WasmBackendInstr::ListBuilderFinish {
        header_ptr_local: "__rr4_list_header_ptr".to_string(),
        n_chunks_local: "__rr4_list_n_chunks".to_string(),
        total_len_local: "__rr4_list_total_len".to_string(),
    }];

    let mut else_instrs = Vec::new();
    for elem in elements {
        else_instrs.push(WasmBackendInstr::ListBuilderPush {
            header_ptr_local: "__rr4_list_header_ptr".to_string(),
            header_cap_local: "__rr4_list_header_cap".to_string(),
            n_chunks_local: "__rr4_list_n_chunks".to_string(),
            chunk_ptr_local: "__rr4_list_chunk_ptr".to_string(),
            total_len_local: "__rr4_list_total_len".to_string(),
            value_instrs: lower_value_ctx(elem, &HashMap::new(), bindings, known_decls)?,
        });
    }

    let mut temp_names = Vec::with_capacity(decl_params.len());
    for param in decl_params {
        let temp_name = format!("__rr4_next_{}_{}", param, *recur_temp_counter);
        *recur_temp_counter += 1;
        decls.insert(temp_name.clone());
        temp_names.push(temp_name);
    }
    for (arg, temp_name) in next_args.iter().zip(temp_names.iter()) {
        else_instrs.extend(lower_value_ctx(
            arg,
            &HashMap::new(),
            bindings,
            known_decls,
        )?);
        else_instrs.push(WasmBackendInstr::StoreLocal {
            name: temp_name.clone(),
        });
    }
    for (param, temp_name) in decl_params.iter().zip(temp_names.iter()) {
        else_instrs.push(WasmBackendInstr::LoadLocal {
            name: temp_name.clone(),
        });
        else_instrs.push(WasmBackendInstr::StoreLocal {
            name: param.clone(),
        });
    }
    else_instrs.push(WasmBackendInstr::ContinueLoop { relative_depth: 1 });

    let mut instrs = lower_value_ctx(cond, &HashMap::new(), bindings, known_decls)?;
    instrs.push(WasmBackendInstr::If {
        then_instrs: std::mem::take(&mut then_instrs),
        else_instrs,
    });
    Ok(instrs)
}

fn matches_empty_list_comp(comp: &CompExpr) -> bool {
    matches!(
        comp,
        CompExpr::Value(ValueExpr::ListLit {
            elements,
            spread: None
        }) if elements.is_empty()
    )
}

#[allow(clippy::too_many_arguments)]
fn lower_self_recursive_int_scan_case(
    decl_name: &str,
    comp: &CompExpr,
    decl_params: &[String],
    acc_local: &str,
    loop_depth: u32,
    aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
    decls: &mut HashSet<String>,
    recur_temp_counter: &mut usize,
) -> Result<Option<Vec<WasmBackendInstr>>, LowerError> {
    match comp {
        CompExpr::If { cond, then_, else_ } => {
            let Some(then_instrs) = lower_self_recursive_int_scan_case(
                decl_name,
                then_,
                decl_params,
                acc_local,
                loop_depth + 1,
                aliases,
                bindings,
                known_decls,
                lambda_decls,
                decls,
                recur_temp_counter,
            )?
            else {
                return Ok(None);
            };
            let Some(else_instrs) = lower_self_recursive_int_scan_case(
                decl_name,
                else_,
                decl_params,
                acc_local,
                loop_depth + 1,
                aliases,
                bindings,
                known_decls,
                lambda_decls,
                decls,
                recur_temp_counter,
            )?
            else {
                return Ok(None);
            };
            let mut instrs = lower_value_ctx(cond, aliases, bindings, known_decls)?;
            instrs.push(WasmBackendInstr::If {
                then_instrs,
                else_instrs,
            });
            Ok(Some(instrs))
        }
        CompExpr::Let {
            name, value, body, ..
        } => {
            if let Some(recur_instrs) = try_lower_self_recursive_int_scan_recur(
                decl_name,
                decl_params,
                name,
                value,
                body,
                acc_local,
                loop_depth,
                aliases,
                bindings,
                known_decls,
                decls,
                recur_temp_counter,
            )? {
                return Ok(Some(recur_instrs));
            }
            if contains_self_decl_call(value, decl_name) {
                return Ok(None);
            }
            decls.insert(name.clone());
            let value_instrs =
                lower_comp_inner(value, false, aliases, bindings, known_decls, lambda_decls)?;
            let mut instrs = hoist_declare_locals(value_instrs, decls);
            instrs.push(WasmBackendInstr::StoreLocal { name: name.clone() });
            let mut scoped_bindings = bindings.clone();
            scoped_bindings.bind_outer_immutable(name.clone());
            let Some(body_instrs) = lower_self_recursive_int_scan_case(
                decl_name,
                body,
                decl_params,
                acc_local,
                loop_depth,
                aliases,
                &scoped_bindings,
                known_decls,
                lambda_decls,
                decls,
                recur_temp_counter,
            )?
            else {
                return Ok(None);
            };
            instrs.extend(body_instrs);
            Ok(Some(instrs))
        }
        CompExpr::Seq { stmts, tail } => {
            let mut instrs = Vec::new();
            for stmt in stmts {
                if contains_self_decl_call(stmt, decl_name) {
                    return Ok(None);
                }
                let stmt_instrs =
                    lower_comp_inner(stmt, false, aliases, bindings, known_decls, lambda_decls)?;
                instrs.extend(hoist_declare_locals(stmt_instrs, decls));
                instrs.push(WasmBackendInstr::Drop);
            }
            let Some(tail_instrs) = lower_self_recursive_int_scan_case(
                decl_name,
                tail,
                decl_params,
                acc_local,
                loop_depth,
                aliases,
                bindings,
                known_decls,
                lambda_decls,
                decls,
                recur_temp_counter,
            )?
            else {
                return Ok(None);
            };
            instrs.extend(tail_instrs);
            Ok(Some(instrs))
        }
        CompExpr::Call { callee, args } if is_self_decl_callee(callee, decl_name) => {
            lower_self_recursive_int_scan_continue(
                decl_params,
                args,
                None,
                acc_local,
                loop_depth,
                aliases,
                bindings,
                known_decls,
                decls,
                recur_temp_counter,
            )
            .map(Some)
        }
        _ => {
            if contains_self_decl_call(comp, decl_name) {
                return Ok(None);
            }
            let base_instrs =
                lower_comp_inner(comp, false, aliases, bindings, known_decls, lambda_decls)?;
            let mut instrs = vec![WasmBackendInstr::LoadLocal {
                name: acc_local.to_string(),
            }];
            instrs.extend(hoist_declare_locals(base_instrs, decls));
            instrs.push(WasmBackendInstr::BinOp { op: IrBinOp::Add });
            Ok(Some(instrs))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn try_lower_self_recursive_int_scan_recur(
    decl_name: &str,
    decl_params: &[String],
    let_name: &str,
    value: &CompExpr,
    body: &CompExpr,
    acc_local: &str,
    loop_depth: u32,
    aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    decls: &mut HashSet<String>,
    recur_temp_counter: &mut usize,
) -> Result<Option<Vec<WasmBackendInstr>>, LowerError> {
    let CompExpr::Call { callee, args } = value else {
        return Ok(None);
    };
    if !is_self_decl_callee(callee, decl_name) {
        return Ok(None);
    }

    let acc_delta = match body {
        CompExpr::Value(ValueExpr::Var(name)) if name == let_name => None,
        CompExpr::Value(ValueExpr::BinOp { op, left, right }) if *op == IrBinOp::Add => {
            match (left.as_ref(), right.as_ref()) {
                (ValueExpr::Var(name), other) | (other, ValueExpr::Var(name))
                    if name == let_name =>
                {
                    Some(other.clone())
                }
                _ => return Ok(None),
            }
        }
        _ => return Ok(None),
    };

    lower_self_recursive_int_scan_continue(
        decl_params,
        args,
        acc_delta.as_ref(),
        acc_local,
        loop_depth,
        aliases,
        bindings,
        known_decls,
        decls,
        recur_temp_counter,
    )
    .map(Some)
}

#[allow(clippy::too_many_arguments)]
fn lower_self_recursive_int_scan_continue(
    decl_params: &[String],
    args: &[ValueExpr],
    acc_delta: Option<&ValueExpr>,
    acc_local: &str,
    loop_depth: u32,
    aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    decls: &mut HashSet<String>,
    recur_temp_counter: &mut usize,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    if args.len() != decl_params.len() {
        return Err(LowerError::UnsupportedForm {
            node: "self-recursive scan lowering requires matching arity".to_string(),
        });
    }

    let mut instrs = Vec::new();
    if let Some(delta) = acc_delta {
        instrs.push(WasmBackendInstr::LoadLocal {
            name: acc_local.to_string(),
        });
        instrs.extend(lower_value_ctx(delta, aliases, bindings, known_decls)?);
        instrs.push(WasmBackendInstr::BinOp { op: IrBinOp::Add });
        instrs.push(WasmBackendInstr::StoreLocal {
            name: acc_local.to_string(),
        });
    }

    let mut temp_names = Vec::with_capacity(decl_params.len());
    for param in decl_params {
        let temp_name = format!("__rr3_next_{}_{}", param, *recur_temp_counter);
        *recur_temp_counter += 1;
        decls.insert(temp_name.clone());
        temp_names.push(temp_name);
    }
    for (arg, temp_name) in args.iter().zip(temp_names.iter()) {
        instrs.extend(lower_value_ctx(arg, aliases, bindings, known_decls)?);
        instrs.push(WasmBackendInstr::StoreLocal {
            name: temp_name.clone(),
        });
    }
    for (param, temp_name) in decl_params.iter().zip(temp_names.iter()) {
        instrs.push(WasmBackendInstr::LoadLocal {
            name: temp_name.clone(),
        });
        instrs.push(WasmBackendInstr::StoreLocal {
            name: param.clone(),
        });
    }
    instrs.push(WasmBackendInstr::ContinueLoop {
        relative_depth: loop_depth,
    });
    Ok(instrs)
}

fn hoist_declare_locals(
    instrs: Vec<WasmBackendInstr>,
    decls: &mut HashSet<String>,
) -> Vec<WasmBackendInstr> {
    let mut lowered = Vec::new();
    for instr in instrs {
        match instr {
            WasmBackendInstr::DeclareLocal { name } => {
                decls.insert(name);
            }
            WasmBackendInstr::If {
                then_instrs,
                else_instrs,
            } => {
                lowered.push(WasmBackendInstr::If {
                    then_instrs: hoist_declare_locals(then_instrs, decls),
                    else_instrs: hoist_declare_locals(else_instrs, decls),
                });
            }
            WasmBackendInstr::Loop { body_instrs } => {
                lowered.push(WasmBackendInstr::Loop {
                    body_instrs: hoist_declare_locals(body_instrs, decls),
                });
            }
            WasmBackendInstr::ListBuilderPush {
                header_ptr_local,
                header_cap_local,
                n_chunks_local,
                chunk_ptr_local,
                total_len_local,
                value_instrs,
            } => lowered.push(WasmBackendInstr::ListBuilderPush {
                header_ptr_local,
                header_cap_local,
                n_chunks_local,
                chunk_ptr_local,
                total_len_local,
                value_instrs: hoist_declare_locals(value_instrs, decls),
            }),
            WasmBackendInstr::CaseMatch {
                scrutinee_local,
                arms,
            } => lowered.push(WasmBackendInstr::CaseMatch {
                scrutinee_local,
                arms: arms
                    .into_iter()
                    .map(|arm| crate::gen_lower::backend_ir::CaseArmInstr {
                        pattern: arm.pattern,
                        body_instrs: hoist_declare_locals(arm.body_instrs, decls),
                    })
                    .collect(),
            }),
            WasmBackendInstr::ListLit { element_instrs } => {
                lowered.push(WasmBackendInstr::ListLit {
                    element_instrs: element_instrs
                        .into_iter()
                        .map(|child| hoist_declare_locals(child, decls))
                        .collect(),
                });
            }
            WasmBackendInstr::TupleLit { element_instrs } => {
                lowered.push(WasmBackendInstr::TupleLit {
                    element_instrs: element_instrs
                        .into_iter()
                        .map(|child| hoist_declare_locals(child, decls))
                        .collect(),
                });
            }
            WasmBackendInstr::RecordLit {
                constructor,
                field_instrs,
            } => lowered.push(WasmBackendInstr::RecordLit {
                constructor,
                field_instrs: field_instrs
                    .into_iter()
                    .map(|child| hoist_declare_locals(child, decls))
                    .collect(),
            }),
            WasmBackendInstr::ListReverseFoldPrepend {
                list_instrs,
                item_local,
                prefix_element_instrs,
            } => lowered.push(WasmBackendInstr::ListReverseFoldPrepend {
                list_instrs: hoist_declare_locals(list_instrs, decls),
                item_local,
                prefix_element_instrs: prefix_element_instrs
                    .into_iter()
                    .map(|child| hoist_declare_locals(child, decls))
                    .collect(),
            }),
            WasmBackendInstr::CreateClosure {
                func_handle_instrs,
                slot_instrs,
            } => lowered.push(WasmBackendInstr::CreateClosure {
                func_handle_instrs: hoist_declare_locals(func_handle_instrs, decls),
                slot_instrs: slot_instrs
                    .into_iter()
                    .map(|child| hoist_declare_locals(child, decls))
                    .collect(),
            }),
            WasmBackendInstr::AllocMutableCell { init_instrs } => {
                lowered.push(WasmBackendInstr::AllocMutableCell {
                    init_instrs: hoist_declare_locals(init_instrs, decls),
                });
            }
            WasmBackendInstr::StoreCellValue {
                cell_ptr_instrs,
                value_instrs,
            } => lowered.push(WasmBackendInstr::StoreCellValue {
                cell_ptr_instrs: hoist_declare_locals(cell_ptr_instrs, decls),
                value_instrs: hoist_declare_locals(value_instrs, decls),
            }),
            other => lowered.push(other),
        }
    }
    lowered
}

fn contains_self_decl_call(comp: &CompExpr, decl_name: &str) -> bool {
    match comp {
        CompExpr::Value(_) => false,
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            contains_self_decl_call(value, decl_name) || contains_self_decl_call(body, decl_name)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts
                .iter()
                .any(|stmt| contains_self_decl_call(stmt, decl_name))
                || contains_self_decl_call(tail, decl_name)
        }
        CompExpr::If { then_, else_, .. } => {
            contains_self_decl_call(then_, decl_name) || contains_self_decl_call(else_, decl_name)
        }
        CompExpr::Call { callee, .. } => is_self_decl_callee(callee, decl_name),
        CompExpr::Assign { value, .. } => contains_self_decl_call(value, decl_name),
        CompExpr::AssignIndex { value, .. } => contains_self_decl_call(value, decl_name),
        CompExpr::Case { arms, .. } => arms
            .iter()
            .any(|arm| contains_self_decl_call(&arm.body, decl_name)),
        CompExpr::PerformEffect { .. } => false,
        CompExpr::Handle { clauses } => clauses
            .iter()
            .any(|clause| contains_self_decl_call(&clause.body, decl_name)),
        CompExpr::WithHandler { handler, body } => {
            contains_self_decl_call(handler, decl_name) || contains_self_decl_call(body, decl_name)
        }
        CompExpr::Resume { .. } => false,
    }
}

fn is_self_decl_callee(callee: &ValueExpr, decl_name: &str) -> bool {
    match callee {
        ValueExpr::Var(name) => name == decl_name,
        ValueExpr::GlobalRef { name, .. } => name == decl_name,
        _ => false,
    }
}

fn direct_decl_call_instr(decl_name: &str, tail_position: bool) -> WasmBackendInstr {
    if tail_position {
        WasmBackendInstr::TailDeclCall {
            decl_name: decl_name.to_string(),
        }
    } else {
        WasmBackendInstr::DeclCall {
            decl_name: decl_name.to_string(),
        }
    }
}

fn lower_comp_inner(
    comp: &CompExpr,
    tail_position: bool,
    aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    match comp {
        // Lambda in value position (e.g. returned from a decl or bound by Let) is lowered
        // through lower_lambda which has access to bindings/known_decls/lambda_decls.
        CompExpr::Value(ValueExpr::Lambda { param, body }) => {
            lower_lambda(param, body, aliases, bindings, known_decls, lambda_decls)
        }
        CompExpr::Value(v) => lower_value_ctx(v, aliases, bindings, known_decls),

        CompExpr::Let {
            name, value, body, ..
        } => {
            if let Some(alias) = alias_value_from_comp(value) {
                let mut scoped_aliases = aliases.clone();
                scoped_aliases.insert(name.clone(), alias);
                let mut scoped_bindings = bindings.clone();
                scoped_bindings.bind_outer_immutable(name.clone());
                let body_instrs = lower_comp_inner(
                    body,
                    tail_position,
                    &scoped_aliases,
                    &scoped_bindings,
                    known_decls,
                    lambda_decls,
                )?;
                if !instrs_load_local(&body_instrs, name) {
                    return Ok(body_instrs);
                }
            }
            let mut instrs = vec![WasmBackendInstr::DeclareLocal { name: name.clone() }];
            let value_instrs =
                lower_comp_inner(value, false, aliases, bindings, known_decls, lambda_decls)?;
            // Track whether the local holds a capturing closure so later call sites can
            // preserve closure-specific metadata even though emission converges on the
            // generic indirect-call path.
            let value_is_capturing_closure = value_instrs
                .iter()
                .any(|i| matches!(i, WasmBackendInstr::CreateClosure { .. }));
            instrs.extend(value_instrs);
            instrs.push(WasmBackendInstr::StoreLocal { name: name.clone() });
            let mut scoped_bindings = bindings.clone();
            scoped_bindings.bind_outer_immutable(name.clone());
            let mut scoped_aliases = aliases.clone();
            if value_is_capturing_closure {
                scoped_aliases.insert(name.clone(), AliasValue::CapturingClosure);
            } else if let Some(alias) = alias_value_from_comp(value) {
                scoped_aliases.insert(name.clone(), alias);
            }
            instrs.extend(lower_comp_inner(
                body,
                tail_position,
                &scoped_aliases,
                &scoped_bindings,
                known_decls,
                lambda_decls,
            )?);
            Ok(instrs)
        }

        CompExpr::Seq { stmts, tail } => {
            let mut instrs = Vec::new();
            for stmt in stmts {
                instrs.extend(lower_comp_inner(
                    stmt,
                    false,
                    aliases,
                    bindings,
                    known_decls,
                    lambda_decls,
                )?);
                instrs.push(WasmBackendInstr::Drop);
            }
            instrs.extend(lower_comp_inner(
                tail,
                tail_position,
                aliases,
                bindings,
                known_decls,
                lambda_decls,
            )?);
            Ok(instrs)
        }

        CompExpr::PerformEffect { effect, op, args } => {
            if effect == "Read" && op == "read_lines" && args.is_empty() {
                return Ok(vec![
                    WasmBackendInstr::EffectOp {
                        op: BackendEffectOp::Read(BackendReadOp::Read),
                    },
                    WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::StringSplitLines,
                    },
                ]);
            }
            let op = backend_effect_op(effect, op).ok_or_else(|| LowerError::UnsupportedForm {
                node: format!("unsupported effect op '{effect}.{op}'"),
            })?;
            let mut instrs = Vec::new();
            for arg in args {
                instrs.extend(lower_value_ctx(arg, aliases, bindings, known_decls)?);
            }
            instrs.push(WasmBackendInstr::EffectOp { op });
            Ok(instrs)
        }

        CompExpr::Call { callee, args } => {
            if let Some(op) = resolve_effect_call_target(callee, aliases) {
                let mut instrs = Vec::new();
                for arg in args {
                    instrs.extend(lower_value_ctx(arg, aliases, bindings, known_decls)?);
                }
                instrs.push(WasmBackendInstr::EffectOp { op });
                Ok(instrs)
            } else if let Some(intrinsic) =
                resolve_intrinsic_call_target(callee, aliases, args.len())
            {
                if intrinsic == BackendIntrinsic::ListFold
                    && let Some(instrs) = lower_supported_inline_list_fold_prepend_builder(
                        args,
                        aliases,
                        bindings,
                        known_decls,
                        lambda_decls,
                    )?
                {
                    return Ok(instrs);
                }
                let mut instrs = Vec::new();
                for arg in args {
                    instrs.extend(lower_value_as_arg(
                        arg,
                        aliases,
                        bindings,
                        known_decls,
                        lambda_decls,
                    )?);
                }
                if args.len() != intrinsic.arity() {
                    return Err(LowerError::UnsupportedForm {
                        node: format!(
                            "Intrinsic call with wrong arity: {:?} expected {}, got {}",
                            intrinsic,
                            intrinsic.arity(),
                            args.len()
                        ),
                    });
                }
                instrs.push(WasmBackendInstr::Intrinsic { intrinsic });
                Ok(instrs)
            } else if let goby_core::ir::ValueExpr::GlobalRef { module, name } = callee.as_ref()
                && module == "list"
                && name == "fold"
                && args.len() == 3
            {
                if let Some(instrs) = lower_supported_inline_list_fold_prepend_builder(
                    args,
                    aliases,
                    bindings,
                    known_decls,
                    lambda_decls,
                )? {
                    Ok(instrs)
                } else {
                    let mut instrs = Vec::new();
                    for arg in args {
                        instrs.extend(lower_value_as_arg(
                            arg,
                            aliases,
                            bindings,
                            known_decls,
                            lambda_decls,
                        )?);
                    }
                    instrs.push(WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::ListFold,
                    });
                    Ok(instrs)
                }
            } else if let goby_core::ir::ValueExpr::GlobalRef { name, .. } = callee.as_ref() {
                // Top-level user declaration call via GlobalRef.
                let mut instrs = Vec::new();
                for arg in args {
                    instrs.extend(lower_value_as_arg(
                        arg,
                        aliases,
                        bindings,
                        known_decls,
                        lambda_decls,
                    )?);
                }
                instrs.push(direct_decl_call_instr(name, tail_position));
                Ok(instrs)
            } else if let goby_core::ir::ValueExpr::Var(name) = callee.as_ref() {
                if let Some(resolved_name) = resolve_var_alias(name, aliases)
                    && known_decls.contains(resolved_name)
                {
                    // Direct call to a known top-level declaration.
                    let mut instrs = Vec::new();
                    for arg in args {
                        instrs.extend(lower_value_as_arg(
                            arg,
                            aliases,
                            bindings,
                            known_decls,
                            lambda_decls,
                        )?);
                    }
                    instrs.push(direct_decl_call_instr(resolved_name, tail_position));
                    Ok(instrs)
                } else if (name == "fold"
                    || resolve_global_ref(name, aliases) == Some(("list", "fold")))
                    && args.len() == 3
                {
                    if let Some(instrs) = lower_supported_inline_list_fold_prepend_builder(
                        args,
                        aliases,
                        bindings,
                        known_decls,
                        lambda_decls,
                    )? {
                        Ok(instrs)
                    } else {
                        let mut instrs = Vec::new();
                        for arg in args {
                            instrs.extend(lower_value_as_arg(
                                arg,
                                aliases,
                                bindings,
                                known_decls,
                                lambda_decls,
                            )?);
                        }
                        instrs.push(WasmBackendInstr::Intrinsic {
                            intrinsic: BackendIntrinsic::ListFold,
                        });
                        Ok(instrs)
                    }
                } else {
                    // Runtime function-value call via `call_indirect`.
                    // `name` is a local variable holding a TAG_FUNC tagged i64 handle.
                    // Stack order: push args left-to-right, then push callee, then IndirectCall.
                    // The arity is derived from the number of args in the flat IR call.
                    if args.is_empty() {
                        return Err(LowerError::UnsupportedForm {
                            node: format!(
                                "IndirectCall via local var '{name}' with zero arguments is not supported"
                            ),
                        });
                    }
                    let arity = args.len() as u8;
                    let mut instrs = Vec::new();
                    for arg in args {
                        instrs.extend(lower_value_as_arg(
                            arg,
                            aliases,
                            bindings,
                            known_decls,
                            lambda_decls,
                        )?);
                    }
                    instrs.push(WasmBackendInstr::LoadLocal { name: name.clone() });
                    instrs.push(WasmBackendInstr::IndirectCall { arity });
                    Ok(instrs)
                }
            } else {
                if args.is_empty() {
                    return Err(LowerError::UnsupportedForm {
                        node: format!(
                            "IndirectCall with unsupported zero-argument callee: {:?}",
                            callee
                        ),
                    });
                }
                let arity = args.len() as u8;
                let mut instrs = Vec::new();
                for arg in args {
                    instrs.extend(lower_value_as_arg(
                        arg,
                        aliases,
                        bindings,
                        known_decls,
                        lambda_decls,
                    )?);
                }
                instrs.extend(lower_value_as_arg(
                    callee,
                    aliases,
                    bindings,
                    known_decls,
                    lambda_decls,
                )?);
                instrs.push(WasmBackendInstr::IndirectCall { arity });
                Ok(instrs)
            }
        }

        CompExpr::If { cond, then_, else_ } => {
            let mut instrs = lower_value(cond)?;
            let then_instrs = lower_comp_inner(
                then_,
                tail_position,
                aliases,
                bindings,
                known_decls,
                lambda_decls,
            )?;
            let else_instrs = lower_comp_inner(
                else_,
                tail_position,
                aliases,
                bindings,
                known_decls,
                lambda_decls,
            )?;
            instrs.push(WasmBackendInstr::If {
                then_instrs,
                else_instrs,
            });
            Ok(instrs)
        }

        CompExpr::LetMut {
            name, value, body, ..
        } => {
            // Ask the analysis layer how this mutable binding should be represented.
            // HeapCell means at least one nested lambda captures it through a shared cell.
            if binding_repr_for_let_mut(name, body, bindings, known_decls) == BindingRepr::HeapCell
            {
                // Cell-promotion path.
                //
                // 1. Lower the init expression into a temporary local (`__cell_init_<name>`)
                //    so that AllocMutableCell.init_instrs is a single LoadLocal (never
                //    heap-allocating, which would clobber the HS_AUX_PTR scratch slot).
                // 2. Allocate the cell: AllocMutableCell { init_instrs: [LoadLocal { init_tmp }] }.
                // 3. Store the TAG_CELL pointer in `__cell_<name>`.
                // 4. Lower the body with `name` aliased as CellPromoted.
                let init_tmp = format!("__cell_init_{name}");
                let cell_local = cell_local_name(name);
                let mut instrs = vec![
                    WasmBackendInstr::DeclareLocal {
                        name: init_tmp.clone(),
                    },
                    WasmBackendInstr::DeclareLocal {
                        name: cell_local.clone(),
                    },
                ];
                instrs.extend(lower_comp_inner(
                    value,
                    false,
                    aliases,
                    bindings,
                    known_decls,
                    lambda_decls,
                )?);
                instrs.push(WasmBackendInstr::StoreLocal {
                    name: init_tmp.clone(),
                });
                instrs.push(WasmBackendInstr::AllocMutableCell {
                    init_instrs: vec![WasmBackendInstr::LoadLocal {
                        name: init_tmp.clone(),
                    }],
                });
                instrs.push(WasmBackendInstr::StoreLocal {
                    name: cell_local.clone(),
                });
                // Lower body with `name` marked as CellPromoted in aliases.
                let mut body_aliases = aliases.clone();
                body_aliases.insert(name.clone(), AliasValue::CellPromoted);
                let mut body_bindings = bindings.clone();
                body_bindings.bind_outer_mutable(name.clone());
                instrs.extend(lower_comp_inner(
                    body,
                    tail_position,
                    &body_aliases,
                    &body_bindings,
                    known_decls,
                    lambda_decls,
                )?);
                Ok(instrs)
            } else {
                // Plain path: DeclareLocal, lower value, StoreLocal, lower body.
                // Fused pattern checks are intentionally skipped for LetMut.
                let mut instrs = vec![WasmBackendInstr::DeclareLocal { name: name.clone() }];
                instrs.extend(lower_comp_inner(
                    value,
                    false,
                    aliases,
                    bindings,
                    known_decls,
                    lambda_decls,
                )?);
                instrs.push(WasmBackendInstr::StoreLocal { name: name.clone() });
                let mut body_bindings = bindings.clone();
                body_bindings.bind_outer_mutable(name.clone());
                instrs.extend(lower_comp_inner(
                    body,
                    tail_position,
                    aliases,
                    &body_bindings,
                    known_decls,
                    lambda_decls,
                )?);
                Ok(instrs)
            }
        }

        CompExpr::Assign { name, value } => {
            // If the binding is cell-promoted, write through the cell.
            if aliases.get(name.as_str()) == Some(&AliasValue::CellPromoted) {
                let cell_local = cell_local_name(name);
                let value_instrs =
                    lower_comp_inner(value, false, aliases, bindings, known_decls, lambda_decls)?;
                let mut instrs = vec![WasmBackendInstr::StoreCellValue {
                    cell_ptr_instrs: vec![WasmBackendInstr::LoadLocal { name: cell_local }],
                    value_instrs,
                }];
                // Assign produces Unit.
                instrs.push(WasmBackendInstr::I64Const(
                    crate::gen_lower::value::encode_unit(),
                ));
                Ok(instrs)
            } else {
                // Plain path: lower value and store in the existing named local.
                // No DeclareLocal is emitted because the local was already declared by the enclosing LetMut.
                let mut instrs =
                    lower_comp_inner(value, false, aliases, bindings, known_decls, lambda_decls)?;
                instrs.push(WasmBackendInstr::StoreLocal { name: name.clone() });
                // Assign produces Unit.
                instrs.push(WasmBackendInstr::I64Const(
                    crate::gen_lower::value::encode_unit(),
                ));
                Ok(instrs)
            }
        }

        CompExpr::AssignIndex { root, path, value } => lower_assign_index(
            root,
            path,
            value,
            aliases,
            bindings,
            known_decls,
            lambda_decls,
        ),

        CompExpr::Case { scrutinee, arms } => lower_case(
            scrutinee,
            arms,
            tail_position,
            aliases,
            bindings,
            known_decls,
            lambda_decls,
        ),

        other => Err(LowerError::UnsupportedForm {
            node: format!("{:?}", other),
        }),
    }
}

/// Lower a `CompExpr::Case` to backend IR.
///
/// Emits:
/// Lower `CompExpr::AssignIndex` to path-copy Wasm backend IR.
///
/// For `root[p0][p1]...[pN-1] := value`:
/// - Descend through the list using `list.get` for each prefix index, storing
///   intermediate lists in temporary locals.
/// - Ascend using `list.set` to build updated copies from the innermost out.
/// - Write the final updated root list back to the root local.
/// - Leaves a tagged `Unit` on the stack (assignment produces Unit).
///
/// Temporary local names use `__lset_<root>_<depth>` to avoid clashes.
fn lower_assign_index(
    root: &str,
    path: &[ValueExpr],
    value: &CompExpr,
    aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    let depth = path.len();
    if depth == 0 {
        return Err(LowerError::UnsupportedForm {
            node: "AssignIndex with empty path".to_string(),
        });
    }

    let mut instrs: Vec<WasmBackendInstr> = Vec::new();

    // Temp local names: __lset_<root>_0, ..., __lset_<root>_<depth-2> hold intermediate lists
    // obtained by list.get descents. There are depth-1 such temps (one per prefix).
    // We also need temps to hold the ascending list_set results:
    // __lset_<root>_up_<d> for d in 0..depth-1.
    //
    // Layout:
    //   descent temps:  __lset_<root>_d<i>  for i in 0..depth-1  (hold list_get results)
    //   ascent temps:   __lset_<root>_u<i>  for i in 0..depth-1  (hold list_set results)
    let descent_tmp = |i: usize| format!("__lset_{}_d{}", root, i);
    let ascent_tmp = |i: usize| format!("__lset_{}_u{}", root, i);

    // Determine whether root is a closure-captured mutable binding (cell-promoted).
    // If so, root loads must go through LoadCellValue and write-back through StoreCellValue,
    // following the same contract as CompExpr::Assign. Non-root temporaries (__lset_* locals)
    // are structurally plain locals declared within this function and are never cell-promoted.
    let root_is_cell = aliases.get(root) == Some(&AliasValue::CellPromoted);

    // If root is cell-promoted, cache its current list value into a plain scratch local once.
    // This avoids reading the cell twice (once during descent, once during ascent's i==0 load),
    // which would produce different results if the rhs expression mutates the same cell.
    let root_local: String = if root_is_cell {
        let scratch = format!("__lset_{}_root", root);
        instrs.push(WasmBackendInstr::DeclareLocal {
            name: scratch.clone(),
        });
        instrs.extend(lower_value_ctx(
            &ValueExpr::Var(root.to_string()),
            aliases,
            bindings,
            known_decls,
        )?);
        instrs.push(WasmBackendInstr::StoreLocal {
            name: scratch.clone(),
        });
        scratch
    } else {
        root.to_string()
    };

    // Declare descent temporaries (depth-1 of them, for depth > 1)
    for i in 0..depth.saturating_sub(1) {
        instrs.push(WasmBackendInstr::DeclareLocal {
            name: descent_tmp(i),
        });
    }
    // Declare ascent temporaries (depth of them — one per list.set call)
    for i in 0..depth {
        instrs.push(WasmBackendInstr::DeclareLocal {
            name: ascent_tmp(i),
        });
    }

    // Descent phase: compute intermediate lists via list.get.
    // descent_tmp(0) = list.get(root, path[0])
    // descent_tmp(1) = list.get(descent_tmp(0), path[1])
    // ...
    // descent_tmp(depth-2) = list.get(descent_tmp(depth-3), path[depth-2])
    for (i, index_value) in path.iter().enumerate().take(depth.saturating_sub(1)) {
        instrs.push(WasmBackendInstr::LoadLocal {
            name: if i == 0 {
                root_local.clone()
            } else {
                descent_tmp(i - 1)
            },
        });
        instrs.extend(lower_value_ctx(
            index_value,
            aliases,
            bindings,
            known_decls,
        )?);
        instrs.push(WasmBackendInstr::Intrinsic {
            intrinsic: BackendIntrinsic::ListGet,
        });
        instrs.push(WasmBackendInstr::StoreLocal {
            name: descent_tmp(i),
        });
    }

    // Ascent phase (innermost to outermost):
    // ascent_tmp(depth-1) = list.set(descent_tmp(depth-2) or root, path[depth-1], rhs_value)
    // ascent_tmp(depth-2) = list.set(descent_tmp(depth-3) or root, path[depth-2], ascent_tmp(depth-1))
    // ...
    // ascent_tmp(0)       = list.set(root, path[0], ascent_tmp(1))
    //
    // For depth=1: ascent_tmp(0) = list.set(root, path[0], rhs_value)
    let rhs_instrs = lower_comp_inner(value, false, aliases, bindings, known_decls, lambda_decls)?;

    for i in (0..depth).rev() {
        instrs.push(WasmBackendInstr::LoadLocal {
            name: if i == 0 {
                root_local.clone()
            } else {
                descent_tmp(i - 1)
            },
        });
        instrs.extend(lower_value_ctx(&path[i], aliases, bindings, known_decls)?);
        if i == depth - 1 {
            // Innermost: use the actual rhs computation
            instrs.extend(rhs_instrs.clone());
        } else {
            // Use the ascent result from the next deeper level
            instrs.push(WasmBackendInstr::LoadLocal {
                name: ascent_tmp(i + 1),
            });
        }
        instrs.push(WasmBackendInstr::Intrinsic {
            intrinsic: BackendIntrinsic::ListSet,
        });
        instrs.push(WasmBackendInstr::StoreLocal {
            name: ascent_tmp(i),
        });
    }

    // Write the final updated root list back.
    // If root is cell-promoted, write through the shared cell; otherwise plain store.
    if root_is_cell {
        instrs.push(WasmBackendInstr::StoreCellValue {
            cell_ptr_instrs: vec![WasmBackendInstr::LoadLocal {
                name: cell_local_name(root),
            }],
            value_instrs: vec![WasmBackendInstr::LoadLocal {
                name: ascent_tmp(0),
            }],
        });
    } else {
        instrs.push(WasmBackendInstr::LoadLocal {
            name: ascent_tmp(0),
        });
        instrs.push(WasmBackendInstr::StoreLocal {
            name: root.to_string(),
        });
    }

    // AssignIndex produces Unit
    instrs.push(WasmBackendInstr::I64Const(
        crate::gen_lower::value::encode_unit(),
    ));

    Ok(instrs)
}

///   1. `DeclareLocal { name: scrutinee_local }`
///   2. scrutinee value instructions
///   3. `StoreLocal { name: scrutinee_local }`
///   4. `CaseMatch { scrutinee_local, arms }`
///
/// The scrutinee local name is `__case_N` where N is a unique counter derived
/// from the arm count to avoid collisions in nested case expressions.
fn lower_case(
    scrutinee: &ValueExpr,
    arms: &[goby_core::ir::IrCaseArm],
    tail_position: bool,
    aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    use crate::gen_lower::backend_ir::{BackendCasePattern, BackendListPatternItem, CaseArmInstr};
    use goby_core::ir::{IrCasePattern, IrListPatternItem, IrListPatternTail};

    // Generate a unique scrutinee local name.
    // Use a static counter for uniqueness across nested case expressions.
    static CASE_COUNTER: AtomicU32 = AtomicU32::new(0);
    let counter = CASE_COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
    let scrutinee_local = format!("__case_scrutinee_{counter}");

    let scrutinee_instrs = lower_value(scrutinee)?;

    let mut backend_arms = Vec::new();
    for arm in arms {
        let pattern = match &arm.pattern {
            IrCasePattern::IntLit(n) => BackendCasePattern::IntLit(*n),
            IrCasePattern::BoolLit(b) => BackendCasePattern::BoolLit(*b),
            IrCasePattern::StringLit(s) => BackendCasePattern::StrLit(s.clone()),
            IrCasePattern::EmptyList => BackendCasePattern::EmptyList,
            IrCasePattern::Wildcard => BackendCasePattern::Wildcard,
            IrCasePattern::ListPattern { items, tail } => {
                let backend_items: Vec<BackendListPatternItem> = items
                    .iter()
                    .map(|item| match item {
                        IrListPatternItem::IntLit(n) => BackendListPatternItem::IntLit(*n),
                        IrListPatternItem::StringLit(s) => {
                            BackendListPatternItem::StrLit(s.clone())
                        }
                        IrListPatternItem::Bind(name) => BackendListPatternItem::Bind(name.clone()),
                        IrListPatternItem::Wildcard => BackendListPatternItem::Wildcard,
                    })
                    .collect();
                let backend_tail = match tail {
                    Some(IrListPatternTail::Bind(name)) => Some(name.clone()),
                    Some(IrListPatternTail::Ignore) | None => None,
                };
                BackendCasePattern::ListPattern {
                    items: backend_items,
                    tail: backend_tail,
                }
            }
        };
        let mut arm_bindings = bindings.clone();
        if let IrCasePattern::ListPattern { items, tail } = &arm.pattern {
            for item in items {
                if let IrListPatternItem::Bind(name) = item {
                    arm_bindings.bind_outer_immutable(name.clone());
                }
            }
            if let Some(IrListPatternTail::Bind(name)) = tail {
                arm_bindings.bind_outer_immutable(name.clone());
            }
        }
        let body_instrs = lower_comp_inner(
            &arm.body,
            tail_position,
            aliases,
            &arm_bindings,
            known_decls,
            lambda_decls,
        )?;
        backend_arms.push(CaseArmInstr {
            pattern,
            body_instrs,
        });
    }

    let mut instrs = Vec::new();
    // 1. Declare the scrutinee local (so it appears in the Wasm function header pre-scan).
    instrs.push(WasmBackendInstr::DeclareLocal {
        name: scrutinee_local.clone(),
    });
    // 1b. Declare locals for all ListPattern Bind/tail variables (pre-scan must see them).
    for arm in &backend_arms {
        if let BackendCasePattern::ListPattern { items, tail } = &arm.pattern {
            for item in items {
                if let BackendListPatternItem::Bind(name) = item {
                    instrs.push(WasmBackendInstr::DeclareLocal { name: name.clone() });
                }
            }
            if let Some(tail_name) = tail {
                instrs.push(WasmBackendInstr::DeclareLocal {
                    name: tail_name.clone(),
                });
            }
        }
    }
    // 2. Evaluate the scrutinee.
    instrs.extend(scrutinee_instrs);
    // 3. Store into the local.
    instrs.push(WasmBackendInstr::StoreLocal {
        name: scrutinee_local.clone(),
    });
    // 4. The CaseMatch instruction reads from the local.
    instrs.push(WasmBackendInstr::CaseMatch {
        scrutinee_local,
        arms: backend_arms,
    });
    Ok(instrs)
}

/// Lower a `ValueExpr` to a flat sequence of `WasmBackendInstr`.
///
/// Cell-promoted names (bindings that have been promoted to heap cells because they are
/// captured mutably by nested lambdas) must be handled at the caller level by passing
/// `aliases` to `lower_value_ctx` instead.
pub(crate) fn lower_value(v: &ValueExpr) -> Result<Vec<WasmBackendInstr>, LowerError> {
    lower_value_ctx(
        v,
        &HashMap::new(),
        &ClosureBindingEnv::default(),
        &HashSet::new(),
    )
}

/// Like `lower_value` but aware of cell-promoted bindings in `aliases`.
///
/// If `aliases` contains `AliasValue::CellPromoted` for a name, reading that name emits
/// `[LoadLocal { __cell_<name> }, LoadCellValue]` instead of `[LoadLocal { name }]`.
fn lower_value_ctx(
    v: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    match v {
        ValueExpr::Unit => Ok(vec![WasmBackendInstr::I64Const(encode_unit())]),
        ValueExpr::IntLit(n) => Ok(vec![WasmBackendInstr::I64Const(encode_int(*n)?)]),
        ValueExpr::BoolLit(b) => Ok(vec![WasmBackendInstr::I64Const(encode_bool(*b))]),
        ValueExpr::StrLit(text) => Ok(vec![WasmBackendInstr::PushStaticString {
            text: text.clone(),
        }]),
        ValueExpr::ListLit { elements, spread } => {
            let mut element_instrs = Vec::with_capacity(elements.len());
            for elem in elements {
                element_instrs.push(lower_value_ctx(elem, aliases, bindings, known_decls)?);
            }
            let mut instrs = vec![WasmBackendInstr::ListLit { element_instrs }];
            if let Some(tail) = spread {
                instrs.extend(lower_value_ctx(tail, aliases, bindings, known_decls)?);
                instrs.push(WasmBackendInstr::Intrinsic {
                    intrinsic: BackendIntrinsic::ListConcat,
                });
            }
            Ok(instrs)
        }
        ValueExpr::TupleLit(items) => {
            if items.is_empty() {
                return Ok(vec![WasmBackendInstr::I64Const(encode_unit())]);
            }
            let mut element_instrs = Vec::with_capacity(items.len());
            for item in items {
                element_instrs.push(lower_value_ctx(item, aliases, bindings, known_decls)?);
            }
            Ok(vec![WasmBackendInstr::TupleLit { element_instrs }])
        }
        ValueExpr::RecordLit {
            constructor,
            fields,
        } => {
            let mut field_instrs = Vec::with_capacity(fields.len());
            for (_, value) in fields {
                field_instrs.push(lower_value_ctx(value, aliases, bindings, known_decls)?);
            }
            Ok(vec![WasmBackendInstr::RecordLit {
                constructor: constructor.clone(),
                field_instrs,
            }])
        }
        ValueExpr::Var(name) => {
            if aliases.get(name.as_str()) == Some(&AliasValue::CellPromoted) {
                // Cell-promoted binding: load the cell ptr local, then dereference the cell.
                Ok(vec![
                    WasmBackendInstr::LoadLocal {
                        name: cell_local_name(name),
                    },
                    WasmBackendInstr::LoadCellValue,
                ])
            } else if known_decls.contains(name.as_str()) && !bindings.is_bound(name) {
                Ok(vec![
                    WasmBackendInstr::I64Const(encode_unit()),
                    WasmBackendInstr::DeclCall {
                        decl_name: name.clone(),
                    },
                ])
            } else {
                Ok(vec![WasmBackendInstr::LoadLocal { name: name.clone() }])
            }
        }
        ValueExpr::GlobalRef { module, name } => {
            // Effect ops (Print.println, Read.read, etc.) cannot be stored as Wasm i64 values;
            // they are only valid as direct callees. Reject when used as a value.
            if backend_effect_op(module, name).is_some() {
                return Err(LowerError::UnsupportedForm {
                    node: format!(
                        "GlobalRef '{module}.{name}' used as a value (effect ops are not first-class Wasm values)"
                    ),
                });
            }
            Ok(vec![WasmBackendInstr::LoadLocal {
                name: format!("{}.{}", module, name),
            }])
        }
        ValueExpr::BinOp { op, left, right } => {
            let mut instrs = lower_value_ctx(left, aliases, bindings, known_decls)?;
            instrs.extend(lower_value_ctx(right, aliases, bindings, known_decls)?);
            instrs.push(WasmBackendInstr::BinOp { op: op.clone() });
            Ok(instrs)
        }
        ValueExpr::Interp(parts) => {
            if parts.is_empty() {
                return Ok(vec![WasmBackendInstr::PushStaticString {
                    text: String::new(),
                }]);
            }
            // Emit all parts, then n-1 StringConcat calls to fold left-to-right.
            let mut instrs = Vec::new();
            for part in parts {
                match part {
                    IrInterpPart::Text(t) => {
                        instrs.push(WasmBackendInstr::PushStaticString { text: t.clone() });
                    }
                    IrInterpPart::Expr(e) => {
                        instrs.extend(lower_value_ctx(e, aliases, bindings, known_decls)?);
                        instrs.push(WasmBackendInstr::Intrinsic {
                            intrinsic: BackendIntrinsic::ValueToString,
                        });
                    }
                }
            }
            for _ in 0..parts.len() - 1 {
                instrs.push(WasmBackendInstr::Intrinsic {
                    intrinsic: BackendIntrinsic::StringConcat,
                });
            }
            Ok(instrs)
        }
        ValueExpr::TupleProject { tuple, index } => {
            // receiver must be a local variable (guaranteed by resolved.rs is_local check).
            let ValueExpr::Var(tuple_local) = tuple.as_ref() else {
                return Err(LowerError::UnsupportedForm {
                    node: format!("TupleProject with non-Var tuple: {:?}", tuple),
                });
            };
            Ok(vec![WasmBackendInstr::TupleGet {
                tuple_local: tuple_local.clone(),
                index: *index,
            }])
        }
        ValueExpr::ListGet { list, index } => {
            let mut instrs = lower_value_ctx(list, aliases, bindings, known_decls)?;
            instrs.extend(lower_value_ctx(index, aliases, bindings, known_decls)?);
            instrs.push(WasmBackendInstr::Intrinsic {
                intrinsic: BackendIntrinsic::ListGet,
            });
            Ok(instrs)
        }
        // NOTE: Lambda is intercepted at the CompExpr::Value(Lambda) level in lower_comp_inner,
        // which has access to bindings/known_decls/lambda_decls.  lower_value is never called
        // with a Lambda directly from lower_comp_inner, but may be called from other contexts
        // (e.g. scrutinee of Case, condition of If) where Lambda is not expected.
        other => Err(LowerError::UnsupportedForm {
            node: format!("{:?}", other),
        }),
    }
}

/// Returns the name of the cell-ptr local for a cell-promoted mutable binding.
/// Convention: `__cell_<name>`.
fn cell_local_name(name: &str) -> String {
    format!("__cell_{name}")
}

/// Lower a `ValueExpr` that appears as a call *argument* (not a callee).
///
/// This is identical to `lower_value` except for:
///
/// - `Var(name)` where `name ∈ known_decls` → `PushFuncHandle { decl_name: name }`
///   (encodes the funcref table slot as a TAG_FUNC i64)
///
/// - `Lambda { param, body }` where body has no free variables beyond `param` →
///   the body is lifted as a `LambdaAuxDecl` and a `PushFuncHandle` is returned.
///   If the body references free variables (captures from enclosing scope), returns
///   `UnsupportedForm` on paths that do not support closure lifting.
///
/// All other cases delegate to `lower_value`.
fn lower_value_as_arg(
    v: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    match v {
        // `graphemes` passed as a function value (e.g. `map lines graphemes`).
        // Generate a wrapper AuxDecl that takes one param and calls StringGraphemesList.
        ValueExpr::Var(name)
            if name == "graphemes"
                || resolve_global_ref(name, aliases) == Some(("string", "graphemes")) =>
        {
            let n = LAMBDA_COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
            let decl_name = format!("__graphemes_wrapper_{n}");
            let param_name = "__s".to_string();
            lambda_decls.push(LambdaAuxDecl {
                decl_name: decl_name.clone(),
                param_names: vec![param_name.clone()],
                env: CallableEnv::default(),
                instrs: vec![
                    WasmBackendInstr::LoadLocal {
                        name: param_name.clone(),
                    },
                    WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::StringGraphemesList,
                    },
                ],
            });
            Ok(vec![WasmBackendInstr::PushFuncHandle { decl_name }])
        }
        ValueExpr::GlobalRef { module, name } if module == "string" && name == "graphemes" => {
            let n = LAMBDA_COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
            let decl_name = format!("__graphemes_wrapper_{n}");
            let param_name = "__s".to_string();
            lambda_decls.push(LambdaAuxDecl {
                decl_name: decl_name.clone(),
                param_names: vec![param_name.clone()],
                env: CallableEnv::default(),
                instrs: vec![
                    WasmBackendInstr::LoadLocal {
                        name: param_name.clone(),
                    },
                    WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::StringGraphemesList,
                    },
                ],
            });
            Ok(vec![WasmBackendInstr::PushFuncHandle { decl_name }])
        }
        ValueExpr::Var(name) if known_decls.contains(name.as_str()) => {
            Ok(vec![WasmBackendInstr::PushFuncHandle {
                decl_name: name.clone(),
            }])
        }
        ValueExpr::Var(name) => {
            if let Some(BackendEffectOp::Print(op)) =
                resolve_effect_call_target(&ValueExpr::Var(name.clone()), aliases)
            {
                return lower_print_effect_as_arg(op, lambda_decls);
            }
            lower_value(v)
        }
        ValueExpr::Lambda { param, body } => {
            lower_lambda(param, body, aliases, bindings, known_decls, lambda_decls)
        }
        ValueExpr::GlobalRef { module, name } => {
            if let Some(BackendEffectOp::Print(op)) = backend_effect_op(module, name) {
                return lower_print_effect_as_arg(op, lambda_decls);
            }
            lower_value(v)
        }
        other => lower_value(other),
    }
}

fn lower_print_effect_as_arg(
    op: BackendPrintOp,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    let n = LAMBDA_COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
    let decl_name = format!("__print_effect_wrapper_{n}");
    let param_name = "__x".to_string();
    lambda_decls.push(LambdaAuxDecl {
        decl_name: decl_name.clone(),
        param_names: vec![param_name.clone()],
        env: CallableEnv::default(),
        instrs: vec![
            WasmBackendInstr::LoadLocal { name: param_name },
            WasmBackendInstr::EffectOp {
                op: BackendEffectOp::Print(op),
            },
        ],
    });
    Ok(vec![WasmBackendInstr::PushFuncHandle { decl_name }])
}

/// Lower a `ValueExpr::Lambda` to backend IR instructions.
///
/// # Zero-capture case
/// Lifts the body as a `LambdaAuxDecl` with `param_names: [param]` and emits
/// `PushFuncHandle { decl_name }`.
///
/// # Capturing case (ByValue)
/// Lifts the body as a `LambdaAuxDecl` with `param_names: ["__clo", params...]`.
/// The body preamble declares a local for each captured slot, loads it from the closure record,
/// and stores it so the rest of the body can use `LoadLocal` as normal.
///
/// # Capturing case (SharedMutableCell)
/// Slot in the closure record holds the cell ptr (TAG_CELL i64).
/// Body preamble stores the cell ptr in `__cell_<name>`.  Reads/writes inside the body
/// go through the cell via `lower_value_ctx` (CellPromoted alias) and `StoreCellValue`.
fn lower_lambda(
    param: &str,
    body: &goby_core::ir::CompExpr,
    outer_aliases: &HashMap<String, AliasValue>,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    let (params, flattened_body) = flatten_lambda_chain(param, body);
    let callable_env =
        analyze_lambda_callable_env_for_params(&params, flattened_body, bindings, known_decls);

    if callable_env.is_empty() {
        // Zero-capture: lift as-is with the lambda params.
        let mut body_bindings = bindings.clone();
        for param in &params {
            body_bindings.bind_outer_immutable(param.clone());
        }
        let body_instrs = lower_comp_inner(
            flattened_body,
            true,
            &HashMap::new(),
            &body_bindings,
            known_decls,
            lambda_decls,
        )?;
        let n = LAMBDA_COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
        let decl_name = format!("__lambda_{n}");
        lambda_decls.push(LambdaAuxDecl {
            decl_name: decl_name.clone(),
            param_names: params,
            env: CallableEnv::default(),
            instrs: body_instrs,
        });
        return Ok(vec![WasmBackendInstr::PushFuncHandle { decl_name }]);
    }

    // Capturing lambda (ByValue and/or SharedMutableCell).
    // param_names = ["__clo", params...].
    // Body preamble: for each captured slot, declare a local, load from closure, store.
    const CLO_LOCAL: &str = "__clo";
    let mut preamble: Vec<WasmBackendInstr> = Vec::new();
    // Aliases for the body: CellPromoted names get cell-load semantics.
    let mut body_aliases: HashMap<String, AliasValue> = HashMap::new();

    for (slot_index, slot) in callable_env.slots.iter().enumerate() {
        match slot.slot_kind {
            CallableEnvSlotKind::ByValue => {
                // Slot holds the value directly; store into a local with the captured name.
                preamble.push(WasmBackendInstr::DeclareLocal {
                    name: slot.name.clone(),
                });
                preamble.push(WasmBackendInstr::LoadClosureSlot {
                    closure_local: CLO_LOCAL.to_string(),
                    slot_index,
                });
                preamble.push(WasmBackendInstr::StoreLocal {
                    name: slot.name.clone(),
                });
            }
            CallableEnvSlotKind::SharedMutableCell { .. } => {
                // Slot holds the cell ptr (TAG_CELL i64).
                // Store into `__cell_<name>`; reads/writes in the body go via the cell.
                let cell_local = cell_local_name(&slot.name);
                preamble.push(WasmBackendInstr::DeclareLocal {
                    name: cell_local.clone(),
                });
                preamble.push(WasmBackendInstr::LoadClosureSlot {
                    closure_local: CLO_LOCAL.to_string(),
                    slot_index,
                });
                preamble.push(WasmBackendInstr::StoreLocal { name: cell_local });
                body_aliases.insert(slot.name.clone(), AliasValue::CellPromoted);
            }
        }
    }

    // Lower the body:
    // - ByValue captured names are bound as immutable locals (accessible via LoadLocal).
    // - SharedMutableCell captured names are cell-promoted (reads/writes go via the cell).
    let mut body_bindings = bindings.clone();
    for slot in &callable_env.slots {
        match slot.slot_kind {
            CallableEnvSlotKind::ByValue => {
                body_bindings.bind_outer_immutable(slot.name.clone());
            }
            CallableEnvSlotKind::SharedMutableCell { .. } => {
                // Bind as mutable so that Assign nodes inside the body are recognised.
                body_bindings.bind_outer_mutable(slot.name.clone());
            }
        }
    }
    for param in &params {
        body_bindings.bind_outer_immutable(param.clone());
    }
    let body_instrs = lower_comp_inner(
        flattened_body,
        true,
        &body_aliases,
        &body_bindings,
        known_decls,
        lambda_decls,
    )?;

    let n = LAMBDA_COUNTER.fetch_add(1, AtomicOrdering::Relaxed);
    let decl_name = format!("__lambda_{n}");
    let mut full_body = preamble;
    full_body.extend(body_instrs);
    let mut param_names = vec![CLO_LOCAL.to_string()];
    param_names.extend(params);
    lambda_decls.push(LambdaAuxDecl {
        decl_name: decl_name.clone(),
        param_names,
        env: callable_env.clone(),
        instrs: full_body,
    });

    // Create the closure record: [PushFuncHandle, slot_instrs per captured slot].
    // - ByValue: LoadLocal { name } (the current value)
    // - SharedMutableCell: LoadLocal { __cell_<name> } (the cell ptr from outer scope)
    let func_handle_instrs = vec![WasmBackendInstr::PushFuncHandle {
        decl_name: decl_name.clone(),
    }];
    let slot_instrs: Vec<Vec<WasmBackendInstr>> = callable_env
        .slots
        .iter()
        .map(|slot| match slot.slot_kind {
            CallableEnvSlotKind::ByValue => vec![WasmBackendInstr::LoadLocal {
                name: slot.name.clone(),
            }],
            CallableEnvSlotKind::SharedMutableCell { .. } => {
                // The outer scope holds the cell ptr in `__cell_<name>`.
                // If this lambda is in a context where the outer name is itself cell-promoted
                // (outer `LetMut` already promoted), use that cell ptr.
                // Otherwise fall back to the plain local (shouldn't happen in valid CC4 code).
                if outer_aliases.get(slot.name.as_str()) == Some(&AliasValue::CellPromoted) {
                    vec![WasmBackendInstr::LoadLocal {
                        name: cell_local_name(&slot.name),
                    }]
                } else {
                    // Outer scope has the cell in `__cell_<name>` (set by LetMut cell-promotion).
                    vec![WasmBackendInstr::LoadLocal {
                        name: cell_local_name(&slot.name),
                    }]
                }
            }
        })
        .collect();
    Ok(vec![WasmBackendInstr::CreateClosure {
        func_handle_instrs,
        slot_instrs,
    }])
}

fn format_callable_env(callable_env: &CallableEnv) -> String {
    callable_env
        .slots
        .iter()
        .map(|slot| match slot.slot_kind {
            CallableEnvSlotKind::ByValue => {
                format!("{}:{:?}:by_value", slot.name, slot.capture_kind)
            }
            CallableEnvSlotKind::SharedMutableCell { storage_id } => format!(
                "{}:{:?}:cell#{}",
                slot.name,
                slot.capture_kind,
                storage_id.index()
            ),
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Try to match the fused graphemes-index-print pattern:
///
#[derive(Debug, Clone, PartialEq, Eq)]
enum AliasValue {
    Var(String),
    Str(String),
    GlobalRef {
        module: String,
        name: String,
    },
    /// The local holds a TAG_CLOSURE value (capturing lambda).
    CapturingClosure,
    /// The binding has been cell-promoted: it is a mutable binding captured by a nested lambda.
    /// The actual value lives in a heap cell; `__cell_<name>` local holds the cell ptr.
    /// Reads become `LoadLocal { __cell_<name> } + LoadCellValue`.
    /// Writes (`Assign`) become `StoreCellValue { cell_ptr_instrs, value_instrs }`.
    CellPromoted,
}

fn alias_value_from_comp(comp: &CompExpr) -> Option<AliasValue> {
    match comp {
        CompExpr::Value(ValueExpr::Var(name)) => Some(AliasValue::Var(name.clone())),
        CompExpr::Value(ValueExpr::StrLit(text)) => Some(AliasValue::Str(text.clone())),
        CompExpr::Value(ValueExpr::GlobalRef { module, name }) => Some(AliasValue::GlobalRef {
            module: module.clone(),
            name: name.clone(),
        }),
        _ => None,
    }
}

fn is_helper_global(
    callee: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
    module: &str,
    name: &str,
) -> bool {
    match callee {
        ValueExpr::GlobalRef {
            module: callee_module,
            name: callee_name,
        } => callee_module == module && callee_name == name,
        ValueExpr::Var(var) => {
            resolve_global_ref(var, aliases).is_some_and(|(callee_module, callee_name)| {
                callee_module == module && callee_name == name
            })
        }
        _ => false,
    }
}

fn resolve_local_name<'a>(
    value: &'a ValueExpr,
    aliases: &'a HashMap<String, AliasValue>,
) -> Option<&'a str> {
    match value {
        ValueExpr::Var(name) => resolve_var_alias(name, aliases),
        _ => None,
    }
}

fn resolve_var_alias<'a>(
    name: &'a str,
    aliases: &'a HashMap<String, AliasValue>,
) -> Option<&'a str> {
    match aliases.get(name) {
        Some(AliasValue::Var(next)) => resolve_var_alias(next.as_str(), aliases),
        Some(AliasValue::Str(_))
        | Some(AliasValue::GlobalRef { .. })
        | Some(AliasValue::CapturingClosure)
        | Some(AliasValue::CellPromoted) => None,
        None => Some(name),
    }
}

fn resolve_global_ref<'a>(
    name: &'a str,
    aliases: &'a HashMap<String, AliasValue>,
) -> Option<(&'a str, &'a str)> {
    match aliases.get(name) {
        Some(AliasValue::GlobalRef { module, name }) => Some((module.as_str(), name.as_str())),
        Some(AliasValue::Var(next)) => resolve_global_ref(next.as_str(), aliases),
        Some(AliasValue::Str(_))
        | Some(AliasValue::CapturingClosure)
        | Some(AliasValue::CellPromoted)
        | None => None,
    }
}

fn resolve_effect_call_target(
    callee: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Option<BackendEffectOp> {
    match callee {
        ValueExpr::GlobalRef { module, name } => backend_effect_op(module, name),
        ValueExpr::Var(name) if name == "print" => {
            Some(BackendEffectOp::Print(BackendPrintOp::Print))
        }
        ValueExpr::Var(name) if name == "println" => {
            Some(BackendEffectOp::Print(BackendPrintOp::Println))
        }
        ValueExpr::Var(name) if name == "read" => Some(BackendEffectOp::Read(BackendReadOp::Read)),
        ValueExpr::Var(name) if name == "read_line" => {
            Some(BackendEffectOp::Read(BackendReadOp::ReadLine))
        }
        ValueExpr::Var(name) => {
            if let Some(op) = resolve_var_alias(name, aliases) {
                if let Some(effect_op) = backend_effect_op("Print", op) {
                    return Some(effect_op);
                }
                if let Some(effect_op) = backend_effect_op("Read", op) {
                    return Some(effect_op);
                }
            }
            let (module, op) = resolve_global_ref(name, aliases)?;
            backend_effect_op(module, op)
        }
        _ => None,
    }
}

fn resolve_intrinsic_call_target(
    callee: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
    arg_count: usize,
) -> Option<BackendIntrinsic> {
    match callee {
        ValueExpr::GlobalRef { module, name } => {
            backend_intrinsic_for(module.as_str(), name.as_str())
        }
        ValueExpr::Var(name) => {
            if let Some(intrinsic) = backend_intrinsic_for_bare(name, arg_count) {
                return Some(intrinsic);
            }
            let (module, name) = resolve_global_ref(name, aliases)?;
            backend_intrinsic_for(module, name)
        }
        _ => None,
    }
}

fn backend_intrinsic_for(module: &str, name: &str) -> Option<BackendIntrinsic> {
    match (module, name) {
        ("list", "get") => Some(BackendIntrinsic::ListGet),
        // M6: explicit immutable point-update boundary for functional-style `set xs i v`.
        ("list", "set") => Some(BackendIntrinsic::ListSet),
        ("string", "graphemes") => Some(BackendIntrinsic::StringGraphemesList),
        ("string", "length") => Some(BackendIntrinsic::StringLength),
        ("string", "split") => Some(BackendIntrinsic::StringSplit),
        _ => None,
    }
}

fn backend_intrinsic_for_bare(name: &str, arg_count: usize) -> Option<BackendIntrinsic> {
    match name {
        "__goby_string_each_grapheme" => match arg_count {
            1 => Some(BackendIntrinsic::StringEachGraphemeCount),
            2 => Some(BackendIntrinsic::StringEachGraphemeState),
            _ => None,
        },
        "__goby_list_push_string" => Some(BackendIntrinsic::ListPushString),
        "__goby_list_join_string" if arg_count == 2 => Some(BackendIntrinsic::ListJoinString),
        "__goby_string_length" => Some(BackendIntrinsic::StringLength),
        "__goby_list_length" => Some(BackendIntrinsic::ListLength),
        "__goby_list_fold" if arg_count == 3 => Some(BackendIntrinsic::ListFold),
        "__goby_list_map" if arg_count == 2 => Some(BackendIntrinsic::ListMap),
        // M6: explicit immutable point-update intrinsic for functional-style use.
        "__goby_list_set" if arg_count == 3 => Some(BackendIntrinsic::ListSet),
        _ => None,
    }
}

fn is_effect_pair(module: &str, op: &str) -> bool {
    backend_effect_op(module, op).is_some()
}

fn backend_effect_op(module: &str, op: &str) -> Option<BackendEffectOp> {
    match (module, op) {
        ("Print", "print") => Some(BackendEffectOp::Print(BackendPrintOp::Print)),
        ("Print", "println") => Some(BackendEffectOp::Print(BackendPrintOp::Println)),
        ("Read", "read") => Some(BackendEffectOp::Read(BackendReadOp::Read)),
        ("Read", "read_line") => Some(BackendEffectOp::Read(BackendReadOp::ReadLine)),
        _ => None,
    }
}

fn instrs_load_local(instrs: &[WasmBackendInstr], name: &str) -> bool {
    instrs.iter().any(|instr| match instr {
        WasmBackendInstr::LoadLocal { name: local_name } => local_name == name,
        // Recurse into nested instruction lists so aliases used only inside
        // nested constructs are not pruned by alias-elision.
        WasmBackendInstr::ListReverseFoldPrepend {
            list_instrs,
            prefix_element_instrs,
            ..
        } => {
            instrs_load_local(list_instrs, name)
                || prefix_element_instrs
                    .iter()
                    .any(|instrs| instrs_load_local(instrs, name))
        }
        WasmBackendInstr::If {
            then_instrs,
            else_instrs,
        } => instrs_load_local(then_instrs, name) || instrs_load_local(else_instrs, name),
        WasmBackendInstr::CaseMatch { arms, .. } => arms
            .iter()
            .any(|arm| instrs_load_local(&arm.body_instrs, name)),
        WasmBackendInstr::ListLit { element_instrs } => {
            element_instrs.iter().any(|e| instrs_load_local(e, name))
        }
        WasmBackendInstr::TupleLit { element_instrs } => {
            element_instrs.iter().any(|e| instrs_load_local(e, name))
        }
        WasmBackendInstr::RecordLit { field_instrs, .. } => {
            field_instrs.iter().any(|f| instrs_load_local(f, name))
        }
        WasmBackendInstr::CreateClosure {
            func_handle_instrs,
            slot_instrs,
        } => {
            instrs_load_local(func_handle_instrs, name)
                || slot_instrs.iter().any(|s| instrs_load_local(s, name))
        }
        _ => false,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gen_lower::backend_ir::{
        BackendEffectOp, BackendIntrinsic, BackendPrintOp, BackendReadOp, WasmBackendInstr as I,
    };

    fn lower_comp_with_aliases(
        comp: &CompExpr,
        aliases: &HashMap<String, AliasValue>,
    ) -> Result<Vec<WasmBackendInstr>, LowerError> {
        let mut lambda_decls = Vec::new();
        let bindings = ClosureBindingEnv::default();
        lower_comp_inner(
            comp,
            true,
            aliases,
            &bindings,
            &HashSet::new(),
            &mut lambda_decls,
        )
    }

    #[test]
    fn lowers_supported_self_recursive_int_scan_to_loop() {
        let module = goby_core::parse_module(
            r#"
import goby/stdio

probe : Int -> Bool can Print
probe n =
  n >= 0

walk : Int -> Int -> Int -> Int -> Int can Print
walk width height x y =
  if y >= height
    0
  else
    if x >= width
      walk width height 0 (y + 1)
    else
      checked =
        if probe x
          1
        else
          0
      checked + walk width height (x + 1) y
"#,
        )
        .expect("source should parse");
        let walk_decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == "walk")
            .expect("walk declaration should exist");
        let ir_decl =
            goby_core::ir_lower::lower_declaration(walk_decl).expect("IR lowering should work");
        let known_decls: HashSet<String> = ["probe".to_string(), "walk".to_string()].into();
        let mut lambda_decls = Vec::new();
        let lowered = lower_supported_self_recursive_int_scan(
            "walk",
            &ir_decl.body,
            &ir_decl
                .params
                .iter()
                .map(|(name, _)| name.clone())
                .collect::<Vec<_>>(),
            &known_decls,
            &mut lambda_decls,
        )
        .expect("specialized lowering should not error");

        assert!(
            lowered
                .as_ref()
                .is_some_and(|instrs| instrs.iter().any(|instr| matches!(instr, I::Loop { .. }))),
            "supported recursive scan should lower to a loop, got: {:?}",
            lowered
        );
        let lowered = lowered.expect("supported scan should lower");
        let rendered = format!("{lowered:?}");
        assert!(
            !rendered.contains("DeclCall { decl_name: \"walk\" }"),
            "specialized loop should eliminate self DeclCall(walk), got: {rendered}"
        );
    }

    #[test]
    fn lowers_supported_self_recursive_list_spread_builder_to_loop_and_builder_ops() {
        let module = goby_core::parse_module(
            r#"
build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]
"#,
        )
        .expect("source should parse");
        let build_decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == "build")
            .expect("build declaration should exist");
        let ir_decl =
            goby_core::ir_lower::lower_declaration(build_decl).expect("IR lowering should work");
        let known_decls: HashSet<String> = ["build".to_string()].into();
        let mut lambda_decls = Vec::new();
        let lowered = lower_supported_self_recursive_list_spread_builder(
            "build",
            &ir_decl.body,
            &ir_decl
                .params
                .iter()
                .map(|(name, _)| name.clone())
                .collect::<Vec<_>>(),
            &known_decls,
            &mut lambda_decls,
        )
        .expect("specialized lowering should not error")
        .expect("builder shape should lower");

        assert!(
            lowered.iter().any(|instr| matches!(instr, I::Loop { .. })),
            "list-spread builder should lower to a loop, got: {lowered:?}"
        );
        assert!(
            lowered
                .iter()
                .any(|instr| matches!(instr, I::ListBuilderNew { .. })),
            "list-spread builder should initialize a builder, got: {lowered:?}"
        );
        let rendered = format!("{lowered:?}");
        assert!(
            !rendered.contains("DeclCall { decl_name: \"build\" }"),
            "specialized builder loop should eliminate self DeclCall(build), got: {rendered}"
        );
    }

    #[test]
    fn lower_perform_read() {
        let comp = CompExpr::PerformEffect {
            effect: "Read".to_string(),
            op: "read".to_string(),
            args: vec![],
        };
        let instrs = lower_comp(&comp).expect("lower should succeed");
        assert_eq!(
            instrs,
            vec![I::EffectOp {
                op: BackendEffectOp::Read(BackendReadOp::Read),
            }]
        );
    }

    #[test]
    fn lower_perform_print_with_var() {
        let comp = CompExpr::PerformEffect {
            effect: "Print".to_string(),
            op: "print".to_string(),
            args: vec![ValueExpr::Var("x".to_string())],
        };
        let instrs = lower_comp(&comp).expect("lower should succeed");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "x".to_string()
                },
                I::EffectOp {
                    op: BackendEffectOp::Print(BackendPrintOp::Print),
                },
            ]
        );
    }

    #[test]
    fn lower_let_read_then_print() {
        // let text = Read.read(); Print.print(text)
        let comp = CompExpr::Let {
            name: "text".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::PerformEffect {
                effect: "Read".to_string(),
                op: "read".to_string(),
                args: vec![],
            }),
            body: Box::new(CompExpr::PerformEffect {
                effect: "Print".to_string(),
                op: "print".to_string(),
                args: vec![ValueExpr::Var("text".to_string())],
            }),
        };
        let instrs = lower_comp(&comp).expect("lower should succeed");
        assert_eq!(
            instrs,
            vec![
                I::DeclareLocal {
                    name: "text".to_string()
                },
                I::EffectOp {
                    op: BackendEffectOp::Read(BackendReadOp::Read)
                },
                I::StoreLocal {
                    name: "text".to_string()
                },
                I::LoadLocal {
                    name: "text".to_string()
                },
                I::EffectOp {
                    op: BackendEffectOp::Print(BackendPrintOp::Print)
                },
            ]
        );
    }

    #[test]
    fn lower_with_handler_returns_err() {
        let comp = CompExpr::WithHandler {
            handler: Box::new(CompExpr::Value(ValueExpr::Unit)),
            body: Box::new(CompExpr::Value(ValueExpr::Unit)),
        };
        assert!(matches!(
            lower_comp(&comp),
            Err(LowerError::UnsupportedForm { .. })
        ));
    }

    #[test]
    fn lower_local_print_alias_call_emits_effect_op() {
        let comp = CompExpr::Let {
            name: "printer".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Value(ValueExpr::Var("print".to_string()))),
            body: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::Var("printer".to_string())),
                args: vec![ValueExpr::Var("text".to_string())],
            }),
        };
        let instrs = lower_comp(&comp).expect("local print alias call should lower");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "text".to_string()
                },
                I::EffectOp {
                    op: BackendEffectOp::Print(BackendPrintOp::Print),
                },
            ]
        );
    }

    #[test]
    fn lower_int_lit_zero() {
        let v = ValueExpr::IntLit(0);
        let instrs = lower_value(&v).expect("should encode 0");
        assert_eq!(instrs, vec![I::I64Const(encode_int(0).unwrap())]);
    }

    #[test]
    fn lower_str_lit_emits_static_string_push() {
        let v = ValueExpr::StrLit("x".to_string());
        let instrs = lower_value(&v).expect("static string lowering should succeed");
        assert_eq!(
            instrs,
            vec![I::PushStaticString {
                text: "x".to_string()
            }]
        );
    }

    #[test]
    fn lower_string_length_call_emits_explicit_intrinsic() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::GlobalRef {
                module: "string".to_string(),
                name: "length".to_string(),
            }),
            args: vec![ValueExpr::StrLit("hello".to_string())],
        };
        let instrs = lower_comp(&comp).expect("string.length should lower");
        assert_eq!(
            instrs,
            vec![
                I::PushStaticString {
                    text: "hello".to_string()
                },
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringLength
                },
            ]
        );
    }

    #[test]
    fn lower_runtime_intrinsic_bare_name_emits_explicit_intrinsic() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("__goby_string_each_grapheme".to_string())),
            args: vec![ValueExpr::Var("text".to_string())],
        };
        let instrs = lower_comp(&comp).expect("runtime intrinsic should lower");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "text".to_string()
                },
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringEachGraphemeCount
                },
            ]
        );
    }

    #[test]
    fn lower_runtime_intrinsic_bare_name_state_mode_emits_explicit_intrinsic() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("__goby_string_each_grapheme".to_string())),
            args: vec![
                ValueExpr::Var("text".to_string()),
                ValueExpr::Var("state".to_string()),
            ],
        };
        let instrs = lower_comp(&comp).expect("runtime intrinsic state mode should lower");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "text".to_string()
                },
                I::LoadLocal {
                    name: "state".to_string()
                },
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringEachGraphemeState
                },
            ]
        );
    }

    #[test]
    fn lower_split_each_uses_general_path_not_fused_split_instr() {
        let comp = CompExpr::Let {
            name: "lines".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "string".to_string(),
                    name: "split".to_string(),
                }),
                args: vec![
                    ValueExpr::Var("text".to_string()),
                    ValueExpr::StrLit("\n".to_string()),
                ],
            }),
            body: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::Var("each".to_string())),
                args: vec![
                    ValueExpr::Var("lines".to_string()),
                    ValueExpr::GlobalRef {
                        module: "Print".to_string(),
                        name: "println".to_string(),
                    },
                ],
            }),
        };
        let known_decls: HashSet<String> = ["each".to_string()].into();
        let mut lambda_decls = Vec::new();
        let instrs = lower_comp_collecting_lambdas(&comp, &known_decls, &mut lambda_decls)
            .expect("split+each should lower through the general path");
        assert!(
            !instrs.iter().any(|i| matches!(i, I::SplitEachPrint { .. })),
            "split+each should no longer emit SplitEachPrint: {instrs:?}"
        );
        assert!(
            instrs.iter().any(|i| matches!(
                i,
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringSplit
                }
            )) && instrs.iter().any(|i| {
                matches!(i, I::DeclCall { decl_name } if decl_name == "each")
                    || matches!(i, I::TailDeclCall { decl_name } if decl_name == "each")
            }),
            "split+each should include general-path pieces: {instrs:?}"
        );
    }

    #[test]
    fn lower_split_get_print_uses_general_path_not_fused_split_instr() {
        let comp = CompExpr::Let {
            name: "lines".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "string".to_string(),
                    name: "split".to_string(),
                }),
                args: vec![
                    ValueExpr::Var("text".to_string()),
                    ValueExpr::StrLit("\n".to_string()),
                ],
            }),
            body: Box::new(CompExpr::Let {
                name: "line".to_string(),
                ty: goby_core::ir::IrType::Unknown,
                value: Box::new(CompExpr::Call {
                    callee: Box::new(ValueExpr::GlobalRef {
                        module: "list".to_string(),
                        name: "get".to_string(),
                    }),
                    args: vec![ValueExpr::Var("lines".to_string()), ValueExpr::IntLit(1)],
                }),
                body: Box::new(CompExpr::PerformEffect {
                    effect: "Print".to_string(),
                    op: "println".to_string(),
                    args: vec![ValueExpr::Var("line".to_string())],
                }),
            }),
        };
        let instrs =
            lower_comp(&comp).expect("split+list.get+print should lower through the general path");
        assert!(
            !instrs.iter().any(|i| matches!(i, I::SplitGetPrint { .. })),
            "split+list.get should no longer emit SplitGetPrint: {instrs:?}"
        );
        assert!(
            instrs.iter().any(|i| matches!(
                i,
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::ListGet
                }
            )) || instrs.iter().any(|i| matches!(
                i,
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringSplit
                }
            )),
            "split+list.get should include general-path lowering: {instrs:?}"
        );
    }

    // --- BinOp lowering ---

    #[test]
    fn lower_binop_add_emits_left_right_binop() {
        use crate::gen_lower::backend_ir::WasmBackendInstr as I;
        use crate::gen_lower::value::encode_int;
        use goby_core::ir::IrBinOp;
        let v = ValueExpr::BinOp {
            op: IrBinOp::Add,
            left: Box::new(ValueExpr::IntLit(2)),
            right: Box::new(ValueExpr::IntLit(3)),
        };
        let instrs = lower_value(&v).expect("BinOp Add should lower");
        assert_eq!(
            instrs,
            vec![
                I::I64Const(encode_int(2).unwrap()),
                I::I64Const(encode_int(3).unwrap()),
                I::BinOp { op: IrBinOp::Add },
            ]
        );
    }

    #[test]
    fn lower_binop_eq_emits_left_right_binop() {
        use crate::gen_lower::backend_ir::WasmBackendInstr as I;
        use crate::gen_lower::value::encode_int;
        use goby_core::ir::IrBinOp;
        let v = ValueExpr::BinOp {
            op: IrBinOp::Eq,
            left: Box::new(ValueExpr::IntLit(5)),
            right: Box::new(ValueExpr::IntLit(5)),
        };
        let instrs = lower_value(&v).expect("BinOp Eq should lower");
        assert_eq!(
            instrs,
            vec![
                I::I64Const(encode_int(5).unwrap()),
                I::I64Const(encode_int(5).unwrap()),
                I::BinOp { op: IrBinOp::Eq },
            ]
        );
    }

    // --- LetMut / Assign ---

    #[test]
    fn lower_let_mut_emits_declare_store_body() {
        use crate::gen_lower::value::encode_int;
        // `mut x = 1; PerformEffect(Read, read)`
        let comp = CompExpr::LetMut {
            name: "x".to_string(),
            ty: goby_core::ir::IrType::Int,
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(1))),
            body: Box::new(CompExpr::PerformEffect {
                effect: "Read".to_string(),
                op: "read".to_string(),
                args: vec![],
            }),
        };
        let instrs = lower_comp(&comp).expect("LetMut should lower");
        assert_eq!(
            instrs,
            vec![
                I::DeclareLocal {
                    name: "x".to_string()
                },
                I::I64Const(encode_int(1).unwrap()),
                I::StoreLocal {
                    name: "x".to_string()
                },
                I::EffectOp {
                    op: BackendEffectOp::Read(BackendReadOp::Read)
                },
            ]
        );
    }

    #[test]
    fn lower_assign_emits_store_then_unit() {
        use crate::gen_lower::value::{encode_int, encode_unit};
        // `x := 2` inside a LetMut body (simulated as standalone Assign)
        let comp = CompExpr::Assign {
            name: "x".to_string(),
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(2))),
        };
        let instrs = lower_comp(&comp).expect("Assign should lower");
        assert_eq!(
            instrs,
            vec![
                I::I64Const(encode_int(2).unwrap()),
                I::StoreLocal {
                    name: "x".to_string()
                },
                I::I64Const(encode_unit()),
            ]
        );
    }

    #[test]
    fn lower_let_mut_with_assign_sequence() {
        use crate::gen_lower::value::{encode_int, encode_unit};
        // mut x = 1
        // x := 2
        // Read.read  (tail — provides Read effect for GeneralLowered path)
        let comp = CompExpr::LetMut {
            name: "x".to_string(),
            ty: goby_core::ir::IrType::Int,
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(1))),
            body: Box::new(CompExpr::Seq {
                stmts: vec![CompExpr::Assign {
                    name: "x".to_string(),
                    value: Box::new(CompExpr::Value(ValueExpr::IntLit(2))),
                }],
                tail: Box::new(CompExpr::PerformEffect {
                    effect: "Read".to_string(),
                    op: "read".to_string(),
                    args: vec![],
                }),
            }),
        };
        let instrs = lower_comp(&comp).expect("LetMut+Assign sequence should lower");
        assert_eq!(
            instrs,
            vec![
                I::DeclareLocal {
                    name: "x".to_string()
                },
                I::I64Const(encode_int(1).unwrap()),
                I::StoreLocal {
                    name: "x".to_string()
                },
                // Seq: Assign stmt + Drop
                I::I64Const(encode_int(2).unwrap()),
                I::StoreLocal {
                    name: "x".to_string()
                },
                I::I64Const(encode_unit()),
                I::Drop,
                // Seq tail
                I::EffectOp {
                    op: BackendEffectOp::Read(BackendReadOp::Read)
                },
            ]
        );
    }

    #[test]
    fn lower_globalref_decl_call_in_tail_position_emits_tail_decl_call() {
        // Tail-position direct call via GlobalRef should normalize to TailDeclCall.
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::GlobalRef {
                module: "mymod".to_string(),
                name: "add".to_string(),
            }),
            args: vec![ValueExpr::IntLit(2), ValueExpr::IntLit(3)],
        };
        let instrs = lower_comp(&comp).expect("GlobalRef decl call should lower");
        assert!(
            matches!(instrs.last(), Some(I::TailDeclCall { decl_name }) if decl_name == "add"),
            "last instruction must be TailDeclCall {{ decl_name: \"add\" }}, got: {:?}",
            instrs
        );
        assert_eq!(instrs.len(), 3, "2 arg pushes + 1 TailDeclCall");
    }

    #[test]
    fn lower_var_callee_not_in_known_decls_emits_indirect_call() {
        // Var(name) callee not in known_decls → IndirectCall (runtime funcref call).
        // lower_comp uses empty known_decls, so "f" is treated as a runtime function value.
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("f".to_string())),
            args: vec![ValueExpr::IntLit(1)],
        };
        let instrs = lower_comp(&comp).expect("Var callee IndirectCall should lower OK");
        // Expected: [I64Const(encode_int(1)), LoadLocal("f"), IndirectCall]
        assert_eq!(
            instrs.len(),
            3,
            "arg push + LoadLocal callee + IndirectCall"
        );
        assert!(
            matches!(instrs[1], I::LoadLocal { ref name } if name == "f"),
            "second instr must be LoadLocal(f), got: {:?}",
            instrs
        );
        assert!(
            matches!(instrs[2], I::IndirectCall { arity: 1 }),
            "last instr must be IndirectCall {{ arity: 1 }}, got: {:?}",
            instrs
        );
    }

    #[test]
    fn lower_var_callee_in_known_decls_emits_tail_decl_call_in_tail_position() {
        // Var(name) where name ∈ known_decls → TailDeclCall in tail position.
        use std::collections::HashSet;
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("helper".to_string())),
            args: vec![ValueExpr::IntLit(2)],
        };
        let known_decls: HashSet<String> = ["helper".to_string()].into();
        let bindings = ClosureBindingEnv::default();
        let instrs = lower_comp_inner(
            &comp,
            true,
            &HashMap::new(),
            &bindings,
            &known_decls,
            &mut Vec::new(),
        )
        .expect("known_decl Var callee should lower");
        assert!(
            matches!(instrs.last(), Some(I::TailDeclCall { decl_name }) if decl_name == "helper"),
            "last instr must be TailDeclCall(helper), got: {:?}",
            instrs
        );
    }

    #[test]
    fn lower_direct_decl_call_in_seq_stmt_stays_non_tail_decl_call() {
        use std::collections::HashSet;
        let comp = CompExpr::Seq {
            stmts: vec![CompExpr::Call {
                callee: Box::new(ValueExpr::Var("helper".to_string())),
                args: vec![ValueExpr::IntLit(1)],
            }],
            tail: Box::new(CompExpr::Value(ValueExpr::Unit)),
        };
        let known_decls: HashSet<String> = ["helper".to_string()].into();
        let instrs = lower_comp_with_decls(&comp, &known_decls).expect("seq should lower");
        assert!(
            instrs.iter().any(|instr| matches!(
                instr,
                I::DeclCall { decl_name } if decl_name == "helper"
            )),
            "non-tail seq stmt should keep DeclCall, got: {instrs:?}"
        );
        assert!(
            !instrs.iter().any(|instr| matches!(
                instr,
                I::TailDeclCall { decl_name } if decl_name == "helper"
            )),
            "non-tail seq stmt must not normalize to TailDeclCall, got: {instrs:?}"
        );
    }

    #[test]
    fn lower_var_arg_in_known_decls_emits_push_func_handle() {
        // When a Var(name) appears as an argument and name ∈ known_decls,
        // it should be lowered as PushFuncHandle (not LoadLocal).
        use std::collections::HashSet;
        // f is NOT in known_decls (it's a runtime funcref); add_one IS in known_decls.
        // Call: f add_one  →  [PushFuncHandle("add_one"), LoadLocal("f"), IndirectCall]
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("f".to_string())),
            args: vec![ValueExpr::Var("add_one".to_string())],
        };
        let known_decls: HashSet<String> = ["add_one".to_string()].into();
        let bindings = ClosureBindingEnv::default();
        let instrs = lower_comp_inner(
            &comp,
            true,
            &HashMap::new(),
            &bindings,
            &known_decls,
            &mut Vec::new(),
        )
        .expect("funcref arg should lower OK");
        assert_eq!(
            instrs.len(),
            3,
            "PushFuncHandle + LoadLocal callee + IndirectCall"
        );
        assert!(
            matches!(&instrs[0], I::PushFuncHandle { decl_name } if decl_name == "add_one"),
            "first instr must be PushFuncHandle(add_one), got: {:?}",
            instrs
        );
        assert!(
            matches!(&instrs[1], I::LoadLocal { name } if name == "f"),
            "second instr must be LoadLocal(f), got: {:?}",
            instrs
        );
        assert!(
            matches!(&instrs[2], I::IndirectCall { arity: 1 }),
            "last instr must be IndirectCall {{ arity: 1 }}, got: {:?}",
            instrs
        );
    }

    // ------------------------------------------------------------------
    // Case lowering tests
    // ------------------------------------------------------------------

    #[test]
    fn case_int_lit_and_wildcard_produces_case_match() {
        use crate::gen_lower::backend_ir::BackendCasePattern;
        use goby_core::ir::{IrCaseArm, IrCasePattern};

        // case x { 0 -> "zero" | _ -> "other" }
        let comp = CompExpr::Case {
            scrutinee: Box::new(ValueExpr::Var("x".to_string())),
            arms: vec![
                IrCaseArm {
                    pattern: IrCasePattern::IntLit(0),
                    body: CompExpr::Value(ValueExpr::StrLit("zero".to_string())),
                },
                IrCaseArm {
                    pattern: IrCasePattern::Wildcard,
                    body: CompExpr::Value(ValueExpr::StrLit("other".to_string())),
                },
            ],
        };
        let result = lower_comp(&comp).expect("Case lowering should succeed");
        // lower_case emits: DeclareLocal, scrutinee instrs, StoreLocal, CaseMatch
        assert_eq!(
            result.len(),
            4,
            "expected 4 instrs (declare+eval+store+CaseMatch)"
        );
        // result[0] = DeclareLocal
        assert!(matches!(&result[0], WasmBackendInstr::DeclareLocal { .. }));
        // result[1] = scrutinee: LoadLocal("x")
        assert_eq!(
            result[1],
            WasmBackendInstr::LoadLocal {
                name: "x".to_string()
            }
        );
        // result[2] = StoreLocal
        assert!(matches!(&result[2], WasmBackendInstr::StoreLocal { .. }));
        // result[3] = CaseMatch
        let WasmBackendInstr::CaseMatch {
            scrutinee_local,
            arms,
        } = &result[3]
        else {
            panic!("expected CaseMatch, got {:?}", result[3]);
        };
        // scrutinee_local matches the DeclareLocal name
        let WasmBackendInstr::DeclareLocal { name: decl_name } = &result[0] else {
            unreachable!()
        };
        assert_eq!(scrutinee_local, decl_name);
        assert_eq!(arms.len(), 2);
        assert_eq!(arms[0].pattern, BackendCasePattern::IntLit(0));
        assert_eq!(arms[1].pattern, BackendCasePattern::Wildcard);
    }

    #[test]
    fn case_bool_lit_produces_case_match() {
        use crate::gen_lower::backend_ir::BackendCasePattern;
        use goby_core::ir::{IrCaseArm, IrCasePattern};

        let comp = CompExpr::Case {
            scrutinee: Box::new(ValueExpr::BoolLit(true)),
            arms: vec![
                IrCaseArm {
                    pattern: IrCasePattern::BoolLit(true),
                    body: CompExpr::Value(ValueExpr::StrLit("yes".to_string())),
                },
                IrCaseArm {
                    pattern: IrCasePattern::Wildcard,
                    body: CompExpr::Value(ValueExpr::StrLit("no".to_string())),
                },
            ],
        };
        let result = lower_comp(&comp).expect("Case bool lowering should succeed");
        assert_eq!(
            result.len(),
            4,
            "expected 4 instrs (declare+eval+store+CaseMatch)"
        );
        let WasmBackendInstr::CaseMatch { arms, .. } = &result[3] else {
            panic!("expected CaseMatch");
        };
        assert_eq!(arms[0].pattern, BackendCasePattern::BoolLit(true));
    }

    #[test]
    fn case_str_lit_produces_case_match() {
        use crate::gen_lower::backend_ir::BackendCasePattern;
        use goby_core::ir::{IrCaseArm, IrCasePattern};

        let comp = CompExpr::Case {
            scrutinee: Box::new(ValueExpr::Var("s".to_string())),
            arms: vec![
                IrCaseArm {
                    pattern: IrCasePattern::StringLit("hello".to_string()),
                    body: CompExpr::Value(ValueExpr::IntLit(1)),
                },
                IrCaseArm {
                    pattern: IrCasePattern::Wildcard,
                    body: CompExpr::Value(ValueExpr::IntLit(0)),
                },
            ],
        };
        let result = lower_comp(&comp).expect("Case str lit lowering should succeed");
        assert_eq!(
            result.len(),
            4,
            "expected 4 instrs (declare+eval+store+CaseMatch)"
        );
        let WasmBackendInstr::CaseMatch { arms, .. } = &result[3] else {
            panic!("expected CaseMatch");
        };
        assert_eq!(
            arms[0].pattern,
            BackendCasePattern::StrLit("hello".to_string())
        );
    }

    #[test]
    fn case_empty_list_produces_case_match() {
        use crate::gen_lower::backend_ir::BackendCasePattern;
        use goby_core::ir::{IrCaseArm, IrCasePattern};

        let comp = CompExpr::Case {
            scrutinee: Box::new(ValueExpr::Var("xs".to_string())),
            arms: vec![
                IrCaseArm {
                    pattern: IrCasePattern::EmptyList,
                    body: CompExpr::Value(ValueExpr::IntLit(0)),
                },
                IrCaseArm {
                    pattern: IrCasePattern::Wildcard,
                    body: CompExpr::Value(ValueExpr::IntLit(1)),
                },
            ],
        };
        let result = lower_comp(&comp).expect("Case empty list lowering should succeed");
        assert_eq!(
            result.len(),
            4,
            "expected 4 instrs (declare+eval+store+CaseMatch)"
        );
        let WasmBackendInstr::CaseMatch { arms, .. } = &result[3] else {
            panic!("expected CaseMatch");
        };
        assert_eq!(arms[0].pattern, BackendCasePattern::EmptyList);
    }

    #[test]
    fn case_list_pattern_produces_case_match() {
        use crate::gen_lower::backend_ir::{BackendCasePattern, BackendListPatternItem};
        use goby_core::ir::{IrCaseArm, IrCasePattern, IrListPatternItem, IrListPatternTail};

        // case xs { [h, ..t] -> h | _ -> 0 }
        let comp = CompExpr::Case {
            scrutinee: Box::new(ValueExpr::Var("xs".to_string())),
            arms: vec![
                IrCaseArm {
                    pattern: IrCasePattern::ListPattern {
                        items: vec![IrListPatternItem::Bind("h".to_string())],
                        tail: Some(IrListPatternTail::Bind("t".to_string())),
                    },
                    body: CompExpr::Value(ValueExpr::Var("h".to_string())),
                },
                IrCaseArm {
                    pattern: IrCasePattern::Wildcard,
                    body: CompExpr::Value(ValueExpr::IntLit(0)),
                },
            ],
        };
        let result = lower_comp(&comp).expect("Case list pattern lowering should succeed");
        // DeclareLocal(scrutinee) + DeclareLocal(h) + DeclareLocal(t) + eval + StoreLocal + CaseMatch
        assert_eq!(
            result.len(),
            6,
            "expected 6 instrs for list pattern with h and tail t"
        );
        let WasmBackendInstr::CaseMatch { arms, .. } = &result[5] else {
            panic!("expected CaseMatch");
        };
        assert_eq!(
            arms[0].pattern,
            BackendCasePattern::ListPattern {
                items: vec![BackendListPatternItem::Bind("h".to_string())],
                tail: Some("t".to_string()),
            }
        );
    }

    #[test]
    fn case_list_pattern_ignore_tail_produces_none() {
        use crate::gen_lower::backend_ir::BackendCasePattern;
        use goby_core::ir::{IrCaseArm, IrCasePattern, IrListPatternItem, IrListPatternTail};

        let comp = CompExpr::Case {
            scrutinee: Box::new(ValueExpr::Var("xs".to_string())),
            arms: vec![IrCaseArm {
                pattern: IrCasePattern::ListPattern {
                    items: vec![IrListPatternItem::Bind("h".to_string())],
                    tail: Some(IrListPatternTail::Ignore),
                },
                body: CompExpr::Value(ValueExpr::Var("h".to_string())),
            }],
        };
        let result = lower_comp(&comp).expect("Case list pattern with ignore tail should succeed");
        // DeclareLocal(scrutinee) + DeclareLocal(h) + eval + StoreLocal + CaseMatch
        assert_eq!(
            result.len(),
            5,
            "expected 5 instrs for list pattern with h (no tail)"
        );
        let WasmBackendInstr::CaseMatch { arms, .. } = &result[4] else {
            panic!("expected CaseMatch");
        };
        // Ignore tail → tail = None in backend pattern
        assert_eq!(
            arms[0].pattern,
            BackendCasePattern::ListPattern {
                items: vec![crate::gen_lower::backend_ir::BackendListPatternItem::Bind(
                    "h".to_string()
                )],
                tail: None,
            }
        );
    }

    #[test]
    fn case_arm_body_with_decl_call() {
        use goby_core::ir::{IrCaseArm, IrCasePattern};
        use std::collections::HashSet;

        // case x { 1 -> helper(x) | _ -> x }  where "helper" is a known decl
        let mut known = HashSet::new();
        known.insert("helper".to_string());

        let comp = CompExpr::Case {
            scrutinee: Box::new(ValueExpr::Var("x".to_string())),
            arms: vec![
                IrCaseArm {
                    pattern: IrCasePattern::IntLit(1),
                    body: CompExpr::Call {
                        callee: Box::new(ValueExpr::Var("helper".to_string())),
                        args: vec![ValueExpr::Var("x".to_string())],
                    },
                },
                IrCaseArm {
                    pattern: IrCasePattern::Wildcard,
                    body: CompExpr::Value(ValueExpr::Var("x".to_string())),
                },
            ],
        };
        let result =
            lower_comp_with_decls(&comp, &known).expect("Case with DeclCall arm should succeed");
        assert_eq!(
            result.len(),
            4,
            "expected 4 instrs (declare+eval+store+CaseMatch)"
        );
        let WasmBackendInstr::CaseMatch { arms, .. } = &result[3] else {
            panic!("expected CaseMatch");
        };
        // The first arm body is in tail position within the arm, so it should
        // normalize to TailDeclCall.
        assert!(
            arms[0].body_instrs.iter().any(
                |i| matches!(i, WasmBackendInstr::TailDeclCall { decl_name } if decl_name == "helper")
            ),
            "arm body should contain TailDeclCall(helper)"
        );
    }

    #[test]
    fn tuple_lit_lowers_to_backend_tuple_lit() {
        let result = lower_value(&ValueExpr::TupleLit(vec![
            ValueExpr::IntLit(1),
            ValueExpr::StrLit("hello".to_string()),
        ]))
        .expect("TupleLit lowering should succeed");

        let [WasmBackendInstr::TupleLit { element_instrs }] = result.as_slice() else {
            panic!("expected a single TupleLit backend instruction");
        };
        assert_eq!(element_instrs.len(), 2, "tuple arity should be preserved");
    }

    #[test]
    fn empty_tuple_lowers_to_unit() {
        let result =
            lower_value(&ValueExpr::TupleLit(vec![])).expect("empty tuple lowering should succeed");

        assert_eq!(
            result,
            vec![WasmBackendInstr::I64Const(encode_unit())],
            "empty tuple must lower to Unit"
        );
    }

    #[test]
    fn record_lit_lowers_to_backend_record_lit() {
        let result = lower_value(&ValueExpr::RecordLit {
            constructor: "Pair".to_string(),
            fields: vec![
                ("left".to_string(), ValueExpr::IntLit(1)),
                ("right".to_string(), ValueExpr::StrLit("hello".to_string())),
            ],
        })
        .expect("RecordLit lowering should succeed");

        let [
            WasmBackendInstr::RecordLit {
                constructor,
                field_instrs,
            },
        ] = result.as_slice()
        else {
            panic!("expected a single RecordLit backend instruction");
        };
        assert_eq!(constructor, "Pair");
        assert_eq!(
            field_instrs.len(),
            2,
            "record field count should be preserved"
        );
    }

    // Lambda lowering tests

    /// Lambda with param-only body passed to `__goby_list_map` should lower to
    /// PushFuncHandle + Intrinsic(ListMap) (not UnsupportedForm).
    #[test]
    fn lower_lambda_as_map_callback_param_only() {
        // map [1, 2, 3] (fn x -> x + 1)
        // Lowered IR:
        //   let list = [1, 2, 3]
        //   map list (fn x -> x + 1)
        let known_decls: HashSet<String> = HashSet::new();
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("__goby_list_map".to_string())),
            args: vec![
                ValueExpr::ListLit {
                    elements: vec![
                        ValueExpr::IntLit(1),
                        ValueExpr::IntLit(2),
                        ValueExpr::IntLit(3),
                    ],
                    spread: None,
                },
                ValueExpr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(CompExpr::Value(ValueExpr::BinOp {
                        op: goby_core::ir::IrBinOp::Add,
                        left: Box::new(ValueExpr::Var("x".to_string())),
                        right: Box::new(ValueExpr::IntLit(1)),
                    })),
                },
            ],
        };
        let mut lambda_decls = Vec::new();
        let result = lower_comp_collecting_lambdas(&comp, &known_decls, &mut lambda_decls);
        assert!(
            result.is_ok(),
            "Lambda as map callback should lower to PushFuncHandle+Intrinsic(ListMap), got: {:?}",
            result
        );
        let instrs = result.unwrap();
        // After M5-5: map emits flat [ListLit, PushFuncHandle, Intrinsic { ListMap }]
        assert!(
            matches!(
                instrs.last(),
                Some(WasmBackendInstr::Intrinsic {
                    intrinsic: BackendIntrinsic::ListMap
                })
            ),
            "expected Intrinsic {{ ListMap }} as last instr, got: {:?}",
            instrs
        );
        // Lambda should have been lifted as a LambdaAuxDecl.
        assert_eq!(lambda_decls.len(), 1, "expected exactly one lambda AuxDecl");
        assert_eq!(lambda_decls[0].param_names, vec!["x"]);
        // The func arg should be a PushFuncHandle referencing the lifted lambda.
        assert!(
            instrs.iter().any(|i| matches!(
                i,
                WasmBackendInstr::PushFuncHandle { decl_name } if decl_name.starts_with("__lambda_")
            )),
            "expected PushFuncHandle(__lambda_N) in instrs, got: {:?}",
            instrs
        );
    }

    #[test]
    fn lower_list_literal_with_spread_uses_list_concat_intrinsic() {
        let comp = CompExpr::Value(ValueExpr::ListLit {
            elements: vec![ValueExpr::IntLit(1), ValueExpr::IntLit(2)],
            spread: Some(Box::new(ValueExpr::Var("rest".to_string()))),
        });
        let instrs = lower_comp(&comp).expect("list spread should lower");
        assert!(
            matches!(
                instrs.as_slice(),
                [
                    I::ListLit { element_instrs },
                    I::LoadLocal { name },
                    I::Intrinsic {
                        intrinsic: BackendIntrinsic::ListConcat
                    }
                ] if element_instrs.len() == 2 && name == "rest"
            ),
            "expected list spread to lower as prefix ListLit + tail + ListConcat, got: {:?}",
            instrs
        );
    }

    /// Lambda with a ByValue-captured free variable should now lower successfully.
    /// The lowering produces a CreateClosure instruction; MutableWrite captures remain
    /// UnsupportedForm.
    #[test]
    fn lower_lambda_with_by_value_capture_succeeds() {
        use std::collections::HashSet;
        // fn x -> x + base   (where `base` is a free variable captured ByValue)
        let known_decls: HashSet<String> = HashSet::new();
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("__goby_list_map".to_string())),
            args: vec![
                ValueExpr::ListLit {
                    elements: vec![ValueExpr::IntLit(1)],
                    spread: None,
                },
                ValueExpr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(CompExpr::Value(ValueExpr::BinOp {
                        op: goby_core::ir::IrBinOp::Add,
                        left: Box::new(ValueExpr::Var("x".to_string())),
                        right: Box::new(ValueExpr::Var("base".to_string())), // free variable → ByValue capture
                    })),
                },
            ],
        };
        let mut lambda_decls = Vec::new();
        let result = lower_comp_collecting_lambdas(&comp, &known_decls, &mut lambda_decls);
        assert!(
            result.is_ok(),
            "Lambda with ByValue-captured free variable should lower successfully, got: {:?}",
            result
        );
        // The lambda should be collected and its first param should be __clo.
        assert_eq!(lambda_decls.len(), 1, "expected exactly one lambda AuxDecl");
        assert_eq!(
            lambda_decls[0].param_names,
            vec!["__clo", "x"],
            "capturing lambda param_names should be [__clo, param]"
        );
    }

    #[test]
    fn lower_multi_param_lambda_with_capture_flattens_params_into_one_aux_decl() {
        let known_decls: HashSet<String> = HashSet::new();
        let comp = CompExpr::Value(ValueExpr::Lambda {
            param: "acc".to_string(),
            body: Box::new(CompExpr::Value(ValueExpr::Lambda {
                param: "x".to_string(),
                body: Box::new(CompExpr::Value(ValueExpr::BinOp {
                    op: goby_core::ir::IrBinOp::Add,
                    left: Box::new(ValueExpr::BinOp {
                        op: goby_core::ir::IrBinOp::Add,
                        left: Box::new(ValueExpr::Var("acc".to_string())),
                        right: Box::new(ValueExpr::Var("x".to_string())),
                    }),
                    right: Box::new(ValueExpr::Var("bias".to_string())),
                })),
            })),
        });
        let mut lambda_decls = Vec::new();
        let instrs = lower_comp_collecting_lambdas(&comp, &known_decls, &mut lambda_decls)
            .expect("multi-param capturing lambda should lower");
        assert!(
            matches!(instrs.as_slice(), [WasmBackendInstr::CreateClosure { .. }]),
            "expected CreateClosure, got: {:?}",
            instrs
        );
        assert_eq!(lambda_decls.len(), 1, "expected one lifted lambda");
        assert_eq!(lambda_decls[0].param_names, vec!["__clo", "acc", "x"]);
    }

    /// Let-bound capturing closure called directly emits the generic indirect-call path.
    /// `base = 10; add10 = (fn x -> base + x); add10 5` — add10 is a closure.
    #[test]
    fn lower_let_bound_capturing_lambda_call_emits_generic_indirect_call() {
        use std::collections::HashSet;
        // Simulate:
        //   base = 10
        //   add10 = fn x -> base + x   <- CreateClosure (capturing 'base')
        //   add10 5                     <- IndirectCall { arity: 1 }
        let known_decls: HashSet<String> = HashSet::new();

        // let base = 10; let add10 = (fn x -> base + x); add10 5
        let comp = CompExpr::Let {
            name: "base".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(10))),
            body: Box::new(CompExpr::Let {
                name: "add10".to_string(),
                ty: goby_core::ir::IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(CompExpr::Value(ValueExpr::BinOp {
                        op: goby_core::ir::IrBinOp::Add,
                        left: Box::new(ValueExpr::Var("base".to_string())),
                        right: Box::new(ValueExpr::Var("x".to_string())),
                    })),
                })),
                body: Box::new(CompExpr::Call {
                    callee: Box::new(ValueExpr::Var("add10".to_string())),
                    args: vec![ValueExpr::IntLit(5)],
                }),
            }),
        };

        let mut lambda_decls = Vec::new();
        let instrs = lower_comp_collecting_lambdas(&comp, &known_decls, &mut lambda_decls)
            .expect("should lower successfully");
        // The output should route through the generic indirect-call instruction.
        assert!(
            instrs
                .iter()
                .any(|i| matches!(i, WasmBackendInstr::IndirectCall { arity: 1 })),
            "expected IndirectCall {{ arity: 1 }} in top-level instrs, got: {:?}",
            instrs
        );
        // Lambda should be collected with __clo as first param.
        assert_eq!(lambda_decls.len(), 1, "expected one lambda AuxDecl");
        assert_eq!(
            lambda_decls[0].param_names,
            vec!["__clo", "x"],
            "capturing lambda should have param_names [__clo, x]"
        );
    }

    #[test]
    fn lower_two_shared_cell_closures_capture_cell_pointer_in_both_envs() {
        let known_decls: HashSet<String> = HashSet::new();
        let comp = CompExpr::LetMut {
            name: "count".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(0))),
            body: Box::new(CompExpr::Let {
                name: "inc".to_string(),
                ty: goby_core::ir::IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::Lambda {
                    param: "_".to_string(),
                    body: Box::new(CompExpr::Assign {
                        name: "count".to_string(),
                        value: Box::new(CompExpr::Value(ValueExpr::BinOp {
                            op: goby_core::ir::IrBinOp::Add,
                            left: Box::new(ValueExpr::Var("count".to_string())),
                            right: Box::new(ValueExpr::IntLit(1)),
                        })),
                    }),
                })),
                body: Box::new(CompExpr::Let {
                    name: "get".to_string(),
                    ty: goby_core::ir::IrType::Unknown,
                    value: Box::new(CompExpr::Value(ValueExpr::Lambda {
                        param: "_".to_string(),
                        body: Box::new(CompExpr::Value(ValueExpr::Var("count".to_string()))),
                    })),
                    body: Box::new(CompExpr::Value(ValueExpr::TupleLit(vec![
                        ValueExpr::Var("inc".to_string()),
                        ValueExpr::Var("get".to_string()),
                    ]))),
                }),
            }),
        };

        let mut lambda_decls = Vec::new();
        let instrs = lower_comp_collecting_lambdas(&comp, &known_decls, &mut lambda_decls)
            .expect("shared-cell pair helper shape should lower");

        let create_closures: Vec<&WasmBackendInstr> = instrs
            .iter()
            .filter(|instr| matches!(instr, WasmBackendInstr::CreateClosure { .. }))
            .collect();
        assert_eq!(create_closures.len(), 2, "expected two captured closures");
        for instr in create_closures {
            let WasmBackendInstr::CreateClosure { slot_instrs, .. } = instr else {
                unreachable!();
            };
            assert_eq!(
                slot_instrs.len(),
                1,
                "expected one shared-cell capture slot"
            );
            assert!(
                matches!(
                    slot_instrs[0].as_slice(),
                    [WasmBackendInstr::LoadLocal { name }] if name == "__cell_count"
                ),
                "shared-cell capture must store the outer cell pointer, got {:?}",
                slot_instrs
            );
        }
        assert_eq!(lambda_decls.len(), 2, "expected two lifted lambda decls");
        for lambda in &lambda_decls {
            assert!(
                lambda
                    .instrs
                    .iter()
                    .any(|instr| matches!(instr, WasmBackendInstr::StoreLocal { name } if name == "__cell_count")),
                "lambda body should hydrate __cell_count from the closure env, got {:?}",
                lambda.instrs
            );
        }
    }

    /// `Call(GlobalRef { "string", "graphemes" }, [Var("text")])` → `Intrinsic { StringGraphemesList }`.
    #[test]
    fn lower_string_graphemes_globalref_lowers_as_intrinsic() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::GlobalRef {
                module: "string".to_string(),
                name: "graphemes".to_string(),
            }),
            args: vec![ValueExpr::Var("text".to_string())],
        };
        let instrs =
            lower_comp(&comp).expect("string.graphemes GlobalRef should lower to intrinsic");
        assert!(
            matches!(
                instrs.as_slice(),
                [
                    I::LoadLocal { .. },
                    I::Intrinsic {
                        intrinsic: BackendIntrinsic::StringGraphemesList
                    }
                ]
            ),
            "expected [LoadLocal, Intrinsic(StringGraphemesList)], got: {:?}",
            instrs
        );
    }

    /// `Var("graphemes")` resolving to `string.graphemes` via alias also lowers as intrinsic.
    #[test]
    fn lower_string_graphemes_alias_var_lowers_as_intrinsic() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("graphemes".to_string())),
            args: vec![ValueExpr::Var("text".to_string())],
        };
        let aliases = HashMap::from([(
            "graphemes".to_string(),
            AliasValue::GlobalRef {
                module: "string".to_string(),
                name: "graphemes".to_string(),
            },
        )]);
        let instrs = lower_comp_inner(
            &comp,
            true,
            &aliases,
            &ClosureBindingEnv::default(),
            &HashSet::new(),
            &mut Vec::new(),
        )
        .expect("graphemes alias Var should lower to intrinsic");
        assert!(
            matches!(
                instrs.as_slice(),
                [
                    I::LoadLocal { .. },
                    I::Intrinsic {
                        intrinsic: BackendIntrinsic::StringGraphemesList
                    }
                ]
            ),
            "expected [LoadLocal, Intrinsic(StringGraphemesList)], got: {:?}",
            instrs
        );
    }

    #[test]
    fn lower_string_split_empty_delimiter_lowers_as_string_split_intrinsic() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::GlobalRef {
                module: "string".to_string(),
                name: "split".to_string(),
            }),
            args: vec![
                ValueExpr::Var("text".to_string()),
                ValueExpr::StrLit(String::new()),
            ],
        };
        let instrs = lower_comp(&comp).expect("string.split with empty delimiter should lower");
        assert!(
            matches!(
                instrs.as_slice(),
                [
                    I::LoadLocal { .. },
                    I::PushStaticString { .. },
                    I::Intrinsic {
                        intrinsic: BackendIntrinsic::StringSplit
                    }
                ]
            ),
            "expected StringSplit intrinsic lowering, got: {:?}",
            instrs
        );
    }

    #[test]
    fn lower_value_arg_graphemes_prefers_intrinsic_wrapper_over_known_decl_handle() {
        let aliases = HashMap::from([(
            "graphemes".to_string(),
            AliasValue::GlobalRef {
                module: "string".to_string(),
                name: "graphemes".to_string(),
            },
        )]);
        let known_decls = HashSet::from(["graphemes".to_string()]);
        let mut lambda_decls = Vec::new();
        let instrs = lower_value_as_arg(
            &ValueExpr::Var("graphemes".to_string()),
            &aliases,
            &ClosureBindingEnv::default(),
            &known_decls,
            &mut lambda_decls,
        )
        .expect("graphemes function value should lower");
        assert!(
            matches!(instrs.as_slice(), [I::PushFuncHandle { decl_name }] if decl_name.starts_with("__graphemes_wrapper_")),
            "expected graphemes wrapper handle, got: {:?}",
            instrs
        );
        assert_eq!(lambda_decls.len(), 1, "expected one wrapper lambda");
        assert!(
            matches!(
                lambda_decls[0].instrs.as_slice(),
                [
                    I::LoadLocal { .. },
                    I::Intrinsic {
                        intrinsic: BackendIntrinsic::StringGraphemesList
                    }
                ]
            ),
            "expected wrapper lambda to call StringGraphemesList, got: {:?}",
            lambda_decls[0].instrs
        );
    }

    // --- FOLD-M2 / FOLD-M3a: 2-argument callback lower ---
    //
    // After FOLD-M3a `IndirectCall { arity: u8 }` was added, these tests assert
    // `IndirectCall { arity: 2 }` for a 2-arg local callback call.

    /// `f acc x` where `f` is a local variable — the prototypical fold callback call.
    /// FOLD-M3a: now lowers to `IndirectCall { arity: 2 }`.
    #[test]
    fn fold_m2_two_arg_local_callback_call_lower_baseline() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("f".to_string())),
            args: vec![
                ValueExpr::Var("acc".to_string()),
                ValueExpr::Var("x".to_string()),
            ],
        };
        let instrs = lower_comp(&comp).expect("2-arg local callback call must lower without error");
        // Expected: push acc, push x, load f, IndirectCall { arity: 2 } — in that order.
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "acc".to_string()
                },
                I::LoadLocal {
                    name: "x".to_string()
                },
                I::LoadLocal {
                    name: "f".to_string()
                },
                I::IndirectCall { arity: 2 },
            ]
        );
    }

    /// Named top-level function used as a fold callback: `add acc x`
    /// where `add` is in `known_decls`.  This path uses `DeclCall`, so it
    /// already works correctly for any arity — confirmed here as a reference.
    #[test]
    fn fold_m2_two_arg_named_decl_callback_call_uses_decl_call() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("add".to_string())),
            args: vec![
                ValueExpr::Var("acc".to_string()),
                ValueExpr::Var("x".to_string()),
            ],
        };
        let known: HashSet<String> = ["add".to_string()].into();
        let instrs = lower_comp_with_decls(&comp, &known)
            .expect("named-decl 2-arg call must lower without error");
        // TailDeclCall is the normalized tail-position form; the backend
        // currently emits it like a direct Wasm `call`.
        // however many args the Wasm function expects — no arity restriction.
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "acc".to_string()
                },
                I::LoadLocal {
                    name: "x".to_string()
                },
                I::TailDeclCall {
                    decl_name: "add".to_string()
                },
            ]
        );
    }

    /// Cell-promoted root (closure-captured mut), depth-1.
    /// xs[0] := 99 where xs is CellPromoted.
    /// Expected: scratch local read from cell, list_set, StoreCellValue write-back.
    #[test]
    fn lower_assign_index_cell_promoted_single_level_uses_cell() {
        use crate::gen_lower::value::{encode_int, encode_unit};
        let comp = CompExpr::AssignIndex {
            root: "xs".to_string(),
            path: vec![ValueExpr::IntLit(0)],
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(99))),
        };
        let aliases: HashMap<String, AliasValue> =
            [("xs".to_string(), AliasValue::CellPromoted)].into();
        let instrs = lower_comp_with_aliases(&comp, &aliases)
            .expect("cell-promoted AssignIndex should lower");
        assert_eq!(
            instrs,
            vec![
                // Scratch local: cache cell value once
                I::DeclareLocal {
                    name: "__lset_xs_root".to_string()
                },
                I::LoadLocal {
                    name: "__cell_xs".to_string()
                },
                I::LoadCellValue,
                I::StoreLocal {
                    name: "__lset_xs_root".to_string()
                },
                // Ascent tmp
                I::DeclareLocal {
                    name: "__lset_xs_u0".to_string()
                },
                // Ascent: list_set(root_scratch, 0, 99) → u0
                I::LoadLocal {
                    name: "__lset_xs_root".to_string()
                },
                I::I64Const(encode_int(0).unwrap()),
                I::I64Const(encode_int(99).unwrap()),
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::ListSet
                },
                I::StoreLocal {
                    name: "__lset_xs_u0".to_string()
                },
                // Write back through cell
                I::StoreCellValue {
                    cell_ptr_instrs: vec![I::LoadLocal {
                        name: "__cell_xs".to_string()
                    }],
                    value_instrs: vec![I::LoadLocal {
                        name: "__lset_xs_u0".to_string()
                    }],
                },
                // Unit
                I::I64Const(encode_unit()),
            ]
        );
    }

    /// Cell-promoted root (closure-captured mut), depth-2.
    /// xs[1][0] := 42 where xs is CellPromoted.
    /// Expected: scratch local, descent via list_get, ascent, StoreCellValue write-back.
    #[test]
    fn lower_assign_index_cell_promoted_two_levels_uses_cell() {
        use crate::gen_lower::value::{encode_int, encode_unit};
        let comp = CompExpr::AssignIndex {
            root: "xs".to_string(),
            path: vec![ValueExpr::IntLit(1), ValueExpr::IntLit(0)],
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(42))),
        };
        let aliases: HashMap<String, AliasValue> =
            [("xs".to_string(), AliasValue::CellPromoted)].into();
        let instrs = lower_comp_with_aliases(&comp, &aliases)
            .expect("cell-promoted two-level AssignIndex should lower");
        assert_eq!(
            instrs,
            vec![
                // Scratch local
                I::DeclareLocal {
                    name: "__lset_xs_root".to_string()
                },
                I::LoadLocal {
                    name: "__cell_xs".to_string()
                },
                I::LoadCellValue,
                I::StoreLocal {
                    name: "__lset_xs_root".to_string()
                },
                // Descent tmp + two ascent tmps
                I::DeclareLocal {
                    name: "__lset_xs_d0".to_string()
                },
                I::DeclareLocal {
                    name: "__lset_xs_u0".to_string()
                },
                I::DeclareLocal {
                    name: "__lset_xs_u1".to_string()
                },
                // Descent: root_scratch[1] → d0
                I::LoadLocal {
                    name: "__lset_xs_root".to_string()
                },
                I::I64Const(encode_int(1).unwrap()),
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::ListGet
                },
                I::StoreLocal {
                    name: "__lset_xs_d0".to_string()
                },
                // Ascent innermost: list_set(d0, 0, 42) → u1
                I::LoadLocal {
                    name: "__lset_xs_d0".to_string()
                },
                I::I64Const(encode_int(0).unwrap()),
                I::I64Const(encode_int(42).unwrap()),
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::ListSet
                },
                I::StoreLocal {
                    name: "__lset_xs_u1".to_string()
                },
                // Ascent outer: list_set(root_scratch, 1, u1) → u0
                I::LoadLocal {
                    name: "__lset_xs_root".to_string()
                },
                I::I64Const(encode_int(1).unwrap()),
                I::LoadLocal {
                    name: "__lset_xs_u1".to_string()
                },
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::ListSet
                },
                I::StoreLocal {
                    name: "__lset_xs_u0".to_string()
                },
                // Write back through cell
                I::StoreCellValue {
                    cell_ptr_instrs: vec![I::LoadLocal {
                        name: "__cell_xs".to_string()
                    }],
                    value_instrs: vec![I::LoadLocal {
                        name: "__lset_xs_u0".to_string()
                    }],
                },
                // Unit
                I::I64Const(encode_unit()),
            ]
        );
    }

    #[test]
    fn lower_assign_index_single_level_emits_list_set_and_store() {
        use crate::gen_lower::value::{encode_int, encode_unit};
        // xs[0] := 99  (single-depth path)
        let comp = CompExpr::AssignIndex {
            root: "xs".to_string(),
            path: vec![ValueExpr::IntLit(0)],
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(99))),
        };
        let instrs = lower_comp(&comp).expect("AssignIndex single-level should lower");
        assert_eq!(
            instrs,
            vec![
                // Declare ascent tmp
                I::DeclareLocal {
                    name: "__lset_xs_u0".to_string()
                },
                // Ascent: list_set(xs, 0, 99), store to ascent_tmp(0)
                I::LoadLocal {
                    name: "xs".to_string()
                },
                I::I64Const(encode_int(0).unwrap()),
                I::I64Const(encode_int(99).unwrap()),
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::ListSet
                },
                I::StoreLocal {
                    name: "__lset_xs_u0".to_string()
                },
                // Write back ascent_tmp(0) to root
                I::LoadLocal {
                    name: "__lset_xs_u0".to_string()
                },
                I::StoreLocal {
                    name: "xs".to_string()
                },
                // Unit
                I::I64Const(encode_unit()),
            ]
        );
    }

    #[test]
    fn lower_assign_index_two_levels_emits_get_and_set_chain() {
        use crate::gen_lower::value::{encode_int, encode_unit};
        // xs[1][0] := 42  (two-depth path)
        let comp = CompExpr::AssignIndex {
            root: "xs".to_string(),
            path: vec![ValueExpr::IntLit(1), ValueExpr::IntLit(0)],
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(42))),
        };
        let instrs = lower_comp(&comp).expect("AssignIndex two-level should lower");
        assert_eq!(
            instrs,
            vec![
                // descent tmp + two ascent tmps
                I::DeclareLocal {
                    name: "__lset_xs_d0".to_string()
                },
                I::DeclareLocal {
                    name: "__lset_xs_u0".to_string()
                },
                I::DeclareLocal {
                    name: "__lset_xs_u1".to_string()
                },
                // Descent: xs[1] → d0
                I::LoadLocal {
                    name: "xs".to_string()
                },
                I::I64Const(encode_int(1).unwrap()),
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::ListGet
                },
                I::StoreLocal {
                    name: "__lset_xs_d0".to_string()
                },
                // Ascent innermost: list_set(d0, 0, 42) → u1
                I::LoadLocal {
                    name: "__lset_xs_d0".to_string()
                },
                I::I64Const(encode_int(0).unwrap()),
                I::I64Const(encode_int(42).unwrap()),
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::ListSet
                },
                I::StoreLocal {
                    name: "__lset_xs_u1".to_string()
                },
                // Ascent outer: list_set(xs, 1, u1) → u0
                I::LoadLocal {
                    name: "xs".to_string()
                },
                I::I64Const(encode_int(1).unwrap()),
                I::LoadLocal {
                    name: "__lset_xs_u1".to_string()
                },
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::ListSet
                },
                I::StoreLocal {
                    name: "__lset_xs_u0".to_string()
                },
                // Write back
                I::LoadLocal {
                    name: "__lset_xs_u0".to_string()
                },
                I::StoreLocal {
                    name: "xs".to_string()
                },
                // Unit
                I::I64Const(encode_unit()),
            ]
        );
    }
}
