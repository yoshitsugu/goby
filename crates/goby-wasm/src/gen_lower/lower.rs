//! Goby IR → `WasmBackendInstr` lowering for the general Wasm path.
//!
//! The IR → backend-IR mapping is documented in `backend_ir.rs`.

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU32, Ordering as AtomicOrdering};

use goby_core::closure_capture::{
    CallableEnv, CallableEnvSlotKind, ClosureBindingEnv, analyze_lambda_callable_env_for_params,
    has_mutable_write_capture_of,
};
use goby_core::ir::{CompExpr, IrInterpPart, ValueExpr};

use crate::gen_lower::backend_ir::{
    BackendEffectOp, BackendIntrinsic, BackendPrintOp, BackendReadOp, WasmBackendInstr,
};
use crate::gen_lower::value::{ValueError, encode_bool, encode_int, encode_unit};

/// Counter for generating unique lambda auxiliary declaration names (`__lambda_N`).
static LAMBDA_COUNTER: AtomicU32 = AtomicU32::new(0);

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
    lower_comp_inner(comp, &HashMap::new(), &bindings, known_decls, lambda_decls)
}

pub(crate) fn lower_comp_collecting_lambdas_with_params(
    comp: &CompExpr,
    decl_params: &[String],
    known_decls: &HashSet<String>,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    let bindings = ClosureBindingEnv::with_decl_params(decl_params.iter().map(String::as_str));
    lower_comp_inner(comp, &HashMap::new(), &bindings, known_decls, lambda_decls)
}

fn lower_comp_inner(
    comp: &CompExpr,
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
        CompExpr::Value(v) => lower_value_ctx(v, aliases),

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
                lower_comp_inner(value, aliases, bindings, known_decls, lambda_decls)?;
            // Track whether the local holds a capturing closure so that call sites can
            // emit IndirectCallClosure instead of the generic IndirectCall.
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
                    aliases,
                    bindings,
                    known_decls,
                    lambda_decls,
                )?);
                instrs.push(WasmBackendInstr::Drop);
            }
            instrs.extend(lower_comp_inner(
                tail,
                aliases,
                bindings,
                known_decls,
                lambda_decls,
            )?);
            Ok(instrs)
        }

        CompExpr::PerformEffect { effect, op, args } => {
            let op = backend_effect_op(effect, op).ok_or_else(|| LowerError::UnsupportedForm {
                node: format!("unsupported effect op '{effect}.{op}'"),
            })?;
            let mut instrs = Vec::new();
            for arg in args {
                instrs.extend(lower_value(arg)?);
            }
            instrs.push(WasmBackendInstr::EffectOp { op });
            Ok(instrs)
        }

        CompExpr::Call { callee, args } => {
            if let Some(op) = resolve_effect_call_target(callee, aliases) {
                let mut instrs = Vec::new();
                for arg in args {
                    instrs.extend(lower_value(arg)?);
                }
                instrs.push(WasmBackendInstr::EffectOp { op });
                Ok(instrs)
            } else if let goby_core::ir::ValueExpr::GlobalRef { module, name } = callee.as_ref()
                && module == "string"
                && name == "split"
                && args.len() == 2
                && matches!(&args[1], goby_core::ir::ValueExpr::StrLit(s) if s.is_empty())
            {
                // split text "" — empty delimiter means split by grapheme clusters.
                // Redirect to StringGraphemesList host intrinsic (same as `graphemes text`).
                let mut instrs = lower_comp_inner(
                    &CompExpr::Value(args[0].clone()),
                    aliases,
                    bindings,
                    known_decls,
                    lambda_decls,
                )?;
                instrs.push(WasmBackendInstr::Intrinsic {
                    intrinsic: BackendIntrinsic::StringGraphemesList,
                });
                Ok(instrs)
            } else if let Some(intrinsic) =
                resolve_intrinsic_call_target(callee, aliases, args.len())
            {
                let mut instrs = Vec::new();
                for arg in args {
                    instrs.extend(lower_value(arg)?);
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
                && name == "each"
                && args.len() == 2
            {
                // stdlib list.each: check if callback is a Print effect op (fused path)
                // or a user funcref (indirect call path).
                let list_instrs = lower_comp_inner(
                    &CompExpr::Value(args[0].clone()),
                    aliases,
                    bindings,
                    known_decls,
                    lambda_decls,
                )?;
                if let Some(op) = resolve_print_callback(&args[1], aliases) {
                    Ok(vec![WasmBackendInstr::ListEachEffect { list_instrs, op }])
                } else {
                    let func_instrs =
                        lower_value_as_arg(&args[1], aliases, bindings, known_decls, lambda_decls)?;
                    Ok(vec![WasmBackendInstr::ListEach {
                        list_instrs,
                        func_instrs,
                    }])
                }
            } else if let goby_core::ir::ValueExpr::GlobalRef { module, name } = callee.as_ref()
                && module == "list"
                && name == "map"
                && args.len() == 2
            {
                // stdlib list.map: iterate list with funcref callback, return new list.
                let list_instrs =
                    lower_value_as_arg(&args[0], aliases, bindings, known_decls, lambda_decls)?;
                let func_instrs =
                    lower_value_as_arg(&args[1], aliases, bindings, known_decls, lambda_decls)?;
                Ok(vec![WasmBackendInstr::ListMap {
                    list_instrs,
                    func_instrs,
                }])
            } else if let goby_core::ir::ValueExpr::GlobalRef { module, name } = callee.as_ref()
                && module == "string"
                && name == "graphemes"
                && args.len() == 1
            {
                // stdlib string.graphemes: lower as StringGraphemesList host intrinsic.
                // Returns a tagged List String containing all Unicode Extended Grapheme Clusters.
                let mut instrs = lower_comp_inner(
                    &CompExpr::Value(args[0].clone()),
                    aliases,
                    bindings,
                    known_decls,
                    lambda_decls,
                )?;
                instrs.push(WasmBackendInstr::Intrinsic {
                    intrinsic: BackendIntrinsic::StringGraphemesList,
                });
                Ok(instrs)
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
                instrs.push(WasmBackendInstr::DeclCall {
                    decl_name: name.clone(),
                });
                Ok(instrs)
            } else if let goby_core::ir::ValueExpr::Var(name) = callee.as_ref() {
                if known_decls.contains(name.as_str()) {
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
                    instrs.push(WasmBackendInstr::DeclCall {
                        decl_name: name.clone(),
                    });
                    Ok(instrs)
                } else if (name == "each"
                    || resolve_global_ref(name, aliases) == Some(("list", "each")))
                    && args.len() == 2
                {
                    // Var resolves to list.each (bare name or alias via `import goby/list (each)`).
                    let list_instrs = lower_comp_inner(
                        &CompExpr::Value(args[0].clone()),
                        aliases,
                        bindings,
                        known_decls,
                        lambda_decls,
                    )?;
                    if let Some(op) = resolve_print_callback(&args[1], aliases) {
                        Ok(vec![WasmBackendInstr::ListEachEffect { list_instrs, op }])
                    } else {
                        let func_instrs = lower_value_as_arg(
                            &args[1],
                            aliases,
                            bindings,
                            known_decls,
                            lambda_decls,
                        )?;
                        Ok(vec![WasmBackendInstr::ListEach {
                            list_instrs,
                            func_instrs,
                        }])
                    }
                } else if (name == "map"
                    || resolve_global_ref(name, aliases) == Some(("list", "map")))
                    && args.len() == 2
                {
                    // Var resolves to list.map (bare name or alias via `import goby/list (map)`).
                    let list_instrs =
                        lower_value_as_arg(&args[0], aliases, bindings, known_decls, lambda_decls)?;
                    let func_instrs =
                        lower_value_as_arg(&args[1], aliases, bindings, known_decls, lambda_decls)?;
                    Ok(vec![WasmBackendInstr::ListMap {
                        list_instrs,
                        func_instrs,
                    }])
                } else if (name == "graphemes"
                    || resolve_global_ref(name, aliases) == Some(("string", "graphemes")))
                    && args.len() == 1
                {
                    // Var resolves to string.graphemes (bare name or alias via `import goby/string (graphemes)`).
                    // Lower as StringGraphemesList host intrinsic.
                    let mut instrs = lower_comp_inner(
                        &CompExpr::Value(args[0].clone()),
                        aliases,
                        bindings,
                        known_decls,
                        lambda_decls,
                    )?;
                    instrs.push(WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::StringGraphemesList,
                    });
                    Ok(instrs)
                } else if (name == "split"
                    || resolve_global_ref(name, aliases) == Some(("string", "split")))
                    && args.len() == 2
                    && matches!(&args[1], goby_core::ir::ValueExpr::StrLit(s) if s.is_empty())
                {
                    // Var resolves to string.split with empty string literal delimiter:
                    // redirect to StringGraphemesList (same as `graphemes text`).
                    let mut instrs = lower_comp_inner(
                        &CompExpr::Value(args[0].clone()),
                        aliases,
                        bindings,
                        known_decls,
                        lambda_decls,
                    )?;
                    instrs.push(WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::StringGraphemesList,
                    });
                    Ok(instrs)
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
                    // Check if this local is known to hold a capturing closure.
                    let is_capturing_closure =
                        aliases.get(name.as_str()) == Some(&AliasValue::CapturingClosure);
                    if is_capturing_closure {
                        // IndirectCallClosure: pass closure ptr as __clo + call arity-2 Wasm fn.
                        if args.len() == 1 {
                            let mut arg_instrs = Vec::new();
                            for arg in args {
                                arg_instrs.extend(lower_value_as_arg(
                                    arg,
                                    aliases,
                                    bindings,
                                    known_decls,
                                    lambda_decls,
                                )?);
                            }
                            Ok(vec![WasmBackendInstr::IndirectCallClosure {
                                closure_local: name.clone(),
                                arg_instrs,
                            }])
                        } else {
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
                }
            } else {
                Err(LowerError::UnsupportedForm {
                    node: format!("Call with unsupported callee: {:?}", callee),
                })
            }
        }

        CompExpr::If { cond, then_, else_ } => {
            let mut instrs = lower_value(cond)?;
            let then_instrs =
                lower_comp_inner(then_, aliases, bindings, known_decls, lambda_decls)?;
            let else_instrs =
                lower_comp_inner(else_, aliases, bindings, known_decls, lambda_decls)?;
            instrs.push(WasmBackendInstr::If {
                then_instrs,
                else_instrs,
            });
            Ok(instrs)
        }

        CompExpr::LetMut {
            name, value, body, ..
        } => {
            // Check whether any nested lambda captures `name` as a mutable write.
            // If so, the binding is promoted to a heap cell so writes inside the lambda
            // update the same cell visible outside.
            if has_mutable_write_capture_of(name, body, bindings, known_decls) {
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
                    lower_comp_inner(value, aliases, bindings, known_decls, lambda_decls)?;
                let mut instrs = vec![WasmBackendInstr::StoreCellValue {
                    cell_ptr_instrs: vec![WasmBackendInstr::LoadLocal {
                        name: cell_local,
                    }],
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
                    lower_comp_inner(value, aliases, bindings, known_decls, lambda_decls)?;
                instrs.push(WasmBackendInstr::StoreLocal { name: name.clone() });
                // Assign produces Unit.
                instrs.push(WasmBackendInstr::I64Const(
                    crate::gen_lower::value::encode_unit(),
                ));
                Ok(instrs)
            }
        }

        CompExpr::Case { scrutinee, arms } => lower_case(
            scrutinee,
            arms,
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
        let body_instrs =
            lower_comp_inner(&arm.body, aliases, &arm_bindings, known_decls, lambda_decls)?;
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
    lower_value_ctx(v, &HashMap::new())
}

/// Like `lower_value` but aware of cell-promoted bindings in `aliases`.
///
/// If `aliases` contains `AliasValue::CellPromoted` for a name, reading that name emits
/// `[LoadLocal { __cell_<name> }, LoadCellValue]` instead of `[LoadLocal { name }]`.
fn lower_value_ctx(
    v: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    match v {
        ValueExpr::Unit => Ok(vec![WasmBackendInstr::I64Const(encode_unit())]),
        ValueExpr::IntLit(n) => Ok(vec![WasmBackendInstr::I64Const(encode_int(*n)?)]),
        ValueExpr::BoolLit(b) => Ok(vec![WasmBackendInstr::I64Const(encode_bool(*b))]),
        ValueExpr::StrLit(text) => Ok(vec![WasmBackendInstr::PushStaticString {
            text: text.clone(),
        }]),
        ValueExpr::ListLit { elements, spread } => {
            if spread.is_some() {
                return Err(LowerError::UnsupportedForm {
                    node: "ListLit with spread".to_string(),
                });
            }
            let mut element_instrs = Vec::with_capacity(elements.len());
            for elem in elements {
                element_instrs.push(lower_value_ctx(elem, aliases)?);
            }
            Ok(vec![WasmBackendInstr::ListLit { element_instrs }])
        }
        ValueExpr::TupleLit(items) => {
            if items.is_empty() {
                return Ok(vec![WasmBackendInstr::I64Const(encode_unit())]);
            }
            let mut element_instrs = Vec::with_capacity(items.len());
            for item in items {
                element_instrs.push(lower_value_ctx(item, aliases)?);
            }
            Ok(vec![WasmBackendInstr::TupleLit { element_instrs }])
        }
        ValueExpr::RecordLit {
            constructor,
            fields,
        } => {
            let mut field_instrs = Vec::with_capacity(fields.len());
            for (_, value) in fields {
                field_instrs.push(lower_value_ctx(value, aliases)?);
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
            let mut instrs = lower_value_ctx(left, aliases)?;
            instrs.extend(lower_value_ctx(right, aliases)?);
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
                        instrs.extend(lower_value_ctx(e, aliases)?);
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
        ValueExpr::Var(name) if known_decls.contains(name.as_str()) => {
            Ok(vec![WasmBackendInstr::PushFuncHandle {
                decl_name: name.clone(),
            }])
        }
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
        ValueExpr::Lambda { param, body } => {
            lower_lambda(param, body, aliases, bindings, known_decls, lambda_decls)
        }
        other => lower_value(other),
    }
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
                preamble.push(WasmBackendInstr::StoreLocal {
                    name: cell_local,
                });
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
    /// Used by the Var-callee branch to emit IndirectCallClosure instead of IndirectCall.
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

fn resolve_print_callback(
    callback: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Option<BackendPrintOp> {
    match callback {
        ValueExpr::GlobalRef { module, name } => backend_print_op(module, name),
        ValueExpr::Var(name) if name == "print" => Some(BackendPrintOp::Print),
        ValueExpr::Var(name) if name == "println" => Some(BackendPrintOp::Println),
        ValueExpr::Var(name) => {
            if let Some(op) = resolve_var_alias(name, aliases)
                && let Some(print_op) = backend_print_op("Print", op)
            {
                return Some(print_op);
            }
            let (module, op) = resolve_global_ref(name, aliases)?;
            backend_print_op(module, op)
        }
        _ => None,
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
        "__goby_string_length" => Some(BackendIntrinsic::StringLength),
        _ => None,
    }
}

fn is_effect_pair(module: &str, op: &str) -> bool {
    backend_effect_op(module, op).is_some()
}

fn backend_print_op(module: &str, op: &str) -> Option<BackendPrintOp> {
    match (module, op) {
        ("Print", "print") => Some(BackendPrintOp::Print),
        ("Print", "println") => Some(BackendPrintOp::Println),
        _ => None,
    }
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
        // Recurse into nested instruction lists so that an alias used only inside
        // a ListEach/ListMap/If/CaseMatch is not pruned by the alias-elision path.
        WasmBackendInstr::ListEach {
            list_instrs,
            func_instrs,
        }
        | WasmBackendInstr::ListMap {
            list_instrs,
            func_instrs,
        } => instrs_load_local(list_instrs, name) || instrs_load_local(func_instrs, name),
        WasmBackendInstr::ListEachEffect { list_instrs, .. } => {
            instrs_load_local(list_instrs, name)
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
        let instrs = lower_comp(&comp).expect("split+each should lower through the general path");
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
            )) || instrs.iter().any(|i| matches!(i, I::ListEachEffect { .. })),
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
    fn lower_globalref_decl_call_emits_decl_call() {
        // Call with a GlobalRef callee that is not an effect or intrinsic
        // must emit DeclCall, not UnsupportedForm.
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::GlobalRef {
                module: "mymod".to_string(),
                name: "add".to_string(),
            }),
            args: vec![ValueExpr::IntLit(2), ValueExpr::IntLit(3)],
        };
        let instrs = lower_comp(&comp).expect("GlobalRef decl call should lower to DeclCall");
        assert!(
            matches!(instrs.last(), Some(I::DeclCall { decl_name }) if decl_name == "add"),
            "last instruction must be DeclCall {{ decl_name: \"add\" }}, got: {:?}",
            instrs
        );
        // Args must be pushed before DeclCall.
        assert_eq!(instrs.len(), 3, "2 arg pushes + 1 DeclCall");
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
    fn lower_var_callee_in_known_decls_emits_decl_call() {
        // Var(name) where name ∈ known_decls → DeclCall (direct call).
        use std::collections::HashSet;
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("helper".to_string())),
            args: vec![ValueExpr::IntLit(2)],
        };
        let known_decls: HashSet<String> = ["helper".to_string()].into();
        let bindings = ClosureBindingEnv::default();
        let instrs = lower_comp_inner(
            &comp,
            &HashMap::new(),
            &bindings,
            &known_decls,
            &mut Vec::new(),
        )
        .expect("known_decl Var callee should lower to DeclCall");
        assert!(
            matches!(instrs.last(), Some(I::DeclCall { decl_name }) if decl_name == "helper"),
            "last instr must be DeclCall(helper), got: {:?}",
            instrs
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
        // The first arm body should contain a DeclCall for "helper"
        assert!(
            arms[0].body_instrs.iter().any(
                |i| matches!(i, WasmBackendInstr::DeclCall { decl_name } if decl_name == "helper")
            ),
            "arm body should contain DeclCall(helper)"
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

    /// Lambda with param-only body passed to list.map should lower to
    /// PushFuncHandle + ListMap (not UnsupportedForm).
    #[test]
    fn lower_lambda_as_map_callback_param_only() {
        // map [1, 2, 3] (fn x -> x + 1)
        // Lowered IR:
        //   let list = [1, 2, 3]
        //   map list (fn x -> x + 1)
        let known_decls: HashSet<String> = HashSet::new();
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::GlobalRef {
                module: "list".to_string(),
                name: "map".to_string(),
            }),
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
            "Lambda as map callback should lower to PushFuncHandle+ListMap, got: {:?}",
            result
        );
        let instrs = result.unwrap();
        assert!(
            matches!(instrs.as_slice(), [WasmBackendInstr::ListMap { .. }]),
            "expected a single ListMap instr, got: {:?}",
            instrs
        );
        // Lambda should have been lifted as a LambdaAuxDecl.
        assert_eq!(lambda_decls.len(), 1, "expected exactly one lambda AuxDecl");
        assert_eq!(lambda_decls[0].param_names, vec!["x"]);
        // The ListMap func_instrs should reference the lifted lambda by PushFuncHandle.
        if let WasmBackendInstr::ListMap { func_instrs, .. } = &instrs[0] {
            assert!(
                matches!(func_instrs.as_slice(), [WasmBackendInstr::PushFuncHandle { decl_name }]
                    if decl_name.starts_with("__lambda_")),
                "func_instrs should be PushFuncHandle(__lambda_N), got: {:?}",
                func_instrs
            );
        }
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
            callee: Box::new(ValueExpr::GlobalRef {
                module: "list".to_string(),
                name: "map".to_string(),
            }),
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

    /// Let-bound capturing closure called directly emits `IndirectCallClosure`.
    /// `base = 10; add10 = (fn x -> base + x); add10 5` — add10 is a closure.
    #[test]
    fn lower_let_bound_capturing_lambda_call_emits_indirect_call_closure() {
        use std::collections::HashSet;
        // Simulate:
        //   base = 10
        //   add10 = fn x -> base + x   <- CreateClosure (capturing 'base')
        //   add10 5                     <- IndirectCallClosure
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
        // The output should contain IndirectCallClosure for add10.
        assert!(
            instrs.iter().any(|i| matches!(
                i,
                WasmBackendInstr::IndirectCallClosure {
                    closure_local,
                    ..
                } if closure_local == "add10"
            )),
            "expected IndirectCallClosure {{ closure_local: 'add10' }} in top-level instrs, got: {:?}",
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
        // Simulates: `import goby/string (graphemes)` → `graphemes` becomes a Var with GlobalRef alias.
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("graphemes".to_string())),
            args: vec![ValueExpr::Var("text".to_string())],
        };
        let instrs =
            lower_comp(&comp).expect("graphemes Var (bare name) should lower to intrinsic");
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
        // DeclCall is emitted as-is; the backend emits a direct Wasm `call` with
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
                I::DeclCall {
                    decl_name: "add".to_string()
                },
            ]
        );
    }
}
