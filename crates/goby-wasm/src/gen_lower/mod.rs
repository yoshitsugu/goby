// Suppress dead_code warnings while the general lowering surface continues to expand.
#![allow(dead_code)]

//! General Wasm lowering for Wasm-owned runtime programs.
//!
//! This module implements the shared lowering pipeline for the Wasm-owned execution
//! subset, including runtime-`Read` programs and the WB-3 safe handler subset:
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
pub(crate) mod emit;
pub(crate) mod lower;
pub(crate) mod value;

use std::collections::{HashMap, HashSet};

use goby_core::Module;
use goby_core::ir::CompExpr;
use goby_core::stdlib::StdlibResolver;

use crate::CodegenError;
use crate::effect_handler_legality::analyze_module_handler_legality;
use crate::effect_handler_lowering::lower_safe_handlers_in_comp;
use crate::gen_lower::backend_ir::{BackendEffectOp, BackendReadOp};
use crate::gen_lower::emit::AuxDecl;
use crate::gen_lower::lower::LambdaAuxDecl;
use crate::layout::MemoryLayout;
use crate::wasm_exec_plan::decl_exec_plan;
use crate::wasm_exec_plan::main_exec_plan;

/// Names from stdlib that have dedicated special lowering in `lower_comp_inner` or
/// `backend_intrinsic_for` and must NOT be routed as generic `DeclCall` targets.
/// - `each`, `map`, `graphemes`: handled by special `lower_comp_inner` branches.
/// - `split`: handled by `StringSplit` intrinsic (non-empty sep) or redirected to
///   `StringGraphemesList` (empty string literal sep) before reaching the intrinsic path.
const SPECIALLY_LOWERED_STDLIB_NAMES: &[&str] = &["each", "map", "graphemes", "split"];

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
    }
}

fn has_handler_constructs(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::Handle { .. } | CompExpr::WithHandler { .. } | CompExpr::Resume { .. } => true,
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
/// once WB-3-M3 Lambda lowering is in place.
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
    // Collect all instructions recursively (WB-1: If branches may contain EffectOp).
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
/// called by `main`. Returns `None` if the body contains unsupported forms.
/// Lambda expressions encountered during lowering are appended to `lambda_decls`.
fn lower_aux_decl(
    name: &str,
    param_names: Vec<String>,
    body: &CompExpr,
    known_decls: &HashSet<String>,
    allow_safe_handler_lowering: bool,
    lambda_decls: &mut Vec<LambdaAuxDecl>,
) -> Result<Option<AuxDecl>, CodegenError> {
    let Some(body) = rewrite_safe_handlers_if_present(body, allow_safe_handler_lowering)? else {
        return Ok(None);
    };
    let instrs = match lower::lower_comp_collecting_lambdas(&body, known_decls, lambda_decls) {
        Ok(i) => i,
        Err(lower::LowerError::UnsupportedForm { .. }) => return Ok(None),
        Err(lower::LowerError::IntOutOfRange(n)) => {
            return Err(CodegenError {
                message: format!("integer {n} is outside the 60-bit representable range"),
            });
        }
    };
    Ok(Some(AuxDecl {
        decl_name: name.to_string(),
        param_names,
        instrs,
    }))
}

/// Recursively collect all `DeclCall` and `PushFuncHandle` target names from a flat
/// backend-IR instruction list, traversing nested instruction vecs.
fn collect_decl_call_names(instrs: &[backend_ir::WasmBackendInstr], out: &mut HashSet<String>) {
    for instr in instrs {
        match instr {
            backend_ir::WasmBackendInstr::DeclCall { decl_name }
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
            backend_ir::WasmBackendInstr::ListEach {
                list_instrs,
                func_instrs,
            }
            | backend_ir::WasmBackendInstr::ListMap {
                list_instrs,
                func_instrs,
            } => {
                collect_decl_call_names(list_instrs, out);
                collect_decl_call_names(func_instrs, out);
            }
            backend_ir::WasmBackendInstr::ListEachEffect { list_instrs, .. } => {
                collect_decl_call_names(list_instrs, out);
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
    let mut pending: Vec<String> = module
        .imports
        .iter()
        .map(|imp| imp.module_path.clone())
        .collect();
    while let Some(path) = pending.pop() {
        if visited.contains(&path) {
            continue;
        }
        visited.insert(path.clone());
        let Ok(resolved) = resolver.resolve_module(&path) else {
            continue;
        };
        // Enqueue transitive imports from this stdlib module.
        for imp in &resolved.module.imports {
            if !visited.contains(&imp.module_path) {
                pending.push(imp.module_path.clone());
            }
        }
        // Register all declarations as potential exports.
        for decl in resolved.module.declarations {
            export_map
                .entry(decl.name.clone())
                .or_insert_with(|| (path.clone(), decl));
        }
    }
    export_map
}

/// Attempt to lower a module's `main` body through the general lowering path.
///
/// Returns `Ok(Some((main_instrs, aux_decls)))` when the general path succeeds,
/// `Ok(None)` when the IR contains unsupported forms (fall through to next path),
/// or `Err` on hard codegen failures.
#[allow(clippy::type_complexity)]
fn lower_module_to_instrs(
    module: &Module,
) -> Result<Option<(Vec<backend_ir::WasmBackendInstr>, Vec<AuxDecl>)>, CodegenError> {
    let plan = match main_exec_plan(module) {
        Some(p) => p,
        None => return Ok(None),
    };
    let ir_decl = match plan.ir_decl {
        Some(d) => d,
        None => return Ok(None),
    };
    let handler_legality = analyze_module_handler_legality(module)?;
    let allow_safe_handler_lowering = handler_legality.all_one_shot_tail_resumptive();
    let Some(main_body) =
        rewrite_safe_handlers_if_present(&ir_decl.body, allow_safe_handler_lowering)?
    else {
        return Ok(None);
    };

    let main_is_handler_only_candidate =
        !has_runtime_read_effect(&ir_decl.body) && has_handler_rewrite_entrypoints(&ir_decl.body);
    if main_is_handler_only_candidate
        && (contains_future_handler_intrinsics(&ir_decl.body)
            || has_effectful_non_main_decl(module)
            || module.declarations.iter().any(|decl| decl.name != "main"))
    {
        return Ok(None);
    }

    // Gate: only enter GeneralLowered for programs that require runtime capabilities.
    // A program qualifies when it has:
    //   - a runtime Read effect, OR
    //   - safe handler constructs (WB-3 handler lowering), OR
    //   - a Lambda expression (WB-3-M3 Lambda lowering).
    // Pure-Print programs without any of the above can stay on the simpler paths.
    if !has_runtime_read_effect(&ir_decl.body)
        && !has_handler_rewrite_entrypoints(&ir_decl.body)
        && !has_lambda_in_comp(&ir_decl.body)
    {
        return Ok(None);
    }

    // Resolve stdlib exports once; reused for both known_decls population and the
    // transitive-closure loop below.
    let stdlib_export_map = if !module.imports.is_empty() {
        let stdlib_resolver = StdlibResolver::new(resolve_stdlib_root());
        build_stdlib_export_map(module, &stdlib_resolver)
    } else {
        HashMap::new()
    };

    // Collect names of all non-main top-level declarations so callee `Var(name)` can be
    // recognised as a DeclCall during lowering.
    let mut known_decls: std::collections::HashSet<String> = module
        .declarations
        .iter()
        .filter(|d| d.name != "main")
        .map(|d| d.name.clone())
        .collect();
    // Add stdlib-exported names to known_decls so user-written helpers that call stdlib
    // functions via bare names are lowered correctly.
    // Exclude names that have dedicated special lowering in lower_comp_inner or backend_intrinsic_for
    // (each, map, graphemes, split) — those must NOT be routed through the generic DeclCall path.
    for name in stdlib_export_map.keys() {
        if !SPECIALLY_LOWERED_STDLIB_NAMES.contains(&name.as_str()) {
            known_decls.insert(name.clone());
        }
    }
    // Lambda auxiliary declarations collected during main and aux-decl lowering.
    // They are appended AFTER user aux_decls so existing table slot indices remain stable.
    let mut lambda_decls: Vec<LambdaAuxDecl> = Vec::new();

    let mut main_instrs =
        match lower::lower_comp_collecting_lambdas(&main_body, &known_decls, &mut lambda_decls) {
            Ok(i) => i,
            Err(lower::LowerError::UnsupportedForm { .. }) => return Ok(None),
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
        return Ok(None);
    }

    // Lower auxiliary declarations (non-main top-level functions).
    // These do not need a Read effect; they are helpers called by main.
    let mut aux_decls = Vec::new();
    for goby_decl in module.declarations.iter().filter(|d| d.name != "main") {
        let Ok(aux_ir_decl) = goby_core::ir_lower::lower_declaration(goby_decl) else {
            // IR lowering failed for this decl — fall back to interpreter path.
            return Ok(None);
        };
        let param_names: Vec<String> = aux_ir_decl.params.iter().map(|(n, _)| n.clone()).collect();
        match lower_aux_decl(
            &aux_ir_decl.name,
            param_names,
            &aux_ir_decl.body,
            &known_decls,
            allow_safe_handler_lowering,
            &mut lambda_decls,
        )? {
            Some(aux) => aux_decls.push(aux),
            None => {
                // If any aux decl cannot be lowered, fall back to the interpreter path.
                return Ok(None);
            }
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
            // Collect all DeclCall/PushFuncHandle names across main + current aux_decls.
            let mut referenced: HashSet<String> = HashSet::new();
            collect_decl_call_names(&main_instrs, &mut referenced);
            for aux in &aux_decls {
                collect_decl_call_names(&aux.instrs, &mut referenced);
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
                    Err(_) => return Ok(None),
                };
                let param_names: Vec<String> =
                    aux_ir_decl.params.iter().map(|(n, _)| n.clone()).collect();
                // Stdlib functions use safe (one-shot tail-resumptive) handlers; always
                // allow handler lowering for stdlib aux decls regardless of whether the
                // user's main body has handlers.
                match lower_aux_decl(
                    &aux_ir_decl.name,
                    param_names,
                    &aux_ir_decl.body,
                    &known_decls,
                    true, // stdlib handlers are one-shot tail-resumptive by construction
                    &mut lambda_decls,
                )? {
                    Some(aux) => {
                        aux_decls.push(aux);
                        aux_added.insert(name.clone());
                        added_any = true;
                    }
                    None => {
                        // This stdlib decl uses unsupported IR forms; fall back.
                        return Ok(None);
                    }
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
            param_names: vec![lam.param_name],
            instrs: lam.instrs,
        });
    }

    Ok(Some((main_instrs, aux_decls)))
}

pub(crate) fn supports_general_lower_module(module: &Module) -> Result<bool, CodegenError> {
    let Some((instrs, aux_decls)) = lower_module_to_instrs(module)? else {
        return Ok(false);
    };
    let aux_supported = aux_decls.iter().all(|d| emit::supports_instrs(&d.instrs));
    Ok(emit::supports_instrs(&instrs) && aux_supported)
}

pub(crate) fn try_general_lower_module(module: &Module) -> Result<Option<Vec<u8>>, CodegenError> {
    try_general_lower_module_with_options(module, emit::EmitOptions::default())
}

fn try_general_lower_module_with_options(
    module: &Module,
    options: emit::EmitOptions,
) -> Result<Option<Vec<u8>>, CodegenError> {
    let Some((instrs, aux_decls)) = lower_module_to_instrs(module)? else {
        return Ok(None);
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

    fn assert_strategy_parity(module: &goby_core::Module) {
        let direct = try_general_lower_module_with_options(
            module,
            EmitOptions {
                effect_emit_strategy: EffectEmitStrategy::Wb3DirectCall,
            },
        )
        .expect("WB-3A lowering should not error")
        .expect("WB-3A general lowering should apply");
        let wasmfx = try_general_lower_module_with_options(
            module,
            EmitOptions {
                effect_emit_strategy: EffectEmitStrategy::Wb3BWasmFxExperimental,
            },
        )
        .expect("WB-3B lowering should not error")
        .expect("WB-3B general lowering should apply");
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
            .expect("general lowering should apply");
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
                .is_some(),
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
}
