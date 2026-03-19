// Suppress dead_code warnings for Track F stubs — these will be used in F3+.
#![allow(dead_code)]

//! General Wasm lowering — Track F.
//!
//! This module implements the Track F general lowering pipeline:
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
//! - `backend_ir`: `WasmBackendInstr` flat instruction set (variants locked in F2).
//! - `lower`: Goby IR → backend IR lowering, including Track F fused split patterns.
//! - `emit`: backend IR → Wasm emission, including WASI-backed `Read`/`Print`.
//!
//! # Import rules
//! This module and its submodules must NOT import from `runtime_io_plan.rs`.
//! They may import from `backend.rs`, `layout.rs`, `planning.rs`, and `goby-core/ir.rs`.

pub(crate) mod backend_ir;
pub(crate) mod emit;
pub(crate) mod lower;
pub(crate) mod value;

use goby_core::Module;
use goby_core::ir::CompExpr;

use crate::CodegenError;
use crate::layout::MemoryLayout;
use crate::wasm_exec_plan::main_exec_plan;

/// Returns `true` if the IR body contains at least one `PerformEffect` node for `Read`.
/// Track F general lowering is intentionally scoped to runtime-stdin programs.
fn has_runtime_read_effect(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::PerformEffect { effect, .. } => effect == "Read",
        CompExpr::Let { value, body, .. } => {
            has_runtime_read_effect(value) || has_runtime_read_effect(body)
        }
        CompExpr::Seq { stmts, tail } => {
            stmts.iter().any(has_runtime_read_effect) || has_runtime_read_effect(tail)
        }
        CompExpr::If { then_, else_, .. } => {
            has_runtime_read_effect(then_) || has_runtime_read_effect(else_)
        }
        CompExpr::Call { .. } | CompExpr::Value(_) => false,
        CompExpr::Handle { .. } | CompExpr::WithHandler { .. } | CompExpr::Resume { .. } => false,
    }
}

fn read_line_instrs_are_supported(instrs: &[backend_ir::WasmBackendInstr]) -> bool {
    let read_line_count = instrs
        .iter()
        .filter(|instr| {
            matches!(
                instr,
                backend_ir::WasmBackendInstr::EffectOp { effect, op }
                    if effect == "Read" && op == "read_line"
            )
        })
        .count();
    if read_line_count == 0 {
        return true;
    }
    if read_line_count != 1 {
        return false;
    }

    !instrs.iter().any(|instr| {
        matches!(
            instr,
            backend_ir::WasmBackendInstr::EffectOp { effect, op }
                if effect == "Read" && op == "read"
        ) || matches!(
            instr,
            backend_ir::WasmBackendInstr::CallHelper { .. }
                | backend_ir::WasmBackendInstr::SplitEachPrint { .. }
                | backend_ir::WasmBackendInstr::SplitGetPrint { .. }
        )
    })
}

/// Attempt to lower a module's `main` body through the general lowering path.
///
/// Returns `Ok(Some(wasm))` when the general path succeeds,
/// `Ok(None)` when the IR contains unsupported forms (fall through to next path),
/// or `Err` on hard codegen failures.
fn lower_module_to_instrs(
    module: &Module,
) -> Result<Option<Vec<backend_ir::WasmBackendInstr>>, CodegenError> {
    let plan = match main_exec_plan(module) {
        Some(p) => p,
        None => return Ok(None),
    };
    let ir_decl = match plan.ir_decl {
        Some(d) => d,
        None => return Ok(None),
    };

    // Only handle programs with effect operations.
    if !has_runtime_read_effect(&ir_decl.body) {
        return Ok(None);
    }

    let instrs = match lower::lower_comp(&ir_decl.body) {
        Ok(i) => i,
        Err(lower::LowerError::UnsupportedForm { .. }) => return Ok(None),
        Err(lower::LowerError::IntOutOfRange(n)) => {
            return Err(CodegenError {
                message: format!("integer {n} is outside the 60-bit representable range"),
            });
        }
    };
    if !read_line_instrs_are_supported(&instrs) {
        return Ok(None);
    }
    Ok(Some(instrs))
}

pub(crate) fn supports_general_lower_module(module: &Module) -> Result<bool, CodegenError> {
    Ok(lower_module_to_instrs(module)?.is_some())
}

pub(crate) fn try_general_lower_module(module: &Module) -> Result<Option<Vec<u8>>, CodegenError> {
    let Some(instrs) = lower_module_to_instrs(module)? else {
        return Ok(None);
    };
    let layout = MemoryLayout::default();
    let wasm = emit::emit_general_module(&instrs, &layout)?;
    Ok(Some(wasm))
}
