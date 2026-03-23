// Suppress dead_code warnings while the general lowering surface continues to expand.
#![allow(dead_code)]

//! General Wasm lowering for runtime-I/O programs.
//!
//! This module implements the shared general lowering pipeline:
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

use std::collections::HashSet;

use goby_core::Module;
use goby_core::ir::CompExpr;

use crate::CodegenError;
use crate::gen_lower::emit::AuxDecl;
use crate::layout::MemoryLayout;
use crate::wasm_exec_plan::main_exec_plan;

/// Returns `true` if the IR body contains at least one `PerformEffect` node for `Read`.
/// General lowering is intentionally scoped to runtime-stdin programs.
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

    !all.iter().any(|instr| {
        matches!(
            instr,
            backend_ir::WasmBackendInstr::EffectOp { effect, op }
                if effect == "Read" && op == "read"
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
fn lower_aux_decl(
    name: &str,
    param_names: Vec<String>,
    body: &CompExpr,
    known_decls: &HashSet<String>,
) -> Result<Option<AuxDecl>, CodegenError> {
    let instrs = match lower::lower_comp_with_decls(body, known_decls) {
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

    // Only handle programs where main has a Read effect.
    if !has_runtime_read_effect(&ir_decl.body) {
        return Ok(None);
    }

    // Collect names of all non-main top-level declarations so callee `Var(name)` can be
    // recognised as a DeclCall during lowering.
    let known_decls: std::collections::HashSet<String> = module
        .declarations
        .iter()
        .filter(|d| d.name != "main")
        .map(|d| d.name.clone())
        .collect();

    let mut main_instrs = match lower::lower_comp_with_decls(&ir_decl.body, &known_decls) {
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
        )? {
            Some(aux) => aux_decls.push(aux),
            None => {
                // If any aux decl cannot be lowered, fall back to the interpreter path.
                return Ok(None);
            }
        }
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
    let Some((instrs, aux_decls)) = lower_module_to_instrs(module)? else {
        return Ok(None);
    };
    let aux_supported = aux_decls.iter().all(|d| emit::supports_instrs(&d.instrs));
    if !emit::supports_instrs(&instrs) || !aux_supported {
        return Ok(None);
    }
    let layout = MemoryLayout::default();
    let wasm = emit::emit_general_module_with_aux(&instrs, &aux_decls, &layout)?;
    Ok(Some(wasm))
}

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::*;
    use crate::gen_lower::backend_ir::WasmBackendInstr;

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
}
