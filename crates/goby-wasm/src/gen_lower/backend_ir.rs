//! Backend IR for the general Wasm lowering path.
//!
//! `WasmBackendInstr` is a flat instruction set that sits between Goby IR and
//! `wasm_encoder`. Its purpose is to be independently testable: backend IR can
//! be constructed in unit tests without Wasm emission.
//!
//! See `doc/wasm_runtime_architecture.md §3` for the design rationale and the
//! mapping from Goby IR nodes to these instructions.

/// A backend instruction in the general Wasm lowering pipeline.
///
/// # Goby IR → Backend IR mapping
///
/// | Goby IR node | Backend IR instruction(s) |
/// |---|---|
/// | `CompExpr::Let { name, value, body }` | lower `value` → `StoreLocal { name }`, lower `body` |
/// | `CompExpr::Value(ValueExpr::Var(name))` | `LoadLocal { name }` |
/// | `CompExpr::Value(ValueExpr::IntLit(n))` | `I64Const(encode_int(n))` |
/// | `CompExpr::Value(ValueExpr::Unit)` | `I64Const(encode_unit())` |
/// | `CompExpr::PerformEffect { effect, op, .. }` | `EffectOp { effect, op }` |
/// | `CompExpr::Call { callee: GlobalRef { name }, .. }` | `CallHelper { name, arg_count }` |
/// | discarded expression (stmt before tail) | lower expr + `Drop` |
///
/// Unsupported IR nodes (`WithHandler`, `Handle`, `Resume`, `Lambda`) must produce
/// a `LowerError::UnsupportedForm` — never a panic.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum WasmBackendInstr {
    /// Allocate a named Wasm local slot for the given name.
    DeclareLocal { name: String },
    /// Push the value of a named local (tagged i64) onto the implicit stack.
    LoadLocal { name: String },
    /// Pop the top-of-stack value and store it in the named local.
    StoreLocal { name: String },
    /// Push a compile-time-known tagged i64 literal (already encoded via `value.rs`).
    I64Const(i64),
    /// Perform a WASI-backed effect operation.
    ///
    /// Arguments are expected to have been pushed onto the stack before this instruction.
    EffectOp { effect: String, op: String },
    /// Call a Wasm-internal helper function by name.
    ///
    /// `arg_count` arguments must be on the stack before this instruction.
    /// The callee is a Goby-internal helper (e.g. `goby_string_split`, `goby_list_get`),
    /// not a WASI import.
    CallHelper { name: String, arg_count: usize },
    /// Discard the top-of-stack value.
    Drop,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backend_instr_variants_are_debug_cloneable() {
        let instrs = vec![
            WasmBackendInstr::DeclareLocal { name: "x".to_string() },
            WasmBackendInstr::LoadLocal { name: "x".to_string() },
            WasmBackendInstr::StoreLocal { name: "x".to_string() },
            WasmBackendInstr::I64Const(42),
            WasmBackendInstr::EffectOp {
                effect: "Print".to_string(),
                op: "print".to_string(),
            },
            WasmBackendInstr::CallHelper {
                name: "goby_string_split".to_string(),
                arg_count: 2,
            },
            WasmBackendInstr::Drop,
        ];
        // Verify all variants round-trip through Clone and PartialEq.
        let cloned = instrs.clone();
        assert_eq!(instrs, cloned);
    }
}
