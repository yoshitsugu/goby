//! Backend IR for the general Wasm lowering path.
//!
//! `WasmBackendInstr` is a flat instruction set that sits between Goby IR and
//! `wasm_encoder`. Its purpose is to be independently testable: backend IR can
//! be constructed in unit tests without Wasm emission.
//!
//! The design rationale and the Goby IR mapping are documented in this module
//! and in `gen_lower/mod.rs`.

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SplitIndexOperand {
    Const(i64),
    Local(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BackendIntrinsic {
    StringSplit,
    ListGet,
    StringLength,
    StringEachGraphemeCount,
    StringEachGraphemeState,
    ListPushString,
}

impl BackendIntrinsic {
    pub(crate) fn arity(self) -> usize {
        match self {
            BackendIntrinsic::StringSplit => 2,
            BackendIntrinsic::ListGet => 2,
            BackendIntrinsic::StringLength => 1,
            BackendIntrinsic::StringEachGraphemeCount => 1,
            BackendIntrinsic::StringEachGraphemeState => 2,
            BackendIntrinsic::ListPushString => 2,
        }
    }
}

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
/// | `CompExpr::Value(ValueExpr::StrLit(text))` | `PushStaticString { text }` |
/// | `CompExpr::PerformEffect { effect, op, .. }` | `EffectOp { effect, op }` |
/// | `CompExpr::Call { callee: GlobalRef { name }, .. }` | `Intrinsic { intrinsic }` |
/// | fused `Let lines = split(text, sep); each lines Effect.op` | `SplitEachPrint { text_local, sep_bytes, effect, op }` |
/// | fused `Let lines = split(text, sep); Let line = list.get(lines, idx); Print.op(line)` | `SplitGetPrint { text_local, sep_bytes, index, op }` |
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
    /// Push a tagged string pointer to a compile-time-known static string blob.
    ///
    /// The emitter places the blob in linear memory as `(len: i32, bytes...)`
    /// and pushes an encoded string pointer to that blob.
    PushStaticString { text: String },
    /// Perform a WASI-backed effect operation.
    ///
    /// Arguments are expected to have been pushed onto the stack before this instruction.
    EffectOp { effect: String, op: String },
    /// Call a Wasm-internal backend intrinsic.
    ///
    /// The intrinsic's fixed arity arguments must be on the stack before this instruction.
    /// These intrinsics form the backend primitive substrate beneath stdlib helpers.
    Intrinsic { intrinsic: BackendIntrinsic },
    /// Discard the top-of-stack value.
    Drop,
    /// Fused: split `text_local` (a tagged-i64 string) on `sep_bytes`, then call
    /// `effect.op` on each resulting segment.
    ///
    /// Equivalent to: `for segment in split(text_local, sep_bytes): effect.op(segment)`.
    ///
    /// Lowered from the combined IR pattern:
    /// `Let lines = Call(GlobalRef("string","split"), [Var(text), StrLit(sep)])`
    /// followed by `Call(Var("each"), [Var(lines), GlobalRef(effect, op)])`.
    ///
    /// Restriction: `sep_bytes` must be exactly 1 byte.
    SplitEachPrint {
        text_local: String,
        sep_bytes: Vec<u8>,
        effect: String,
        op: String,
    },
    /// Fused: split `text_local` on `sep_bytes`, select the zero-based `index`th
    /// segment, then call `Print.op` on that segment.
    ///
    /// Equivalent to:
    /// `segments = split(text_local, sep_bytes); Print.op(segments[index])`
    ///
    /// Restriction: `sep_bytes` must be exactly 1 byte.
    SplitGetPrint {
        text_local: String,
        sep_bytes: Vec<u8>,
        index: SplitIndexOperand,
        op: String,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backend_instr_variants_are_debug_cloneable() {
        let instrs = vec![
            WasmBackendInstr::DeclareLocal {
                name: "x".to_string(),
            },
            WasmBackendInstr::LoadLocal {
                name: "x".to_string(),
            },
            WasmBackendInstr::StoreLocal {
                name: "x".to_string(),
            },
            WasmBackendInstr::I64Const(42),
            WasmBackendInstr::PushStaticString {
                text: "hello".to_string(),
            },
            WasmBackendInstr::EffectOp {
                effect: "Print".to_string(),
                op: "print".to_string(),
            },
            WasmBackendInstr::Intrinsic {
                intrinsic: BackendIntrinsic::StringSplit,
            },
            WasmBackendInstr::Drop,
            WasmBackendInstr::SplitEachPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                effect: "Print".to_string(),
                op: "println".to_string(),
            },
            WasmBackendInstr::SplitGetPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                index: SplitIndexOperand::Const(1),
                op: "println".to_string(),
            },
        ];
        // Verify all variants round-trip through Clone and PartialEq.
        let cloned = instrs.clone();
        assert_eq!(instrs, cloned);
    }
}
