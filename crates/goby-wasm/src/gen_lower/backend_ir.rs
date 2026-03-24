//! Backend IR for the general Wasm lowering path.
//!
//! `WasmBackendInstr` is a flat instruction set that sits between Goby IR and
//! `wasm_encoder`. Its purpose is to be independently testable: backend IR can
//! be constructed in unit tests without Wasm emission.
//!
//! The design rationale and the Goby IR mapping are documented in this module
//! and in `gen_lower/mod.rs`.

use goby_core::ir::IrBinOp;

use crate::host_runtime::IntrinsicExecutionBoundary;

// ---------------------------------------------------------------------------
// Case pattern types (WB-2B)
// ---------------------------------------------------------------------------

/// A single item in a `BackendCasePattern::ListPattern`.
///
/// Each item corresponds to one `IrListPatternItem` from the IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BackendListPatternItem {
    /// Match an exact integer literal at this position.
    IntLit(i64),
    /// Match an exact string literal at this position.
    StrLit(String),
    /// Bind the item at this position to a local variable.
    Bind(String),
    /// Ignore the item at this position (wildcard `_`).
    Wildcard,
}

/// A compiled pattern in a `CaseArmInstr`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BackendCasePattern {
    /// Match an exact integer value (tagged i64 Int).
    IntLit(i64),
    /// Match an exact boolean value (tagged i64 Bool).
    BoolLit(bool),
    /// Match an exact string value (byte-wise comparison in Wasm).
    StrLit(String),
    /// Match an empty list (`len == 0`).
    EmptyList,
    /// Match a non-empty list with at least `items.len()` elements.
    ///
    /// Each element in `items` may match a literal or bind a name.
    /// `tail` is `Some(name)` to bind the remaining suffix as a sub-list,
    /// or `None` to require that the list has exactly `items.len()` elements.
    ListPattern {
        items: Vec<BackendListPatternItem>,
        tail: Option<String>,
    },
    /// Unconditional arm (always matches, must be the last arm).
    Wildcard,
}

/// A single arm in a `WasmBackendInstr::CaseMatch`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CaseArmInstr {
    pub(crate) pattern: BackendCasePattern,
    pub(crate) body_instrs: Vec<WasmBackendInstr>,
}

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
    StringConcat,
    /// Collect all Unicode Extended Grapheme Clusters from a string as a tagged list.
    ///
    /// Signature: `(tagged_str: i64) -> i64` (tagged `List String`).
    /// Implemented as a host import; the host allocates grapheme strings and a list
    /// in the host bump region and returns a tagged list pointer.
    StringGraphemesList,
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
            BackendIntrinsic::StringConcat => 2,
            BackendIntrinsic::StringGraphemesList => 1,
        }
    }

    pub(crate) const fn execution_boundary(self) -> IntrinsicExecutionBoundary {
        match self {
            BackendIntrinsic::StringEachGraphemeCount
            | BackendIntrinsic::StringEachGraphemeState
            | BackendIntrinsic::StringConcat
            | BackendIntrinsic::StringGraphemesList => IntrinsicExecutionBoundary::HostImport,
            BackendIntrinsic::StringSplit
            | BackendIntrinsic::ListGet
            | BackendIntrinsic::StringLength
            | BackendIntrinsic::ListPushString => IntrinsicExecutionBoundary::InWasm,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BackendReadOp {
    Read,
    ReadLine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BackendPrintOp {
    Print,
    Println,
}

impl BackendPrintOp {
    pub(crate) const fn append_newline(self) -> bool {
        matches!(self, Self::Println)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BackendEffectOp {
    Read(BackendReadOp),
    Print(BackendPrintOp),
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
/// | `CompExpr::PerformEffect { effect, op, .. }` | `EffectOp { op }` |
/// | `CompExpr::Call { callee: GlobalRef { name }, .. }` (intrinsic) | `Intrinsic { intrinsic }` |
/// | `CompExpr::Call { callee: GlobalRef { name }, .. }` (user decl) | `[push args..., DeclCall { decl_name }]` |
/// | fused `Let lines = split(text, sep); each lines Effect.op` | `SplitEachPrint { text_local, sep_bytes, op }` |
/// | fused `Let lines = split(text, sep); Let line = list.get(lines, idx); Print.op(line)` | `SplitGetPrint { text_local, sep_bytes, index, op }` |
/// | fused `Let parts = graphemes(text); Let item = list.get(parts, N); Print.op(item)` | `[LoadLocal(text), I64Const(N), Intrinsic(StringEachGraphemeState), EffectOp { op: Print(op) }]` |
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
    EffectOp { op: BackendEffectOp },
    /// Call a Wasm-internal backend intrinsic.
    ///
    /// The intrinsic's fixed arity arguments must be on the stack before this instruction.
    /// These intrinsics form the backend primitive substrate beneath stdlib helpers.
    Intrinsic { intrinsic: BackendIntrinsic },
    /// Discard the top-of-stack value.
    Drop,
    /// Conditional expression.
    ///
    /// The condition (a tagged Bool) must be on the Wasm stack before this instruction.
    /// The emitter converts it to an i32 (0 or 1) and emits a Wasm `if/else/end` block.
    /// Both branches produce exactly one tagged i64 result.
    If {
        then_instrs: Vec<WasmBackendInstr>,
        else_instrs: Vec<WasmBackendInstr>,
    },
    /// Binary operation on two tagged i64 values.
    ///
    /// Both operands are expected to be on the Wasm stack (left operand deeper, right on top).
    /// The result replaces both operands with a single tagged i64.
    ///
    /// # Int arithmetic (`Add`, `Sub`, `Mul`, `Div`, `Mod`)
    /// Operands must be tagged Int. The emitter untaggs, applies the Wasm i64 op,
    /// and retags the result as Int. `Mod` uses `i64.rem_s` (truncated division).
    ///
    /// # Int comparison (`Eq`, `Lt`, `Gt`, `Le`, `Ge`)
    /// Operands must be tagged Int. The emitter untags, compares, and produces a tagged Bool.
    ///
    /// # Bool logical and (`And`)
    /// Operands must be tagged Bool. The emitter extracts payload bits, applies `i64.and`,
    /// and retags the result as Bool.
    ///
    /// String equality is not supported in WB-1; `Eq` with string operands returns
    /// `UnsupportedForm` at lowering time.
    BinOp { op: IrBinOp },
    /// Call a user-defined top-level declaration by name.
    ///
    /// Arguments must be pushed onto the stack (left-to-right) before this instruction.
    /// The callee returns exactly one tagged i64 result.
    ///
    /// `decl_name` is the unqualified function name as it appears in the IR.
    /// The emitter resolves it to a Wasm function index using the module-level
    /// `decl_name → func_idx` table built during `emit_general_module`.
    DeclCall { decl_name: String },
    /// Push a tagged function-handle (TAG_FUNC i64) for a named top-level declaration.
    ///
    /// The emitter encodes the funcref table slot index for `decl_name` as a tagged i64
    /// (`encode_func_handle(table_slot)`), then emits `i64.const <tagged>`.
    ///
    /// Use this when a top-level declaration is passed as a *value* (not directly called).
    /// For direct calls, use `DeclCall` instead.
    PushFuncHandle { decl_name: String },
    /// Call a runtime function value via `call_indirect`.
    ///
    /// # Stack discipline
    /// Before this instruction:
    ///   `[..., arg0: i64, callee_tagged: i64]`  (callee is top of stack)
    ///
    /// The emitter decodes the TAG_FUNC i64 to a funcref table slot index (i32),
    /// then emits `call_indirect (type $single_arg_func_type)`.
    ///
    /// # Type restriction (WB-2A)
    /// Only `(i64) -> i64` call sites are supported (one argument).
    /// In Goby's current IR, all first-class function calls are single-argument
    /// (`f x` in the desugared form), so this restriction covers all current cases.
    IndirectCall,
    /// Pattern-match the scrutinee against a sequence of arms.
    ///
    /// # Stack discipline
    /// Before this instruction, the lowering phase emits:
    ///   `DeclareLocal { name: scrutinee_local }` + scrutinee value instrs + `StoreLocal { name: scrutinee_local }`
    /// `CaseMatch` itself does not push the scrutinee; it reads it from `scrutinee_local`.
    /// The result is exactly one tagged i64 (the matched arm body value).
    ///
    /// Arms are tested in order. The last arm must be `Wildcard`.
    CaseMatch {
        /// Name of the local that holds the scrutinee value.
        ///
        /// This local must have been declared and populated (via `DeclareLocal` +
        /// scrutinee instructions + `StoreLocal`) *before* this `CaseMatch` instruction.
        scrutinee_local: String,
        arms: Vec<CaseArmInstr>,
    },
    /// Construct a list value from a fixed number of element instruction sequences.
    ///
    /// Each inner `Vec<WasmBackendInstr>` produces one tagged i64 element.
    /// The elements are pushed left-to-right onto the Wasm stack; the emitter then
    /// allocates `4 + 8 * len` bytes via the bump allocator, writes the length
    /// header (`i32`) and each element (`i64`), and pushes a tagged list pointer.
    ///
    /// An empty list (`element_instrs` is empty) allocates 4 bytes and writes len=0.
    ListLit {
        element_instrs: Vec<Vec<WasmBackendInstr>>,
    },
    /// Construct a tuple value from a fixed number of element instruction sequences.
    ///
    /// Each inner `Vec<WasmBackendInstr>` produces one tagged i64 element.
    /// The emitter allocates `4 + 8 * len` bytes via the bump allocator, writes
    /// the tuple arity (`i32`) followed by each field value (`i64`), and pushes
    /// a tagged tuple pointer.
    ///
    /// The empty tuple is represented as `Unit` at lowering time and therefore
    /// never reaches this backend instruction.
    TupleLit {
        element_instrs: Vec<Vec<WasmBackendInstr>>,
    },
    /// Construct a record value from a constructor tag and field instruction sequences.
    ///
    /// The emitter allocates `8 + 8 * field_count` bytes via the bump allocator,
    /// writes a module-local constructor tag (`i64`) at offset 0, writes each field
    /// value (`i64`) in IR field order, and pushes a tagged record pointer.
    RecordLit {
        constructor: String,
        field_instrs: Vec<Vec<WasmBackendInstr>>,
    },
    /// Fused: iterate a list and call an effect operation on each element (e.g. `Print.println`).
    ///
    /// Equivalent to `for item in list: effect.op(item)`.
    ///
    /// Lowered from: `Call(GlobalRef("list","each"), [list_expr, GlobalRef(effect, op)])`.
    /// The result is a tagged Unit.
    ListEachEffect {
        list_instrs: Vec<WasmBackendInstr>,
        op: BackendPrintOp,
    },
    /// Iterate a list and call a function for each element (stdlib `list.each`).
    ///
    /// # Stack discipline
    /// Before this instruction:
    ///   - `list_instrs` evaluates to a tagged List i64 (the source list)
    ///   - `func_instrs` evaluates to a tagged Func i64 (the callback funcref)
    ///
    /// The emitter decodes the list pointer, loops over elements, and calls the
    /// callback via `call_indirect` for each element.  The callback must have type
    /// `(i64) -> i64`.  The return value of each call is discarded.
    /// This instruction pushes a tagged Unit value as its result.
    ListEach {
        list_instrs: Vec<WasmBackendInstr>,
        func_instrs: Vec<WasmBackendInstr>,
    },
    /// Map a function over a list and return a new list (stdlib `list.map`).
    ///
    /// # Stack discipline
    /// Before this instruction:
    ///   - `list_instrs` evaluates to a tagged List i64 (the source list)
    ///   - `func_instrs` evaluates to a tagged Func i64 (the callback funcref)
    ///
    /// The emitter decodes the list pointer, loops over elements, calls the callback
    /// via `call_indirect` for each element, collects results into a new heap-allocated
    /// list, and pushes the tagged List pointer of the result.
    /// The callback must have type `(i64) -> i64`.
    ListMap {
        list_instrs: Vec<WasmBackendInstr>,
        func_instrs: Vec<WasmBackendInstr>,
    },
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
        op: BackendPrintOp,
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
        op: BackendPrintOp,
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
                op: BackendEffectOp::Print(BackendPrintOp::Print),
            },
            WasmBackendInstr::Intrinsic {
                intrinsic: BackendIntrinsic::StringSplit,
            },
            WasmBackendInstr::Drop,
            WasmBackendInstr::SplitEachPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                op: BackendPrintOp::Println,
            },
            WasmBackendInstr::SplitGetPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                index: SplitIndexOperand::Const(1),
                op: BackendPrintOp::Println,
            },
        ];
        // Verify all variants round-trip through Clone and PartialEq.
        let cloned = instrs.clone();
        assert_eq!(instrs, cloned);
    }

    #[test]
    fn case_match_and_list_lit_round_trip() {
        // CaseMatch with literal, EmptyList, ListPattern, and Wildcard arms.
        let case_instr = WasmBackendInstr::CaseMatch {
            scrutinee_local: "__case_scrutinee_0".to_string(),
            arms: vec![
                CaseArmInstr {
                    pattern: BackendCasePattern::IntLit(0),
                    body_instrs: vec![WasmBackendInstr::PushStaticString {
                        text: "zero".to_string(),
                    }],
                },
                CaseArmInstr {
                    pattern: BackendCasePattern::BoolLit(true),
                    body_instrs: vec![WasmBackendInstr::PushStaticString {
                        text: "true".to_string(),
                    }],
                },
                CaseArmInstr {
                    pattern: BackendCasePattern::StrLit("hello".to_string()),
                    body_instrs: vec![WasmBackendInstr::PushStaticString {
                        text: "matched".to_string(),
                    }],
                },
                CaseArmInstr {
                    pattern: BackendCasePattern::EmptyList,
                    body_instrs: vec![WasmBackendInstr::I64Const(0)],
                },
                CaseArmInstr {
                    pattern: BackendCasePattern::ListPattern {
                        items: vec![
                            BackendListPatternItem::IntLit(1),
                            BackendListPatternItem::StrLit("x".to_string()),
                            BackendListPatternItem::Bind("h".to_string()),
                            BackendListPatternItem::Wildcard,
                        ],
                        tail: Some("rest".to_string()),
                    },
                    body_instrs: vec![WasmBackendInstr::LoadLocal {
                        name: "h".to_string(),
                    }],
                },
                CaseArmInstr {
                    pattern: BackendCasePattern::Wildcard,
                    body_instrs: vec![WasmBackendInstr::PushStaticString {
                        text: "other".to_string(),
                    }],
                },
            ],
        };
        let list_instr = WasmBackendInstr::ListLit {
            element_instrs: vec![
                vec![WasmBackendInstr::I64Const(1)],
                vec![WasmBackendInstr::I64Const(2)],
            ],
        };
        let instrs = vec![case_instr, list_instr];
        let cloned = instrs.clone();
        assert_eq!(instrs, cloned);
    }
}
