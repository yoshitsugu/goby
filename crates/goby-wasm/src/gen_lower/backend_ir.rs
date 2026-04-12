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
// Case pattern types
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
    /// Read one element from a list by index through the shared sequence index boundary.
    ///
    /// Signature: `(list: i64, index: i64) -> i64`.
    /// Out-of-bounds or non-list/non-int abort.
    ///
    /// M6 ownership note:
    /// - surface syntax `xs[i]` lowers to `ValueExpr::ListGet` in `goby-core`,
    ///   then to this intrinsic in `goby-wasm` lowering.
    /// - canonical helper spelling `list.get xs i` also reaches this intrinsic.
    ListGet,
    StringLength,
    ValueToString,
    StringEachGraphemeCount,
    StringEachGraphemeState,
    ListPushString,
    /// Set an element of a list by index, returning a new list (path-copy semantics).
    ///
    /// Signature: `(list: i64, index: i64, value: i64) -> i64` (tagged `List`).
    /// Out-of-bounds or non-list/non-int abort.
    ///
    /// M6 ownership note:
    /// - surface syntax `xs[i] := v` lowers to `CompExpr::AssignIndex` in
    ///   `goby-core`.
    /// - `goby-wasm` lowers that node through the shared index/update boundary
    ///   by descending prefixes with `ListGet` and rebuilding outward with
    ///   `ListSet` (see `lower_assign_index`).
    ListSet,
    ListConcat,
    StringConcat,
    /// Join a `List String` with a separator string into one string.
    ///
    /// Signature: `(tagged_list: i64, tagged_sep: i64) -> i64` (tagged `String`).
    /// Implemented as a host import to avoid allocating O(n) intermediate strings.
    ListJoinString,
    /// Collect all Unicode Extended Grapheme Clusters from a string as a tagged list.
    ///
    /// Signature: `(tagged_str: i64) -> i64` (tagged `List String`).
    /// Implemented as a host import; the host allocates grapheme strings and a list
    /// in the host bump region and returns a tagged list pointer.
    StringGraphemesList,
    /// Split a string into lines using Goby's `read_lines` newline normalization rules.
    ///
    /// Signature: `(tagged_str: i64) -> i64` (tagged `List String`).
    /// Implemented as a host import so CRLF/CR/LF normalization stays aligned with the
    /// runtime-bridge `Read.read_lines` behavior.
    StringSplitLines,
    /// Read the total element count from a chunked list header in O(1).
    ///
    /// Signature: `(tagged_list: i64) -> i64` (tagged `Int`).
    /// Reads the `total_len` i32 field at header offset 0 and returns it as a
    /// tagged Int. This is unreachable from Goby code because it requires direct
    /// access to the chunked internal structure.
    ListLength,
    /// Left fold over a chunked list: chunk-walk loop + accumulator + callback dispatch.
    ///
    /// Signature: `(tagged_list: i64, init_acc: i64, func: i64) -> i64`.
    /// The callback `func(acc, elem)` is called for each element via
    /// `emit_callable_dispatch` with 2 arguments. After the loop, returns the
    /// final accumulator value. Chunk traversal is internal to the runtime and
    /// unreachable from Goby code.
    ///
    /// stdlib surface ownership:
    /// - `fold xs acc f` is a one-line wrapper over this intrinsic.
    /// - `each xs f` is expressed as a fold that discards its accumulator.
    /// - prepend callbacks may lower to `ListReverseFoldPrepend` optimization.
    ListFold,
    /// Map over a chunked list: chunk-walk loop + callback dispatch, builds new list.
    ///
    /// Signature: `(tagged_list: i64, func: i64) -> i64`.
    /// The callback `func(elem)` is called for each element via
    /// `emit_callable_dispatch` with 1 argument. Returns a new tagged list.
    ///
    /// stdlib surface ownership:
    /// - `map xs f` is a one-line wrapper over this intrinsic.
    ListMap,
}

impl BackendIntrinsic {
    pub(crate) fn arity(self) -> usize {
        match self {
            BackendIntrinsic::StringSplit => 2,
            BackendIntrinsic::ListGet => 2,
            BackendIntrinsic::StringLength => 1,
            BackendIntrinsic::ValueToString => 1,
            BackendIntrinsic::StringEachGraphemeCount => 1,
            BackendIntrinsic::StringEachGraphemeState => 2,
            BackendIntrinsic::ListPushString => 2,
            BackendIntrinsic::ListSet => 3,
            BackendIntrinsic::ListConcat => 2,
            BackendIntrinsic::StringConcat => 2,
            BackendIntrinsic::ListJoinString => 2,
            BackendIntrinsic::StringGraphemesList => 1,
            BackendIntrinsic::StringSplitLines => 1,
            BackendIntrinsic::ListLength => 1,
            BackendIntrinsic::ListFold => 3,
            BackendIntrinsic::ListMap => 2,
        }
    }

    pub(crate) const fn execution_boundary(self) -> IntrinsicExecutionBoundary {
        match self {
            BackendIntrinsic::ValueToString
            | BackendIntrinsic::StringEachGraphemeCount
            | BackendIntrinsic::StringEachGraphemeState
            | BackendIntrinsic::StringConcat
            | BackendIntrinsic::ListJoinString
            | BackendIntrinsic::StringGraphemesList
            | BackendIntrinsic::StringSplitLines => IntrinsicExecutionBoundary::HostImport,
            BackendIntrinsic::StringSplit
            | BackendIntrinsic::ListGet
            | BackendIntrinsic::StringLength
            | BackendIntrinsic::ListPushString
            | BackendIntrinsic::ListSet
            | BackendIntrinsic::ListConcat
            | BackendIntrinsic::ListLength
            | BackendIntrinsic::ListFold
            | BackendIntrinsic::ListMap => IntrinsicExecutionBoundary::InWasm,
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
    /// Structured loop expression used by shared lowering rewrites that replace
    /// self-recursive scans with explicit local-state iteration.
    ///
    /// `body_instrs` either:
    /// - leaves one tagged i64 result on the stack to exit the loop expression, or
    /// - executes `ContinueLoop`, which branches back to the loop head after any
    ///   required local updates have already been stored.
    Loop { body_instrs: Vec<WasmBackendInstr> },
    /// Continue the innermost `Loop`.
    ///
    /// This instruction is polymorphic in Wasm terms: callers should update all
    /// loop-carried locals before emitting it.
    ContinueLoop { relative_depth: u32 },
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
    /// String equality is not supported in the current backend path; `Eq` with string operands returns
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
    /// Tail-position direct call to a user-defined top-level declaration.
    ///
    /// This is the RR-5 direct-tail-call normalization marker. For now the
    /// emitter still executes it like `DeclCall`; later RR-5 slices will give
    /// this form a distinct constant-stack execution model.
    TailDeclCall { decl_name: String },
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
    /// Before this instruction (for arity N):
    ///   `[..., arg0: i64, ..., argN-1: i64, callee_tagged: i64]`  (callee is top of stack)
    ///
    /// The emitter decodes the TAG_FUNC i64 to a funcref table slot index (i32),
    /// then emits `call_indirect (type $func_type_arity_N)`.
    ///
    /// # Supported arities (FOLD-M3)
    /// - `arity = 1`: `(i64) -> i64`  (used for `each`/`map` callbacks)
    /// - `arity = 2`: `(i64, i64) -> i64`  (fold callback `f acc elem`)
    ///
    /// # Design note: closure dispatch
    /// Closure callbacks are handled transparently by `emit_callable_dispatch`.
    /// `list.fold` now uses `BackendIntrinsic::ListFold` with its own chunk-walk
    /// loop; this arity-2 indirect call is still used for other 2-argument HOFs.
    IndirectCall { arity: u8 },
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
    /// Initialize a growable Chunked Sequence list builder stored in named locals.
    ///
    /// The builder maintains a chunked sequence header + per-chunk state.
    /// After `ListBuilderFinish` the result is indistinguishable from a `ListLit`
    /// result in the Candidate B (Chunked Sequence) layout.
    ///
    /// Named locals:
    ///   - `header_ptr_local`: u32 ptr to the current header allocation
    ///   - `header_cap_local`: i32 number of chunk-ptr slots allocated in header
    ///   - `n_chunks_local`:   i32 number of chunks currently used
    ///   - `chunk_ptr_local`:  u32 ptr to the current (last) chunk being filled
    ///   - `total_len_local`:  i32 total elements pushed so far
    ListBuilderNew {
        header_ptr_local: String,
        header_cap_local: String,
        n_chunks_local: String,
        chunk_ptr_local: String,
        total_len_local: String,
        initial_capacity: u32,
    },
    /// Append one tagged value to a growable Chunked Sequence list builder.
    ///
    /// `value_instrs` must leave exactly one tagged i64 on the stack.
    /// The emitter fills the current chunk; when it is full (len == CHUNK_SIZE),
    /// it grows the header if needed (geometric reallocation under bump allocator)
    /// and starts a new chunk.
    ListBuilderPush {
        header_ptr_local: String,
        header_cap_local: String,
        n_chunks_local: String,
        chunk_ptr_local: String,
        total_len_local: String,
        value_instrs: Vec<WasmBackendInstr>,
    },
    /// Finalize a growable Chunked Sequence list builder and push a tagged `List` pointer.
    ///
    /// Writes `total_len` and `n_chunks` into the header and returns a tagged header ptr.
    /// The result is indistinguishable from an ordinary `ListLit` result.
    ListBuilderFinish {
        header_ptr_local: String,
        n_chunks_local: String,
        total_len_local: String,
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
    /// Specialized left-fold lowering for list-builder callbacks of the form
    /// `fold xs [] (fn acc x -> [prefix..., ..acc])`.
    ///
    /// The callback shape prepends a fixed prefix chunk onto the accumulator at
    /// each step. That is equivalent to iterating the source list in reverse
    /// order and appending the same prefix chunk into a freshly allocated result
    /// list.
    ///
    /// `item_local` names the Wasm local that receives the current source
    /// element before each `prefix_element_instrs` sequence runs.
    ListReverseFoldPrepend {
        list_instrs: Vec<WasmBackendInstr>,
        item_local: String,
        prefix_element_instrs: Vec<Vec<WasmBackendInstr>>,
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
    /// Load the `index`-th element of a tuple stored in the named local variable.
    ///
    /// # Memory layout
    /// Tuple layout: `(arity: i32, items: [i64]...)`.
    /// The `index`-th field is at byte offset `4 + 8 * index` from the tuple base pointer.
    ///
    /// # Stack discipline
    /// Before this instruction: nothing extra.
    /// After: one tagged `i64` field value on the stack.
    ///
    /// No heap allocation or bump-allocator scratch is needed; this is a pure load.
    TupleGet {
        /// Name of the local variable holding the tagged tuple pointer.
        tuple_local: String,
        /// Zero-based index of the tuple field to load.
        index: usize,
    },

    // -----------------------------------------------------------------------
    // Closure record and mutable cell instructions
    // -----------------------------------------------------------------------
    /// Allocate a closure record and push a TAG_CLOSURE-tagged pointer.
    ///
    /// # Memory layout
    /// `(func_handle: i64, slots: [i64; N])` = `8 + 8*N` bytes (8-byte aligned).
    /// - `func_handle` at offset 0: a TAG_FUNC-encoded i64 (funcref table slot index).
    /// - Slot `i` at offset `8 + 8*i`.
    ///
    /// # Stack discipline
    /// `func_handle_instrs` produces the TAG_FUNC i64.
    /// Each inner `Vec<WasmBackendInstr>` in `slot_instrs` produces one tagged i64 slot value.
    /// The zero-slot case (`slot_instrs = []`) is valid: allocates 8 bytes (func_handle only).
    /// After: one TAG_CLOSURE-tagged i64 on the stack.
    CreateClosure {
        func_handle_instrs: Vec<WasmBackendInstr>,
        slot_instrs: Vec<Vec<WasmBackendInstr>>,
    },

    /// Load a single slot from a closure record in the named local.
    ///
    /// # Stack discipline
    /// Before: nothing extra.
    /// After: one tagged i64 (the slot value) on the stack.
    ///
    /// For a ByValue slot this is the captured value directly.
    /// For a SharedMutableCell slot this is the TAG_CELL-tagged cell pointer.
    LoadClosureSlot {
        /// Name of the local variable holding the TAG_CLOSURE-tagged pointer.
        closure_local: String,
        /// Zero-based slot index within the closure record.
        slot_index: usize,
    },

    /// Allocate an 8-byte mutable cell, store the initial value, push TAG_CELL-tagged pointer.
    ///
    /// # Memory layout
    /// `(value: i64)` = 8 bytes at offset 0.
    ///
    /// # Stack discipline
    /// `init_instrs` produces the initial i64 value.
    /// After: one TAG_CELL-tagged i64 pointer on the stack.
    AllocMutableCell { init_instrs: Vec<WasmBackendInstr> },

    /// Load the current value from a mutable cell.
    ///
    /// # Stack discipline
    /// Before: one TAG_CELL-tagged i64 pointer on the top of the stack.
    /// After: the i64 value stored in the cell (replacing the cell ptr).
    LoadCellValue,

    /// Store a new value into a mutable cell.
    ///
    /// # Stack discipline
    /// - `value_instrs` produces the new i64 value.
    /// - `cell_ptr_instrs` produces the TAG_CELL-tagged pointer to the cell.
    /// - After: the store is performed; a tagged Unit i64 is pushed as the result.
    StoreCellValue {
        cell_ptr_instrs: Vec<WasmBackendInstr>,
        value_instrs: Vec<WasmBackendInstr>,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn closure_and_cell_instrs_round_trip() {
        let instrs = vec![
            // Zero-slot closure (empty env).
            WasmBackendInstr::CreateClosure {
                func_handle_instrs: vec![WasmBackendInstr::PushFuncHandle {
                    decl_name: "my_fn".to_string(),
                }],
                slot_instrs: vec![],
            },
            // Closure with ByValue and cell slots.
            WasmBackendInstr::CreateClosure {
                func_handle_instrs: vec![WasmBackendInstr::PushFuncHandle {
                    decl_name: "my_fn".to_string(),
                }],
                slot_instrs: vec![
                    vec![WasmBackendInstr::I64Const(42)],
                    vec![WasmBackendInstr::LoadLocal {
                        name: "cell_ptr".to_string(),
                    }],
                ],
            },
            WasmBackendInstr::LoadClosureSlot {
                closure_local: "clo".to_string(),
                slot_index: 0,
            },
            WasmBackendInstr::LoadClosureSlot {
                closure_local: "clo".to_string(),
                slot_index: 1,
            },
            WasmBackendInstr::AllocMutableCell {
                init_instrs: vec![WasmBackendInstr::I64Const(0)],
            },
            WasmBackendInstr::LoadCellValue,
            WasmBackendInstr::StoreCellValue {
                cell_ptr_instrs: vec![WasmBackendInstr::LoadLocal {
                    name: "cell".to_string(),
                }],
                value_instrs: vec![WasmBackendInstr::I64Const(7)],
            },
        ];
        let cloned = instrs.clone();
        assert_eq!(instrs, cloned);
    }

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
