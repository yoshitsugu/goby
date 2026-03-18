# Wasm Runtime Architecture — Track F

This document locks the architecture for Track F: General Wasm Lowering.
No implementation may deviate from the decisions recorded here without updating this document first.

Last updated: 2026-03-18

---

## Overview

**Problem**: `goby run` for effectful programs (those using `Read`) is backed by a collection of
handwritten shape-recognizers in `runtime_io_plan.rs` (`Echo`, `SplitLinesEach`).
Each new program shape requires a new recognizer.

**Goal**: Replace recognizer-driven Wasm emission with a *general* lowering pipeline that
compiles effectful `main` bodies from Goby IR to Wasm without enumerating per-program patterns.

---

## 1. Pipeline Layers

```
Goby source (.gb)
      ↓  parser (goby-core)
    AST (Expr / Stmt)
      ↓  ir_lower.rs
    Goby IR (CompExpr / ValueExpr)   ← lowering input for general path
      ↓  gen_lower/lower.rs          ← NEW: general backend lowering
    Backend IR (WasmBackendInstr)    ← NEW: flat instruction set
      ↓  backend.rs (WasmProgramBuilder, extended)
    wasm_encoder calls
      ↓
    Wasm binary (.wasm)
```

### Layer responsibilities

| Layer | File(s) | Responsibility |
|---|---|---|
| Frontend | `goby-core/src/parser*.rs` | Source → AST |
| IR lowering | `goby-core/src/ir_lower.rs` | AST → Goby IR |
| General backend lowering | `goby-wasm/src/gen_lower/` (NEW) | Goby IR → Backend IR → Wasm bytes |
| Value/layout | `goby-wasm/src/gen_lower/value.rs` | `RtValue` representation |
| Wasm emission | `goby-wasm/src/backend.rs` | Wasm bytes via `wasm_encoder` (used by both paths) |
| Direct-style fast path | `goby-wasm/src/lower.rs` | Compile-time IR evaluator + top-level dispatch (pure `main` and effect-boundary handoff) |
| Fallback bridge | `goby-wasm/src/wasm_exec_plan.rs` | IR-to-AST for runtime interpreter fallback |

The general lowering path (`gen_lower/`) must not import from `runtime_io_plan.rs`.

### `PerformEffect` readiness (G5 status)

G5 is complete. `ir_lower.rs` already lowers qualified effect calls to `PerformEffect` nodes:
- `Read.read()` → `PerformEffect { effect: "Read", op: "read", args: [] }`
- `Print.print(x)` → `PerformEffect { effect: "Print", op: "print", args: [x] }`

Unqualified calls (`read()`, `print(x)`) remain as `CompExpr::Call` nodes.
The general lowering pipeline (F3) must handle **both** `PerformEffect` nodes and
`Call` nodes with known effect function names.

---

## 2. Value Representation

All runtime values are represented as tagged `i64` words in Wasm.

### Encoding layout

The tag occupies the **high 4 bits** (bits 63–60) of the `i64` word.
The payload occupies the **lower 60 bits** (bits 59–0).

Consequence: `Int` values are representable in the range `[-2^59, 2^59 - 1]`.
Integers outside this range cannot be represented as scalar immediates and are currently
rejected with a compile-time error. This restriction is acceptable for MVP; a heap-boxed
integer path may be added in a later track.

### Scalar immediates (inline encoding)

| Type | Tag bits (63–60) | Payload (bits 59–0) |
|---|---|---|
| `Unit` | `0x0` | 0 |
| `Int` | `0x1` | 60-bit two's complement integer |
| `Bool` | `0x2` | 0 = False, 1 = True |

### Pointer-bearing tags (Wasm linear memory)

| Type | Tag bits | Payload |
|---|---|---|
| `String` | `0x3` | i32 pointer to `(len: i32, bytes...)` in linear memory |
| `List` | `0x4` | i32 pointer to `(len: i32, items: [i64]...)` in linear memory |

`List` elements are stored as tagged `i64` values, allowing heterogeneous lists in principle
and simplifying future extension (e.g., `List Int` stores `i64` tagged integers).

### Relationship to existing `NativeValue`

`NativeValue` in `lower.rs` is the compile-time evaluator's value type.
It exists only during compilation (Rust process) and is never encoded into Wasm memory.
`RtValue` (Track F) is the runtime representation encoded in Wasm linear memory.
These are distinct types and must not be conflated.

### Effect payload extensibility

Effect operation arguments and resume values are passed as tagged `i64` values using the
same encoding table above. New types (records, tuples, sum types) acquire new tag values
(0x5, 0x6, ...) without changing the encoding of existing types. This ensures that adding
a new effect or value type does not require ABI reset for existing helpers.

---

## 3. Backend IR Shape

The backend IR (`WasmBackendInstr`) is a flat instruction set that sits between Goby IR and
`wasm_encoder`. Its purpose is to be independently testable: backend IR can be constructed in
unit tests without Wasm emission, and the emitter can be tested against known instruction sequences.

**Responsibility boundaries**:
- Goby IR knows nothing about Wasm; it expresses computation in terms of variables, calls, and effects.
- Backend IR expresses computation in terms of Wasm locals, linear memory, and function calls.
- `wasm_encoder` emission translates backend IR to binary; it contains no semantic logic.

**Locked enum definition** (committed in F2):

```rust
pub(crate) enum WasmBackendInstr {
    DeclareLocal { name: String },   // allocate a named Wasm local slot
    LoadLocal    { name: String },   // push local (tagged i64) onto stack
    StoreLocal   { name: String },   // pop stack top into local slot
    I64Const(i64),                   // push compile-time-known tagged i64
    EffectOp { effect: String, op: String }, // WASI-backed effect call
    CallHelper { name: String, arg_count: usize }, // Goby-internal helper call
    Drop,                            // discard top-of-stack value
}
```

**Goby IR → Backend IR mapping:**

| Goby IR node | Backend IR instruction(s) |
|---|---|
| `CompExpr::Let { name, value, body }` | lower `value` → `StoreLocal { name }`, lower `body` |
| `CompExpr::Value(ValueExpr::Var(name))` | `LoadLocal { name }` |
| `CompExpr::Value(ValueExpr::IntLit(n))` | `I64Const(encode_int(n))` |
| `CompExpr::Value(ValueExpr::Unit)` | `I64Const(encode_unit())` |
| `CompExpr::PerformEffect { effect, op, .. }` | `EffectOp { effect, op }` |
| `CompExpr::Call { callee: GlobalRef { name }, .. }` | `CallHelper { name, arg_count }` |
| discarded expression (stmt before tail) | lower expr + `Drop` |

Unsupported IR nodes (`WithHandler`, `Handle`, `Resume`, lambdas) must produce
`LowerError::UnsupportedForm` — never panic.

Future extension points (scalar ops, control flow for `if`) are added as new variants in later milestones.

---

## 4. Call and Effect Lowering Boundaries

### Direct calls (`CompExpr::Call`)

`CompExpr::Call { callee: ValueExpr::Var(name), args }` where `name` is a known
Goby declaration → lower to backend IR direct call instruction.

Curried partial application (callee with fewer args than declared) is **not** supported in
the first slice; emit a `LowerError::UnsupportedForm` explicitly.

### Effect operations

`CompExpr::PerformEffect { effect, op, args }` → lower to the corresponding backend IR
effect-op instruction, which the emitter translates to WASI imports:

| Effect | Op | WASI import |
|---|---|---|
| `Read` | `read` | `wasi_snapshot_preview1::fd_read` (stdin, fd=0) |
| `Read` | `read_line` | `wasi_snapshot_preview1::fd_read` (emitted code reads buffer in a loop and scans for `\n`) |
| `Print` | `print` | `wasi_snapshot_preview1::fd_write` (stdout, fd=1, no newline) |
| `Print` | `println` | `wasi_snapshot_preview1::fd_write` (stdout, fd=1; data buffer includes trailing `\n` byte) |

Unqualified `CompExpr::Call` nodes with callee names `read`, `print`, `println` etc.
are treated identically to the corresponding `PerformEffect` nodes.

### Effect forms not yet supported in F3

`CompExpr::WithHandler`, `CompExpr::Handle`, `CompExpr::Resume` are **not** lowered
by the general path in F3–F5. When these nodes appear, the general lowerer must return
`LowerError::UnsupportedForm { node: "..." }` — **never panic**.

---

## 5. `wasm_exec_plan.rs` Disposition

`wasm_exec_plan.rs` contains an IR-to-AST round-trip (`comp_to_stmts`, `comp_to_expr`) that
converts Goby IR back to AST so the runtime interpreter can evaluate it.
This is architecturally opposite to Track F's goal (IR → Wasm).

**Decision**: `wasm_exec_plan.rs` is **kept as a fallback layer** during F2–F5 development.
The general path and the fallback path are separate entry points; there is no shared execution.

After F6 convergence (Track F §4.6 F6), `wasm_exec_plan.rs` becomes a candidate for deletion
or sharp reduction. A deletion plan must be committed as part of F6.

---

## 6. Runtime Helper ABI Boundary

The general lowering path calls a set of Wasm-imported helper functions. Their signatures
are locked before F2 implementation starts and must not change without a migration plan.

### Locked helper signatures (WASI Preview 1)

```
(import "wasi_snapshot_preview1" "fd_read"
  (func (param i32 i32 i32 i32) (result i32)))

(import "wasi_snapshot_preview1" "fd_write"
  (func (param i32 i32 i32 i32) (result i32)))
```

### Runtime memory helpers (Goby-internal, defined in emitted Wasm)

These are emitted as Wasm functions within the module (not imported).
Signatures confirmed in F2 based on the `CallHelper { name, arg_count }` backend IR interface
and the `value.rs` tagged-i64 representation.

```
goby_alloc_string(len: i32) -> i32   # returns i32 pointer to (len: i32, bytes...) in heap
goby_alloc_list(cap: i32) -> i32     # returns i32 pointer to (len: i32, items: [i64]...) in heap
goby_string_split(ptr: i32, sep_ptr: i32) -> i32  # returns i32 List pointer (wrapped in encode_list_ptr by caller)
goby_list_get(list_ptr: i32, idx: i32) -> i64     # returns tagged i64 (encode_* value); traps on OOB
```

`CallHelper` naming convention: the `name` field matches the Goby-internal function name exactly
(e.g. `"goby_string_split"`). `arg_count` must equal the number of arguments pushed before the call.

**Memory layout**: linear memory starts at byte 0.
- Bytes 0–7: iovec slot (ptr at 0, len at 4) used by `fd_write` / `fd_read`.
- Bytes 8–11: nread/nwritten counter (single 4-byte slot reused by `fd_read` and `fd_write` for byte-count output at different times; not two separate slots).
- Bytes 12–15: reserved / alignment padding.
- Byte 16 (`layout::HEAP_BASE`): heap starts here; bump-pointer allocation; no GC in F-track scope.

---

## 7. Module Ownership Table

### New module: `gen_lower/`

| File | Responsibility |
|---|---|
| `gen_lower/mod.rs` | Public re-exports; declares the `GeneralLowerer` entry point |
| `gen_lower/value.rs` | `RtValue` tagged-i64 representation; encode/decode helpers |
| `gen_lower/lower.rs` | `GeneralLowerer`: Goby IR → Backend IR |
| `gen_lower/emit.rs` | Backend IR → `wasm_encoder` calls (extends `backend.rs` or wraps it) |

### Relationship to existing files

| Existing file | Track F disposition |
|---|---|
| `lower.rs` | Kept as direct-style fast path; called before general path in `compile_module` |
| `backend.rs` | Kept; extended with helpers called by `gen_lower/emit.rs` |
| `wasm_exec_plan.rs` | Kept as fallback bridge during F2–F5; shrink candidate after F6 |
| `runtime_io_plan.rs` | Shrink target; kept temporarily for parity; deleted or reduced in F6 |
| `planning.rs` | Kept; provides `LoweringStyle` and `LoweringPlan` for the fast path |
| `fallback.rs` | Kept; provides `native_unsupported_reason` for error reporting |

`gen_lower/` must not import from `runtime_io_plan.rs`.
`gen_lower/` may import from `backend.rs`, `layout.rs`, `goby-core/src/ir.rs`, and `planning.rs`
(for `LoweringPlan`/`LoweringStyle` when distinguishing direct-style from effect-boundary declarations).
`gen_lower/` must not call into `ir_lower.rs` directly; it consumes pre-lowered `IrDecl`/`CompExpr` only.

---

## 8. Test Strategy by Layer

### Unit tests (per-module)

- `gen_lower/value.rs`: encode/decode round-trip for all tag variants; boundary cases.
- `gen_lower/lower.rs`: lower individual `CompExpr` nodes to expected backend IR sequences.
- `gen_lower/emit.rs`: emit backend IR for a single-instruction sequence; check Wasm binary structure.

### Focused backend tests

- Lower the three representative programs to backend IR and assert structural properties
  (e.g., "contains an fd_read call instruction before an fd_write call instruction").
- These live in `gen_lower/lower_tests.rs` or `compile_tests.rs`.

### Parity tests

- Programs that currently pass through `DynamicWasiIo(Echo)` must produce equivalent output
  when lowered through the general path. Use `assert_mode_parity` from `runtime_parity.rs`
  (already in the codebase).
- Location: `runtime_parity.rs` or a new `gen_lower/parity_tests.rs`.

### End-to-end tests

- `goby run tests/track-f/f3_print_read.gb` with stdin `"hello\n"` → stdout `"hello"`.
- These are CLI-level tests; use the existing `compile_tests.rs` pattern.

---

## 9. Fast Paths vs General Path Boundary

| Program shape | Execution path |
|---|---|
| Pure `main` (no effects, no `Read`/`Print`) | `lower.rs` compile-time evaluator |
| `main` with `Print` only (static or local-binding) | `lower.rs` compile-time evaluator |
| `main` with `Read` + simple `Print` (Echo, SplitLinesEach shapes) | `runtime_io_plan.rs` temporarily |
| **All effectful programs** (general target) | `gen_lower/` general lowering path |

The general path entry condition is: `main` contains any `PerformEffect` node, or any
direct call to a known effect function name (`read`, `read_line`, `print`, `println`), and the
direct-style fast path returned `NativeLoweringResult::NotLowered` (i.e., neither `Emitted` nor
`EffectBoundaryHandoff`). The general path is tried after the direct-style fast path declines.

**Optimization fast paths**: `runtime_io_plan.rs` recognizers may remain after F6 *only* if
they are documented as strict optimization layers with the same semantics and ABI as the
general path. They must not be the *semantic source of truth*.

---

## 10. Rejected Alternatives

### A. InterpreterBridge revival

**Description**: Re-enable the interpreter-bridge path so unsupported programs execute via the
Rust runtime evaluator, embedding program output into the Wasm binary.

**Rejected because**:
- It does not produce a Wasm module that executes against real stdin at runtime.
- It conflates compile-time and runtime; programs that read stdin cannot be compiled this way.
- It would grow the set of programs that bypass the Wasm execution model,
  making it harder to shrink the fallback surface over time.
- PLAN.md §4.6 explicitly forbids treating `InterpreterBridge` revival as the target architecture.

### B. Extending `RuntimeIoPlan` recognizers

**Description**: Add new variants to `RuntimeIoPlan` for each new program shape (e.g., `ReadLine`,
`SplitFirstLine`, `EchoWithTransform`, ...).

**Rejected because**:
- Each new shape requires bespoke pattern-matching code in both the classifier and the emitter.
- The recognizer surface grows quadratically with the number of distinct program patterns.
- There is no compositional lower bound; novel programs always require new cases.
- The architecture debt this creates is precisely why Track F exists.

### C. Direct `wasm_encoder` calls from Goby IR (no backend IR layer)

**Description**: Translate `CompExpr` nodes directly to `wasm_encoder` `Instruction` calls,
with no intermediate backend IR.

**Rejected because**:
- There is no independently testable intermediate representation.
- Backend IR allows unit tests that verify lowering logic without producing Wasm binaries.
- Without a backend IR, every lowering correctness test must parse and validate full Wasm binaries,
  making tests fragile and slow.
- The backend IR layer also provides a clean boundary for future optimizations (e.g., dead-code
  elimination, constant folding in the IR before emission).

---

## 11. Invariant Checklist

These invariants must hold at all times in the general lowering path.
PRs that touch `gen_lower/` must verify each item.

- [ ] **Panic-free lowering**: the general lowerer must not call `panic!`, `unwrap()`, or `expect()`
  on any user-controlled input. All failure modes must return `LowerError` or `CodegenError`.
- [ ] **Explicit unsupported errors**: when an IR node is not yet supported (`WithHandler`,
  `Handle`, `Resume`, lambda calls, etc.), the lowerer returns `LowerError::UnsupportedForm`
  with the node name. It never silently returns `None` or `Unit`.
- [ ] **Stable helper ABI**: runtime helper function signatures (§6) are frozen. New helpers
  require a signature entry in §6 before implementation. Changing an existing signature requires
  updating §6 and a migration note.
- [ ] **Single semantic source**: for each effect operation (`Read.read`, `Print.print`, etc.),
  there is exactly one place in the codebase that defines the lowering behavior (the backend IR
  effect-op instruction and its emitter). `runtime_io_plan.rs` shapes may coexist temporarily
  but must not define conflicting semantics.
- [ ] **`PerformEffect` canonical by F3**: by the end of F3, all effectful programs consumed
  by the general path have their effect calls represented as `PerformEffect` IR nodes.
  `CompExpr::Call` nodes with effect function names are normalized to `PerformEffect` at the
  entry of the general lowerer.
- [ ] **`wasm_exec_plan.rs` deletion plan committed in F6**: the F6 milestone must include a
  committed deletion plan for `wasm_exec_plan.rs` (full deletion, reduction to a thin stub,
  or an explicit documented rationale for keeping it). F6 must not merge without this item.

---

## 12. Representative Programs

The following three programs are the primary correctness targets for Track F.
They are committed as fixture files in `tests/track-f/` and must pass by the specified milestone.

### F3 target: `tests/track-f/f3_print_read.gb`

```goby
main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  Print.print text
```

Architecture path:
1. `ir_lower` lowers to `Let { name: "text", value: PerformEffect(Read.read), body: PerformEffect(Print.print, [Var("text")]) }`.
2. `gen_lower/lower.rs` lowers to backend IR: alloc buffer → fd_read → store string → fd_write.
3. Emitter produces Wasm that reads all of stdin and writes it to stdout.

### F4 target: `tests/track-f/f4_split_each.gb`

```goby
main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  lines = string.split text "\n"
  each lines Print.println
```

Note: `each lines Print.println` passes `Print.println` as a higher-order argument to `each`.
This requires the F4 general lowering to support calling an effect function reference stored
in a local variable — distinct from directly calling `Print.println(x)`.
The §4 restriction ("curried partial application is not supported in the first slice") applies
to *user-defined* curried functions; passing a built-in effect operation as a named reference to
`each` is a separate mechanism that F4 must handle explicitly.
If F4 scope does not include higher-order effect references, the fixture must be updated
with an explicit per-element loop and this section revised.

Architecture path (target):
1. IR contains `PerformEffect(Read.read)`, `Call(string.split, [text, "\n"])`, `Call(each, [lines, Print.println ref])`.
2. `gen_lower` handles the `string.split` helper call via `goby_string_split` helper ABI.
3. `each` with a `Print.println` reference is lowered as a loop: `goby_list_get` per iteration → `fd_write` with trailing `\n`.

### F5 target: `tests/track-f/f5_index.gb`

```goby
main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  lines = string.split text "\n"
  Print.println (list.get lines 1)
```

Architecture path:
1. IR contains `Call(list.get, [lines, 1])`.
2. `gen_lower` emits `goby_list_get(lines_ptr, 1)` → tagged i64 result → fd_write.
3. Out-of-range: `goby_list_get` traps with a diagnostic message.

---

## Change Log

- 2026-03-18: Initial document created for Track F F1 milestone.
