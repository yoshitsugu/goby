# Real Wasm Code Generation Plan (Post-MVP) — COMPLETE (Scope: Phase 0-8)

Plan status:
- Completed on 2026-03-02 (sessions 44-45 sign-off).
- This document is now a closed record for the Phase 0-8 migration scope.

## 1. Goal and Constraints

Goal: replace the current compile-time interpreter path in `crates/goby-wasm/src/lib.rs` with real instruction-level Wasm emission from Goby AST.

Hard constraints:
- Keep public API: `compile_module(module: &Module) -> Result<Vec<u8>, CodegenError>`.
- Keep runtime target: WASI Preview 1 (`wasi_snapshot_preview1.fd_write`) for stdout.
- Preserve current `goby-cli run examples/*.gb` output behavior during migration.
- Phase A may keep fallback for `Stmt::Using` (effect handler execution).
- Prefer `wasm-encoder` (not `walrus`/`wat`) for module construction.

Non-goal (initial phases): full effect runtime lowering.

## 2. Current State (Baseline)

`compile_module` currently does:
1. Find `main` declaration.
2. If `supports_native_codegen(module)` is true, try native lowering (`lower::try_emit_native_module`).
3. If native lowering succeeds, return wasm-encoder-built Wasm.
4. Otherwise, use compatibility fallback: `resolve_main_runtime_output(...)` + `compile_print_module(...)`.
5. If fallback cannot resolve printable output, return `Err(CodegenError)`.

As of 2026-03-02, native lowering covers a meaningful Phase A subset in tests:
- literal/binding `print`,
- `Int`/`Bool` expressions (`+`, `*`, `==`),
- direct first-order function calls in expressions,
- `List Int` literals + `print`/pipeline print,
- `if` and `case` expressions used by `examples/control_flow.gb`.

## 3. Target Backend Architecture

Implement a native codegen pipeline inside `crates/goby-wasm`:
- `WasmProgramBuilder`: owns module-level sections (`TypeSection`, `ImportSection`, `FunctionSection`, `MemorySection`, `ExportSection`, `CodeSection`, `DataSection`) using `wasm-encoder`.
- `StringInterner/DataLayout`: assigns static data offsets for string literals.
- `FnLowerer`: lowers Goby decl/expr AST into Wasm function bodies.
- `MainLowering`: lowers `main` statement block with local environment.
- `FallbackGate`: determines if module/decl uses unsupported constructs and must use compile-time interpreter path.

Keep old interpreter path during migration, but put it behind an explicit fallback gate.

## 4. Wasm-Level Representation Decisions

### 4.1 Module/Imports/Memory layout
- Import: `(import "wasi_snapshot_preview1" "fd_write" (func ...))` via `wasm-encoder::ImportSection`.
- Single linear memory, exported as `memory`.
- Reserve low memory for iovec scratch:
  - `IOVEC_OFFSET = 0` (8 bytes)
  - `NWRITTEN_OFFSET = 8` (4 bytes)
  - `HEAP_BASE = 16` (first free byte)
- Add mutable global `heap_ptr: i32`, initialized to end of static data segment.

### 4.2 Runtime value model (Phase A/B)
- `Int`: Wasm `i64`.
- `Bool`: Wasm `i32` (`0/1`).
- `String`: pair `(ptr: i32, len: i32)` on stack/locals.
  - String literals are static bytes in data segments.
  - Dynamic strings (e.g. int-to-string, concat result) allocated from bump heap.
- `List Int`: pair `(ptr: i32, len: i32)`, elements packed as contiguous little-endian `i64`.
- `List String` and record runtime layout are deferred; keep fallback until supported.

### 4.3 Locals and function call lowering
- For each Goby binding (`Stmt::Binding`), allocate Wasm local(s) according to lowered type.
  - `Int` -> one `i64` local.
  - `String` -> two `i32` locals.
  - `List Int` -> two `i32` locals.
- `f x` and `f(x)` are already normalized as `Expr::Call`; same lowering path.
- First stage supports only direct named callee calls (`Expr::Var`).
- Lambda values / closure environment / higher-order indirect calls are deferred (fallback).

### 4.4 `print` built-in to WASI `fd_write`
Lower `print <expr>` to:
1. Lower `<expr>` to printable `(ptr,len)` string (direct for string; helper conversion for int/list).
2. Store `ptr`/`len` into iovec at `IOVEC_OFFSET`.
3. Call imported `fd_write(fd=1, iovs=IOVEC_OFFSET, iovs_len=1, nwritten=NWRITTEN_OFFSET)`.
4. Ignore return code in Phase 1-3 (validate non-trap only); add error handling later.

## 5. Phased Implementation Plan

### 5.0 Incremental Execution Policy (mandatory gates)

To keep feedback cycles short, each phase is treated as a hard stop-and-check milestone.

Rules:
- Do not start Phase N+1 until all Phase N gate checks pass.
- Keep each phase small enough to be merged independently.
- Add at least one test that proves native path selection for that phase subset.
- Keep fallback path green in parallel (no regressions in unsupported features).
- If a gate fails, rollback or fix within the same phase before proceeding.

Per-phase gate format:
- `Build gate`: `cargo check`, `cargo test -p goby-wasm`.
- `Behavior gate`: one or more `goby-cli run examples/X.gb` output checks.
- `Path gate`: test/assertion that native path (not fallback) is selected for newly supported forms.
- `Fallback gate`: at least one unsupported example still correctly uses fallback.

## Phase 0 - Backend skeleton and dual-path gate
Status (2026-03-02): Completed.

AST subset lowered natively:
- None yet (infra only).

Interpreter coexistence:
- Default to fallback while scaffold lands.
- Introduce explicit capability check: `supports_native_codegen(module) -> bool`.

Add:
- `wasm-encoder` dependency in `crates/goby-wasm/Cargo.toml`.
- New internal modules in `crates/goby-wasm/src/`:
  - `backend.rs` (module builder)
  - `layout.rs` (memory/data offsets)
  - `lower.rs` (future expr/stmt lowering entry)
  - `fallback.rs` (feature detector)
- Refactor `compile_module` to:
  - try native path first when supported,
  - otherwise call existing `resolve_main_runtime_output` path.

Delete/replace:
- No functional deletion yet. Keep `compile_print_module` and interpreter unchanged.

Definition of done:
- `cargo check`
- `cargo test -p goby-wasm`
- existing Wasm unit tests still pass.

Gate checks before Phase 1:
- Build gate passes.
- Path gate: capability checker exists and is test-covered.
- Fallback gate: `examples/effect.gb` still uses fallback path.

## Phase 1 - Minimal real Wasm: `print "hello"`
Status (2026-03-02): Completed.

AST subset lowered natively:
- `Stmt::Expr(Expr::Call{callee=Var("print"), arg=StringLit})` in `main`.
- `Stmt::Binding` with `StringLit` + `print <var>` in `main`.

Interpreter coexistence:
- Fallback for everything else.

Add:
- `emit_module_for_main(...)` using `wasm-encoder` sections (no `wat::parse_str`).
- Static string data interning and offset assignment.
- `emit_fd_write_print(ptr,len)` helper.
- `main` lowering over parsed `Stmt` list for tiny supported subset.

Delete/replace:
- Replace `compile_print_module` usage for supported subset with new native emitter.
- Keep function for fallback-only path temporarily.

Definition of done:
- `cargo run -p goby-cli -- run examples/hello.gb` output unchanged (`Hello Goby!`).
- Existing tests plus new tests:
  - native path selected for literal print,
  - produced module has valid Wasm header,
  - runtime output equals baseline.

Gate checks before Phase 2:
- Behavior gate: `examples/hello.gb` exact output match.
- Path gate: literal print + local string binding print confirmed native.
- Fallback gate: unsupported forms still route to interpreter.

## Phase 2 - Int expressions and local bindings in `main`
Status (2026-03-02): Completed.

AST subset lowered natively:
- `Expr::IntLit`, `Expr::Var`, `Expr::BinOp(Add|Mul|Eq)`.
- `Stmt::Binding`/shadowing in `main`.
- `print` of `Int` and `Bool` (with helper conversion).
- `Expr::If` (expression form) where branches return `Int`/`String`/`Bool`.

Interpreter coexistence:
- Fallback for `List*`, `Case`, function calls, `Using`, lambda.

Add:
- Int-to-string routine in Wasm (or runtime helper function emitted once).
- Bool print canonicalization (`True`/`False`) to match current behavior.
- Local symbol table for lowered `main` bindings.

Delete/replace:
- Reduce `resolve_main_runtime_output` reliance for `hello.gb` and simple arithmetic cases.

Definition of done:
- `cargo run -p goby-cli -- run examples/control_flow.gb` still may fallback due to `case`, but no regressions.
- New focused tests for native `main` arithmetic + print.

Gate checks before Phase 3:
- Behavior gate: arithmetic-focused fixtures match baseline output.
- Path gate: `Int`/`Bool` print cases confirmed native.
- Fallback gate: `case`/`using` examples remain fallback with unchanged output.

## Phase 3 - Direct function declarations/calls (first non-main lowering)
Status (2026-03-02): Completed for direct first-order expression calls in the current native subset.

AST subset lowered natively:
- Top-level declarations with signatures in subset:
  - `Int -> Int`, `Int -> Unit`, `Unit -> Int`, `Unit -> Unit`.
- `Expr::Call` where callee is named declaration and arg is supported scalar expr.
- `Pipeline` to named function with supported value type.

Interpreter coexistence:
- Fallback for higher-order args, lambda bodies, list/map, `Using`.

Add:
- Function index table (decl name -> wasm function index).
- Per-declaration lowering pass before lowering `main`.
- Call lowering with correct param/result Wasm signatures.

Delete/replace:
- Stop interpreter-evaluating direct scalar function calls in supported subset.

Definition of done:
- `cargo run -p goby-cli -- run` on a new scalar function-call fixture (or reduced subset of `examples/function.gb`) matches baseline.
- `cargo test` includes regression covering both `f x` and `f(x)` lowering.

Gate checks before Phase 4:
- Behavior gate: direct scalar decl calls match baseline.
- Path gate: both `f x` and `f(x)` are natively lowered.
- Fallback gate: HOF/lambda call sites still fallback.

## Phase 4 - List Int and pipeline print path
Status (2026-03-02): Completed for `List Int` literal print and pipeline print in the current native subset.

AST subset lowered natively:
- `Expr::ListLit` for `List Int`.
- `print` of `List Int` in current textual format (`[1, 2, 3]`).
- `Pipeline` where left is `List Int` and right is `print` or direct named callee.

Interpreter coexistence:
- Fallback for `map` with lambda/HOF and `List String` operations.

Add:
- List allocation helper (bump allocator).
- List-to-string print helper for exact current formatting.

Delete/replace:
- Remove list printing cases from interpreter fast path when native subset covers them.

Definition of done:
- Native path handles `print [1, 2, 3]` and list local binding + print.
- existing `resolve_runtime_output_for_pipeline_print` semantics preserved.

Gate checks before Phase 5:
- Behavior gate: list print formatting exactly matches current (`[a, b, c]` spacing included).
- Path gate: `List Int` print and pipeline print are native.
- Fallback gate: `List String` flows still fallback.

## Phase 5 - Control flow and case lowering
Status (2026-03-02): Completed for current `Expr::If`/`Expr::Case` subset.

AST subset lowered natively:
- `Expr::Case` for patterns: `IntLit`, `StringLit`, `BoolLit`, `Wildcard`.
- `Expr::If` complete (if not fully covered in Phase 2).
- `==` over `Int` and `Bool` as currently supported.

Interpreter coexistence:
- Fallback for unsupported pattern/value combinations and effectful branches.

Add:
- Case lowering strategy:
  - evaluate scrutinee once,
  - emit chained branch blocks,
  - enforce wildcard as final fallback if present.

Delete/replace:
- Stop interpreter fallback for `examples/control_flow.gb` once full output matches.

Definition of done:
- `cargo run -p goby-cli -- run examples/control_flow.gb` uses native path and matches exact output:
  - `Five!`
  - `50`
  - `30`

Gate checks before Phase 6:
- Behavior gate: full `examples/control_flow.gb` output exact match.
- Path gate: `case` and `if` AST forms are native.
- Fallback gate: effectful/control-flow combinations outside supported subset fallback safely.

Current gate result (2026-03-02): Passed in test suite (`compile_module_uses_native_emitter_for_control_flow_example`).

## Phase 6 - `examples/function.gb` native coverage (except HOF/lambda)
Status (2026-03-02, session 42): Completed.

AST subset lowered natively:
- all direct first-order parts of `examples/function.gb`.
- explicit fallback marker when lambda/HOF appears (`Expr::Lambda`, passing function values).
- include two-argument direct-call shape (`f a b`, parsed as nested `Expr::Call`) with
  capability-check + lowering consistency.

Interpreter coexistence:
- Keep fallback for `map ns (|n| -> ...)`, `_ * 10`, and function-typed params.

Add:
- Mixed-mode compile decision:
  - if any declaration required by `main` contains HOF/lambda forms, fallback entire module for determinism.

Delete/replace:
- Remove legacy unsupported-form analyzer that is only string-heuristic-based once AST capability checks are complete.

Definition of done:
- no behavior regression on `cargo run -p goby-cli -- run examples/function.gb` (may still fallback due to HOF).
- capability checker provides explicit reason codes (for diagnostics/tests).

Gate checks before Phase 7:
- Behavior gate: first-order subset of `function.gb` validated against baseline fixtures.
- Path gate: explicit reason-coded fallback for lambda/HOF is test-covered.
- Fallback gate: full `examples/function.gb` remains stable when fallback triggers.

Current notes:
- 2026-03-02 (session 32):
- Re-entry rule was executed end-to-end:
  1) AST shape lock test added for spaced multi-arg calls (`f a b c` nested `Expr::Call`),
  2) `fallback` + `lower` were extended together using the same call-flattening strategy,
  3) reason-coded fallback tests remained green.
- Native direct-call support now handles arbitrary arity call chains (`f a b ...`) as long as:
  - callee resolves to a direct named declaration,
  - declaration parameter count matches flattened call arity,
  - declaration body stays within the current native-supported subset.
- `examples/function.gb` still intentionally falls back due to lambda/HOF usage.
- 2026-03-02 (session 42):
  - Added explicit Phase-6 boundary tests:
    - first-order subset derived from `function.gb` is confirmed native,
    - unused HOF declarations do not block native path,
    - transitively required HOF declarations force deterministic fallback with reason
      `call_target_body_not_native_supported`.
  - Phase-6 gates are now covered by focused tests plus full workspace quality gates.

### Phase 6.1 Cleanup Slice
Status (2026-03-02, session 42): Completed.

Objective:
- close remaining Phase 6 cleanup work and create a clean handoff into Phase 7.

Scope (in order):
1. Lock deterministic fallback reasons for `examples/function.gb`:
   - ensure `native_unsupported_reason` reports lambda/HOF-related reason codes before
     generic call-shape reasons when both are present.
   - add table-driven tests for representative unsupported call patterns.
2. Remove legacy string-heuristic unsupported analyzer from `lib.rs`:
   - retire `find_unsupported_form` / `UnsupportedFormAnalyzer` once AST-gated fallback is
     the sole unsupported-form decision path.
   - preserve existing user-visible error messages for unsupported print/type cases.
3. Add native-vs-fallback path coverage matrix tests for `examples/*.gb`:
   - expected native: `hello.gb`, `control_flow.gb`.
   - expected fallback: `effect.gb`, full `function.gb` (current phase boundary).
4. Tighten lower/fallback shared surface:
   - keep call-shape checks (`flatten_named_call`, arity check, direct named callee)
     in sync between `fallback.rs` and `lower.rs` through focused helper tests.

Definition of done (Phase 6.1):
- `cargo check`
- `cargo test -p goby-wasm`
- `cargo test` (workspace)
- `cargo clippy -- -D warnings`
- no output change for:
  - `cargo run -p goby-cli -- run examples/function.gb`
  - `cargo run -p goby-cli -- run examples/effect.gb`

Phase 6.1 exit criteria:
- no legacy string-only unsupported analyzer remains in `lib.rs`.
- reason-coded fallback tests cover lambda/HOF + call-shape combinations.
- path coverage matrix exists and is committed.

## Phase 7 - Phase A completion and interpreter retirement boundary
Status (2026-03-02): Completed.

AST subset lowered natively (Phase A target):
- `hello.gb`, `control_flow.gb`, major non-HOF subset of function/basic usage.
- `Stmt::Using` explicitly out of native scope in Phase A.

Interpreter coexistence:
- Retain fallback only for:
  - `Stmt::Using` / effect handlers,
  - lambda/HOF closures,
  - deferred runtime types (`List String`, records in runtime operations, advanced stdlib calls).

Add:
- Metrics/logging hook in tests to assert native-vs-fallback path per example.
- clear documentation of unsupported constructs and fallback reasons.

Delete/replace:
- Delete `minimal_main_module()` once native codegen always emits real instruction bodies.
- Remove `wat` dependency if no longer used.
- Keep `resolve_main_runtime_output` temporarily as isolated compatibility layer.

Definition of done:
- `cargo check && cargo test && cargo clippy -- -D warnings` green.
- `goby-cli run` outputs unchanged for all current `examples/*.gb`.
- Fallback scope is narrow, explicit, and tested.

Gate checks for Phase A sign-off:
- All phase gates from 0-7 recorded as passed.
- Native/fallback path coverage matrix exists for `examples/*.gb`.
- Remaining fallback reasons are documented and intentional.

Current gate result (2026-03-02): Passed.
- `cargo check && cargo test && cargo clippy -- -D warnings` green (215 tests).
- `wat` dependency removed; `minimal_main_module()` deleted.
- `compile_print_module` migrated to `wasm-encoder` (section ordering bug fixed).
- Unsupported-construct fallback boundary documented in `fallback.rs` module doc.
- Native/fallback path coverage matrix confirmed in `native_fallback_path_matrix_for_examples`.

## Phase 8 - WASI-standard entrypoint (`_start`) and runtime portability — COMPLETE

### Motivation

Previously `goby-cli run` invoked wasmtime with `--invoke main`, which was a
non-standard workaround. WASI Preview 1 mandates `_start` as the module
entrypoint. Without it:
- `wasmtime run *.wasm` (no flags) produces no output.
- `wasmer run *.wasm` exits with error (exit 45, no output).
- Any other WASI-compatible runtime (WasmEdge, wazero, browser WASI shims)
  will silently do nothing.

### Scope

- Both code paths that emit Wasm bytes must export `_start`:
  - `backend::WasmProgramBuilder::emit_static_print_module` (native + fallback path).
  - Any future multi-function native module builder.
- `goby-cli` must drop the `--invoke main` workaround and call wasmtime without it.
- `main` export can be kept as an alias or removed (see decision below).

### Decision: `_start` only vs. dual export

Export `_start` as the sole WASI entrypoint and remove the `main` export.
Rationale: `main` is not a WASI concept; keeping it alongside `_start` adds no
runtime value and misleads tooling. Tests that currently use `--invoke main` are
updated to use the standard invocation path instead.

### Changes

1. **`crates/goby-wasm/src/backend.rs`**
   - In `emit_static_print_module`: replace `exports.export("main", ...)` with
     `exports.export("_start", ...)`.
   - `memory` export stays (required by WASI).

2. **`crates/goby-cli/src/main.rs`**
   - In `execute_wasm`: remove `.arg("--invoke").arg("main")`.
   - Add wasmer as a secondary runtime probe (optional): try `wasmtime` first,
     then `wasmer` if wasmtime is not found.

3. **Tests**
   - Update integration tests that assert wasmtime output or invocation shape.
   - Add a test that validates the exported function name is `_start`
     (inspect the wasm binary export section).

### Definition of done

- `wasmtime run examples/hello.wasm` (no flags) prints `Hello Goby!`.
- `wasmer run examples/hello.wasm` (no flags) prints `Hello Goby!`.
- `cargo run -p goby-cli -- run examples/hello.gb` output unchanged.
- All other `examples/*.gb` outputs unchanged.
- `cargo check && cargo test && cargo clippy -- -D warnings` green.

### Gate checks before Phase 9

- Behavior gate: `wasmtime run` and `wasmer run` (no extra flags) both produce
  correct output for `hello.gb` and `control_flow.gb`.
- Path gate: wasm binary export section contains `_start`, not `main`.
- Regression gate: `goby-cli run` output unchanged for all `examples/*.gb`.
- CLI gate: `--invoke main` no longer appears in `execute_wasm`.

Current gate result (2026-03-02): Passed.
- `_start` export asserted by wasm export-section test.
- `execute_wasm` no longer injects `--invoke main`.
- Workspace checks stayed green (`cargo check`, `cargo test`, `cargo clippy -- -D warnings`).

## 6. Concrete File-Level Work Items

Primary files to add/modify:
- `crates/goby-wasm/Cargo.toml`
  - ~~add `wasm-encoder`.~~ (done Phase 0)
  - ~~remove `wat` after Phase 7 cleanup.~~ (done Phase 7)
- `crates/goby-wasm/src/lib.rs`
  - keep public API.
  - ~~replace direct `compile_print_module` path with `native_codegen_or_fallback` dispatcher.~~ (done Phase 0)
  - ~~progressively delete string-based unsupported-form heuristics.~~ (done Phase 6.1)
- new files (all created):
  - ~~`crates/goby-wasm/src/backend.rs`~~ (done Phase 0)
  - ~~`crates/goby-wasm/src/layout.rs`~~ (done Phase 0)
  - ~~`crates/goby-wasm/src/lower.rs`~~ (done Phase 0)
  - ~~`crates/goby-wasm/src/fallback.rs`~~ (done Phase 0)
  - `crates/goby-wasm/src/call.rs` (added Phase 6.1)
  - `crates/goby-wasm/src/support.rs` (added Phase 6.1)
- ~~Phase 8 files to modify:~~ (done Phase 8)
  - ~~`crates/goby-wasm/src/backend.rs` — export `_start` instead of `main`.~~
  - ~~`crates/goby-cli/src/main.rs` — drop `--invoke main` from `execute_wasm`.~~
- tests:
  - ~~expand `crates/goby-wasm/src/lib.rs` tests or split into `crates/goby-wasm/tests/` integration tests.~~
    (done: added integration smoke/export tests in `crates/goby-wasm/tests/wasm_exports_and_smoke.rs`)

## 7. Risks and Mitigations

### 7.1 String memory management (static vs dynamic)
Risk:
- String literals are easy (static data), but `int -> string`, concat, and list print require dynamic allocation.

Mitigation:
- Phase A uses monotonic bump allocator only (no free), scoped to one process run.
- Keep representation as `(ptr,len)` to avoid null-termination dependence.
- Add bounds checks for allocation overflow and return `CodegenError` on impossible size.

### 7.2 Lambda / higher-order lowering complexity
Risk:
- Closures require environment capture + callable representation (code ptr + env ptr), which is a major jump from direct calls.

Mitigation:
- Keep HOF/lambda behind fallback through Phase A.
- For Phase B, choose one strategy explicitly:
  - closure conversion + env structs in linear memory, or
  - defunctionalization for limited patterns (e.g. `map` known lambdas).
- Do not partially support ambiguous closure cases.

### 7.3 `Stmt::Using` fallback limitation in Phase A
Risk:
- Mixed native/fallback semantics can drift if supported subset expands around effectful code.

Mitigation:
- Capability checker must reject whole-module native mode when `Stmt::Using` appears (Phase A).
- Add regression tests for `examples/effect.gb` to guarantee fallback path remains behavior-identical.
- Keep native and fallback path selection deterministic and test-visible.

## 9. Post-Plan Handoff (Out of Scope for This Closed Plan)

The following are intentionally moved to follow-up planning and are not blockers for
marking this document complete (Phase 0-8 scope is already done).

Interpreter (`resolve_main_runtime_output`) can be removed only when all are true:
1. `examples/*.gb` run via native path with identical output.
2. Effect runtime (`Stmt::Using` + handler dispatch) has native lowering or a replacement runtime.
3. Lambda/HOF lowering is implemented for required language surface.
4. No tests assert fallback path for core examples.

Until those follow-up items are implemented, keep fallback as compatibility layer
with explicit, narrow boundaries.
