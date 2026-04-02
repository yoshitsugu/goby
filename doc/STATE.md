# Goby Project State Snapshot

Last updated: 2026-04-02

## Current Focus

**Track CC / CC5 — next**: CC4 core slices and post-review refactoring are complete. Next milestone is CC5 (diagnostics and regression safety).

## Recently Completed

- **Track CC / CC4 post-review refactoring** (2026-04-02): Three review findings addressed.
  - `fix`: zero-param aux decls now receive a synthetic `_unit` Wasm parameter, fixing the operand-stack leak when `helper()` (no explicit params) was called from main. `runtime_read_captured_lambda.gb` now executes correctly (`"hello world\n"`). `outer_mutation_after_closure_creation` test enabled (was `#[ignore]`).
  - `refactor(emit)`: heap-cursor sync protocol encapsulated in three helpers:
    - `emit_heap_aware_direct_call` — sync-call-sync for `DeclCall`.
    - `emit_callable_dispatch` — TAG_CLOSURE dispatch + sync for all indirect-call sites (`IndirectCall` arity-1/2, `emit_list_each`, `emit_list_map`); TAG_CLOSURE branching expressed once instead of four times inline.
    - `emit_function_body` — prologue/epilogue lifecycle shared between main and aux-decl emit loops.
  - `refactor(closure)`: `BindingRepr { Local, HeapCell }` and `binding_repr_for_let_mut` added to `closure_capture.rs`; `lower.rs` now asks "how should this binding be represented?" rather than calling `has_mutable_write_capture_of` directly.
- **Track CC / CC4 helper-decl shared-cell completion** (2026-04-02): helper-local multiline lambda bindings now preserve `parsed_body`, so helper decls such as `pair _ =` lower through resolved IR instead of silently degrading to an empty body.
  - `fix(parser)`: `parse_body_stmts` now accepts multiline lambda bindings in declaration bodies (`name = fn x ->` followed by an indented block).
  - `fix(parser_expr)`: tuple-member calls such as `p.0()` now parse as call expressions, preserving `parsed_body` for the Wasm routing path.
  - `fix(wasm-lower)`: generic indirect-call lowering now accepts tuple-projected callees, so tuple-returned closures can execute through the shared callable dispatch path.
  - Acceptance test `two_closures_sharing_mutable_cell_execute_correctly_on_wasm_path` now passes without `#[ignore]`.
- **Track CC / CC4 SharedMutableCell** (2026-04-02): Mutable-write capture via `each` executes correctly on Wasm path.
  - Root cause: bump allocator was independently re-initialized to `STATIC_STRING_LIMIT` in each Wasm function, causing callee allocations to overwrite caller heap data.
  - Fix: persistent global heap cursor at `GLOBAL_HEAP_CURSOR_OFFSET = 12` in linear memory; main writes initial value, aux decls load it; every `DeclCall`, `IndirectCall`, `IndirectCallClosure`, `list.each`, and `list.map` syncs cursor to/from global around `call_indirect`; aux decl epilogue writes final cursor back to global on return.
  - `needs_helper_state` now includes `DeclCall` and `IndirectCall` as triggers.
  - Acceptance test `mutable_write_capture_via_each_executes_correctly_on_wasm_path` passes.
- **Track CC / CC4** (2026-04-01): Main immutable/by-value callback parity landed on the Wasm path.
  - `emit.rs` now handles TAG_FUNC vs TAG_CLOSURE dispatch for generic indirect calls and `list.each` / `list.map`
  - nested/helper alloc-cursor resets no longer clobber earlier heap allocations during recursive emission
  - `lower_lambda()` flattens curried multi-param lambdas into one aux decl, enabling inline `fold` callbacks
  - focused tests cover capturing `each`, capturing `map`, inline capturing `fold`, and compile-time smoke coverage
- **Track CC / CC3** (2026-04-01): Immutable/by-value lowering and call-dispatch slice complete.
  - `lower_lambda()` handles zero-capture (PushFuncHandle) and ByValue-capture (CreateClosure) paths
  - Preamble locals pattern: `DeclareLocal + LoadClosureSlot + StoreLocal` per captured slot
  - `LambdaAuxDecl.param_names` extended to `["__clo", param]` for capturing lambdas
  - `HS_CLOSURE_BASE_PTR=13` protects closure ptr from HS_AUX_PTR clobber during slot emit
  - `AliasValue::CapturingClosure` tracks let-bound closures for dispatch routing
  - `IndirectCallClosure` backend IR + emit (Wasm call_indirect arity-2 with correct stack order)
  - CC3 acceptance tests: inline ByValue capture and string interpolation capture both execute correctly
  - mutable-write captures remain part of the follow-up shared-cell lowering work
- **Track CC / CC2** (2026-03-31): All CC2 milestones complete.
  - `TAG_CLOSURE = 0x8`, `TAG_CELL = 0x9` in `value.rs`; encode/decode helpers and orthogonality tests.
  - `CallableEnv::slot_index_of` and `MutableStorageId::new` in `goby-core`.
  - 5 new `WasmBackendInstr` variants with emit support in `backend_ir.rs` and `emit.rs`.
  - `ClosureEnvHelper` in `gen_lower/closure_env.rs` with `slot_load_instrs` and `cell_store_instrs`.
  - Fixed `upsert_capture` to keep `slot_kind` consistent when `capture_kind` is upgraded.
  - Fixed `has_heap_pattern` to recurse into CC2 instruction sub-vecs.
- **Track CC / CC1** (2026-03-31): All CC1 milestones complete.
  - Shared closure-capture analysis in `crates/goby-core/src/closure_capture.rs`
  - `CallableEnv` / `CallableEnvSlot` / `MutableStorageId` ownership model
  - Wasm backend rejects capturing lambdas via shared metadata (not backend-local traversal)
  - Acceptance-program rejection tests for all five Section 3 shapes
- **`fn`-only anonymous functions** (2026-03-31): Pipe-lambda `|x| ->` removed. `fn x -> expr` is the only form. Parser rejects old syntax; formatter, tooling, docs, and all tests updated.
- **Track H / HOF milestone series** (complete through HOF-M7): `fn` keyword, multi-param lambdas, effectful callbacks, `fold`, end-to-end acceptance gate.
- **Track E** (HOF type checking, 2026-03-27): Callback arity mismatches rejected at `goby-cli check`.
- **Track F** (stdlib `int.to_string`, 2026-03-25): End-to-end; direct calls and named callback use covered.
- **Track ER** (compiler error reporting, 2026-03-29): Unresolved/ambiguous names, import diagnostics, CLI/LSP parity.

## Immediate Next Steps

Next track candidates are in `doc/PLAN.md` §4:

- **Track CC / CC5**: Diagnostics and regression safety — precise errors for deferred cases, spec-conformance test set for all Section 3 acceptance programs.
- **Track D follow-ups** (§4.1): D5 (`goby lint` unused-binding rule), D6c shared grammar asset.
- **Track WB-3B** (deferred): WasmFX typed continuations — on hold until WebAssembly stack switching reaches Phase 4.
- **Float support** (§4.7): `Float` type backed by Wasm `f64`; semantics to be locked before coding.

## Architecture State

- Resolved-form → shared IR boundary is stable (IR0–IR11 done).
- Wasm backend lowering design is locked in `doc/PLAN_IR.md`:
  - Phase WB-1: pure control flow and operators ✓
  - Phase WB-2: pattern matching and structured data ✓
  - Phase WB-3: function values and effect handlers (direct-call lowering, one-shot tail-resumptive) ✓
  - Phase WB-3B (future): WasmFX stack switching when proposal reaches Phase 4
- `GeneralLowered` coverage includes:
  - Pure control flow: `If`, `BinOp`, `Interp`, `LetMut`, `Assign`
  - Pattern matching: `Case` with literal/list patterns
  - Structured data: `ListLit`, `TupleLit`, `RecordLit`
  - Decl calls / recursion / higher-order funcref calls
  - Backend effect dispatch (typed `BackendEffectOp` / `BackendPrintOp`)
  - stdlib `list.each` / `list.map` / `list.fold`
  - Effect handlers: `Handle` / `WithHandler` / tail `Resume` (one-shot tail-resumptive subset)
  - Function values: `Lambda` (zero-capture via `PushFuncHandle`; ByValue-capture via `CreateClosure`); direct calls and main callback dispatch now branch correctly for closures
  - Host intrinsics: `StringGraphemesList` (`__goby_string_graphemes_list`)
- Known limitations:
  - non-tail / multi-resume handlers → `BackendLimitation` error
  - interpreter path: capturing lambdas work but use snapshot semantics for `mut` captures (not the spec's shared-cell model); will be corrected as part of Track CC

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `doc/PLAN_CLOSURE_CAPTURE.md` — closure capture design and milestones
- `crates/goby-core/src/closure_capture.rs` — shared closure-capture analysis and callable-environment metadata
- `crates/goby-wasm/src/gen_lower/closure_env.rs` — `ClosureEnvHelper`: closure-environment load/store helper layer (CC2)
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
