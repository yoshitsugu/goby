# Goby Project State Snapshot

Last updated: 2026-04-03

## Current Focus

**Runtime memory plan (`doc/PLAN_MEMORY.md`)**: M0/M1 landed; M2 heap-only growth landed; M3 explicit exhaustion diagnostics landed; M4 proof set is now covered by host-only, heap-only, and mixed-pressure success cases.

- Locked direction:
  - Goby should move from fixed-page-aware memory assumptions to a bounded grow-aware memory model.
  - initial memory stays modest, runtime growth is allowed, and growth must stop at a configured maximum rather than becoming effectively unbounded.
  - host temporary allocation and Wasm heap allocation must follow one explicit shared address-space rule.
  - explicit exhaustion errors are required; generic Wasm traps and pointer corruption are not acceptable memory-failure outcomes.
- Execution target:
  - M0/M1 are now complete for the host-write side: shared defaults are `256 KiB` initial memory, `64 MiB` maximum memory, and a shared host-bump/stack configuration boundary in `crates/goby-wasm/src/memory_config.rs`.
  - the first M2 slice is now complete for heap-only pressure shapes: `emit_alloc_from_top` can grow memory before heap exhaustion, and a compiled heap-allocation regression now crosses the initial page allocation without host temporary pressure.
  - M3 is now complete: bounded host-temporary exhaustion and bounded compiled-heap exhaustion both surface `runtime error: memory exhausted [E-MEMORY-EXHAUSTION]` instead of generic Wasm traps.
  - M4 proof is now complete for the current scope: default-policy regressions cover host-temp-heavy pressure, heap-heavy pressure, and one mixed execution that allocates a large heap value and then formats it through the host boundary in the same `_start`.
  - the next implementation slice should either measure representative workloads for the M5 reclamation decision or explicitly move to another feature track, rather than reopening allocator basics.
  - final closure for this track requires representative memory-pressure regressions that succeed under the default bounded-growth policy and explicit low-maximum regressions that fail with the intended exhaustion error.

## Recently Completed

- **Runtime memory M2 heap-growth slice (heap-only pressure)** (2026-04-03): compiled-Wasm heap allocation on the shared emitter path can now grow linear memory past the initial page allocation for heap-only pressure shapes.
  - `emit(heap)`: `emit_alloc_from_top` now attempts `memory.grow` when the tentative heap allocation would cross the current heap floor, rebases the local/global heap cursor by the newly added pages, and preserves the existing shared allocation entrypoint for lists, tuples, records, closures, cells, and helper-produced lists.
  - `test(heap-growth)`: added a focused regression that compiles and runs a program with a 30,000-element list literal, proving heap allocation can exceed the initial `256 KiB` allocation without relying on host temporary allocation pressure.
  - `status`: this closes the first M2 slice only for heap-only pressure shapes; explicit exhaustion diagnostics and host-temp-plus-heap coexistence still need a follow-up slice.

- **Runtime memory M3 explicit exhaustion diagnostics** (2026-04-03): bounded memory-growth failure now surfaces one stable Goby runtime error on both the host-temporary path and the compiled heap path.
  - `emit(exhaustion)`: `emit_alloc_from_top` now checks available bytes before subtracting from the top-down cursor, fixing the prior underflow path that could skip growth and trap with out-of-bounds memory access under low maximums; failed heap growth sets the shared runtime-error slot and returns through the current function/call boundary instead of falling into a raw Wasm trap.
  - `runtime(error-surface)`: Goby-owned Wasm execution now reads the shared runtime-error slot after `_start`, so bounded memory failures normalize to `runtime error: memory exhausted [E-MEMORY-EXHAUSTION]: allocation exceeded the configured Wasm memory limit`.
  - `test(low-max)`: added/locked separate low-maximum regressions for host-backed `string_concat` growth and compiled heap growth, proving both paths report the same exhaustion error.

- **Runtime memory M4 mixed-pressure coexistence proof** (2026-04-03): the default bounded-growth policy is now exercised by host-only, heap-only, and mixed host-plus-heap success regressions.
  - `test(mixed-pressure)`: added a compiled-Wasm regression that allocates a 30,000-element list on the Goby heap and then formats that same list through host-backed interpolation in the same execution, proving the host bump and heap-growth paths can coexist without reintroducing fixed carve-out assumptions.
  - `status`: the current memory-growth track now has focused success/failure coverage for the intended shared policy; the next question is whether measured workloads justify any reclamation follow-up.

- **Runtime memory M0/M1 shared configuration + host growth helpers** (2026-04-03): Goby-owned Wasm execution and Wasm emission now share one bounded-memory configuration, and host-backed string/list writes can grow linear memory past the initial page allocation.
  - `memory(config)`: added `crates/goby-wasm/src/memory_config.rs` as the shared source of truth for initial pages, maximum pages, host bump reservation, and Wasmtime stack defaults; `backend.rs`, `gen_lower/emit.rs`, and `host_runtime.rs` now read from it instead of duplicating local constants.
  - `runtime(host-write)`: `wasm_exec.rs` now routes host-backed writes through grow-aware helpers that inspect current linear-memory size, call `memory.grow` when required, and refuse requests that exceed the configured maximum.
  - `test(memory-pressure)`: added a focused Wasm execution regression that drives `__goby_string_concat` past the initial memory allocation and proves the host path still executes successfully under bounded growth.

- **Inline multi-parameter lambda future-helper proof via list-spread lowering** (2026-04-03): general-lowering now supports list-spread execution on the shared Wasm path, and the `pairwise_apply` acceptance case executes with an inline multi-parameter lambda without helper-specific compiler/runtime branches.
  - `gen_lower(lower/emit)`: `ValueExpr::ListLit { spread: Some(..) }` now lowers as prefix-list construction plus a generic `ListConcat` backend intrinsic, and Wasm emission includes the in-Wasm helper that concatenates two tagged lists.
  - `test(wasm/runtime-output)`: enabled the compiled `pairwise_apply` acceptance test and added a portable fallback runtime-output regression for the same higher-order helper proof case.
  - `semantics`: the newly exposed fallback/Wasm divergence for no-tail list patterns was resolved by aligning both paths and docs on exact-length matching for `[x]`, `[x, y]`, and similar forms, while tail patterns remain prefix matches.

- **Inline multi-parameter lambda emit cleanup (partial)** (2026-04-03): generic indirect-call emission no longer duplicates separate arity-1 and arity-2 stack-spill branches.
  - `emit(indirect-call)`: `WasmBackendInstr::IndirectCall` now goes through one helper that spills callee/args into scratch locals, derives the plain-vs-closure type indices from arity, and reuses the shared callable dispatch path.
  - `status`: this was the first cleanup slice; the later cleanup-closure slice removed the remaining dedicated closure-only indirect-call path and closed the current-scope cleanup checklist.

- **Inline multi-parameter lambda list-spread cleanup (partial)** (2026-04-03): simple list-spread programs are now explicitly locked on the compiled Wasm path instead of only via runtime-output coverage.
  - `test(wasm)`: added compiled-Wasm execution tests for `[1, ..rest]` and `["a", ..rest]`, proving shared list-spread lowering is not only exercised through the `pairwise_apply` acceptance case.
  - `test(cleanup)`: removed stale runtime-output assertions that described simple list-spread programs as fallback-only.
  - `status`: this was the first list-spread cleanup slice; the later cleanup-closure slice closed the current-scope list-spread checklist.

- **Inline multi-parameter lambda cleanup closure + broader proof** (2026-04-03): the current-scope cleanup/proof checklist is now complete.
  - `cleanup(multi-arg)`: removed the dedicated `IndirectCallClosure` path so local capturing closures now use the same generic indirect-call dispatch path as other callable values.
  - `proof(helper)`: added a second user-defined higher-order helper proof (`apply_two`) on both Wasm/runtime-stdin and portable fallback/runtime-output paths, confirming the feature is not tied to the earlier `pairwise_apply` shape.
  - `track`: inline multi-parameter lambda support is complete for the current scope; treat future arity expansion or later helper introductions as separate follow-up work.

- **Inline multi-parameter lambda fallback callable parity** (2026-04-03): portable fallback/runtime-output now applies local and captured callable values sequentially, so nested `fn a b -> ...` lambdas execute through the shared callable boundary instead of falling out after the first argument.
  - `runtime(apply)`: `apply_named_value_call_args_out` now detects local/captured callable values and applies multi-argument calls by repeated shared callable evaluation rather than only via named decl lookup.
  - `test(runtime-output)`: enabled pure and effectful inline `fold` acceptance tests on the portable fallback path and added fallback regressions for let-bound user-defined HOF reuse and capturing inline multi-parameter lambdas.
  - `status`: runtime-stdin/Wasm and portable fallback are aligned for the tracked acceptance shapes: pure/effectful `fold`, let-bound user-defined higher-order reuse, and capturing inline multi-parameter lambdas.

- **Track CC interpreter helper-returned closure parity** (2026-04-02): fallback/interpreter runtime now supports helper-returned closure values and shared mutable-cell closure pairs.
  - `runtime(value)`: `RuntimeValue` now carries callable values, allowing declaration bodies and locals to retain closures as first-class runtime values instead of dropping them at the value boundary.
  - `runtime(eval)`: `Expr::Lambda` now evaluates to a captured callable value on the AST runtime path, and generic call evaluation now supports non-name callees that resolve to callable values (for example tuple-projected closures like `p.0 ()`).
  - `test(runtime)`: helper-returned `make_adder` and shared-cell `pair` parity tests now assert concrete outputs (`15`, `2`) on both fallback and typed modes.
  - `doc`: removed the temporary implementation-status note from `LANGUAGE_SPEC.md` and cleared the last closure-capture follow-up from `PLAN.md`.
- **Track CC / CC6 documentation and examples closure** (2026-04-02): CC6 milestone closed.
  - `doc(spec)`: `LANGUAGE_SPEC.md` now describes closure semantics directly with no temporary implementation-status note.
  - `test(runtime)`: interpreter parity tests cover helper-returned ByValue closure (`make_adder`) and shared mutable cell pair closure (`pair`).
  - `doc(plan)`: `PLAN.md` §4.6 updated to CC0–CC6 complete and the completed milestone tracker was retired.
  - Prior CC6 work: `closure_capture.gb` and `closure_mut.gb` examples, CLI run regressions, stale comment cleanup, interpreter callback shared-cell parity.
- **Track CC / CC6 interpreter callback shared-cell parity** (2026-04-02): fallback/interpreter callback closure paths now preserve `mut` shared-cell semantics instead of cloning snapshot values.
  - `runtime(storage)`: `RuntimeLocals` now models `mut` bindings as shared cells, so cloning locals for captured callbacks preserves shared mutable storage while ordinary `let` bindings still copy by value.
  - `runtime(exec)`: string-path and AST-path local binding/assignment handlers now distinguish `mut` binding creation from immutable binding creation, and assignments update existing mutable cells instead of replacing them with immutable snapshots.
  - `test(runtime)`: added focused regressions for shared mutable cells in cloned locals and for fallback/typed-mode parity of `each` mutating a captured outer binding.
  - `doc`: narrowed the remaining implementation-status note to helper-returned/local closure-value parity rather than the old generic interpreter snapshot gap.
- **Track CC / CC6 examples/docs refresh (partial)** (2026-04-02): runnable closure-capture examples were added and stale closure-design status wording was pushed further into historical-note territory.
  - `examples`: added `closure_capture.gb` (ByValue capture and captured `map` callback) and `closure_mut.gb` (two closures sharing one `mut` binding).
  - `examples/readme`: indexed the new closure examples in `examples/README.md`.
  - `test(cli)`: added run regressions for both new examples so docs/examples stay executable.
  - `doc(closure-design)`: rewrote stale "current WB-3A" / "UnsupportedIrForm" wording as historical context instead of present-tense status.
- **Track CC / CC5 spec-conformance coverage** (2026-04-02): Section 3 closure-capture acceptance programs are now locked by one Wasm execution test set.
  - `test(wasm-smoke)`: added a focused Section 3 spec-conformance test that executes all five acceptance programs on the Wasm path and checks their expected stdout.
  - `test(wasm-smoke)`: added a helper-returned ByValue closure execution regression (`make_adder` / `add10 5`) so helper-produced capturing closures are pinned end-to-end rather than only via lowering smoke.
  - `doc(plan)`: normalized Section 3.1 and 3.4 acceptance programs to bind the call result before interpolation/printing, keeping the acceptance set focused on closure semantics instead of the separate "call inside interpolation" IR limitation.
- **Track CC / CC5 interpolation diagnostic tightening** (2026-04-02): call expressions inside interpolation now fail with actionable shared-IR guidance instead of collapsing to the generic `no IR decl available for main` boundary.
  - `fix(ir-lower)`: interpolation lowering now distinguishes call expressions and reports `bind the result to a local before interpolation`.
  - `fix(gen-lower)`: when `main` IR lowering fails, `supports_general_lower_module` now preserves that concrete lowering reason instead of degrading to `NoIrDecl`.
  - `test(cli)`: `goby run` regression now locks the user-visible diagnostic for `println "${add_one 5}"`.
  - `status`: this closes the remaining CC5 diagnostics/regression-safety checklist; CC6 is the next milestone.
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

- Decide whether to start the M5 reclamation follow-up from measurements or leave the current bounded-growth design consciously deferred without GC work.
- If memory-track work continues immediately, add representative benchmark-style workloads rather than returning to allocator-constant churn or synthetic failure-only cases.
- Keep the explicit exhaustion surface stable; do not reintroduce generic Wasm traps on bounded-failure paths.
- Keep the completed inline multi-parameter lambda track closed unless a future helper or arity expansion explicitly requires a new follow-up slice.

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

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `doc/LANGUAGE_SPEC.md` — current closure semantics and language behavior
- `crates/goby-core/src/closure_capture.rs` — shared closure-capture analysis and callable-environment metadata
- `crates/goby-wasm/src/gen_lower/closure_env.rs` — `ClosureEnvHelper`: closure-environment load/store helper layer (CC2)
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
