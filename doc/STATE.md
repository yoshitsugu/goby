# Goby Project State Snapshot

Last updated: 2026-04-03

## Current Focus

**Inline multi-parameter lambda track — next active development**: proceed with `doc/PLAN_INLINE_MULTI_PARAM_LAMBDA.md` as the next implementation track, using the completed closure-capture/callable groundwork as the base.

- Immediate goal:
  - make `fn a b -> expr` work as an ordinary first-class callable across both Wasm `GeneralLowered` and fallback/runtime-stdin paths, not only in specific stdlib helpers.
- First implementation boundary:
  - lock current failure shapes with acceptance tests from `doc/PLAN_INLINE_MULTI_PARAM_LAMBDA.md` §4,
  - then tighten one shared callable/invocation boundary instead of adding `fold`/`map`-specific workarounds.
- Current constraint:
  - closure capture itself is complete; the remaining gap is shared callable shape/invocation parity for inline multi-parameter lambdas across execution paths.
  - acceptance-lock findings from the first slice:
    - runtime-stdin execution already handles pure `fold` callbacks and let-bound user-defined higher-order reuse for inline multi-parameter lambdas.
    - portable fallback runtime-output still does not execute inline multi-parameter `fold` callbacks end-to-end.
    - effectful inline multi-parameter `fold` callbacks still fail on the Wasm/runtime-stdin execution path.
    - the future-facing `pairwise_apply` acceptance case is currently blocked earlier by general-lowering list-spread support, so it is not yet a clean callable-boundary regression.

## Recently Completed

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

Next implementation track is `doc/PLAN_INLINE_MULTI_PARAM_LAMBDA.md`.

- **IMP0 acceptance lock**: add or enable the acceptance programs from §4 so the current gaps are pinned at the true ownership boundary.
- **Shared callable audit**: inspect current callable metadata / lambda-lifting / invocation-planning flow and identify where inline multi-parameter lambdas still depend on path-specific handling.
- **Next executable fix target**: start with the effectful inline multi-parameter `fold` callback failure on the Wasm/runtime-stdin path, because the pure and let-bound non-effectful shapes are already passing.
- **Doc sync during implementation**:
  - keep `doc/PLAN.md` aligned with this as the next active track,
  - update `doc/LANGUAGE_SPEC.md` only when user-visible semantics/status wording needs to change,
  - keep `doc/STATE.md` explicit about which execution paths are complete vs still limited while the track is in progress.

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
