# Goby Project State Snapshot

Last updated: 2026-04-01

## Current Focus

**Track CC / CC4** (next): Higher-order stdlib parity — make `each`, `map`, `fold`
accept capturing closures.

CC3 (lowering and call dispatch) is complete:
- ByValue-capturing lambdas lower via `CreateClosure` + preamble locals
- `IndirectCallClosure` dispatches direct closure calls with correct arity-2 signature
- `instrs_load_local` recurses into `CreateClosure.slot_instrs` for alias detection
- Effect-op GlobalRefs rejected as values in `lower_value`
- Acceptance tests pass: `base=10; add=fn x→base+x; add 5` → 15

## Recently Completed

- **Track CC / CC3** (2026-04-01): All CC3 milestones complete.
  - `lower_lambda()` handles zero-capture (PushFuncHandle) and ByValue-capture (CreateClosure) paths
  - Preamble locals pattern: `DeclareLocal + LoadClosureSlot + StoreLocal` per captured slot
  - `LambdaAuxDecl.param_names` extended to `["__clo", param]` for capturing lambdas
  - `HS_CLOSURE_BASE_PTR=13` protects closure ptr from HS_AUX_PTR clobber during slot emit
  - `AliasValue::CapturingClosure` tracks let-bound closures for dispatch routing
  - `IndirectCallClosure` backend IR + emit (Wasm call_indirect arity-2 with correct stack order)
  - CC3 acceptance tests: inline ByValue capture and string interpolation capture both execute correctly
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

- **Track CC: Closure Capture** (§4.6): CC3 complete. Next: CC4 — make `each`, `map`, `fold` accept capturing closures; mutable-write captures (SharedMutableCell) lowering.
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
  - Function values: `Lambda` (zero-capture via `PushFuncHandle`; ByValue-capture via `CreateClosure` + `IndirectCallClosure`); stdlib `graphemes` via wrapper AuxDecl
  - Host intrinsics: `StringGraphemesList` (`__goby_string_graphemes_list`)
- Known limitations:
  - non-tail / multi-resume handlers → `BackendLimitation` error
  - capturing lambda passed to HOF callbacks via `list.each/map/fold` — Track CC4 will implement
  - mutable-write captures (SharedMutableCell) remain `UnsupportedForm` — Track CC4
  - interpreter path: capturing lambdas work but use snapshot semantics for `mut` captures (not the spec's shared-cell model); will be corrected as part of Track CC

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `doc/PLAN_CLOSURE_CAPTURE.md` — closure capture design and milestones
- `crates/goby-core/src/closure_capture.rs` — shared closure-capture analysis and callable-environment metadata
- `crates/goby-wasm/src/gen_lower/closure_env.rs` — `ClosureEnvHelper`: closure-environment load/store helper layer (CC2)
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
