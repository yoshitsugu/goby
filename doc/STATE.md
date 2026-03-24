# Goby Project State Snapshot

Last updated: 2026-03-25

## Current Focus

- IR0–IR11 complete. `doc/PLAN_IR.md` now contains the Wasm backend lowering design (§4–§5).
- Track E E1–E7 complete.
- Phase WB-1 complete (2026-03-24): `If`, `BinOp`, `Interp`, `LetMut`, `Assign` all lowered and emitted.
- Phase WB-2A complete (2026-03-24): top-level `DeclCall`, recursion, funcref-table indirect calls, typed backend effect identities.
- Phase WB-2B complete (2026-03-24): `Case` literal/wildcard/list patterns, `ListLit`, `TupleLit`, `RecordLit`, stdlib `list.each` / `list.map`.
- Phase WB-3 is partially complete (2026-03-25): `Handle` / `WithHandler` / tail `Resume` lowered for one-shot tail-resumptive subset; `ValueExpr::Lambda` lowered; `graphemes` end-to-end via `StringGraphemesList` host intrinsic; `InterpreterBridge` graphemes classification removed; remaining work is the composed runtime-`Read` stdlib path tracked as WB-3-M7 in `doc/PLAN_IR.md`.
  - M1: legality analysis implemented.
  - M2: safe handler lowering complete; `examples/iterator.gb` executes via `GeneralLowered`.
  - M3: `ValueExpr::Lambda` lowered; `map [1,2,3] (fn x -> x+1)` executes correctly.
  - M4: `StringGraphemesList` host intrinsic; graphemes classifies as `GeneralLowered` end-to-end.
  - M5: `graphemes-get-print` fused pattern deleted; `SplitEachPrint`/`SplitGetPrint` retained in `DynamicWasiIo` path only.
  - M6: `InterpreterBridge` graphemes classification removed; 4 dead helper functions deleted.
  - M7: `graphemes`-as-funcref wrapper AuxDecl exists, but the representative runtime-`Read`
    composed path (`read -> split -> map(graphemes) -> list.get -> each(println)`) is not yet
    closed end-to-end via `GeneralLowered`; milestone reopened in `doc/PLAN_IR.md`.
  - M8: quality gates pass (`cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings`).
- WB-3B prep slice in progress (2026-03-24): `gen_lower/emit.rs` now has an
  `EffectEmitStrategy` boundary and `wasmfx-experimental` feature flag so future WasmFX work can
  replace the emit path without redesigning IR/lowering; current strategies are parity-tested to
  emit identical bytes for supported effect ops and representative general-lowered modules
  (safe handler-only main, helper decl + read path).
- WB-3B compile-path prep extended (2026-03-24): general lowering now exposes an internal
  option-aware Wasm emission helper so strategy parity tests run through the same
  `try_general_lower_module` entrypoint used by `compile_module`.
- WB-3B remains externally blocked as of 2026-03-24:
  - WebAssembly official proposals tracker does not yet satisfy the Phase 4 restart condition.
  - Local `wasm-encoder` source exposes no stack-switching/WasmFX instruction support.
- WB-3B is on hold until all restart conditions in `doc/PLAN_IR.md` Phase WB-3B are met.
- **WB-3 requires one follow-up convergence slice.** All 13 `CompExpr` variants and all 12
  `ValueExpr` variants are lowered somewhere in the current backend, but the representative
  runtime-`Read` composed stdlib path is not yet consistently reaching `GeneralLowered`.

## Track Priority

**Next active work: WB-3-M7 convergence, then stdlib track resumes at C4-S2.**
The immediate backend task is to make the representative composed runtime-`Read` stdlib path
execute via `GeneralLowered` without adding new `RuntimeIoPlan` branches. After that,
stdlib work resumes at C4-S2.

See `doc/PLAN_STANDARD_LIBRARY.md` for the remaining C4 milestones.

## Immediate Next Steps

**Track stdlib (C4-S1) — complete (2026-03-24):**
`cargo run -p goby-cli -- check stdlib/goby/string.gb` now succeeds.
The typechecker accepts the required `List String` state initialization shape and preserves
outer `mut` locals through stdlib-style `with ... in` bodies.

**Track WB-3-M7 convergence — primary:**
Close the representative composed runtime-`Read` stdlib path:
`read -> split -> map(graphemes) -> list.get -> each(println)`.
Exit criteria:
- exact-shape classification test added,
- exact-shape runtime-output regression test added,
- equivalent alias/import variants added,
- all of them classify and execute via `GeneralLowered`,
- no new `RuntimeIoPlan` branch is added for this shape.
Required execution order:
1. add classification tests,
2. add runtime-output tests,
3. expose the failing general-lowering stage if tests fail,
4. repair `gen_lower` / emitter / aux-decl registration / gating,
5. run quality gates.
Anti-adhoc rule:
- do not solve this slice by adding a new runtime-shape-specific fallback or bridge path.

**Track stdlib (C4-S2) — next:**
Stabilize the shared iterator state contract by keeping `GraphemeState` in
`stdlib/goby/iterator.gb` as the canonical declaration and removing duplicated local copies
from `stdlib/goby/string.gb`.
Exit criterion: no duplicated `Iterator` / `GraphemeState` declarations remain across stdlib modules.
See `doc/PLAN_STANDARD_LIBRARY.md` §5.

**Track WB-3B (future, deferred):**
WasmFX typed continuations — currently on hold.
Restart only when the external prerequisites in `doc/PLAN_IR.md` Phase WB-3B are satisfied.

## Architecture State

- Resolved-form → shared IR boundary is stable (IR0–IR11 done).
- Wasm backend lowering design is locked in `doc/PLAN_IR.md`:
  - Phase WB-1: pure control flow and operators ✓
  - Phase WB-2: pattern matching and structured data ✓
  - Phase WB-3: function values and effect handlers (direct-call lowering, one-shot tail-resumptive)
    with one remaining convergence slice (WB-3-M7)
  - Phase WB-3B (future): WasmFX stack switching when proposal reaches Phase 4
- All `CompExpr` and `ValueExpr` variants have backend lowering coverage, but not every
  representative runtime-`Read` composed stdlib path has reached stable `GeneralLowered`
  execution yet.
- `GeneralLowered` coverage includes:
  - Pure control flow: `If`, `BinOp`, `Interp`, `LetMut`, `Assign`
  - Pattern matching: `Case` with literal/list patterns
  - Structured data: `ListLit`, `TupleLit`, `RecordLit`
  - Decl calls / recursion / higher-order funcref calls
  - Backend effect dispatch (typed `BackendEffectOp` / `BackendPrintOp`)
  - stdlib `list.each` / `list.map`
  - Effect handlers: `Handle` / `WithHandler` / tail `Resume` (one-shot tail-resumptive subset)
  - Function values: `Lambda` (no-capture only); stdlib `graphemes` via wrapper AuxDecl
  - Host intrinsics: `StringGraphemesList` (`__goby_string_graphemes_list`)
- Fused patterns deleted or retained as optimization only:
  - `graphemes-get-print` deleted (WB-3-M5)
  - `SplitEachPrint` / `SplitGetPrint` retained in `DynamicWasiIo` path as optimization (correctness not required)
- WB-3 exit state:
  - non-tail / multi-resume handlers produce `BackendLimitation` error (not silent miscompilation)
  - lambda with free variables (closure capture) produces `UnsupportedForm` (WB-3B deferred)
  - composed runtime-`Read` stdlib path (`split -> map(graphemes) -> list.get -> each`) remains
    the active convergence target before WB-3 is treated as fully closed

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `doc/PLAN_STANDARD_LIBRARY.md` — stdlib split/grapheme C4–C8
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
