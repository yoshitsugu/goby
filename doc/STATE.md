# Goby Project State Snapshot

Last updated: 2026-03-25

## Current Focus

- IR0–IR11 complete. `doc/PLAN_IR.md` now contains the Wasm backend lowering design (§4–§5).
- Track E E1–E7 complete.
- Phase WB-1 complete (2026-03-24): `If`, `BinOp`, `Interp`, `LetMut`, `Assign` all lowered and emitted.
- Phase WB-2A complete (2026-03-24): top-level `DeclCall`, recursion, funcref-table indirect calls, typed backend effect identities.
- Phase WB-2B complete (2026-03-24): `Case` literal/wildcard/list patterns, `ListLit`, `TupleLit`, `RecordLit`, stdlib `list.each` / `list.map`.
- Phase WB-3 complete (2026-03-25): `Handle` / `WithHandler` / tail `Resume` lowered for one-shot tail-resumptive subset; `ValueExpr::Lambda` lowered; `graphemes` end-to-end via `StringGraphemesList` host intrinsic; `InterpreterBridge` graphemes classification removed; the representative runtime-`Read` composed stdlib path is now covered by classification and execution regressions.
  - M1: legality analysis implemented.
  - M2: safe handler lowering complete; `examples/iterator.gb` executes via `GeneralLowered`.
  - M3: `ValueExpr::Lambda` lowered; `map [1,2,3] (fn x -> x+1)` executes correctly.
  - M4: `StringGraphemesList` host intrinsic; graphemes classifies as `GeneralLowered` end-to-end.
  - M5: `graphemes-get-print` fused pattern deleted; `SplitEachPrint`/`SplitGetPrint` retained in `DynamicWasiIo` path only.
  - M6: `InterpreterBridge` graphemes classification removed; 4 dead helper functions deleted.
  - M7: `graphemes`-as-funcref wrapper AuxDecl plus regression coverage for the representative
    runtime-`Read` composed path (`read -> split -> map(graphemes) -> list.get -> each(println)`)
    and required alias/import variants.
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
- **WB-3 is complete.** All 13 `CompExpr` variants and all 12 `ValueExpr` variants are handled
  in the `GeneralLowered` path within the currently supported subsets, including the representative
  runtime-`Read` composed stdlib path.

## Track Priority

**Next active work: stdlib track moves to C4-S4.**
The representative WB-3-M7 composed runtime-`Read` path is fixed by regression coverage, and
C4-S3 is now complete. The active work returns to behavior-hardening for the final stdlib-owned
`split` implementation.

See `doc/PLAN_STANDARD_LIBRARY.md` for the remaining C4 milestones.

## Immediate Next Steps

**Track stdlib (C4-S1) — complete (2026-03-24):**
`cargo run -p goby-cli -- check stdlib/goby/string.gb` now succeeds.
The typechecker accepts the required `List String` state initialization shape and preserves
outer `mut` locals through stdlib-style `with ... in` bodies.

**Track stdlib (C4-S2) — complete (2026-03-25):**
Stabilize the shared iterator state contract by keeping `GraphemeState` in
`stdlib/goby/iterator.gb` as the canonical declaration and removing duplicated local copies
from `stdlib/goby/string.gb`.
Exit criterion: no duplicated `Iterator` / `GraphemeState` declarations remain across stdlib modules.
See `doc/PLAN_STANDARD_LIBRARY.md` §5.

**Track stdlib (C4-S3) — complete (2026-03-25):**
`stdlib/goby/string.gb` now handles multi-grapheme delimiters through the stdlib-owned path, and
`split` no longer calls `string.split(value, sep)` for any delimiter case.
Focused Wasm execution coverage now locks a representative multi-delimiter case with
leading/consecutive/trailing empty segments preserved.

**Track stdlib (C4-S4) — next:**
Harden behavior coverage for the final stdlib-owned `split` path.
Exit criterion: runtime tests cover empty delimiter, single-grapheme delimiter, multi-grapheme
delimiter, consecutive/leading/trailing empties, and Unicode grapheme cases.

**Track WB-3B (future, deferred):**
WasmFX typed continuations — currently on hold.
Restart only when the external prerequisites in `doc/PLAN_IR.md` Phase WB-3B are satisfied.

## Architecture State

- Resolved-form → shared IR boundary is stable (IR0–IR11 done).
- Wasm backend lowering design is locked in `doc/PLAN_IR.md`:
  - Phase WB-1: pure control flow and operators ✓
  - Phase WB-2: pattern matching and structured data ✓
  - Phase WB-3: function values and effect handlers (direct-call lowering, one-shot tail-resumptive) ✓
  - Phase WB-3B (future): WasmFX stack switching when proposal reaches Phase 4
- All `CompExpr` and `ValueExpr` variants have backend lowering coverage, including the
  representative runtime-`Read` composed stdlib path used as the acceptance shape for WB-3-M7.
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

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `doc/PLAN_STANDARD_LIBRARY.md` — stdlib split/grapheme C4–C8
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
