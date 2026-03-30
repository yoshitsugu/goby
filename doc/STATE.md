# Goby Project State Snapshot

Last updated: 2026-03-31

## Current Focus

No active track. See Immediate Next Steps for candidates.

Earlier completed work (for reference):

- IR0–IR11, WB-1/WB-2/WB-3 complete; `doc/PLAN_IR.md` contains the Wasm backend lowering design.
- WB-3B on hold: externally blocked; WebAssembly stack switching proposal has not reached Phase 4.
- Track H complete (2026-03-31): HOF callback reliability + `fn a b -> expr` multi-param lambda surface; all milestones HOF-M1 through M7 done.
- Track E complete (2026-03-27): HOF callback typechecking; shared matcher in `typecheck_unify.rs`.
- Track ER complete (2026-03-29): error reporting with CLI/LSP parity; see `doc/PLAN_ERROR.md`.
- Track fold complete (2026-03-28): `fold : List a -> b -> (b -> a -> b) -> b` in `stdlib/goby/list.gb`.
- Track F complete (2026-03-25): `goby/int.to_string`.
- Track operators complete (2026-03-26): `||`, unary `!`, comparison precedence locked.
- Track C4/S1–S4 complete (2026-03-25): stdlib `split` ownership fully stdlib-driven.

## Track Priority

**No active stdlib split-retirement work remains.**
C4 plus S1/S2/S3 are complete: source-level `split` ownership is fully stdlib-driven, and the
fallback/runtime-output path no longer carries a source-level legacy `string.split` branch.

## Immediate Next Steps

Next track candidates are in `doc/PLAN.md` §4. Deferred items:

- **Track D follow-ups** (§4.1): D5 (`goby lint` unused-binding rule), D6c shared grammar asset.
- **Track WB-3B** (deferred): WasmFX typed continuations — on hold until WebAssembly stack switching reaches Phase 4.
- **Float support** (§4.6): `Float` type backed by Wasm `f64`; semantics to be locked before coding.

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
  - this remains an execution limitation, not a runtime-I/O-shape limitation; next work is to
    surface that distinction explicitly in diagnostics and internal lowering results

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
