# Goby Project State Snapshot

Last updated: 2026-03-23

## Current Focus

- IR0–IR11 complete. `doc/PLAN_IR.md` now contains the Wasm backend lowering design (§4–§5).
- Track E E1–E7 complete.
- Next active work: `doc/PLAN_STANDARD_LIBRARY.md` C4-S1, OR `doc/PLAN_IR.md` Phase WB-1.

## Immediate Next Steps

Two independent tracks. Either can be started next:

**Track stdlib (C4-S1):**
Unblock `List String` as a record field type in the type checker.
Exit criterion: `cargo run -p goby-cli -- check stdlib/goby/string.gb` no longer fails on the state record field type.
See `doc/PLAN_STANDARD_LIBRARY.md` §5.

**Track Wasm backend (Phase WB-1):**
Add `If`, `BinOp`, `Interp`, `LetMut`, `Assign` to `lower_comp` / `lower_value`.
Entry files: `crates/goby-wasm/src/gen_lower/lower.rs`, `crates/goby-wasm/src/gen_lower/emit.rs`.
Exit criterion: simple programs using `if` and arithmetic classify as `GeneralLowered`.
See `doc/PLAN_IR.md` §5 Phase WB-1.

## Architecture State

- Resolved-form → shared IR boundary is stable (IR0–IR11 done).
- Wasm backend lowering design is locked in `doc/PLAN_IR.md`:
  - Phase WB-1: pure control flow and operators
  - Phase WB-2: pattern matching and structured data
  - Phase WB-3: function values and effect handlers (direct-call lowering, one-shot tail-resumptive)
  - Phase WB-3B (future): WasmFX stack switching when proposal reaches Phase 4
- Effect handler strategy: selective CPS degenerating to direct-call lowering for one-shot
  tail-resumptive handlers; captured vars as explicit Wasm function parameters.
- Fused patterns (`SplitEachPrint`, `SplitGetPrint`, `graphemes-get-print`) are deletion targets,
  not extension points. They become obsolete after Phase WB-2/WB-3.

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `doc/PLAN_STANDARD_LIBRARY.md` — stdlib split/grapheme C4–C8
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` (add Phase WB-1 variants here)
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
