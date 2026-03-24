# Goby Project State Snapshot

Last updated: 2026-03-24

## Current Focus

- IR0–IR11 complete. `doc/PLAN_IR.md` now contains the Wasm backend lowering design (§4–§5).
- Track E E1–E7 complete.
- Phase WB-1 complete (2026-03-24): `If`, `BinOp`, `Interp`, `LetMut`, `Assign` all lowered and emitted.
- Phase WB-2A complete (2026-03-24): top-level `DeclCall`, recursion, funcref-table indirect calls, typed backend effect identities.
  - `lower_comp_with_decls` passes `known_decls` set so `Var(name)` callee resolves as `DeclCall`.
  - `emit_general_module_with_aux` places main first, aux after; builds `decl_name → func_idx` table.
  - Higher-order helper-call and recursive-call execution tests pass.
  - `gen_lower` backend IR now uses typed `BackendEffectOp` / `BackendPrintOp` instead of raw effect/op strings for general-lowering dispatch.
- Phase WB-2B M1–M5 complete (2026-03-24): `Case` literal/wildcard patterns, list patterns, `ListLit`, `TupleLit`, and `RecordLit` lowered/emitted.
- Phase WB-2B M6 complete (2026-03-24): `stdlib/goby/list.gb` `each` / `map` execute via `GeneralLowered`.
  - `each` uses `ListEach` / `IndirectCall` for named callbacks.
  - `map` uses backend `ListMap`; generic list-spread lowering remains separate future work.
- Phase WB-2B M7 complete (2026-03-24): obsolete fused split recognizers removed from `gen_lower/lower.rs`.
  - `string.split` + `list.each` / `list.get` now lower through the normal general path.
  - retained for later: `graphemes-get-print` in `lower.rs`, and backend-IR
    `SplitEachPrint` / `SplitGetPrint` as optional `RuntimeIoPlan` optimisations.
- Phase WB-2B M8 complete (2026-03-24): WB-2 quality gates passed.
  - `cargo fmt --check`
  - `cargo check`
  - `cargo test`
  - `cargo clippy -- -D warnings`
- WB-2 is now complete.
- Next active work: Phase WB-3 Step 1 (handler legality analysis for one-shot tail-resumptive lowering).

## Track Priority

**Wasm backend (WB) is the primary track.** Complete WB phases in order before starting stdlib work.
Rationale: the IR/backend pipeline must stabilise before the stdlib can depend on it.
stdlib track (C4-S1 onwards) is deferred until WB is in a stable state.

## Immediate Next Steps

**Track Wasm backend (Phase WB-3 Step 1) — primary:**
Implement legality analysis for `WithHandler` nodes before effect-handler lowering.
See `doc/PLAN_IR.md` §5 Phase WB-2B.

**Track stdlib (C4-S1) — deferred:**
Unblock `List String` as a record field type in the type checker.
Exit criterion: `cargo run -p goby-cli -- check stdlib/goby/string.gb` no longer fails on the state record field type.
See `doc/PLAN_STANDARD_LIBRARY.md` §5.

## Architecture State

- Resolved-form → shared IR boundary is stable (IR0–IR11 done).
- Wasm backend lowering design is locked in `doc/PLAN_IR.md`:
  - Phase WB-1: pure control flow and operators
  - Phase WB-2: pattern matching and structured data
  - Phase WB-3: function values and effect handlers (direct-call lowering, one-shot tail-resumptive)
  - Phase WB-3B (future): WasmFX stack switching when proposal reaches Phase 4
- General-lowered coverage already includes:
  - `Case` with literal/list patterns
  - `ListLit`
  - `TupleLit`
  - `RecordLit`
  - stdlib `list.each` / `list.map`
- Removed as WB-2-obsolete:
  - `gen_lower/lower.rs` fused split recognition for `string.split` + `list.each` / `list.get`
- WB-2 exit state:
  - pure control flow/operators complete
  - decl calls / recursion / higher-order funcref calls complete
  - backend effect dispatch identity is locked for general lowering
  - pattern matching and structured data complete for current IR surface
- Effect handler strategy: selective CPS degenerating to direct-call lowering for one-shot
  tail-resumptive handlers; captured vars as explicit Wasm function parameters.
- Fused patterns (`SplitEachPrint`, `SplitGetPrint`, `graphemes-get-print`) are deletion targets,
  not extension points. They become obsolete after Phase WB-2/WB-3.

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `doc/PLAN_STANDARD_LIBRARY.md` — stdlib split/grapheme C4–C8
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
