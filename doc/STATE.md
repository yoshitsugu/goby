# Goby Project State Snapshot

Last updated: 2026-03-18

## Current Focus

- Track F F2 is complete: `gen_lower/` module created with `value.rs` (tagged-i64 encoding),
  `backend_ir.rs` (`WasmBackendInstr` skeleton), helper ABI confirmed.
- Next milestone is F3: general runtime execution core (`Read.read` + `Print.print/println`).

## Immediate Next Steps

1. F3: implement `GeneralLowerer` in `gen_lower/lower.rs`:
   - Lower `CompExpr::Let`, `CompExpr::Seq`, `CompExpr::PerformEffect` to `WasmBackendInstr`
   - Handle `Read.read` → `EffectOp { "Read", "read" }` → `fd_read` WASI call
   - Handle `Print.print/println` → `EffectOp { "Print", "print/println" }` → `fd_write` WASI call
2. F3 done when `track_f_f3_print_read_is_general_lowered` test passes (remove `#[ignore]`).
3. F4 follows: helper-call ABI for `string.split` and list iteration.

## Decisions To Carry Forward

- Architecture locked in `doc/wasm_runtime_architecture.md`.
- `gen_lower/` must not import from `runtime_io_plan.rs`.
- F3 fixture (`tests/track-f/f3_print_read.gb`) currently routes through `DynamicWasiIo(Echo)`;
  F3 goal: route through general lowering path instead.
- Unqualified effect calls (`read()`, `print(x)`) must be normalized to `PerformEffect` nodes
  at the general lowerer entry (per §11 invariant).

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split`
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset

## Restart Notes

- `doc/PLAN.md` is the roadmap reference.
- `doc/wasm_runtime_architecture.md` is the architecture reference for Track F.
