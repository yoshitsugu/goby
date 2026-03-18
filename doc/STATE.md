# Goby Project State Snapshot

Last updated: 2026-03-18

## Current Focus

- Track F F3 is complete: `gen_lower/lower.rs` and `gen_lower/emit.rs` implemented.
  - `lower_comp`/`lower_value` map Goby IR → `WasmBackendInstr`
  - `emit_general_module` maps `WasmBackendInstr` → Wasm bytes via WASI fd_read/fd_write
  - `try_general_lower_module` wired into `compile_module` before shape-specific classification
  - `track_f_f3_print_read_is_general_lowered` passes (no longer `#[ignore]`)
- Next milestone is F4: helper-call ABI for `string.split` and list iteration.

## Immediate Next Steps

1. F4: implement `CallHelper` emission in `gen_lower/emit.rs`:
   - Helper functions for `string.split` → returns a list ptr
   - `each` iteration over lists
2. F4 done when `track_f_f4_split_each_is_general_lowered` test passes (remove `#[ignore]`).
3. F5 follows: collection indexing (`list.get`).

## Decisions To Carry Forward

- Architecture locked in `doc/wasm_runtime_architecture.md`.
- `gen_lower/` must not import from `runtime_io_plan.rs`.
- General lowering path only activates when IR body contains `PerformEffect` nodes.
- `Print.print/println` EffectOp pushes `encode_unit()` onto stack after writing stdout.
- String layout: len (i32) at `heap_base`, bytes at `heap_base+4`.
- For F3 programs, there is only one string in flight at a time (single heap_base).

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split`
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset

## Restart Notes

- `doc/PLAN.md` is the roadmap reference.
- `doc/wasm_runtime_architecture.md` is the architecture reference for Track F.
