# Goby Project State Snapshot

Last updated: 2026-03-18

## Current Focus

- Track F F5 is complete: fused `SplitGetPrint` backend IR instruction handles
  `string.split text "\n"` + `line = list.get lines 1` + `Print.println line`
  in the general lowering path.
- `track_f_f5_index_is_general_lowered` passes and runtime fallback regressions
  cover seeded-stdin success and out-of-range abort behavior.
- Next milestone is F6: converge the remaining shape-specific runtime-I/O plans
  onto the general lowering path and delete/reduce special cases.

## Immediate Next Steps

1. F6: route remaining runtime-I/O recognizers through the general lowering path.
2. Decide which DynamicWasiIo special cases remain as explicit optimizations versus
   semantic lowering paths to delete.
3. Reduce `RuntimeIoPlan` surface after parity is proven.

## Decisions To Carry Forward

- Architecture locked in `doc/wasm_runtime_architecture.md`.
- `gen_lower/` must not import from `runtime_io_plan.rs`.
- General lowering path only activates when IR body contains `PerformEffect` nodes.
- F4 uses fused pattern (SplitEachPrint) — no intermediate list in memory.
- F5 also uses a fused pattern (SplitGetPrint) rather than a heap-allocated list.
- Out-of-range indexed split access aborts the Wasm program via generated trap checks,
  matching the existing runtime abort policy.
- String layout: len (i32) at heap_base, bytes at heap_base+4.
- I32 scratch locals (5 total) are declared when fused split instructions are present.

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split`
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset

## Restart Notes

- `doc/PLAN.md` is the roadmap reference.
- `doc/wasm_runtime_architecture.md` is the architecture reference for Track F.
