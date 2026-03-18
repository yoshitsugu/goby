# Goby Project State Snapshot

Last updated: 2026-03-18

## Current Focus

- Track F F6 is complete.
- F3/F4/F5 representative runtime-I/O programs now classify as `GeneralLowered`
  and compile through the general lowering path.
- Remaining `RuntimeIoPlan` shapes are explicitly documented as optimization-only
  paths rather than semantic source-of-truth code paths.
- Size guardrail tests now cover both general-lowered Track F fixtures and a
  remaining optimization-backed transformed split callback.

## Immediate Next Steps

1. If desired, continue shrinking `RuntimeIoPlan::Echo` by normalizing more bare-name
   read/print forms into general lowering.
2. Revisit whether transformed split-callback optimization should move into
   general lowering proper or remain as a documented fast path.
3. Start the next roadmap item outside Track F convergence.

## Decisions To Carry Forward

- Architecture locked in `doc/wasm_runtime_architecture.md`.
- `gen_lower/` must not import from `runtime_io_plan.rs`.
- General lowering path only activates when IR body contains `PerformEffect` nodes.
- F4 uses fused pattern (SplitEachPrint) — no intermediate list in memory.
- F5 also uses a fused pattern (SplitGetPrint) rather than a heap-allocated list.
- Out-of-range indexed split access aborts the Wasm program via generated trap checks,
  matching the existing runtime abort policy.
- Public runtime-I/O classification now distinguishes `GeneralLowered` from
  `DynamicWasiIo` so convergence progress is observable in tests and callers.
- `RuntimeIoPlan` is no longer the semantic source of truth; any remaining plan-backed
  lowering is treated and documented as an optimization layer with parity obligations.
- String layout: len (i32) at heap_base, bytes at heap_base+4.
- I32 scratch locals (5 total) are declared when fused split instructions are present.

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split`
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset

## Restart Notes

- `doc/PLAN.md` is the roadmap reference.
- `doc/wasm_runtime_architecture.md` is the architecture reference for Track F.
