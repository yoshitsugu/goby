# Goby Project State Snapshot

Last updated: 2026-03-18

## Current Focus

- Track F F4 is complete: fused `SplitEachPrint` backend IR instruction handles
  `string.split text "\n"` + `each lines Print.println` pattern inline.
  `track_f_f4_split_each_is_general_lowered` passes.
- Next milestone is F5: `list.get` indexing for `Print.println (list.get lines 1)`.

## Immediate Next Steps

1. F5: implement `list.get` in the general lowering path.
   - IR: `Call(GlobalRef("list","get"), [Var("lines"), IntLit(1)])`
   - Requires: intermediate list representation (from split result) consumable by index
   - OR: detect fused `split + get(idx)` pattern similar to F4's fused approach
2. F5 done when `track_f_f5_index_is_general_lowered` test passes (remove `#[ignore]`).
3. F6 follows: convergence — route old shape-specific programs through general path;
   delete/reduce RuntimeIoPlan machinery.

## Decisions To Carry Forward

- Architecture locked in `doc/wasm_runtime_architecture.md`.
- `gen_lower/` must not import from `runtime_io_plan.rs`.
- General lowering path only activates when IR body contains `PerformEffect` nodes.
- F4 uses fused pattern (SplitEachPrint) — no intermediate list in memory.
- For F5, must decide: fused `SplitGetPrint` pattern vs. real list representation.
  - Fused is simpler but less general; real list enables F5 and future list ops.
- String layout: len (i32) at heap_base, bytes at heap_base+4.
- I32 scratch locals (4 total) declared when SplitEachPrint is present.

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split`
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset

## Restart Notes

- `doc/PLAN.md` is the roadmap reference.
- `doc/wasm_runtime_architecture.md` is the architecture reference for Track F.
