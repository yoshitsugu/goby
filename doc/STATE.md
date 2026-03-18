# Goby Project State Snapshot

Last updated: 2026-03-18

## Current Focus

- Track F F6 is complete.
- Track D5 has started with a first `goby lint` slice.
- `goby lint <file.gb>` now exists for human-readable warnings.
- First rule implemented: unreachable `case` arm after wildcard `_`.

## Immediate Next Steps

1. D5 rule 2: add unused local binding lint on top of the new lint command path.
2. Add JSON output mode for lint diagnostics once multiple rules exist.
3. Continue D5 in priority order after validating warning UX and exit-code policy.

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
- `goby lint` currently exits non-zero when warnings are emitted and reuses the same
  caret-snippet diagnostic renderer as `goby check`.
- Diagnostic severity now supports `Warning` in addition to `Error`.
- String layout: len (i32) at heap_base, bytes at heap_base+4.
- I32 scratch locals (5 total) are declared when fused split instructions are present.

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split`
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset

## Restart Notes

- `doc/PLAN.md` is the roadmap reference.
- `doc/wasm_runtime_architecture.md` is the architecture reference for Track F.
