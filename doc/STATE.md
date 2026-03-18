# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- Track F F1-F5 are complete; F6 convergence is still in progress.
- Track D5 has started with a first `goby lint` slice.
- `goby lint <file.gb>` now exists for human-readable warnings.
- First rule implemented: unreachable `case` arm after wildcard `_`.

## Immediate Next Steps

1. Continue Track F F6 convergence by shrinking `RuntimeIoPlan` usage in the normal runtime-I/O compile path.
2. D5 rule 2: add unused local binding lint on top of the new lint command path.
3. Add JSON output mode for lint diagnostics once multiple rules exist.
4. Continue D5 in priority order after validating warning UX and exit-code policy.

## Decisions To Carry Forward

- Track F architecture is now carried by the `gen_lower/` module docs, `runtime_io_plan.rs`
  convergence notes, and the Track F regression tests.
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
- `compile_module` still falls back to `runtime_io_plan` after `gen_lower`; Track F is not
  fully converged until that remaining special-case path is either deleted or reduced to
  documented optimization-only use.
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
- For Track F restart context, read `crates/goby-wasm/src/gen_lower/mod.rs`,
  `crates/goby-wasm/src/runtime_io_plan.rs`, `crates/goby-wasm/src/lib.rs`, and the
  `tests/track-f/` fixtures.
