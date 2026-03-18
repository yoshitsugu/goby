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

- Integer operator support now includes `+`, `-`, `*`, `/`, `%`, `<`, `>`, `<=`, `>=`,
  with runtime/typecheck/native-support coverage for the current subset.
- Equality is now aligned for the current runtime-supported comparable subset
  (`Int`, `Bool`, `String`, `Unit`, tuples/lists/records where runtime equality exists).
- Spaced single-argument calls still require parentheses around binary-expression arguments
  under the current parser precedence; for example `println (1 + 1)` works while
  `println 1 + 1` remains a planned syntax/precedence decision.
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
- Plain `RuntimeIoPlan::Echo` for `Read.read`, `Read.read` echo with static suffix prints,
  and plain `RuntimeIoPlan::SplitLinesEach` now delegate Wasm emission to the shared
  backend-IR/general-emitter path.
- General lowering now supports static string values in backend IR, so qualified
  `Read.read()` programs followed by static `Print.print`/`Print.println` suffixes can
  classify as `GeneralLowered` instead of `DynamicWasiIo`.
- Specialized builders still remain for `read_line` and transform-heavy split shapes.
- Remaining F6 blockers are:
  - bare prelude `read` / `read_line` names still fall out of IR lowering and rely on AST fallback,
  - `read_line` does not yet have a fully general Wasm stdin-cursor/runtime-allocation story,
  - transformed or suffix-bearing split callbacks still depend on `RuntimeIoPlan`.
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
