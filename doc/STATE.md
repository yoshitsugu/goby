# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0 through IR11 are landed.
- The IR-lowering roadmap is complete.

## Immediate Next Steps

1. Keep future lowering work aligned with `resolved form -> shared IR -> backend`.
2. Treat backend limitations as backend limitations rather than restoring AST-shaped recognizers.
3. Finish the remaining `CallHelper` backend gap by designing an emitter-level helper ABI for collection-producing helpers, rather than widening planner fallback again.
4. Reopen `doc/PLAN_IR.md` only if a genuinely new architectural gap appears.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- Mutation lowering is landed through shared IR: `mut` bindings lower to `CompExpr::LetMut`, assignment lowers to `CompExpr::Assign`, and non-local assignment targets are rejected during IR lowering.
- Backend-boundary convergence is landed for the current representative slice: native fallback no longer rejects `mut` bindings without assignment, and assignment is reported as an explicit native backend limitation rather than a generic unsupported statement.
- General Wasm lowering classification now checks emitter support instead of assuming every lowered backend IR sequence is emit-ready.
- Wasm compile-path tests now run structural validation with `wasmparser::Validator`, which caught and now guards against invalid-stack-shape regressions.
- Runtime-I/O plans that delegate to general backend emission append an explicit final `Drop`, matching the `_start : () -> ()` Wasm contract.
- The remaining backend architecture gap is not IR lowering; it is helper emission for non-fused `CallHelper` families such as collection-producing helpers.
- The IR-lowering roadmap is complete; follow-up work should stay within the converged lowering architecture.
- Then inspect:
  - `crates/goby-wasm/src/fallback.rs`
  - `crates/goby-wasm/src/lower.rs`
  - `doc/PLAN_IR.md`
