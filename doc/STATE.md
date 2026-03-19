# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0 through IR10 are landed.
- Current priority order is IR11.

## Immediate Next Steps

1. Advance IR11 by removing stale "pure subset" framing from docs, comments, tests, and diagnostics.
2. Delete dead names or branches that only exist for the old architecture wording.
3. Keep the cleanup behavior-preserving unless a wording fix requires a matching test update.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- Mutation lowering is landed through shared IR: `mut` bindings lower to `CompExpr::LetMut`, assignment lowers to `CompExpr::Assign`, and non-local assignment targets are rejected during IR lowering.
- Backend-boundary convergence is landed for the current representative slice: native fallback no longer rejects `mut` bindings without assignment, and assignment is reported as an explicit native backend limitation rather than a generic unsupported statement.
- The remaining work is pure-subset cleanup, not additional shared-IR expansion or backend-boundary redesign.
- Then inspect:
  - `crates/goby-wasm/src/fallback.rs`
  - `crates/goby-wasm/src/lower.rs`
  - `doc/PLAN_IR.md`
