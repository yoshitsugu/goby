# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0 through IR9 are landed.
- Current priority order is IR10, then IR11.

## Immediate Next Steps

1. Advance IR10 by finding remaining failures that still surface as "not lowerable" instead of backend limitations.
2. Remove or narrow fallback/recognizer paths that only exist because of pre-convergence AST assumptions.
3. Keep IR11 limited to cleanup that becomes true after IR10 lands in code.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- Mutation lowering is landed through shared IR: `mut` bindings lower to `CompExpr::LetMut`, assignment lowers to `CompExpr::Assign`, and non-local assignment targets are rejected during IR lowering.
- The remaining work is backend-boundary convergence and deletion of stale pure-subset framing, not additional shared-IR shape expansion.
- Then inspect:
  - `crates/goby-wasm/src/lower.rs`
  - `crates/goby-wasm/src/fallback.rs`
  - `crates/goby-wasm/src/runtime_io_plan.rs`
