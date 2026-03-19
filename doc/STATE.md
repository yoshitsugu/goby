# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0, IR1, IR2, IR3, and IR4 are landed.
- Current priority is IR5, then IR6/IR7/IR8/IR9 by clean semantic-family slices.

## Immediate Next Steps

1. Finish IR5 so collection lowering fails, if at all, for backend reasons rather than missing shared-IR shape.
2. Start IR6 by lowering `case` through `CompExpr::Case` and `IrCasePattern`.
3. Keep diagnostics and backend fallbacks aligned with the fact that shared IR can now represent tuples, records, lambdas, `case`, and mutation.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- Then inspect:
  - `crates/goby-core/src/ir.rs`
  - `crates/goby-core/src/resolved.rs`
  - `crates/goby-core/src/ir_lower.rs`
