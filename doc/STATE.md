# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0, IR1, IR2, IR3, and IR4 are landed.
- Current priority is IR5, then IR6/IR7/IR8/IR9 by clean semantic-family slices.

## Immediate Next Steps

1. Finish IR5 so collection lowering fails, if at all, for backend reasons rather than missing shared-IR shape.
2. Before switching runtime execution plans to prefer IR artifacts over parsed AST, normalize the runtime/native/static-output layers so canonical bridge spellings (`Read.read`, `Print.println`, `list.get`, `string.split`) are accepted as the same semantic family.
3. Start IR6 by lowering `case` through `CompExpr::Case` and `IrCasePattern`.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- Then inspect:
  - `crates/goby-core/src/ir.rs`
  - `crates/goby-core/src/resolved.rs`
  - `crates/goby-core/src/ir_lower.rs`
