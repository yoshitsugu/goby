# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0, IR1, IR2, IR3, and IR4 are landed.
- Current priority is IR5, then IR6/IR7/IR8/IR9 by clean semantic-family slices.

## Immediate Next Steps

1. Finish IR5 so collection lowering fails, if at all, for backend reasons rather than missing shared-IR shape.
2. Switch the runtime execution plan boundary to prefer IR-derived artifacts where available, now that `Read` / `Print` and the `string` / `list` helper families converge across selective-import and canonical-qualifier forms.
3. Start IR6 by lowering `case` through `CompExpr::Case` and `IrCasePattern`.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- `Read` / `Print` family normalization is landed in runtime fallback classification, including qualified and method-style AST shapes such as `Print.print (Read.read ())` and qualified split callbacks like `each lines Print.println`.
- `string` / `list` helper-family normalization is also landed for selective-import plus canonical qualifier shapes such as `string.split(...)`, `list.each ...`, and qualified imported runtime decl calls like `string.length`.
- Then inspect:
  - `crates/goby-core/src/ir.rs`
  - `crates/goby-core/src/resolved.rs`
  - `crates/goby-core/src/ir_lower.rs`
