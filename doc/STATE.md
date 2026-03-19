# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0, IR1, IR2, IR3, IR4, and IR5 are landed.
- Current priority order is IR6, then IR8, then IR7, then IR9.

## Immediate Next Steps

1. Start IR6 by lowering `case` through `CompExpr::Case` and `IrCasePattern`.
2. Ensure effectful scrutinees and branch bodies survive lowering without AST fallback.
3. After IR6, move to IR8 for tuples/records before starting lambda lowering.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- `Read` / `Print` family normalization is landed in runtime fallback classification, including qualified and method-style AST shapes such as `Print.print (Read.read ())` and qualified split callbacks like `each lines Print.println`.
- `string` / `list` helper-family normalization is also landed for selective-import plus canonical qualifier shapes such as `string.split(...)`, `list.each ...`, and qualified imported runtime decl calls like `string.length`.
- Collection lowering is landed through shared IR plus backend parity: list literal/spread/index lower through the canonical architecture, and `lines[1]` converges with `list.get lines 1` at compile/runtime planning.
- Then inspect:
  - `crates/goby-core/src/ir.rs`
  - `crates/goby-core/src/resolved.rs`
  - `crates/goby-core/src/ir_lower.rs`
