# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0, IR1, IR2, IR3, and IR4 are landed.
- Current priority is IR5, then IR6/IR7/IR8/IR9 by clean semantic-family slices.

## Immediate Next Steps

1. Finish IR5 so collection lowering fails, if at all, for backend reasons rather than missing shared-IR shape.
2. Before switching runtime execution plans to prefer IR artifacts over parsed AST, finish the remaining helper-family normalization so `list.get` / `string.split` are accepted across runtime/native/static-output paths with the same canonical semantics as their sugared spellings.
3. Start IR6 by lowering `case` through `CompExpr::Case` and `IrCasePattern`.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- `Read` / `Print` family normalization is landed in runtime fallback classification, including qualified and method-style AST shapes such as `Print.print (Read.read ())` and qualified split callbacks like `each lines Print.println`.
- Then inspect:
  - `crates/goby-core/src/ir.rs`
  - `crates/goby-core/src/resolved.rs`
  - `crates/goby-core/src/ir_lower.rs`
