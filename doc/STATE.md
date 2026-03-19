# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0, IR1, IR2, IR3, IR4, IR5, IR6, and IR8 are landed.
- Current priority order is IR7, then IR9.

## Immediate Next Steps

1. Start IR7 by lowering lambdas / higher-order values through the shared-IR function-value vocabulary.
2. Keep closure/capture fallout as explicit design at the shared-IR boundary rather than pushing ad-hoc callable rewrites into backend code.
3. After IR7, move to IR9 for mutation.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- `Read` / `Print` family normalization is landed in runtime fallback classification, including qualified and method-style AST shapes such as `Print.print (Read.read ())` and qualified split callbacks like `each lines Print.println`.
- `string` / `list` helper-family normalization is also landed for selective-import plus canonical qualifier shapes such as `string.split(...)`, `list.each ...`, and qualified imported runtime decl calls like `string.length`.
- Collection lowering is landed through shared IR plus backend parity: list literal/spread/index lower through the canonical architecture, and `lines[1]` converges with `list.get lines 1` at compile/runtime planning.
- Control-flow lowering is landed through shared IR: `case` lowers to `CompExpr::Case`, effectful scrutinees ANF-normalize via `let`, and effectful branch bodies survive lowering without falling back to AST-only execution.
- Product-data lowering is landed through shared IR: non-unit tuples and record construction lower to `ValueExpr::TupleLit` / `ValueExpr::RecordLit`, and effectful tuple items / record fields ANF-normalize via `let`.
- Then inspect:
  - `crates/goby-core/src/ir.rs`
  - `crates/goby-core/src/resolved.rs`
  - `crates/goby-core/src/ir_lower.rs`
