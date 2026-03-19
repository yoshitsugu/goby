# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0, IR1, and IR3 are landed.
- Current priority is IR2 -> IR4 -> IR5.

## Landed This Slice

- added a distinct resolved front-end form in `crates/goby-core/src/resolved.rs`
- `crates/goby-core/src/ir_lower.rs` now lowers through `ResolvedModule` / `ResolvedDeclaration`
- bare/imported/qualified helper/effect identity no longer depends on raw-name heuristics inside `ir_lower`
- list index now canonicalizes to `list.get` at the resolved boundary
- shared IR now carries `ValueExpr::ListLit { elements, spread }`
- list literals/spread lower through ANF into shared IR instead of failing at the lowering boundary

## Immediate Next Steps

1. Continue IR2 by deciding the shared-IR representation for the next semantic family after lists.
2. Continue IR4 by broadening convergence tests around equivalent effect/helper spellings.
3. Continue IR5 with the remaining collection gap beyond list values, or move to tuples/records if that gives a cleaner IR slice.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- Then inspect:
  - `crates/goby-core/src/resolved.rs`
  - `crates/goby-core/src/ir_lower.rs`
  - `crates/goby-core/src/ir.rs`
