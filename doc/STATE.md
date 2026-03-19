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

## Immediate Next Steps

1. Decide the next shared-IR expansion boundary in `crates/goby-core/src/ir.rs`.
2. Add convergence-focused IR tests for equivalent effect-call spellings.
3. Lock the collection lowering strategy for list literal/spread/indexing on top of the resolved boundary.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- Then inspect:
  - `crates/goby-core/src/resolved.rs`
  - `crates/goby-core/src/ir_lower.rs`
  - `crates/goby-core/src/ir.rs`
