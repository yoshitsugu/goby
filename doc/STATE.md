# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active execution plan.
- Current priority is IR0 -> IR1:
  - implement the distinct resolved front-end form chosen in `PLAN_IR`,
  - continue expanding the construct inventory and mapping table.

## Immediate Next Steps

1. Update `doc/PLAN_IR.md` with the concrete resolved-input decision if it changes while implementing.
2. Start the resolved-form implementation for callable/effect identity.
3. Expand the inventory table as new semantic families are analyzed.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- Then inspect:
  - `crates/goby-core/src/ir.rs`
  - `crates/goby-core/src/ir_lower.rs`
  - the front-end resolution/typecheck modules that currently determine symbol identity
- Do not use `doc/STATE.md` as a second roadmap; keep durable planning detail in `doc/PLAN_IR.md`.
