# Goby Project State Snapshot

Last updated: 2026-03-12

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Planning slice recorded for Track D developer tooling.
- `doc/PLAN.md` now contains a phased `goby-lsp` implementation plan under Active Track D.

## Current State

- Compatibility cleanup backlog has been closed and removed from `doc/PLAN.md`.
- Next actionable tooling sequence is:
  - machine-readable diagnostic/span hardening in `goby-core`,
  - `crates/goby-lsp` workspace scaffold,
  - LSP diagnostics/hover/definition MVP.

## Verified

- No code verification recorded for this planning-only update.

## Next Work

- Start Phase D1 from `doc/PLAN.md`: normalize diagnostics and close remaining source-span metadata gaps needed by LSP.
- Record concrete crate/binary naming decisions once implementation starts.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
