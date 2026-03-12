# Goby Project State Snapshot

Last updated: 2026-03-12

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Planning slice recorded for Track D developer tooling.
- `doc/PLAN.md` now contains a phased `goby-lsp` implementation plan under Active Track D.
- `doc/PLAN.md` also records Active Track E for `Float` support backed by Wasm `f64`.

## Current State

- Compatibility cleanup backlog has been closed and removed from `doc/PLAN.md`.
- Next actionable tooling sequence is:
  - machine-readable diagnostic/span hardening in `goby-core`,
  - `crates/goby-lsp` workspace scaffold,
  - LSP diagnostics/hover/definition MVP.
- Numeric-type expansion is now captured as a separate follow-on track:
  - lock `Float` literal/coercion/equality semantics,
  - implement parser/typechecker/runtime/Wasm support,
  - sync docs/examples/tooling once behavior lands.

## Verified

- No code verification recorded for this planning-only update.

## Next Work

- Start Phase D1 from `doc/PLAN.md`: normalize diagnostics and close remaining source-span metadata gaps needed by LSP.
- Record concrete crate/binary naming decisions once implementation starts.
- When Track D is stable enough, start Track E Phase E1 and lock `Float` semantics before code changes.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
