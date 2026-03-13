# Goby Project State Snapshot

Last updated: 2026-03-13

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Planning slice recorded for Track D developer tooling.
- `doc/PLAN.md` now contains a phased `goby-lsp` implementation plan under Active Track D.
- `doc/PLAN.md` also records Active Track E for `Float` support backed by Wasm `f64`.
- Higher-order named function references are now documented explicitly
  (for example: `map xs add_ten`).
- Track F Phase F1 containment is now implemented:
  compile-time fallback no longer consumes stdin for `Read` programs during `goby run`.

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
- Current language docs/examples/tests now make explicit that named functions can be
  passed directly where a function-typed argument is expected.
- `goby-wasm::compile_module` now runs compile-time fallback output resolution in a
  "no live stdin" mode and surfaces an explicit codegen error if `Read.read` or
  `Read.read_line` would consume compiler-process stdin.
- CLI and Wasm regression tests now cover the containment behavior.

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test`

## Next Work

- Start Track F Phase F2/F3:
  split compile-time static-output fallback from runtime execution capability planning,
  then add dynamic Wasm/WASI stdin support for `Read`.
- Keep Track D queued after the runtime I/O containment/runtime split work is in a stable state.
- Once Track F runtime support lands, sync `doc/LANGUAGE_SPEC.md` and runnable stdin examples.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
