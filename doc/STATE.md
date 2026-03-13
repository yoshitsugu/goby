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
- Track F capability split has advanced:
  `goby run` now executes stdin-backed `Read` programs through the runtime resolver
  instead of failing once compile-time fallback is blocked.
- Track F dynamic Wasm work has started:
  the exact `print(read())` shape now compiles to a WASI Wasm module that imports
  `fd_read` and `fd_write`.

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
- `goby-wasm` now exposes a runtime execution path with seeded stdin so the CLI can
  execute `Read` programs without compile-time stdin capture.
- `goby-core` parser now lowers parenthesized multi-arg calls like `split(a, b)` into
  the same left-associative call shape as spaced calls, which unblocks runtime handling
  of selective-import stdlib helpers in this path.
- CLI and Wasm regression tests now cover both containment and runtime stdin execution
  for the `read` + `split` + `each` shape.
- `goby-wasm` backend now has a first dynamic stdin/stdout Wasm path for the simple
  read-all echo case (`print(read())`), while more complex `Read` programs still use
  the interpreter-backed runtime execution bridge.

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test`

## Next Work

- Continue Track F toward dynamic Wasm/WASI stdin support:
  extend the new `fd_read`/`fd_write` backend work beyond the simple echo shape so
  more `Read` programs stop depending on the interpreter-backed runtime execution path.
- Decide whether the new runtime execution path should become an explicit internal API
  boundary before dynamic Wasm support lands, or remain a temporary CLI-only bridge.
- Keep Track D queued after the runtime I/O containment/runtime split work is in a stable state.
- Once Track F runtime support lands, sync `doc/LANGUAGE_SPEC.md` and runnable stdin examples.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
