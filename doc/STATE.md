# Goby Project State Snapshot

Last updated: 2026-03-12

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- C1, C2, and C3 are complete: stdlib import fallback is gone, bare `print` resolves through implicit prelude, and planning docs are aligned with that behavior.
- The next adjacent cleanup is C4: re-review embedded default handler/runtime special-case boundaries now that name-resolution compatibility work is closed.

## Current State

- `crates/goby-core/src/typecheck_validate.rs` no longer uses builtin export tables for missing stdlib modules in import validation or `module_exports_for_import_with_resolver`.
- `stdlib/goby/*.gb` is now expected to be present for all supported `goby/...` imports; missing files should surface a clear attempted-path diagnostic.
- `crates/goby-core/src/typecheck_build.rs` no longer injects `print` as a builtin global.
- Bare `print` / `println` still work in normal code because implicit `goby/prelude` import remains active when the prelude file exists.
- `doc/PLAN_STANDARD_LIBRARY.md` no longer describes builtin import fallback as active behavior.
- Remaining related cleanup after this slice:
  - move to C4/runtime-side cleanup for remaining print/embed special cases if they still count as architectural debt.

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test -p goby-core`

## Next Work

- Re-review whether remaining runtime-side `print` / embedded-handler special-casing should be simplified under C4 or a later stdlib-runtime milestone.
- If that re-review finds no additional compatibility debt, shift to the next feature/runtime milestone instead of more cleanup.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
