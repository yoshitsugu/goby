# Goby Project State Snapshot

Last updated: 2026-03-12

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- C1 and C2 are complete: checked-in stdlib files are the only import/export source of truth, and bare `print` now comes from implicit prelude rather than a separately injected builtin symbol.
- The next adjacent cleanup is C3/C4: prune leftover fallback wording from docs, then re-review embedded default handler/runtime special-case boundaries.

## Current State

- `crates/goby-core/src/typecheck_validate.rs` no longer uses builtin export tables for missing stdlib modules in import validation or `module_exports_for_import_with_resolver`.
- `stdlib/goby/*.gb` is now expected to be present for all supported `goby/...` imports; missing files should surface a clear attempted-path diagnostic.
- `crates/goby-core/src/typecheck_build.rs` no longer injects `print` as a builtin global.
- Bare `print` / `println` still work in normal code because implicit `goby/prelude` import remains active when the prelude file exists.
- Remaining related cleanup after this slice:
  - remove outdated builtin-fallback wording from the remaining standard-library plan sections,
  - then move to C4/runtime-side cleanup for remaining print/embed special cases if they still count as architectural debt.

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test -p goby-core`

## Next Work

- Finish pruning obsolete builtin-fallback wording from the remaining standard-library planning docs under C3.
- Then re-review whether remaining runtime-side `print` / embedded-handler special-casing should be simplified under C4 or a later stdlib-runtime milestone.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
