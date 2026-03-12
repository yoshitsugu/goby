# Goby Project State Snapshot

Last updated: 2026-03-12

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- C1 is complete: checked-in stdlib files are now the only import/export source of truth for `goby/...` imports.
- The next adjacent cleanup is C3/C2: remove leftover builtin-fallback wording from docs, then decide the long-term bare-`print` policy.

## Current State

- `crates/goby-core/src/typecheck_validate.rs` no longer uses builtin export tables for missing stdlib modules in import validation or `module_exports_for_import_with_resolver`.
- `stdlib/goby/*.gb` is now expected to be present for all supported `goby/...` imports; missing files should surface a clear attempted-path diagnostic.
- Remaining related cleanup after this slice:
  - remove outdated builtin-fallback wording from the remaining standard-library plan sections,
  - then move to C2/C3 policy cleanup for bare `print` and fallback-oriented tests/docs.

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test -p goby-core`

## Next Work

- Finish pruning obsolete builtin-fallback wording from the remaining standard-library planning docs under C3.
- Then decide whether bare `print` stays as permanent prelude sugar or moves to import/prelude-only under C2.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
