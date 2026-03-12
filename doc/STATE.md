# Goby Project State Snapshot

Last updated: 2026-03-12

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Compatibility cleanup backlog C1-C4 is complete.
- The next milestone should come from active runtime/stdlib feature work, not additional compatibility-bridge pruning.

## Current State

- `crates/goby-core/src/typecheck_validate.rs` no longer uses builtin export tables for missing stdlib modules in import validation or `module_exports_for_import_with_resolver`.
- `stdlib/goby/*.gb` is now expected to be present for all supported `goby/...` imports; missing files should surface a clear attempted-path diagnostic.
- `crates/goby-core/src/typecheck_build.rs` no longer injects `print` as a builtin global.
- Bare `print` / `println` still work in normal code because implicit `goby/prelude` import remains active when the prelude file exists.
- `doc/PLAN_STANDARD_LIBRARY.md` no longer describes builtin import fallback as active behavior.
- `crates/goby-wasm/src/runtime_unit.rs` and `crates/goby-wasm/src/runtime_apply.rs` now route bare/pipeline/value `print` / `println` through embedded-effect dispatch instead of separate AST-level output shortcuts.
- Remaining runtime `print` recognition is limited to native-lowering / string-fallback mechanics and is no longer tracked as embedded-handler compatibility debt.

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test -p goby-wasm`

## Next Work

- Pick the next active runtime/stdlib milestone from `doc/PLAN.md`.
- Treat compatibility cleanup as closed unless new compatibility-only behavior is intentionally introduced.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
