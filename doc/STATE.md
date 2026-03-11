# Goby Project State Snapshot

Last updated: 2026-03-11

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Active work is `doc/PLAN.md` Track F: maintainability hardening.
- `Milestone F1` (`goby-wasm` split) is complete.
- `Milestone F2` (`goby-core` typecheck split) is complete.
- Current target is `Milestone F3`: start splitting `crates/goby-core/src/parser.rs`
  by responsibility without changing behavior.

## Current State

- `goby-wasm` runtime split completed so far:
  - `crates/goby-wasm/src/runtime_value.rs`
  - `crates/goby-wasm/src/runtime_env.rs`
  - `crates/goby-wasm/src/runtime_flow.rs`
  - `crates/goby-wasm/src/runtime_eval.rs`
  - `crates/goby-wasm/src/runtime_resolver.rs`
  - `crates/goby-wasm/src/runtime_dispatch.rs`
  - `crates/goby-wasm/src/runtime_decl.rs`
  - `crates/goby-wasm/src/runtime_exec.rs`
  - `crates/goby-wasm/src/runtime_replay.rs`
- `crates/goby-wasm/src/lib.rs` now mainly holds the public codegen entrypoint,
  fallback orchestration glue, and remaining helper methods that were not part of F1 extraction scope.
- `crates/goby-core/src/typecheck.rs` is now reduced to phase orchestration plus
  residual validation/helpers after the `F2` split.
- `F2.1` is now landed:
  - `crates/goby-core/src/typecheck_env.rs` owns `Ty`, `TypeEnv`, `ResumeContext`,
    effect-map structs, and related internal binding data.
  - `crates/goby-core/src/typecheck.rs` now consumes that module instead of defining those internals inline.
- `F2.2` is now landed:
  - `crates/goby-core/src/typecheck_validate.rs` owns import resolution helpers,
    stdlib-root policy, embed/intrinsic validation, imported effect/type collection,
    and import-backed symbol injection.
  - `crates/goby-core/src/typecheck.rs` now calls that module instead of carrying those helpers inline.
- `F2.3` is now landed:
  - `crates/goby-core/src/typecheck_effect.rs` owns effect declaration validation,
    member `can`-clause validation, effect dependency cycle checks, and effect-map builders.
  - `crates/goby-core/src/typecheck.rs` now consumes those effect-phase helpers instead of defining them inline.
- `F2.4` is now landed:
  - `crates/goby-core/src/typecheck_check.rs` owns expression inference, statement checking,
    resume validation, branch-consistency checks, and type rendering helpers.
  - `crates/goby-core/src/typecheck.rs` now calls that module instead of carrying the checking core inline.
- `F2.5` is now landed:
  - `typecheck_module_with_context` now sequences explicit validation, checking-state preparation,
    and declaration-body checking phases through dedicated helpers.
  - `Milestone F2` can be treated as complete; next work moves to `parser.rs`.
- Runtime model to preserve while refactoring:
  - `Out<T> = Done | Suspend | Escape | Err`
  - `Escape::WithScope { with_id, value }`
  - lexical nearest-handler dispatch
  - parity between fallback runtime behavior and current tests

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test -p goby-core`
- `cargo test -p goby-wasm`
- `cargo test --workspace`

## Next Work

- Start `Milestone F3` in `goby-core`:
  - isolate shared parser helpers and top-level declaration parsing seams first
  - keep `parse_module` as the public entrypoint while shrinking `parser.rs`
  - preserve current parse diagnostics and parser test corpus during moves

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
