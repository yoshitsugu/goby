# Goby Project State Snapshot

Last updated: 2026-03-10

This file is a restart-safe snapshot for resuming work after context reset.

## Current Status

- Scoped handler exit + multi-resume progression (former 4.7 task) is implemented.
- List spread + stdlib `List.map` consolidation (former 4.5 task) is implemented.
- `doc/PLAN.md` has been pruned to remove completed active-task sections.
- Active docs are aligned for current shipped behavior:
  - `doc/LANGUAGE_SPEC.md`
  - `doc/PLAN.md`
  - `examples/effect.gb`

## Runtime Shape

- Runtime result model:
  - `Out<T> = Done | Suspend | Escape | Err`
  - `Escape::WithScope { with_id, value }`
- Scope handling:
  - value-position `with` consumes matching scoped escapes at `FinishKind::WithBody { with_id }`.
  - statement-position `with` uses `execute_unit_ast_stmt_sequence` for continuation replay and scoped-exit absorption.
- Legacy cleanup state:
  - old AST continuation compatibility layer is removed.
  - legacy runtime abort flag state is removed.
  - `eval_expr_ast` remains as optional compatibility fallback helper (non-blocking follow-up only).

## Verified

- `cargo fmt`
- `cargo check`
- `cargo clippy -p goby-wasm -- -D warnings`
- `cargo test -p goby-wasm`
- `cargo test --workspace`
- `cargo run -p goby-cli -- check` / `run` on scoped-exit and multi-resume sample programs

## Next Work

- No required remaining task for former 4.5/4.7 tracks.
- Optional cleanup only:
  - remove or shrink `eval_expr_ast` compatibility fallback incrementally if future refactors need it.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
