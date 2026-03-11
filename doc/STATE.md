# Goby Project State Snapshot

Last updated: 2026-03-11

This file is a restart-safe snapshot for resuming work after context reset.

## Current Status

- `doc/PLAN.md` now includes an active maintainability-hardening track focused on
  reducing parser/typechecker/Wasm-runtime black-box risk through responsibility-based decomposition.
- Scoped handler exit + multi-resume progression (former 4.7 task) is implemented.
- List spread + stdlib `List.map` consolidation (former 4.5 task) is implemented.
- Embedded default handler execution now goes through a dedicated `EmbeddedEffectRuntime` layer; stdout/stderr accumulation and stdin cursor state are no longer stored directly on `RuntimeOutputResolver`.
- Embedded handler-name strings are now resolved into runtime handler kinds before dispatch, so resolver-side effect dispatch no longer branches on raw `__goby_embeded_effect_*` names.
- `goby-core` stdlib resolution now exposes parsed embedded handler kinds together with parsed stdlib modules, and wasm runtime builds one `RuntimeImportContext` from that shared typed metadata instead of running a second embedded-default collection pass.
- `@embed` is now intentionally treated as the narrow prelude `Print` / `Read` onboarding mechanism, not as the general future path for host capabilities such as file/clock/network access.
- `doc/PLAN.md` has been pruned to remove completed active-task sections.
- Active docs are aligned for current shipped behavior:
  - `doc/LANGUAGE_SPEC.md`
  - `doc/PLAN.md`
  - `examples/effect.gb`
- Added `examples/list_spread.gb` and synced `doc/PLAN.md` so list-spread execution/typing is no longer tracked as planned work.

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

- Maintainability hardening:
  - start with Track F milestone F0/F1:
    - lock focused refactor harness (`cargo test -p goby-wasm`, `cargo test -p goby-core`, `cargo test --workspace`, `cargo check`).
    - reduce `crates/goby-wasm/src/lib.rs` boundary width first by extracting runtime value/state helpers, embedded runtime/import loading, then fallback evaluator internals.
  - after F1, separate typecheck phases and parser responsibilities into smaller modules with contract tests.
- Runtime architecture cleanup:
  - continue shrinking remaining embedded default-handler special cases around the runtime-owned `Print` / `Read` intrinsic I/O hook without broadening `@embed` beyond that role.
  - continue tightening state-threading semantics for structural expression evaluation so stdlib/user code keep sharing one runtime path.
- Follow-up cleanup remains optional:
  - remove or shrink `eval_expr_ast` compatibility fallback incrementally if future refactors need it.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
