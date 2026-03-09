# Goby Project State Snapshot

Last updated: 2026-03-09 (session 243)

This file is a restart-safe snapshot for resuming work after context reset.

## Current Status

- 4.7 scoped handler exit semantics are implemented.
- Current shipped behavior:
  - if a handler clause finishes without `resume`, evaluation exits only the
    current `with ... in ...` scope,
  - the clause result becomes the whole-`with` result,
  - explicit `resume` progression and continuation-consumed runtime errors still
    work.
- Active docs are in sync:
  - `doc/LANGUAGE_SPEC.md`
  - `doc/PLAN.md`
  - `examples/effect.gb`

## Runtime Shape

- Active value-path runtime uses:
  - `Out<T> = Done | Suspend | Escape | Err`
  - `Escape::WithScope { with_id, value }`
  - `FinishKind::WithBody { with_id }`
  - `FinishKind::HandlerBody { token_idx, produce_value, with_id }`
- Value-position `with` runs on the active `eval_expr` / `eval_stmts` /
  `apply_cont` path and consumes matching scoped exits at the `with` boundary.
- Statement-position `with` still uses the legacy unit stmt-sequence wrapper for
  replay, but scoped exit is absorbed at that `with` boundary instead of
  aborting the whole program.

## Verified

- `cargo fmt`
- `cargo clippy -p goby-wasm -- -D warnings`
- `cargo test -p goby-wasm`
- `cargo check`
- latest focused re-check (session 243):
  - `cargo fmt`
  - `cargo clippy -p goby-wasm -- -D warnings`
  - `cargo check`
  - `cargo test --workspace`

## Next Work

- Phase 5 is substantially complete. Remaining legacy items:
  - `eval_ast_side_effect` / `eval_expr_ast` legacy path still active for
    `Expr::With`, `Expr::Case`, `Expr::If`, `Expr::Pipeline`, `Expr::InterpolatedString`
    statement-position and unsupported branches.
  - `runtime_aborted` / `set_runtime_abort_once` / `has_abort_without_error`
    still used by legacy `eval_ast_side_effect` / ingest path.
  - These are lower priority — migrate only when needed for new functionality.
- Next restart point: continue Phase 5 Step 4 (full workspace gate) or begin
  next feature work.

## Completed in Last Session (2026-03-09, session 243)

  - Phase 5 Steps 1–3b complete:
    - Step 1: `resume_through_active_continuation_*_outcome` → `resume_through_active_continuation_out`
    - Step 2: `eval_named_call_args_outcome` → `eval_named_call_args_out`; `pending_value_continuations` removed
    - Step 3a: `AstContinuationFrame` / `AstContinuation::Frame` removed; `HandlerContinuationState::Suspended` simplified to unit variant; `dispatch_depth` argument removed from push_resume_token functions
    - Step 3b: All `eval_expr_ast_outcome` + `complete_ast_value_outcome` pairs replaced with `eval_expr_to_option`; `eval_expr_ast_outcome`, `complete_ast_value_outcome`, `execute_ast_continuation` deleted; `AstContinuation` and `AstEvalOutcome<T>` types deleted
  - Quality gate: `cargo fmt`, `cargo clippy -p goby-wasm -- -D warnings`, `cargo check`, `cargo test --workspace` all pass (211 unit + 6 integration tests)

## Completed in Previous Session (2026-03-09, session 242)

  - Synchronized restart docs for immediate continuation:
    - `doc/PLAN.md` Phase 5 now includes an explicit progress checkpoint
      (done vs remaining vs execution guard).
    - `doc/STATE.md` updated restart target and focused validation snapshot.
  - Working-tree resume note:
    - current uncommitted code includes the post-session-241 step where
      `apply_named_value_call_args_ast_outcome` has been removed and its
      conversion logic inlined in `execute_saved_value_continuation`.
    - run `git status --short` first when resuming to confirm expected 3-file diff
      (`crates/goby-wasm/src/lib.rs`, `doc/PLAN.md`, `doc/STATE.md`).

## Completed in Previous Session (2026-03-09, session 241)

  - Removed `apply_named_value_call_args_ast_outcome` and inlined its conversion
    logic into `execute_saved_value_continuation` (`MultiArgNamedCall` branch).
  - Preserved existing abort propagation (`set_runtime_abort_once`) and unsupported
    mapping behavior on that path.
  - Quality gate passing: `cargo fmt`, `cargo check -p goby-wasm`,
    `cargo test -p goby-wasm`.

## Completed in Previous Session (2026-03-09, session 240)

  - Removed dead AST compatibility wrappers that were no longer called:
    `apply_pipeline_ast_outcome`,
    `apply_receiver_method_value_call_ast_outcome`,
    `apply_named_value_call_ast_outcome`,
    `dispatch_handler_method_as_value_outcome`.
  - Removed `ast_outcome_from_option` and `ast_outcome_to_out`; replaced
    with explicit conversion matches at call sites.
  - Quality gate passing: `cargo fmt`, `cargo clippy -p goby-wasm -- -D warnings`,
    `cargo check`, `cargo test --workspace`.

## Completed in Previous Session (2026-03-09, session 239)

  - `eval_expr_ast_outcome` fully migrated to a thin wrapper around
    `eval_expr`; the unreachable legacy AST fallback body (internal self-calls,
    value-continuation push/pop paths, and AST-local expression handling) was
    removed.
  - Added `AstContinuation::ContBridge` so `AstEvalOutcome::Suspended` can
    continue through unified `apply_cont` in `execute_ast_continuation`.
  - Marked remaining transitional AST-only helpers/enum variants with
    scoped `#[allow(dead_code)]` to keep `clippy -D warnings` green during
    staged Phase 5 cleanup.
  - Quality gate passing: `cargo fmt`, `cargo clippy -p goby-wasm -- -D warnings`,
    `cargo test -p goby-wasm` (unit 211 passed, integration 6 passed),
    `cargo check`.

## Completed in Previous Session (2026-03-09, session 238)

  - `apply_named_value_call_ast_outcome` is now a thin wrapper around
    `apply_named_value_call_out` (Out path). Handler dispatch, `__goby_`
    intrinsics, and declaration bodies all use the Out path via this function.
    `Out::Suspend` maps to `unreachable!()` (known unsupported on AST path).
    commit: d840e79
  - `apply_named_value_call_args_ast_outcome` is now a thin wrapper around
    `apply_named_value_call_args_out` (same pattern as above, multi-arg).
    commit: 02de41f
  - `apply_named_value_call_args_out` now uses Out-first path (handler →
    intrinsic → decl → AST fallback). commit: bef7b9a (session 237)
  - `dispatch_handler_method_as_value_with_args_flow` added. commit: bef7b9a

## Previously Completed

  - `execute_unit_expr_ast` `Expr::With` migrated to `eval_stmts` +
    `FinishKind::WithBody`; legacy `execute_ast_stmt_sequence` removed.
  - `execute_unit_ast_stmt` Binding/Assign arms migrated from
    `eval_expr_ast_outcome` to `eval_expr` (`Out` path).
  - `execute_unit_ast_stmt` return type migrated to `Out<()>`; legacy
    `execute_unit_ast_stmt_outcome` removed and `eval_stmts` fallback now consumes
    `Out<()>` directly.
  - `execute_unit_expr_ast` return type migrated to `Out<()>`; key call sites and
    legacy fallback branches now consume `Out` instead of `Option`.
  - `eval_ast_side_effect` print/println/pipeline side-effect branches now use
    `eval_expr` (`Out` path) instead of `eval_expr_ast` (`Option` path).
  - `eval_ast_side_effect` `Case` / `If` handling now uses `eval_expr` (`Out` path).
  - `eval_expr` `Case` / `If` fall back to `execute_unit_expr_ast` for unsupported
    selected branches.
  - `apply_named_value_call_out` evaluates declaration bodies via
    `eval_decl_as_value_with_args_out` before falling back to legacy AST path.

- Quality gate passing: `cargo fmt`, `cargo clippy -p goby-wasm -- -D warnings`,
  `cargo test -p goby-wasm` (unit 211 passed, integration 6 passed), `cargo check`.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
