# Step Review

## Step

- Track 4.7 Step 3.2a frame-entry groundwork.

## Contradiction Check

- Step 3 had already committed to a unified suspended-frame direction, but `resume` still replayed
  state directly from separate token-side statement/value fields.
- That contradicted the new plan because the evaluator could not grow toward a single continuation
  entrypoint while token structure remained the de facto continuation API.
- This slice removes that contradiction by introducing one AST continuation transport boundary
  (`AstContinuationFrame`) and routing `resume` through it without changing covered semantics.

## Extensibility Check

- A single `execute_ast_continuation(...)` entrypoint gives the next slice a stable place to plug
  in the first real `AstEvalOutcome::Suspended(...)` producer.
- The new frame is intentionally minimal and transport-oriented, which keeps Step 3.2b free to
- replace one migrated shape without preserving parallel token APIs.
- Existing exploratory replay logic remains behind the frame boundary, so later cleanup can shrink
  old paths one shape at a time.

## Maintainability Check

- `ResumeToken` and `OptimizedResumeToken` no longer need to know about both statement and value
  continuation fields separately.
- The runtime now has one obvious place to look when following resumed control flow:
  `resume_through_ast_continuation_frame` -> `execute_ast_continuation` ->
  `execute_ast_continuation_frame`.
- Because this slice preserves behavior, the full `cargo test -p goby-wasm` suite provides a solid
  regression guard for the refactor boundary itself.

## Security Check

- No new external capability, I/O surface, or unsafe behavior was added.
- The main remaining risk is architectural incompleteness, not safety:
  `AstEvalOutcome::Suspended(...)` still is not emitted by evaluator checkpoints.
- Existing deterministic runtime errors for missing/consumed continuations remain unchanged.

## Issues Found

- The first draft only introduced `AstContinuationFrame` as token storage, which still left
  `AstContinuation` unused at the execution boundary and would have forced another small rename
  pass in the next slice.
- The final version adds `execute_ast_continuation(...)` so the next migration can plug directly
  into the enum used by `AstEvalOutcome::Suspended(...)`.

## Fixes Applied

- Replaced separate token-side statement/value continuation fields with `AstContinuationFrame`.
- Added `current_pending_resume_frame_for_dispatch(...)` to build the transport shape centrally.
- Routed fallback and typed `resume` paths through
  `resume_through_ast_continuation_frame(...)` and `execute_ast_continuation(...)`.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
