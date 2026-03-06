# Step Review

## Step

- Track 4.7 Step 3.2b single-arg suspended-frame slice.

## Contradiction Check

- Step 3.2a established a unified frame entrypoint, but no evaluator path actually emitted
  `AstEvalOutcome::Suspended(...)`.
- That left the new frame boundary architecturally correct but semantically unproven.
- This slice removes that contradiction for the smallest viable nested value-position shape:
  value-only `single-arg named call`.

## Extensibility Check

- `HandlerContinuationState` / `HandlerCompletion` can now distinguish suspended completion, which
  is the minimum contract needed for later `BinOp` and control-flow migrations.
- The suspension trigger is intentionally narrow (`SingleArgNamedCall` frame with no statement
  continuation), which keeps the change surface small while still proving the new path is real.
- Broader replay shapes remain on the old path for now, so Step 3.2c can remove the migrated
  single-arg fallback path before the next shape is added.

## Maintainability Check

- Outcome-aware handling for `Expr::Call` and `Expr::Resume` is now explicit instead of being
  buried in token-side replay only.
- The slice avoids broad breakage by keeping non-target shapes on the old replay path, which makes
  the architectural change reviewable in isolation.
- Full `cargo test -p goby-wasm` coverage stayed green after tightening the suspend trigger to the
  intended narrow frame shape.

## Security Check

- No new I/O or external capability surface was introduced.
- The main risk is partial migration, not safety: old and new replay paths still coexist for now.
- Existing deterministic runtime errors for missing/consumed continuations remain intact.

## Issues Found

- The first draft suspended any frame seen by outcome-aware `resume`, which broke existing
  statement-tail and ordinary handler dispatch behavior.
- That overreach surfaced as `None` runtime outputs and widespread test failures, with many
  follow-on `PoisonError` failures due to the shared environment mutex.

## Fixes Applied

- Added suspended completion to handler continuation state and surfaced it through
  `dispatch_handler_method_as_value_outcome`.
- Made `eval_expr_ast_outcome` handle `Expr::Call` / `Expr::Resume` for the narrow single-arg path.
- Restricted the new suspend trigger to value-only `SingleArgNamedCall` frames and kept other
  shapes on the old replay path.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
