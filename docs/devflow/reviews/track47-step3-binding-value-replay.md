# Step Review

## Step

- Track 4.7 Step 3 direct binding-value replay slice.

## Contradiction Check

- Step 3 already replayed unit-position statement tails, but direct value-position bindings still
  contradicted the language contract: `x = op ...` resumed with a value, then dropped the pending
  local-binding progression instead of continuing to later statements.
- This slice removes that contradiction for direct statement RHS handled operations by restoring the
  resumed value into the target local before replaying the remaining AST statements.
- The scope remains intentionally narrow: deeper nested expression-tree checkpoints are still open.

## Extensibility Check

- `AstStmtContinuationKind` provides a stable place to add more replay forms without overloading a
  single unit-tail shape.
- The dispatch-depth filter keeps continuation capture associated with the direct statement RHS,
  which avoids conflating outer statement replay with inner helper/intrinsic dispatches.
- Fallback and typed mode still share the same externally visible replay contract for this slice.

## Maintainability Check

- Continuation replay stays centralized in `execute_saved_stmt_continuation` instead of scattering
  binding/assignment restore logic across multiple resume paths.
- The new tests cover both the positive replay case and typed/fallback parity, which protects the
  narrow slice from future regressions.
- The implementation avoids claiming general value-position support before the evaluator can truly
  suspend inside nested expression trees.

## Security Check

- No new I/O or external capability surface was introduced.
- The main risk is semantic partiality, not safety: only direct statement RHS replay is covered.
- Existing deterministic runtime error behavior for invalid `resume` usage remains intact.

## Issues Found

- Capturing binding continuations too broadly initially broke many existing runtime tests because
  inner helper-driven dispatches (for example within intrinsic loops) incorrectly replayed the outer
  binding tail on every inner `resume`.
- That bug also poisoned the shared test mutex after the first failure, obscuring the real root
  cause behind many follow-on `PoisonError` failures.

## Fixes Applied

- Added `AstStmtContinuationKind::{BindValue, AssignValue, UnitTail}`.
- Replayed resumed values into locals inside `execute_saved_stmt_continuation`.
- Filtered continuation capture by dispatch depth so only direct statement-RHS handler dispatches
  receive binding/assignment replay checkpoints.
- Added fallback and typed parity tests for direct binding-value replay.
- Re-ran `cargo test -p goby-wasm` after fixing the over-capture regression.
