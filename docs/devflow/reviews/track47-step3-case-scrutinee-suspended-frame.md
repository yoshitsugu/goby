# Step Review

## Step

- Track 4.7 Step 3.2d `case` scrutinee suspended-frame slice.

## Contradiction Check

- After the `if` slice, Step 3 had a real branch/control-flow consumer boundary, but `case`
  still duplicated its selection logic and did not use that boundary.
- Leaving `case` behind would have kept control-flow migration partial in a way that contradicted
  the compact "migrate one shape, then reuse the same frame path" plan.
- This slice removes that contradiction by moving `case` scrutinee replay onto the same suspended
  continuation model.

## Extensibility Check

- `CaseScrutinee` is still a narrow frame shape: it captures only the arm set and reuses the
  resumed scrutinee value.
- Sharing arm selection through `select_case_arm(...)` should also make future `case` work
  cleaner if pattern forms or branch semantics evolve.
- The slice keeps the migration focused on AST paths only, which matches the Step 3 plan and
  avoids mixing in string-fallback cleanup.

## Maintainability Check

- The duplication between outcome-aware and legacy `case` handling is reduced: matching now lives
  in one helper, and legacy `Expr::Case` evaluation delegates through
  `complete_ast_value_outcome(...)`.
- This makes the consumer boundary more credible as a stable abstraction, rather than a special
  case only used by `if`.
- Full `cargo test -p goby-wasm` coverage stayed green.

## Security Check

- No new capability surface was introduced.
- Remaining risk is migration incompleteness: broader call shapes and `resume (op ...)` are still
  outside the unified suspended-frame path.

## Issues Found

- None beyond the expected scope boundary: this slice does not migrate string fallback or arm-body
  nested checkpoints beyond scrutinee replay.

## Fixes Applied

- Added `CaseScrutinee` as a unified value continuation shape.
- Shared case-arm selection in `select_case_arm(...)`.
- Routed legacy `Expr::Case` evaluation through `complete_ast_value_outcome(...)`.
- Added fallback/typed parity coverage for `case` scrutinee replay.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
