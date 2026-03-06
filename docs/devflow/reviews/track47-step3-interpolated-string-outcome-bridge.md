# Step Review

## Step

- Track 4.7 Step 3 interpolated-string outcome bridge.

## Contradiction Check

- Nested handled values already replayed through the unified outcome boundary in
  calls, pipelines, `if`, `case`, and block bodies, but legacy
  `Expr::InterpolatedString` still evaluated `${...}` segments only through the
  direct evaluator.
- That left a small but real arbitrary-expression gap in Step 3.

## Extensibility Check

- Reusing the existing outcome consumer for interpolation does not add any new
  continuation kind.
- It extends Step 3 to another general expression family rather than another
  call-shape special case.

## Maintainability Check

- The change is small and removes a separate direct recursive evaluator path for
  one composite expression kind.
- Focused regressions lock both fallback and typed parity for effectful
  interpolation.

## Security Check

- No new I/O or capability behavior was introduced.
- Risk is limited to expression-evaluation alignment inside string
  interpolation.

## Issues Found

- Legacy interpolated-string evaluation could not replay handled values inside
  `${...}` segments via the shared suspended-frame consumer.

## Fixes Applied

- Routed legacy `Expr::InterpolatedString` through
  `eval_expr_ast_outcome(...)` plus `complete_ast_value_outcome(...)`.
- Added fallback and typed/fallback parity regressions for
  `print "value=${next 0}"`.

## Validation

- `cargo fmt`
- `cargo test -p goby-wasm interpolated_string_replays_handled_value -- --nocapture`
- `cargo test -p goby-wasm typed_mode_matches_fallback_for_interpolated_string_replay -- --nocapture`
