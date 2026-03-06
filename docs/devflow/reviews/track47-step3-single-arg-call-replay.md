# Step Review

## Step

- Track 4.7 Step 3 single-arg named-call value continuation slice.

## Contradiction Check

- Step 3 had reached statement/declaration replay, but nested value-position call shapes such as
  `print (id (next 0))` still contradicted the intended progression contract because `resume`
  returned the raw resumed value without replaying the pending call-argument continuation.
- This slice removes that contradiction for the first narrow nested expression shape by carrying a
  saved single-arg named-call continuation on the resume token.
- The scope remains intentionally partial: broader nested expression checkpoints are still open.

## Extensibility Check

- `AstValueContinuation` gives the runtime a dedicated place to add more nested expression replay
  forms without overloading statement continuations further.
- Replaying the value continuation before any statement continuation matches the intended layering:
  finish the interrupted expression first, then continue later statements.
- The new helper for named value-call application centralizes existing call behavior so future
  continuation shapes can reuse it.

## Maintainability Check

- The implementation is narrow and localized:
  - one new value-continuation stack,
  - token storage for that continuation,
  - a single initial continuation kind.
- The regression tests lock both fallback behavior and typed/fallback parity for the new nested
  shape.
- Existing `iterator_unified` and statement-level replay coverage stayed green, which reduces risk
  that the new continuation layer regressed earlier Step 3 slices.

## Security Check

- No new I/O or privilege surface was introduced.
- The main remaining risk is semantic incompleteness, not safety.
- Invalid `resume` behavior still follows the existing deterministic runtime error paths.

## Issues Found

- The initial helper extraction accidentally used `fn_name.as_str()` on an already-`&str` value,
  which does not compile on the current toolchain.
- Without a dedicated value continuation, nested call-argument shapes returned the resumed raw
  operand value instead of replaying the interrupted call expression.

## Fixes Applied

- Added `AstValueContinuation` storage to both fallback and optimized resume tokens.
- Replayed saved single-arg named-call continuations before saved statement continuations.
- Extracted `apply_named_value_call_ast` so replay and normal call evaluation share one path.
- Added fallback and typed parity tests for `print (id (next 0))`.
- Re-ran `cargo test -p goby-wasm` after fixing the helper compile issue.
