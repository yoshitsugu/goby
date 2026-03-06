# Step Review

## Step

- Track 4.7 Step 3.2d `if` condition suspended-frame slice.

## Contradiction Check

- The plan pivot for Step 3 said the next work should stop adding token-side replay shapes and make
  `AstEvalOutcome::Suspended(...)` real.
- Before this slice, `single-arg` and `BinOp` could suspend, but there was still no real consumer
  boundary for branch/control-flow paths.
- This slice resolves that gap by making the continuation payload self-contained and consuming it
  through `complete_ast_value_outcome(...)`.

## Extensibility Check

- `if` is the first branch/control-flow boundary on the unified suspended-frame path, which is a
  better precursor to `case` than adding more call-shape special cases.
- Carrying the resumed value inside `AstContinuation` reduces coupling between producer and
  consumer, which is the right direction for later control-flow and nested expression work.
- The slice stays compact: it adds one new migrated shape and reuses the same consumer boundary
  instead of inventing another transport mechanism.

## Maintainability Check

- The main improvement is structural: suspended continuations are now closer to self-contained
  evaluator steps rather than tokens that need an external resumed argument to execute.
- The tests were adjusted to stay on the AST path intentionally, because Step 3 is not targeting
  the string fallback evaluator.
- Full `cargo test -p goby-wasm` stayed green after the change.

## Security Check

- No new external capability or I/O surface was added.
- Remaining risk is architectural incompleteness, not safety: `case`, broader call shapes, and
  `resume (op ...)` still need migration to the same consumer model.

## Issues Found

- The first draft of the test fell through to the legacy string path because `main.parsed_body`
  was `None`, which made the new AST continuation logic look broken when it was not.
- The fix was to move the exercised `if` shape onto an AST declaration path instead of trying to
  force Step 3 guarantees onto the string evaluator.

## Fixes Applied

- Made `AstContinuation` carry the resumed value payload directly.
- Added real suspended-frame consumption through `complete_ast_value_outcome(...)`.
- Migrated `if` condition replay to the unified frame path and added fallback/typed parity tests on
  the AST path.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
