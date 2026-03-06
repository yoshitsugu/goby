# Step Review

## Step

- Track 4.7 Step 3 declaration/value-expression AST path slice.

## Contradiction Check

- Step 3 had gained statement-level replay, but important progression examples still fell back to
  the old string evaluator because AST value evaluation did not support value-position `with`
  expressions or general declaration value calls.
- That contradicted the stated Step 3 plan, which explicitly targets AST-backed continuation-aware
  progression.
- This slice removes that contradiction for the covered shapes and makes
  `examples/iterator_unified.gb` runnable through the fallback runtime again.

## Extensibility Check

- `eval_decl_as_value_with_args_ast` creates one AST-backed entrypoint for declaration value calls
  that can later be extended with richer continuation-aware outcomes.
- Supporting value-position `with` by reusing block evaluation keeps the design aligned with the
  existing expression model rather than inventing a special one-off execution path.
- Zero-arity `f ()` and flattened multi-arg calls are both covered, which keeps future declaration
  progression work from splitting by call shape.

## Maintainability Check

- The new logic reuses existing AST expression evaluation rather than duplicating a second ad hoc
  declaration interpreter.
- `iterator_unified.gb` now has locked runtime coverage plus typed/fallback parity coverage, which
  gives a concrete regression target for future Step 3 work.
- The remaining scope boundary is still explicit: arbitrary nested expression checkpoints are not
  yet claimed.

## Security Check

- No new I/O surface or external capability boundary was introduced.
- The main risk remains semantic incompleteness, not safety.
- Existing runtime error behavior and handler dispatch rules remain unchanged for already-covered
  cases.

## Issues Found

- Initial declaration value-call support still returned `None` for `count_values ()` because the AST
  helper treated zero-parameter declarations as arity mismatch when invoked with explicit `Unit`.
- Even after declaration calls were supported, `iterator_unified.gb` still failed because
  value-position `with ... in <expr>` was unsupported and therefore dropped out of the AST runtime.

## Fixes Applied

- Added AST-backed general declaration value-call evaluation, including zero-arity `f ()` and
  flattened multi-arg calls.
- Added value-position `with` evaluation by executing the body as a handler-scoped block
  expression.
- Added regression/parity tests for declaration progression and the iterator unified example.
- Verified fallback CLI output for `examples/iterator_unified.gb` is `tick:atick:btick:c31`.
