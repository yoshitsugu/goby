# Step Review

## Step

- Track 4.7 Step 3 recursive AST-outcome groundwork.

## Contradiction Check

- Step 3 required `AstEvalOutcome<T>` to carry future `Suspended(...)` states, but the old
  implementation only wrapped `eval_expr_ast` after the fact.
- That wrapper shape was inconsistent with the planned value-position checkpoints because a child
  suspension inside `if`, `case`, block expressions, or binary operators would be flattened back
  into `Option`.
- The new slice removes that architectural contradiction for the covered composite AST forms while
  intentionally preserving current runtime behavior.

## Extensibility Check

- Recursive outcome propagation through `InterpolatedString`, `BinOp`, `ListLit`, `TupleLit`,
  `Block`, `Case`, and `If` gives a concrete path to add the first real value-position checkpoint
  without another broad evaluator rewrite.
- The fallback runtime still has a single outcome carrier (`AstEvalOutcome<T>`) that typed mode can
  mirror rather than inventing a second ad hoc progression path.
- This keeps Step 3 incremental: suspension producers remain a follow-up slice instead of being
  conflated with the propagation refactor.

## Maintainability Check

- The new helper `ast_outcome_from_option` centralizes `Option` -> `AstEvalOutcome` conversion and
- keeps existing abort/error mapping stable.
- Composite expression branches now spell out how child outcomes are propagated, which reduces
  hidden coupling between future checkpoint code and the legacy `eval_expr_ast` fallback path.
- Existing tests remained green, confirming the refactor did not silently change current semantics.

## Security Check

- No new I/O, external process, or capability surface was added.
- The main risk remains semantic incompleteness: no `Suspended(...)` producer exists yet, so
  multi-resume progression is still partial.
- Existing runtime error behavior stays unchanged because unsupported paths still collapse through
  the same abort/error mapping.

## Issues Found

- The initial refactor used `?` inside `AstEvalOutcome`-returning code paths, which does not
  compile and also hid the intended abort-vs-unsupported mapping.
- A direct fallback expression `self.ast_outcome_from_option(self.eval_expr_ast(...))` triggered a
  borrow conflict because `self` was used mutably and immutably in one expression.

## Fixes Applied

- Replaced the invalid `?` uses with explicit `Option` checks that preserve the existing
  `runtime_aborted` mapping.
- Split the fallback expression path into a temporary variable before converting to
  `AstEvalOutcome`.
- Verified the refactor with `cargo test -p goby-wasm`.
