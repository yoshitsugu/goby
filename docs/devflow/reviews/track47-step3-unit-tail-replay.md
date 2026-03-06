# Step Review

## Step

- Track 4.7 Step 3 unit-position statement-tail replay slice.

## Contradiction Check

- The previous runtime claimed Step 3 direction in docs, but `dispatch_handler_method_core`
  still exited on the first observed `resume`, so handler code after `resume` never ran.
- The new slice removes that contradiction for AST-backed unit-position statement tails by
  replaying the saved continuation before handler execution proceeds.
- This remains intentionally partial: value-position progression is still not implemented and is
  recorded as open in `doc/PLAN.md`.

## Extensibility Check

- `AstStmtContinuation` and sequence-owner tracking create a concrete place to extend progression
  toward value-position checkpoints instead of reworking token semantics again.
- Keeping the capture limited to AST-backed unit-position statement sequences narrows the current
  change while preserving a path to broader continuation forms later.
- Fallback and typed mode still share the same externally visible resume/exhaustion contract for
  this slice.

## Maintainability Check

- Continuation capture is localized to statement-sequence helpers rather than scattered across each
  individual effect call site.
- Sequence-owner ids prevent double execution of replayed tails without relying on fragile global
  booleans.
- Existing runtime error kinds remain unchanged, so parity assertions and downstream diagnostics do
  not need a parallel migration.

## Security Check

- No new I/O surface or privilege boundary is introduced.
- The main risk is semantic incompleteness, not safety: value-position resumptions are still open.
- Runtime error behavior for invalid `resume` usage remains deterministic.

## Issues Found

- Replaying a saved tail from inside nested AST loops initially caused either stack overflow or
  double execution of the outer remaining statements.
- Replacing all top-level AST execution with unit-statement helpers regressed broad runtime
  behavior because the top-level path needs richer `ingest_ast_statement` semantics.

## Fixes Applied

- Added `AstStmtContinuation` capture on AST-backed unit-position statement sequences.
- Added sequence-owner tracking so replayed tails unwind the correct enclosing sequence exactly once.
- Restored top-level AST execution to `ingest_ast_statement` semantics and introduced a dedicated
  top-level sequence helper only where continuation capture is needed.
- Added fallback and typed parity tests for replay-then-exhaustion behavior.
