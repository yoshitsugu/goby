# Step Review

## Step

- Track 4.7 Step 1 semantic alignment audit for abortive handlers and `resume`.

## Contradiction Check

- `doc/LANGUAGE_SPEC.md` requires no-`resume` handler completion to abort at the
  handled operation boundary.
- Current runtime already behaves abortively for many value-position calls
  because value dispatch returns `None` when no resumed value is present.
- Current unit-position dispatch still returns success from
  `dispatch_handler_method`, so statements after a handled operation can
  continue running. This is the main semantic contradiction to fix in Step 2.

## Extensibility Check

- Effect-operation entrypoints are concentrated around
  `eval_ast_side_effect`, `eval_expr_ast`, and `execute_unit_expr_ast`, all of
  which funnel through `dispatch_handler_method_core`.
- Introducing an explicit handler-completion outcome at the continuation bridge
  level should scale better than inferring abort from nested `Option` values.
- Keeping fallback and typed-continuation modes on the same bridge contract will
  reduce parity drift before Step 3 multi-resume work.

## Maintainability Check

- `Option<Option<RuntimeValue>>` currently overloads three meanings: resumed
  completion, no-`resume` completion, and execution failure.
- The current design makes unit-position propagation easy to miss and obscures
  intent in tests.
- Refactoring toward an explicit outcome type should make dispatch call sites
  and tests easier to reason about.

## Security Check

- No external I/O or privilege boundary changes are involved in this track.
- The main safety concern is deterministic runtime error handling for invalid
  continuation states; existing error strings must remain stable.
- Handler-stack and resume-token stack balance checks must remain intact during
  the Step 2 refactor.

## Issues Found

- No explicit runtime-level abort state exists today.
- Unit-position handled operations do not yet honor the abortive handler
  contract.
- Typed and fallback modes share semantics only indirectly via current token
  behavior, not through an explicit outcome model.

## Fixes Applied

- Audit completed; no implementation changes in this review file.
