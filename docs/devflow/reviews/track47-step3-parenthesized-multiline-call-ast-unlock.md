# Step Review

## Step

- Track 4.7 Step 3 parenthesized multiline call AST unlock.

## Contradiction Check

- The failing `print (\n  case ...\n)` regression initially looked like a runtime
  suspension gap, but the real contradiction was earlier: the declaration never
  reached the AST runtime because `parsed_body` was missing.
- Leaving that parser gap in place would keep some Step 3 runtime slices
  unreachable from natural source shapes and make runtime debugging misleading.

## Extensibility Check

- The parser change is narrow and reuses existing multiline expression forms
  (`case`, `if`) rather than introducing a new call syntax.
- This keeps future AST-path expansion incremental: more runtime slices become
  reachable without adding more fallback-only exceptions.

## Maintainability Check

- The fix is localized to statement parsing and does not touch the suspension
  model itself.
- New parser tests and runtime parity tests lock the exact shape that previously
  escaped to string fallback.

## Security Check

- No new I/O or capability surface was added.
- The main risk is parser over-acceptance, but the implementation only accepts
  parenthesized multiline call arguments when the inner form is already a known
  multiline expression and the closing `)` shape is explicit.

## Issues Found

- `main.parsed_body` became `None` for parenthesized multiline call arguments
  such as `print (\n  case ...\n)`.
- Because of that, fallback runtime resolution bypassed the AST-backed Step 3
  path entirely.

## Fixes Applied

- Added parser support for parenthesized multiline call arguments whose inner
  expression is a multiline `case` or `if`.
- Added parser regressions for both shapes.
- Added wasm runtime regressions proving the source now keeps `parsed_body` and
  preserves fallback/typed parity for handled replay in the selected `case` arm.

## Validation

- `cargo fmt`
- `cargo test -p goby-core`
- `cargo test -p goby-wasm`
- `bash /home/yoshitsugu/.codex/skills/codex-dev-flow/scripts/devflow_review_check.sh docs/devflow/reviews/track47-step3-parenthesized-multiline-call-ast-unlock.md`
- `bash /home/yoshitsugu/.codex/skills/codex-dev-flow/scripts/devflow_step_gate.sh .codex-devflow/commands.env`
