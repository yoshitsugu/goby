# Goby Project State Snapshot

Last updated: 2026-03-08 (session 230)

This file is a restart-safe snapshot for resuming work after context reset.

## Restart Override

If any older note in this file conflicts with the current restart direction,
this section takes priority.

- Current uncommitted work:
  - [doc/PLAN.md](/home/yoshitsugu/src/gitlab.com/yoshitsugu/goby/doc/PLAN.md)
  - [doc/STATE.md](/home/yoshitsugu/src/gitlab.com/yoshitsugu/goby/doc/STATE.md)
- Current Step 3 status:
  - unified suspended-frame replay covers all previously migrated AST value-position
    families (single-arg/multi-arg named calls, receiver-method calls, pipeline,
    `BinOp`, `if`, `case`, nested `resume`, `InterpolatedString`, `TupleLit`,
    `ListLit`, `RecordConstruct`, positional single-field constructor sugar).
  - `eval_expr_ast` `Expr::Call` branch is now fully outcome-bridged (session 218-219):
    all named-call and qualified callee arms route through outcome consumer.
    Dead-code ctor later arm removed.
  - unit-position qualified-call arg evaluation migrated (session 220):
    - `execute_unit_expr_ast` Expr::Qualified arm → outcome consumer
    - `eval_ast_side_effect` Expr::Qualified arm → outcome consumer
  - unit-position Expr::Var call arg evaluation migrated (session 221):
    - `eval_ast_side_effect` Expr::Var arm → outcome consumer (depth=1)
    - `execute_unit_expr_ast` Expr::Var arm → outcome consumer (depth unchanged)
    - regression test added: `resume_replays_bare_var_call_arg_in_side_effect_position`
- Required next restart point:
  - all major unit-position and value-position Expr::Call legacy direct-eval seams are
    now on the outcome-aware path.
  - Step 3 unit-position migration is complete (session 221).
  - Step 3.5 regression test coverage extended (session 222):
    - execute_unit_expr_ast Var arm: locked by `resume_replays_bare_var_call_arg_in_execute_unit_expr_ast_path`
    - BinOpLeft -> BinOpRight nested suspension: locked by `typed_mode_matches_fallback_for_binop_both_operands_suspend`
    - `pending_value_continuations` is still a necessary snapshot mechanism (not dead code)
  - Step 3.5 broader matrix coverage added (session 223):
    - Shape A: handler body sequential value binding with synchronous inner effect call
      (`handler_body_sequential_value_binding_with_inner_effect_call`)
    - Shape B (in-block): declaration called from `in` block that invokes effect
      (`in_block_calls_declaration_that_invokes_effect`)
    - TODO comments added at two Suspended drop sites in dispatch_handler_method_core
      (Stmt::Binding and Stmt::Assign arms) — remaining handler body stmts are silently
      lost when the inner eval truly suspends.
  - Session 224: nested-with handler body binding test added (synchronous dispatch path):
    `handler_body_binding_resumes_via_outer_with_block_synchronous_dispatch`
    Investigation: TODO Suspended drop sites in dispatch_handler_method_core are not
    reachable via normal Goby programs in the current runtime. The async suspension
    path requires should_suspend_resume_outcome(frame)=true inside inner dispatch,
    which needs frame.stmt=None && value continuation present — not triggered here.
  - Session 225: Shape C added — declaration body two-binding progression with value combination:
    `declaration_body_two_binding_progression_value_combination`
    (sum_two n: a=next(n), b=next(a), a+b=3 with n=0)
  - Session 226: Shape E and Shape F added:
    - Shape E: `three_step_in_block_binding_progression` — 3 sequential in-block bindings via
      same handler (x=next(0)=1, y=next(1)=2, z=next(2)=3, output "3")
    - `eval_expr_ast_outcome` now has `Expr::With` arm — handler push/eval-block/pop,
      matching `execute_unit_expr_ast` pattern (TODO: pop before replay is a known limitation)
    - Shape F: `handler_body_with_inner_with_block_value` — inner `with` in statement position
      inside outer handler body; inner step drives 2 bindings; resume(a+b)=6, output "6"
    - Parser limitation discovered: `result = with ... in ...` where `with` is on next indented
      line is NOT parsed (parse_multiline_rhs_expr returns None when rhs=""). Shape F test uses
      statement-position inner with to work around this.
  - Session 227: parser fix for `name =\n  with ...` binding RHS (commit d8f0d2f):
    - Added `name =\n  with ...` and `mut name =\n  with ...` branches in parse_stmts_from_lines.
    - `next_indent` passed to parse_multiline_rhs_expr so `in` aligns with `with`.
    - `!rhs.is_empty()` guard added to existing same-line branches for safety.
    - 3 parser tests added; Shape F test updated to use binding-RHS design.
    - All three binding/assignment forms (`name =`, `mut name =`, `name :=`) now support next-line `with` RHS.
  - Session 228: `name :=\n  with ...` (Assign) parser gap closed (commit 314adcf):
    - New branch in parse_stmts_from_lines using split_once(":=") + is_non_reserved_identifier guard.
    - Parser test `parses_next_line_with_rhs_in_assignment` added.
    - All parser gaps for next-line `with` binding RHS are now resolved.
  - Session 229: Step 3.5 matrix coverage extended:
    - Shape G: `assignment_rhs_next_line_with_block_value` — `name :=\n  with ...` runtime test (commit 4a7d921).
    - Shape H: `case_arm_body_calls_effect_operation` — effect call in case arm body (commit fb5be59).
    - Discovered and fixed: `eval_ast_side_effect` evaluated `print`/`println`/`|> print` args via
      `eval_ast_value` (non-outcome path). Declaration bodies calling effects inside print args
      produced None. Migrated to `eval_expr_ast_outcome` + `complete_ast_value_outcome` at depth=1.
    - Shape I: `declaration_block_body_with_binding_and_effect_call` — exercises the fixed path (commit 00e3e00).
    - goby-wasm tests: 192 → 195 passing.
  - Session 230: eval_ast_side_effect arg evaluation paths migrated:
    - Shape J: `interpolated_string_with_declaration_body_effect_call` (commit 9dfd496).
    - Pipeline `value` arm in `eval_ast_side_effect` migrated to `eval_expr_ast_outcome` (commit 8024716).
    - Shape K: `pipeline_value_with_declaration_body_effect_call` (commit 8024716).
    - Remaining `eval_ast_value` calls in `eval_ast_side_effect`: Binding/Assign value (frame-covered)
      and `Expr::With` handler expr (no effect call needed) — both correct as-is.
    - goby-wasm tests: 195 → 197 passing.
  - Session 231: parity test batch (commits d3271dd, d42b20f, 77f1812, 05c86a7, 0091bd4):
    - typed_mode_matches_fallback_for_with_captures_lexical_local
    - typed_mode_matches_fallback_for_nested_with_nearest_handler_wins
    - typed_mode_matches_fallback_for_qualified_effect_call_dispatch
    - typed_mode_matches_fallback_for_pipeline_effect_call_dispatch
    - typed_mode_matches_fallback_for_basic_with_inline_handler_dispatch
    - typed_mode_matches_fallback_for_resume_outside_handler_error
    - typed_mode_matches_fallback_for_multi_arg_effect_op_call
    - goby-wasm tests: 197 → 206 passing.
    - All Shape A–K tests confirmed to use assert_mode_parity already.
  - Next: dispatch_handler_method_core for-loop replacement (Step 3 architecture), or more Step 3.5 coverage.
- External internal records:
  - devflow notes live outside the repo under
    `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
  - use that location for plan/review continuation; do not recreate repo-local
    `docs/devflow` or `.codex-devflow`.

## 1. Current Architecture

- Rust Cargo workspace (root `Cargo.toml`).
- Crates:
  - `crates/goby-core` (language core: AST/parser/typechecker).
  - `crates/goby-cli` (CLI entrypoint).
- `crates/goby-wasm` (Wasm backend with native+fallback dual path).

## 2. Locked MVP Decisions

- First backend target is Wasm.
- Runtime model (current): prefer native lowering (`supports_native_codegen` + `lower`),
  fallback to compile-time interpreter (`resolve_main_runtime_output`) for unsupported subsets.
- Entry function is `main` only.
- `main` type is `Unit -> Unit`; annotation required for `run`, optional for `check`.
- CLI commands:
  - `run`: parse + typecheck + requires `main` + emits Wasm + executes via `wasmtime run <path>`.
    Wasm module exports `_start` (WASI Preview 1 standard); no `--invoke` flags needed.
  - `check`: parse + typecheck (no runtime entry requirement).
- Statement separator is newline or `;`.
- Generic type application syntax: Haskell-style spacing (`List Int`, `TypeX a b`).
- Indentation-based blocks accept tabs and spaces (mixing allowed in MVP).
- Function calls: `f x` and `f(x)`; callee can be bare identifier or qualified name.
- Block-local binding semantics: `name = expr` is binding only; re-binding shadows; declaration-local.
- Operator precedence: `|>` < `+` < `*` < call/application; all left-associative.
  - `+` and `*` require spaces on both sides in MVP.
- Legacy `using` / top-level `handler ... for ...` syntax is rejected by
  `goby-cli check/run` by default (2026-03-04); migration diagnostics point to
  canonical `with`.
- String escape sequences: `\n`, `\t`, `\\`, `\"` expanded at parse time via `unescape_string`.
- String interpolation `${...}` is parsed into `Expr::InterpolatedString`.
  Runtime stringifies embedded expression values when materializing the final `String`.
- `case` arm separator: ` -> `; parsed by `split_case_arm` (safe for lambda bodies).
- `case` arms support both inline and block bodies:
  - inline: `pattern -> expr`
  - block: `pattern ->` + deeper-indented statements, where the last expression is the arm value.
- Current implementation `CasePattern` variants:
  `IntLit(i64)`, `StringLit(String)`, `BoolLit(bool)`, `EmptyList`,
  `ListPattern { items, tail }`, `Wildcard`.
- List `case` patterns are implemented:
  - `[]`
  - fixed/prefix list patterns (e.g. `[1]`, `[_, _]`, `[a, b]`)
  - head/tail with rest bind/ignore (e.g. `[a, ..b]`, `[4, ..]`)
  - `_` is wildcard/non-binding in list items and tail binder position (`.._`).
- Native Wasm capability checker still treats list patterns as unsupported,
  so list-pattern `case` executes via fallback runtime path.
- MVP built-ins: `print`, `fetch_env_var`, `string.split`, `list.join`.
  - list mapping path is being consolidated on stdlib `goby/list.map`.
- `print` is an internal runtime-resolved operation; `DefaultStdioPrintHandler`-equivalent behavior
  is compiler/runtime-owned and not required to appear as a user-visible stdlib handler definition.
- `examples/function.gb` expected runtime output (locked):
  - `90`
  - `[30, 40, 50]`
  - `[60, 70]`
  - `something`
  - `15`

## 3. Known Open Decisions

- MVP locked subset remains complete for currently implemented syntax.
- Remaining open point for list `case` patterns:
  - native lowering support (capability checker + native evaluator path).
- List spread expression task (2026-03-06) status:
  - expression-side list spread/concat syntax (`[a, b, ..xs]`) is implemented across
    AST/parser/typecheck/fallback runtime.
  - native lowering currently routes spread-containing modules to fallback path
    (safe parity boundary).
- Map consolidation task (2026-03-06) status:
  - callsites are migrated to stdlib-imported `goby/list.map` usage.
  - native-lowering/fallback capability checker `map`-specific builtin paths are trimmed.
  - runtime list-evaluator `map` shortcut is now gated behind selective import
    (`import goby/list ( map )`) to avoid builtin-only behavior.
- Post-MVP open items tracked in `doc/PLAN.md` §3 and §4.
- Post-MVP effect implementation direction is now fixed in `doc/PLAN.md` §2.3:
  - deep handlers with one-shot resumptions,
  - selective CPS + evidence passing lowering,
  - compiled `EffectId`/`OpId` dispatch (no map lookup on hot path),
  - phased Wasm lowering (portable trampoline first, typed-continuation optimization later).
- `PLAN_EFFECT_RENEWAL` completion status:
  - P6 removal is complete (2026-03-04).
  - parser/runtime/typecheck legacy compatibility paths are removed.
  - follow-up work moved to post-MVP tracks in `doc/PLAN.md`.

## 4. Recent Milestones

Recent (detailed):

- 2026-03-07 (session 222): Track 4.7 Step 3.5 suspension-path regression tests added.
  - Audit finding: `pending_value_continuations` is a necessary snapshot mechanism, not dead code.
    BinOpLeft replay itself re-pushes BinOpRight continuation during right-operand evaluation.
  - New tests:
    - `resume_replays_bare_var_call_arg_in_execute_unit_expr_ast_path`: locks execute_unit_expr_ast
      Var arm via declaration body route (not eval_ast_side_effect).
    - `typed_mode_matches_fallback_for_binop_both_operands_suspend`: locks BinOpLeft -> BinOpRight
      nested suspension with order-sensitive oracle (output "01016").
  - validation: `cargo test --workspace` (186 goby-wasm, 332 others)
  - commit: f8b1866

- 2026-03-07 (session 221): Track 4.7 Step 3 unit-position Expr::Var call arg migrated.
  - runtime:
    - `eval_ast_side_effect` Expr::Var arm: replaced `eval_ast_value(arg)` with
      `eval_expr_ast_outcome + complete_ast_value_outcome` (depth=1, matching Qualified arm).
    - `execute_unit_expr_ast` Expr::Var arm: replaced `eval_expr_ast(arg, ...)` with
      `eval_expr_ast_outcome + complete_ast_value_outcome` (same depth, locals, callables).
  - added regression test: `resume_replays_bare_var_call_arg_in_side_effect_position`
    — exercises `log (next 0)` pattern with suspension and replay.
  - validation completed:
    - `cargo fmt`
    - `cargo clippy -p goby-wasm -- -D warnings`
    - `cargo test --workspace` (184 goby-wasm, 332 others)
  - commits: 2cd398f (Step 1), 876295f (Step 2)

- 2026-03-07 (session 220): Track 4.7 Step 3 unit-position qualified-call arg migrated.
  - runtime:
    - `execute_unit_expr_ast` and `eval_ast_side_effect` Expr::Qualified call arms now
      evaluate arg via `eval_expr_ast_outcome` + `complete_ast_value_outcome` instead of
      legacy `eval_ast_value` / `eval_expr_ast`.
    - all qualified-call arg evaluation paths are now outcome-aware.
  - validation completed:
    - `cargo fmt`
    - `cargo clippy -p goby-wasm -- -D warnings`
    - `cargo test -p goby-wasm` (183 passed)

- 2026-03-07 (session 219): Track 4.7 Step 3 legacy `Expr::Qualified` callee arm bridged.
  - runtime:
    - `eval_expr_ast` `Expr::Call` / `Expr::Qualified` arm no longer evaluates arg via
      `eval_expr_ast` directly; now delegates to `eval_expr_ast_outcome` +
      `complete_ast_value_outcome`.
    - dispatch precedence (`runtime bridge → handler match → member-name fallback →
      embedded default`) is preserved through `apply_receiver_method_value_call_ast_outcome`.
    - all value-position `Expr::Call` legacy direct-eval arms are now bridged.
  - validation completed:
    - `cargo fmt`
    - `cargo clippy -p goby-wasm -- -D warnings`
    - `cargo test -p goby-wasm` (183 passed)

- 2026-03-07 (session 218): Track 4.7 Step 3 legacy `Expr::Call` named-call seams bridged.
  - runtime:
    - `eval_expr_ast` `Expr::Call` `flatten_named_call` multi-arg arm no longer evaluates
      args via legacy `eval_expr_ast`; now routes through `eval_expr_ast_outcome` +
      `complete_ast_value_outcome`.
    - `eval_expr_ast` `Expr::Call` `Expr::Var` single-arg arm similarly routes through
      outcome consumer instead of `eval_expr_ast` + `apply_named_value_call_ast`.
    - `flatten_named_call` arm gained `args.len() > 1` guard to match outcome-path split
      (previously, single-arg calls were intercepted by this arm before the Var arm).
  - result:
    - all plain named-call fallback seams in legacy `eval_expr_ast` are now on the
      outcome-aware path, matching the pattern used by `Pipeline`, `RecordConstruct`,
      `ListLit`, `TupleLit`, `InterpolatedString`, and other migrated shapes.
  - validation completed:
    - `cargo fmt`
    - `cargo clippy -p goby-wasm -- -D warnings`
    - `cargo test -p goby-wasm` (183 passed)

- 2026-03-06 Step 3 snapshot:
  - unified suspended-frame progression now covers the main AST value-position families already
    migrated in runtime:
    - named calls,
    - receiver/method one-arg calls,
    - pipelines,
    - `BinOp`,
    - `if`,
    - `case`,
    - nested `resume`.
  - recent cleanup slices also removed the most obvious shared replay seams for:
    - single-arg named calls,
    - `BinOp`,
    - pipelines.
  - current restart focus:
    - this snapshot is superseded by `Restart Override` above when there is any conflict.
    - cleanup slices are now paying less than before.
    - prefer continuing from the remaining legacy `Expr::Call` direct-eval seams before inventing
      broader new targets.

- 2026-03-06 (session 179): Track 4.7 Step 1 and partial Step 2 completed.
  - Planning/devflow:
    - initialized `.codex-devflow/` and `docs/devflow/` artifacts.
    - added Track 4.7 execution plan and committed plan/audit checkpoints.
  - Step 1 semantic audit:
    - confirmed handled operation dispatch currently enters through
      `eval_ast_side_effect`, `eval_expr_ast`, and `execute_unit_expr_ast`,
      converging on `dispatch_handler_method_core`.
    - confirmed value-position no-`resume` already behaved abortively via
      implicit `None`, while unit-position no-`resume` still continued.
  - Step 2 partial implementation:
    - added explicit handler continuation/completion state in
      `crates/goby-wasm/src/lib.rs`.
    - added resolver-level `runtime_aborted` propagation so no-`resume`
      handlers stop execution at the handled operation boundary in both
      value-position and unit-position calls.
    - preserved existing deterministic runtime errors for invalid resume usage.
  - Tests/examples:
    - added runtime + typed-mode parity tests for value-position/unit-position
      abortive handlers.
    - updated continuation-intended fixtures/examples to use explicit
      `resume ()` (including `examples/effect.gb` and list-each runtime tests).
  - Remaining within Step 2:
    - nested-handler abort propagation review/coverage is still open
      (`doc/PLAN.md` Step 2.10 / 2.11 follow-up).

- 2026-03-06 (session 180): Track 4.7 Step 2 follow-up and Step 4 completed.
  - Parser/runtime alignment:
    - fixed handler-clause body parsing to preserve relative indentation for
      nested blocks (`with`, `if`, `case`) instead of trimming it away.
    - added parser regression `parses_nested_with_inside_handler_clause_body`.
  - Runtime:
    - nested abortive handlers now propagate as explicit abortive completion
      through enclosing handler dispatch instead of collapsing to generic `None`.
    - added fallback + typed-mode parity tests for nested abortive handler
      propagation.
  - Typecheck:
    - removed the conservative syntactic "multiple `resume`" rejection.
    - retained `resume` placement and type-compatibility checks.
    - `examples/iterator_unified.gb` now typechecks via the normal path because
      valid multi-branch handler bodies are no longer blocked.
  - Quality gates:
    - `cargo fmt`, `cargo clippy -- -D warnings`, and `cargo test` all pass.
  - Remaining within Track 4.7:
    - Step 3 multi-resume progression runtime semantics is still pending.

- 2026-03-06 (session 181): legacy syntax cleanup for `with` / `()`.
  - removed remaining stdlib/examples/tooling references to legacy
    `with_handler`; canonical handler application syntax is now `with` only.
  - removed runtime compatibility paths that treated expression-form `Unit` as a
    value; canonical Unit value syntax is `()`.
  - added explicit typecheck diagnostic:
    - `legacy_unit_value_syntax: \`Unit\` is no longer a value expression; use \`()\``
  - updated parser/runtime/typecheck tests and syntax-pack docs accordingly.

- 2026-03-06 (session 182): Track 4.7 Step 3 investigation recorded before implementation.
  - no code changes landed in this session; the goal was restart-safe planning.
  - `doc/PLAN.md` §4.7 now records the concrete blocker and staged implementation
    plan for multi-resume progression.
  - confirmed runtime blocker:
    - current `resume` bridge in `crates/goby-wasm/src/lib.rs` is one-shot and
      returns only a plain resumed value.
    - `dispatch_handler_method_core` stops after the first observed resume and
      does not retain caller continuation checkpoints.
    - therefore Step 3 requires continuation-aware AST runtime results, not a
      token-field tweak.
  - confirmed next entry points:
    - `eval_expr_ast`
    - `execute_unit_expr_ast`
    - `execute_unit_ast_stmt`
    - `dispatch_handler_method_core`
    - `resume_through_active_continuation_fallback`
    - `resume_through_active_continuation_optimized`
  - recommended restart order:
    1. introduce explicit continuation/suspend result types in the AST runtime,
    2. implement fallback progression first,
    3. mirror semantics in typed mode,
    4. add parity/exhaustion tests,
    5. rerun full quality gates.

- 2026-03-06 (session 183): Track 4.7 Step 3 groundwork started in runtime.
  - introduced `AstEvalOutcome<T>` in `crates/goby-wasm/src/lib.rs` as the
    explicit AST-runtime outcome shape for Step 3.
  - refactored handler-dispatch statement execution to branch on explicit AST
    outcomes (`Complete` / `Aborted` / future `Suspended`) instead of relying
    only on `Option` returns plus resume-token side observation.
  - current limitation:
    - `Suspended` is scaffolded but not emitted yet.
    - real continuation checkpoints and multi-resume progression semantics are
      still pending.
  - validation completed:
    - `cargo fmt`
    - `cargo check`
    - `cargo test -p goby-wasm`

- 2026-03-06 (session 184): Track 4.7 Step 3 unit-position replay slice landed.
  - runtime:
    - resume tokens now optionally capture AST statement-tail continuations
      (`AstStmtContinuation`) for unit-position statement sequences.
    - `resume` can execute the saved remaining statements before handler-body
      execution continues.
    - `dispatch_handler_method_core` no longer exits immediately after the first
      observed `resume`; handler code after `resume` can now run.
    - sequence-owner tracking prevents replayed statement tails from being
      executed twice by enclosing AST loops.
  - coverage:
    - added fallback runtime regression for:
      - replaying remaining unit statements after `resume`,
      - deterministic consumed-continuation error on a second `resume`.
    - added typed-mode parity coverage for the same slice.
  - still open within Step 3:
    - value-position continuation checkpoints are not implemented.
    - no general `Suspended(...)` AST result is emitted yet; current progression
      support is limited to unit-position statement-tail replay.
  - validation completed:
    - `cargo fmt`
    - `cargo check`
    - `cargo test -p goby-wasm`

- 2026-03-06 (session 185): Track 4.7 Step 3 recursive AST-outcome groundwork.
  - runtime:
    - `crates/goby-wasm/src/lib.rs` `eval_expr_ast_outcome` now evaluates
      composite AST forms recursively instead of only wrapping `eval_expr_ast`.
    - covered composite forms:
      - interpolated strings,
      - binary operators,
      - list / tuple literals,
      - block expressions,
      - `case`,
      - `if`.
    - this does not emit `Suspended(...)` yet, but it establishes the control-flow
      shape needed for future value-position checkpoint propagation.
  - reason for landing this slice:
    - the current Step 3 blocker is not only token storage; value-position
      progression needs child-expression outcomes to bubble through composite AST
      nodes without collapsing back to `Option`.
    - the previous wrapper-only `eval_expr_ast_outcome` would have forced another
      large refactor before any value-position suspension producer could be added.
  - validation completed:
    - `cargo test -p goby-wasm`
  - immediate next step:
    - introduce the first real value-position suspension producer/checkpoint,
      likely starting from a narrowly scoped composite expression boundary
      (`BinOp`, call-argument chain, or `if` branch progression) and mirror the
      same external behavior in typed mode.

- 2026-03-06 (session 186): Track 4.7 Step 3 direct binding-value replay slice.
  - runtime:
    - `AstStmtContinuation` now distinguishes:
      - plain unit tail replay,
      - binding-value replay,
      - assignment-value replay.
    - `resume` can restore the resumed value into a direct statement RHS binding /
      assignment and then execute the remaining AST statements.
    - continuation capture is now filtered by dispatch depth so inner helper
      dispatches (for example intrinsic-driven handler calls inside a larger RHS)
      do not incorrectly steal the outer statement continuation.
  - coverage:
    - added fallback regression for:
      - `x = next 0; y = next x; print y` style replay through later statements.
    - added typed/fallback parity coverage for the same direct binding-value replay.
  - still open within Step 3:
    - deeper value-position checkpoints inside nested expression trees are still
      not implemented.
    - no general `Suspended(...)` producer exists yet; replay is still driven by
      captured statement continuations.
  - validation completed:
    - `cargo test -p goby-wasm binding_value`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - extend value-position progression beyond direct statement RHS into nested
      composite expressions (`if`, `case`, call chains, etc.) using the recursive
      `AstEvalOutcome` groundwork from session 185.

- 2026-03-06 (session 187): Track 4.7 Step 3 declaration/value-expression slice.
  - runtime:
    - AST value evaluation now supports value-position `with ... in <expr>` by
      evaluating the `with` body as a handler-scoped block expression.
    - AST value evaluation now supports general declaration calls as values:
      - single-arg declarations,
      - zero-arity declarations invoked as `f ()`,
      - flattened multi-arg named calls.
    - this keeps progression-relevant declaration bodies on the AST path instead
      of dropping back to the old string evaluator.
  - user-visible outcome:
    - `examples/iterator_unified.gb` now resolves in the fallback runtime path
      and via `goby-cli run`, producing locked output `tick:atick:btick:c31`.
  - coverage:
    - added fallback regression for declaration value-call progression.
    - added typed/fallback parity regression for the same shape.
    - added locked runtime-output coverage for `examples/iterator_unified.gb`.
    - added typed/fallback parity coverage for the iterator unified example shape.
  - still open within Step 3:
    - continuation progression still relies on statement-level replay, not a
      general `Suspended(...)` producer.
    - nested intermediate checkpoints inside arbitrary expression trees
      (`resume (op ...)`, arithmetic/call subexpressions, etc.) are still open.
  - validation completed:
    - `cargo test -p goby-wasm declaration_value_call_replays_nested_binding_progression`
    - `cargo test -p goby-wasm locks_runtime_output_for_iterator_unified_gb`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_iterator_unified_example_shape`
    - `cargo run -p goby-cli -- run examples/iterator_unified.gb`
  - immediate next step:
    - move from statement/declaration-level replay toward true nested
      expression-level checkpoints, likely starting with direct call-argument or
      branch subexpression suspension.

- 2026-03-06 (session 188): Track 4.7 Step 3 single-arg call continuation slice.
  - runtime:
    - resume tokens now optionally carry an AST value continuation in addition
      to statement continuations.
    - the first supported value continuation shape is a single-argument named
      call replay (`id (next 0)`-style nested value-position continuation).
    - on `resume`, the resumed value is first threaded through the saved
      single-arg call continuation and only then through any saved statement
      continuation.
  - coverage:
    - added fallback regression for:
      - `print (id (next 0))` style nested value-position replay.
    - added typed/fallback parity coverage for the same nested call-argument slice.
  - still open within Step 3:
    - no general `Suspended(...)` producer exists yet.
    - nested checkpoints are still partial:
      - single-arg named calls are covered,
      - binops, branch subexpressions, `resume (op ...)`, and broader call shapes
        are still pending.
  - validation completed:
    - `cargo test -p goby-wasm single_arg_call_value_replay`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - extend value continuations beyond single-arg named calls to another
      composite expression boundary, likely binop or branch progression.

- 2026-03-06 (session 189): Track 4.7 Step 3 binop operand replay slice.
  - runtime:
    - `AstValueContinuationKind` now also supports:
      - left-operand binop replay,
      - right-operand binop replay.
    - handled operations suspended inside direct `BinOp` operands now replay
      through the interrupted arithmetic/equality expression before returning to
      outer flow.
  - coverage:
    - added fallback regression for:
      - left operand replay (`next 0 + 4`),
      - right operand replay (`4 + next 0`).
    - added typed/fallback parity coverage for the same binop operand slice.
  - still open within Step 3:
    - no general `Suspended(...)` producer exists yet.
    - nested checkpoints still do not cover:
      - `resume (op ...)`,
      - multi-arg / non-named call subexpressions,
      - branch/case re-entry,
      - deeper general expression-tree replay.
  - validation completed:
    - `cargo test -p goby-wasm resume_replays_binop_left_operand_continuation`
    - `cargo test -p goby-wasm resume_replays_binop_right_operand_continuation`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_binop_operand_replay`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - extend nested value continuations to a control-flow boundary (`if` or
      `case`) or to `resume (op ...)`, depending on which gives the smallest
      checkpoint shape.

- 2026-03-06 (session 190): Track 4.7 Step 3 strategy pivot recorded.
  - planning decision:
    - Step 3 should no longer continue by adding more expression-shape-specific
      replay variants on top of the current token-side continuation machinery.
    - the slices completed in sessions 184-189 remain useful as working semantic
      proof points, but they are now treated as exploratory scaffolding rather
      than the desired final architecture.
  - locked rationale:
    - Goby is still in an early personal concept-validation phase, so a cleaner
      destructive refactor is preferred over preserving incremental machinery
      that would make future language changes expensive.
    - the current split between `AstStmtContinuation` and
      `AstValueContinuationKind::*` would likely expand across `if`, `case`,
      broader call shapes, and `resume (op ...)`, increasing change surface.
  - updated implementation target:
    - make `AstEvalOutcome::Suspended(Box<...>)` a real runtime result.
    - shift Step 3 toward unified continuation frames that model "what to do
      next" rather than AST-shape-specific replay enums.
    - keep the refactor compact:
      - define the frame interface first,
      - convert exactly one small nested value-position shape,
      - then shrink/remove the replaced old replay path before moving on.
    - prefer `single-arg call` as the first suspended-frame conversion because
      it is smaller than `BinOp`.
  - immediate next step:
    - begin Step 3.2a in `crates/goby-wasm/src/lib.rs`:
      - define the minimal unified continuation-frame interface,
      - keep handler/token state as transport only,
      - avoid adding any new ad hoc replay variant in the same slice.
    - restart reading order:
      - `doc/PLAN.md` Step 3 quick view
      - `doc/PLAN.md` Step 3.2 implementation order
      - then this session note

- 2026-03-06 (session 191): Track 4.7 Step 3.2a frame-entry groundwork landed.
  - runtime:
    - `ResumeToken` / `OptimizedResumeToken` now store one AST continuation transport field
      (`AstContinuationFrame`) instead of separate statement/value continuation fields.
    - `resume` now enters AST continuation replay through a single entrypoint:
      - `resume_through_ast_continuation_frame`
      - `execute_ast_continuation`
      - `execute_ast_continuation_frame`
    - the new frame currently wraps the existing exploratory statement/value replay machinery, so
      semantics are unchanged for covered slices.
  - why this matters:
    - Step 3.2a required a minimal responsibility boundary where handler/token state is transport
      only and continuation replay logic lives behind one AST continuation interface.
    - this reduces the amount of token-shape churn needed when Step 3.2b migrates the first real
      nested checkpoint to `AstEvalOutcome::Suspended(...)`.
  - still open:
    - evaluator paths still do not emit `AstEvalOutcome::Suspended(...)`.
    - inner expression checkpoints are still modeled by exploratory ad hoc replay shapes.
    - statement/value replay are not yet unified beyond the transport boundary.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - start Step 3.2b by migrating the `single-arg call` nested value-position slice to suspend via
      the new frame entrypoint instead of the old token-only replay path.

- 2026-03-06 (session 192): Track 4.7 Step 3.2b single-arg suspended-frame slice landed.
  - runtime:
    - `eval_expr_ast_outcome` now handles `Expr::Call` and `Expr::Resume` through outcome-aware
      branches for the narrow `single-arg named call` path.
    - `HandlerContinuationState` / `HandlerCompletion` now distinguish suspended completion in
      addition to resumed and abortive completion.
    - when `resume` sees a value-only `SingleArgNamedCall` frame, it now returns
      `AstEvalOutcome::Suspended(Box<AstContinuation>)` instead of immediately replaying that frame
      inside the token bridge.
    - `dispatch_handler_method_as_value_outcome` can now surface that suspension back to the
      caller-side outcome evaluator.
  - scope boundary:
    - this slice is intentionally narrow.
    - statement continuations and mixed frame shapes still execute through the old replay path so
      previously landed Step 3 behavior remains stable.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - start Step 3.2c by deleting or collapsing the remaining old
      `SingleArgNamedCall` token-only replay path now that the outcome path can suspend it
      directly.

- 2026-03-06 (session 193): Track 4.7 Step 3.2c single-arg legacy replay cleanup landed.
  - runtime:
    - legacy `eval_expr_ast` no longer pushes `SingleArgNamedCall` replay checkpoints for plain
      named calls.
    - shared continuation replay still keeps a narrow guard for `SingleArgNamedCall`, but that path
      now delegates back through the outcome-aware named-call application instead of duplicating the
      old direct replay behavior.
  - result:
    - the single-arg migrated shape now depends on the new outcome/suspension path for checkpoint
      capture.
    - old/new dual-path overlap for that shape is smaller, while broader continuation shapes remain
      unchanged.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - choose the next migrated shape for Step 3.2d.
    - `BinOp` is the natural next target because it already has a bounded continuation shape and
      existing parity tests.

- 2026-03-06 (session 194): Track 4.7 Step 3.2d `BinOp` cleanup landed.
  - runtime:
    - legacy `eval_expr_ast` no longer pushes `BinOpLeft` / `BinOpRight` replay checkpoints.
    - `eval_expr_ast_outcome` now owns `BinOp` checkpoint capture for the migrated shape.
    - value-only `BinOp` frames can suspend via the new outcome path, while shared replay fallback
      remains temporarily in place for compatibility during the migration.
  - result:
    - `BinOp` now follows the same migration pattern as `single-arg named call`:
      - real suspended-frame path for outcome-aware evaluation,
      - legacy checkpoint capture removed,
      - only a narrow shared replay seam still left.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - migrate the first branch/control-flow boundary.
    - `if` is the smallest next target because it has one condition checkpoint and two branch-entry
      checkpoints, without `case` arm matching complexity.

- 2026-03-06 (session 195): Track 4.7 Step 3.2d `if` condition suspended-frame slice landed.
  - runtime:
    - `AstContinuation` now carries the resumed value payload directly instead of relying on an
      external argument at consume time.
    - `complete_ast_value_outcome(...)` now acts as the first real suspended-frame consumer on the
      AST value path.
    - `eval_expr_ast_outcome` / `execute_saved_value_continuation(...)` cover `if` condition replay
      through the unified frame path.
  - result:
    - the first branch/control-flow boundary now suspends and resumes through the new model.
    - Step 3 no longer relies on token-only transport for this slice; the continuation payload is
      self-contained enough to re-enter evaluation.
    - regression coverage now locks fallback/typed parity for `if` condition replay on an AST
      declaration path.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm if_condition_replay -- --nocapture`
    - `cargo test -p goby-wasm single_arg_call_value_replay -- --nocapture`
    - `cargo test -p goby-wasm binop_operand_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - extend the same consumer boundary to the next branch/control-flow shape.
    - `case` is the natural next target; after that, revisit broader call shapes and
      `resume (op ...)`.

- 2026-03-06 (session 196): Track 4.7 Step 3.2d `case` scrutinee suspended-frame slice landed.
  - runtime:
    - added `CaseScrutinee` as the next unified frame-backed value continuation shape.
    - `case` arm selection is now shared through `select_case_arm(...)` instead of duplicated
      across legacy and outcome-aware paths.
    - legacy `eval_expr_ast` for `Expr::Case` now completes through `complete_ast_value_outcome(...)`,
      so the AST value path uses the same suspended-frame consumer boundary as `if`.
  - result:
    - both branch/control-flow entry points currently targeted by Step 3 (`if` condition and
      `case` scrutinee) now suspend and resume through the unified frame path.
    - the next migration target can move away from control-flow and focus on broader call shapes or
      `resume (op ...)` without reopening token-only transport just for branching.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm case_scrutinee_replay -- --nocapture`
    - `cargo test -p goby-wasm if_condition_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - reuse the same consumer boundary for broader call shapes / nested call chains.
    - after that, revisit `resume (op ...)` as the remaining nested progression gap.

- 2026-03-06 (session 197): Track 4.7 Step 3.2d multi-arg named-call replay slice landed.
  - runtime:
    - added `MultiArgNamedCall` as a unified value continuation shape for direct named-call
      argument lists.
    - `eval_named_call_args_outcome(...)` now evaluates flattened multi-arg call chains
      incrementally and suspends through the shared frame path when an argument hits a handled
      operation.
    - resumed multi-arg argument evaluation now returns to existing
      `apply_named_value_call_ast(...)`, so the slice changes checkpoint transport without widening
      call semantics.
  - result:
    - broader call-shape migration has started without reopening token-only replay.
    - Step 3 now covers nested progression on:
      - single-arg named calls,
      - multi-arg named-call argument lists,
      - `BinOp`,
      - `if`,
      - `case`.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm multi_arg_named_call_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - revisit `resume (op ...)` as the remaining high-value nested progression gap.
    - if call-shape work continues first, target non-direct callees or mixed call/effect chains.

- 2026-03-06 (session 198): Track 4.7 Step 3.2d nested `resume (...)` slice landed.
  - runtime:
    - added `ResumeValue` as the next unified value continuation shape.
    - `Expr::Resume` now routes through `eval_expr_ast_outcome(...)` +
      `complete_ast_value_outcome(...)` instead of the old direct bridge path.
    - old non-outcome resume bridge helpers were removed after the AST runtime no longer used them.
  - result:
    - nested `resume (op ...)` now re-enters through the unified suspended-frame consumer path.
    - existing double-resume error coverage now exercises the new nested resume-value route rather
      than the legacy direct bridge.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_double_resume_error -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - revisit non-direct callees or mixed call/effect chains as the next nested progression gap.
    - keep shrinking any remaining direct token-only replay seams when a migrated shape no longer
      needs them.

- 2026-03-06 (session 199): Track 4.7 Step 3.2d receiver/method-call slice landed.
  - runtime:
    - added `ReceiverMethodCall` as a unified continuation shape for one-arg non-direct callees.
    - direct receiver-method value dispatch now has an outcome-aware helper, shared by the call
      site and resumed continuation path.
    - declaration value evaluation now completes through `complete_ast_value_outcome(...)`, which
      keeps migrated nested shapes on the AST outcome path instead of dropping back to legacy
      `eval_expr_ast`.
  - result:
    - non-direct callee migration has started without introducing another token-only replay path.
    - receiver/method-call arguments now join the same suspended-frame model already used by named
      calls, `resume (...)`, `BinOp`, `if`, and `case`.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm receiver_method_call_argument_replay -- --nocapture`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_double_resume_error -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - revisit pipeline or other mixed call/effect chains as the next non-direct nested gap.
    - continue shrinking any remaining legacy direct-eval seams that are no longer needed.

- 2026-03-06 (session 200): Track 4.7 Step 3.2d pipeline-value slice landed.
  - runtime:
    - added `PipelineCall` as a unified continuation shape for `value |> callee`.
    - `Expr::Pipeline` on the AST value path now routes through `eval_expr_ast_outcome(...)` /
      `complete_ast_value_outcome(...)` instead of the old direct evaluator path.
    - resumed pipeline values re-enter through the same consumer boundary already used by named
      calls, receiver/method calls, and `resume (...)`.
  - result:
    - the first mixed call/effect chain now uses the unified suspended-frame model.
    - Step 3 covers the main AST value-position families that previously forced immediate
      token-side replay: named calls, receiver-method calls, pipelines, `BinOp`, `if`, `case`,
      and nested `resume`.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm pipeline_value_replay -- --nocapture`
    - `cargo test -p goby-wasm receiver_method_call_argument_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - revisit any remaining legacy token-only replay seams and decide whether the next cut should
      be cleanup or additional mixed expression shapes.
    - if another shape is added, keep it as narrow as the slices above.

- 2026-03-06 (session 201): Track 4.7 Step 3 cleanup trimmed the remaining single-arg replay seam.
  - runtime:
    - `SingleArgNamedCall` replay no longer treats a suspended outcome as an internal-error-only
      legacy path.
    - the shared continuation replay path now reuses `complete_ast_value_outcome(...)` for that
      shape as well.
  - result:
    - one more leftover old/new split was collapsed without adding a new continuation kind.
    - the remaining cleanup work is now more about deleting redundant transport seams than fixing
      correctness gaps.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm single_arg_call_value_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - continue shrinking remaining compatibility seams, especially around `BinOp` and any replay
      branch still on a legacy helper path.

- 2026-03-06 (session 202): Track 4.7 Step 3 cleanup trimmed the remaining `BinOp` replay seam.
  - runtime:
    - `AstValueContinuationKind::BinOpLeft` no longer evaluates the right operand through
      legacy `eval_expr_ast(...)` during shared continuation replay.
    - the right operand now runs through `eval_expr_ast_outcome(...)` and
      `complete_ast_value_outcome(...)`, so suspended outcomes re-enter the same unified frame
      consumer boundary used by the migrated shapes.
  - result:
    - another old/new split was collapsed without introducing a new continuation kind.
    - `BinOp` replay now uses the outcome-aware path both for checkpoint capture and replay-time
      right-operand evaluation.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm binop_operand_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - inspect whether any other shared replay branch still drops to legacy direct evaluation, then
      either trim it the same way or move to the next remaining expression family.

- 2026-03-06 (session 203): Track 4.7 Step 3 cleanup trimmed the remaining pipeline replay seam.
  - runtime:
    - `PipelineCall` replay no longer routes back through `apply_pipeline(...)` string
      reconstruction.
    - both `Expr::Pipeline` and replay-time pipeline continuation application now use an
      outcome-aware helper that delegates to the named-call AST path directly.
  - result:
    - another compatibility seam was removed without adding a new continuation kind.
    - pipeline replay now follows the same suspended-frame consumer boundary as the other migrated
      call-like shapes.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm pipeline_value_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - keep checking whether any remaining replay branch still depends on string reconstruction or
      legacy direct evaluation before moving on to larger cleanup.

- 2026-03-06 (session 204): Track 4.7 Step 3 cleanup aligned multi-arg named-call exit paths.
  - runtime:
    - added an outcome-aware helper for applying named calls with an already evaluated argument
      slice.
    - both `Expr::Call` multi-arg direct named calls and `MultiArgNamedCall` replay now finish
      through that shared outcome helper instead of using a separate direct wrapper path.
  - result:
    - call-like migrated shapes now leave evaluation through fewer bespoke helper edges.
    - this is mostly a structural cleanup; no new continuation kind or semantic branch was added.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm multi_arg_named_call_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - continue checking whether any replay/apply branch can be collapsed into the same
      outcome-aware helper style before moving back to larger semantic work.

- 2026-03-06 (session 205): Track 4.7 Step 3 cleanup unified replay-time outcome consumption.
  - runtime:
    - `execute_saved_value_continuation(...)` now uses a shared helper to consume
      `AstEvalOutcome<RuntimeValue>` for replay-time call-like branches.
    - `ResumeValue`, `PipelineCall`, `ReceiverMethodCall`, `SingleArgNamedCall`, and
      `MultiArgNamedCall` no longer each open-code the same suspended/completed outcome match.
  - result:
    - replay-time outcome handling is less repetitive and easier to audit.
    - this is a structural cleanup only; no new continuation kind or semantic branch was added.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_multi_arg_named_call_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - continue trimming remaining bespoke helper edges or move back to a larger semantic target if
      the remaining cleanup slices stop paying for themselves.

- 2026-03-06 (session 206): Track 4.7 Step 3 cleanup extended shared replay consumption to control-flow branches.
  - runtime:
    - `CaseScrutinee` and `IfCondition` replay-time branch execution now also uses the shared
      replay outcome consumer helper.
    - this removes another small split between call-like and control-flow replay branches.
  - result:
    - replay-time value continuation exits are more uniform across migrated Step 3 shapes.
    - this is structural cleanup only; semantics are unchanged.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_case_scrutinee_replay -- --nocapture`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_if_condition_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - decide whether the remaining cleanup slices are still buying enough clarity, or pivot back to
      a larger semantic target.

- 2026-03-06 (session 207): Track 4.7 Step 3 widened `if` progression into unit position.
  - runtime:
    - `execute_unit_expr_ast(...)` now evaluates `if` conditions through
      `eval_expr_ast_outcome(...)` plus `complete_ast_value_outcome(...)` instead of the legacy
      direct value path.
    - this lets unit-position `if` condition replay use the same suspended-frame contract already
      used by value-position `if`.
  - coverage:
    - added fallback regression for unit-position `if flag 0` replay.
    - added typed/fallback parity coverage for the same shape.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm unit_position_if_condition_replay_uses_suspended_frame_path -- --nocapture`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_unit_position_if_condition_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - extend the same unit-position outcome-path treatment to the next narrow control-flow seam if
      it is still reachable, or move to the next remaining semantic gap.

- 2026-03-06 (session 208): Track 4.7 Step 3 widened unit-position `if` branch execution.
  - runtime:
    - after selecting a unit-position `if` branch, `execute_unit_expr_ast(...)` now probes
      `eval_expr_ast_outcome(...)` plus `complete_ast_value_outcome(...)` before falling back to
      recursive unit execution.
    - this keeps unit-position branch bodies closer to the same suspended-frame path already used
      by value-position expressions.
  - coverage:
    - added fallback regression where the selected unit-position branch prints through a nested
      handled value expression.
    - added typed/fallback parity coverage for the same branch-value replay shape.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm unit_position_if_selected_branch_replays_value_path -- --nocapture`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_unit_position_if_branch_value_replay -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - inspect whether `case` in unit position has an analogous remaining seam; otherwise move on to
      the next semantic gap rather than continuing small cleanup slices indefinitely.

- 2026-03-06 (session 209): Track 4.7 Step 3 widened statement-level RHS evaluation.
  - runtime:
    - `execute_unit_ast_stmt(...)` now evaluates binding/assignment RHS expressions through
      `eval_expr_ast_outcome(...)` plus `complete_ast_value_outcome(...)` before storing them.
    - this keeps statement-level value production closer to the same suspended-frame contract used
      by migrated expression paths.
  - coverage:
    - added fallback regression for `value = if flag 0 ... else ...` with nested handled value
      replay in the selected branch.
    - added typed/fallback parity coverage for the same binding-RHS shape.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm binding_rhs_if_replays_through_outcome_path -- --nocapture`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_binding_rhs_if_outcome_path -- --nocapture`
    - `cargo test -p goby-wasm`
  - immediate next step:
    - decide whether the next high-value seam is assignment-specific coverage or a broader
      expression family such as value-position `case` arm bodies.

- 2026-03-06 (session 210): Track 4.7 Step 3 locked assignment-RHS parity on the new path.
  - coverage:
    - added fallback regression for `value = if flag 0 ... else ...` reassignment with nested
      handled value replay in the selected branch.
    - added typed/fallback parity coverage for the same assignment-RHS shape.
  - result:
    - the statement-RHS outcome path now has both binding and assignment regression coverage.
    - no runtime semantics changed in this slice; it locks the already migrated path.
  - validation completed:
    - `cargo fmt`
    - `cargo test -p goby-wasm assignment_rhs_if_replays_through_outcome_path -- --nocapture`
    - `cargo test -p goby-wasm typed_mode_matches_fallback_for_assignment_rhs_if_outcome_path -- --nocapture`
    - `cargo test -p goby-wasm`

- 2026-03-06 (session 211): Track 4.7 Step 3 unlocked AST parsing for parenthesized multiline call args.
  - root cause found:
    - the attempted `case` arm-body replay regression was not failing in the Step 3 runtime layer.
    - `main.parsed_body` was `None` for sources like `print (\n  case ...\n)` so execution fell
      straight to the string fallback path before any suspended-frame machinery could run.
  - implementation:
    - `crates/goby-core/src/parser.rs` now parses parenthesized multiline call arguments when the
      inner expression is a multiline `case` or `if`.
    - this keeps sources such as `print (\n  case 0\n    ...\n)` on the AST-backed declaration path.
  - coverage:
    - added parser regressions for parenthesized multiline `case` and `if` call arguments.
    - added fallback runtime regression showing `main.parsed_body` exists and
      `print (\n  case ...\n)` resolves through the AST path.
    - added typed/fallback parity coverage for the same `case` shape with handled replay in the
      selected arm.
  - immediate next step:
    - revisit the remaining semantic gap after this parser unlock, starting with whether
      parenthesized multiline forms expose any new true runtime seams or whether the next target is
      still value-position `case` arm bodies beyond direct-arm calls.

- 2026-03-06 (session 212): Track 4.7 Step 3 moved legacy `case` / block value replay onto the outcome consumer.
  - implementation:
    - `crates/goby-wasm/src/lib.rs` now routes legacy `eval_expr_ast(...)` handling for
      `Expr::Block` and `Expr::Case` through `eval_expr_ast_outcome(...)` plus
      `complete_ast_value_outcome(...)` instead of maintaining a separate direct evaluator path.
    - this keeps selected `case` arm block bodies on the same suspended-frame consumer boundary
      already used by migrated control-flow slices.
  - coverage:
    - added fallback regression for a parenthesized multiline `case` call whose selected arm is a
      block body binding `x = next 0` and returning `x + 10`.
    - added typed/fallback parity coverage for the same block-arm value replay shape.
  - immediate next step:
    - inspect the remaining non-control-flow legacy value seams and decide whether the next slice
      should target top-level AST value entrypoints more broadly or another still-unmigrated
      expression family.

- 2026-03-06 (session 213): Track 4.7 Step 3 moved legacy interpolated-string replay onto the outcome consumer.
  - implementation:
    - `crates/goby-wasm/src/lib.rs` now routes legacy `Expr::InterpolatedString` value evaluation
      through `eval_expr_ast_outcome(...)` plus `complete_ast_value_outcome(...)` instead of
      concatenating `${...}` segments only through the direct evaluator.
    - this lets interpolation segments replay handled values on the same suspended-frame boundary
      already used by migrated nested expression paths.
  - coverage:
    - added fallback regression for `print "value=${next 0}"`.
    - added typed/fallback parity coverage for the same interpolation replay shape.
  - immediate next step:
    - continue auditing remaining legacy direct-evaluation shapes with the same bias toward small
      expression families rather than another broad entrypoint swap.

- 2026-03-06 (session 214): Track 4.7 Step 3 moved legacy tuple-literal replay onto the outcome consumer.
  - implementation:
    - `crates/goby-wasm/src/lib.rs` now routes legacy `Expr::TupleLit` value evaluation through
      `eval_expr_ast_outcome(...)` plus `complete_ast_value_outcome(...)` instead of collecting
      tuple items only through the direct evaluator.
    - this lets tuple elements replay handled values on the same suspended-frame boundary before
      later tuple-member access.
  - coverage:
    - added fallback regression for `pair = (next 0, 2); print pair.0`.
    - added typed/fallback parity coverage for the same tuple replay shape.
  - immediate next step:
    - keep shrinking remaining legacy direct-evaluation shapes one family at a time; `ListLit`
      without spread is the next obvious candidate, but spread-related fallback rules still make it
      a larger slice than tuple/interpolation.

- 2026-03-06 (session 215): Track 4.7 Step 3 moved legacy list-literal replay onto the outcome consumer.
  - implementation:
    - `crates/goby-wasm/src/lib.rs` now routes legacy `Expr::ListLit` value evaluation through
      `eval_expr_ast_outcome(...)` plus `complete_ast_value_outcome(...)` instead of evaluating
      list elements and spread tails only through the direct evaluator.
    - this keeps list elements on the same suspended-frame boundary used by the migrated nested
      expression shapes before the final list value flows into later statements or prints.
  - coverage:
    - added fallback regression for `xs = [next 0, 2]; print xs`.
    - added typed/fallback parity coverage for the same list-literal replay shape.
  - immediate next step:
    - continue shrinking remaining legacy direct-evaluation shapes one family at a time; record
      construction is the next obvious value family with a similarly compact bridge.

- 2026-03-06 (session 216): Track 4.7 Step 3 moved legacy record-constructor replay onto the outcome consumer.
  - implementation:
    - `crates/goby-wasm/src/lib.rs` now routes legacy `Expr::RecordConstruct` value evaluation
      through `eval_expr_ast_outcome(...)` plus `complete_ast_value_outcome(...)` instead of
      evaluating named fields only through the direct evaluator.
    - this keeps named record fields on the same suspended-frame boundary used by the migrated
      nested expression shapes before later record-field access.
  - coverage:
    - added fallback regression for `box = Box(value: next 0); print box.value`.
    - added typed/fallback parity coverage for the same record-constructor replay shape.
  - immediate next step:
    - inspect whether positional single-field constructor sugar in legacy `Expr::Call` should be
      bridged next, or whether another small direct-evaluation family remains cheaper to migrate.

- 2026-03-06 (session 217): Track 4.7 Step 3 moved positional single-field constructor sugar onto the outcome consumer.
  - implementation:
    - `crates/goby-wasm/src/lib.rs` now routes legacy positional single-field constructor sugar
      (`Ctor(value)` synthesized from `Expr::Call`) through `eval_expr_ast_outcome(...)` plus
      `complete_ast_value_outcome(...)` instead of evaluating the single constructor argument only
      through the direct evaluator.
    - this keeps `Ctor(op ...)` on the same suspended-frame boundary before the synthesized record
      reaches later field access or handler dispatch.
  - coverage:
    - added fallback regression for `raise Error(next ())`.
    - added typed/fallback parity coverage for the same positional single-field constructor replay
      shape.
  - immediate next step:
    - revisit the remaining legacy `Expr::Call` direct-evaluation branches and choose whether the
      next compact slice should target plain named-call fallback seams or another still-isolated
      value family.

- 2026-03-06 (session 177): map consolidation Step 8-9 completed.
  - PLAN.md §4.5 checklist updated:
    - completed: Step 8-9 (map callsite migration + builtin-path trim).
  - Examples/tests:
    - `examples/function.gb` now explicitly imports `map` from `goby/list`.
    - inline runtime regression snippets that use `map` now import from `goby/list`.
  - Runtime/lowering:
    - removed native lowering `map` builtin shortcut path (`crates/goby-wasm/src/lower.rs`).
    - removed native capability checker `map`-special handling (`crates/goby-wasm/src/fallback.rs`).
    - runtime list-evaluator `map` shortcut now activates only when selective import
      `goby/list ( map )` is present.
  - Docs:
    - builtins list updated to treat list mapping as stdlib `goby/list.map` path.

- 2026-03-06 (session 176): list spread Step 4-7, 10-11 completed.
  - PLAN.md §4.5 checklist updated:
    - completed: Step 4-7 (typecheck + runtime + native parity via fallback boundary),
      Step 10-11 (spec sync + regression tests).
    - remaining: Step 8-9 (map callsite migration + builtin map-path trim).
  - Typecheck:
    - `check_expr` now infers list literal type with spread-aware merge.
    - added spread constraints diagnostics:
      - prefix element mismatch,
      - tail non-list mismatch,
      - tail element mismatch.
  - Runtime:
    - fallback AST evaluator now executes list spread literals with deterministic
      prefix-then-tail evaluation order.
    - supports both `List Int` and `List String` spread materialization.
    - allows empty tail-list bridge for mixed empty-list seed cases.
  - Tests:
    - `goby-core` typecheck tests added for spread success/failure matrix.
    - `goby-wasm` runtime tests added for Int/String spread output.
  - Docs:
    - `doc/LANGUAGE_SPEC.md` updated with expression-side list spread syntax and type rule.

- 2026-03-06 (session 175): list spread expression — AST + parser implementation.
  - PLAN.md §4.5 Step 1-3 (AST/parser slice) — Step 1 and Step 2 completed.
  - Step 1 (committed): `Expr::ListLit(Vec<Expr>)` → `Expr::ListLit { elements, spread }`.
    - All 18 match sites across 7 files updated mechanically.
    - Combined `ListLit | TupleLit` arms split into separate arms.
    - Typecheck/runtime guards return `Ty::Unknown` / `None` when spread is present.
    - Codex pass 1 + pass 2 review completed, no issues.
  - Step 2 (committed): `parse_list_expr` now accepts trailing `..expr` in list literals.
    - Accepts: `[a, b, ..xs]`, `[f(x), ..ys]`, `[1, ..rest]`.
    - Rejects: `[..xs]` (no prefix), `[a, ..]` (missing expr), `[..a, b]` / `[a, ..b, c]` (non-trailing).
    - 7 parser tests + 1 `to_str_repr` test added.
    - Codex pass 1 review pending (session ended before pass 1 completion).
  - Remaining for this slice:
    - Step 3: quality gate + `examples/function.gb` integration verification (trivial).
  - Next slices (PLAN.md §4.5 Steps 4-11):
    - Step 4-5: typecheck rules (prefix element unification + spread tail `List a` check).
    - Step 6-7: runtime/fallback + native lowering support.
    - Step 8-9: migrate builtin `map` to stdlib `goby/list.map`.
    - Step 10-11: docs sync + regression tests.
  - Workspace files: `~/.claude/workspaces/home_yoshitsugu_src_gitlab-com_yoshitsugu_goby/`.

- 2026-03-06 (session 174): list spread + map consolidation task planning added.
  - `doc/PLAN.md` updated with step-by-step checkbox plan.
  - `stdlib/goby/list.gb` already includes `map` draft using `[f(x), ..ys]`.

- 2026-03-05 (session 173): `doc/PLAN.md` cleanup for closed tracks.
  - verified language-facing changes are synchronized in
    `doc/LANGUAGE_SPEC.md` before cleanup.
  - removed detailed Track A/B/C descriptions from `doc/PLAN.md`.
  - retained closed-track history in `doc/STATE.md` and git history.
- 2026-03-05 (session 172): Track B officially closed.
  - `doc/PLAN.md` marked Track B as completed (before later archive pruning).
  - acceptance criteria confirmed via existing regression matrix and passing
    project quality gates (`check/test/clippy`).
- 2026-03-05 (session 171): Track C officially closed.
  - `doc/PLAN.md` marked Track C as completed (before later archive pruning).
  - all Track C milestones are complete:
    - Step 0-8 completed,
    - PR1-PR5 completed.
- 2026-03-05 (session 170): Track C Step 8 regression matrix + quality gates completed.
  - expanded examples typecheck regression coverage to include
    `examples/iterator_unified.gb` in `typechecks_examples`.
  - iterator examples are now split for stability and coverage:
    - `iterator.gb`: runtime lock-compatible minimal sample,
    - `iterator_unified.gb`: unified iterator modes sample for typecheck/docs regression.
  - required gates executed and passing:
    - `cargo fmt`,
    - `cargo check`,
    - `cargo test`,
    - `cargo clippy -- -D warnings`.
  - Track C PR5 checklist is now complete in `doc/PLAN.md`.
- 2026-03-05 (session 169): Track C Step 7 docs/examples sync completed.
  - updated language spec runtime notes for unified iterator intrinsic contract:
    - `yield : String -> state -> (Bool, state)`,
    - implicit Unit state in 1-arg intrinsic mode,
    - explicit state in 2-arg mode,
    - early stop via `(False, state)`.
  - examples split by purpose:
    - `examples/iterator.gb`: minimal unified state-less sample (runtime lock target),
    - `examples/iterator_unified.gb`: state-less/state-threaded/early-stop consolidated sample.
  - updated `examples/README.md` output expectations accordingly.
- 2026-03-05 (session 168): tuple index access strictness fix.
  - fixed regression where non-tuple numeric qualified access (for example
    `Status.0`) could parse/typecheck/run through fallback paths.
  - typecheck now rejects numeric qualified member access unless receiver type
    resolves to a tuple; out-of-range tuple index now reports explicit error.
  - fallback runtime now rejects numeric member access when receiver is absent
    from locals (instead of treating it as constructor/member string).
  - regression tests added for rejecting non-tuple numeric qualified access.
- 2026-03-05 (session 167): Track C Step 6 diagnostics hardening landed.
  - effect-op argument checks in handler scope now emit explicit unresolved-generic
    diagnostics when argument type remains unresolved under generic constraints
    (`effect_op_unresolved_generic_constraints`).
  - `resume` checks now emit explicit unresolved-generic diagnostics when resume
    argument type remains unresolved under generic expected types
    (`resume_unresolved_generic_constraints`).
  - mismatch diagnostics now append an anonymous type-hole conflict hint when
    `_` constraints are involved.
  - regression tests added for:
    - unresolved generic constraints in handler effect-op call,
    - unresolved generic constraints in `resume`,
    - `_` type-hole conflict message readability.
- 2026-03-05 (session 166): Track C PR4-2 legacy iterator compatibility shim removed.
  - fallback runtime intrinsic `__goby_string_each_grapheme` now accepts only
    unified contract handlers:
    - count mode: `yield : String -> state -> (Bool, state)` with implicit Unit state,
    - state-thread mode: `yield : String -> state -> (Bool, state)`.
  - removed runtime fallback paths for:
    - legacy one-arg `yield : String -> Int`,
    - legacy `yield_state` state-thread operation.
  - updated typecheck/runtime regression tests to unified-only fixtures.
- 2026-03-05 (session 165): Track C PR4 stdlib iterator migration completed.
  - `stdlib/goby/list.gb` migrated to unified iterator effect contract:
    - `effect Iterator a b`,
    - `yield : a -> b -> (Bool, b)`,
    - `iter : List Int -> b -> b can Iterator`.
  - tuple member index access (`pair.0` / `pair.1`) is now wired through:
    - parser (`receiver.<digits>` qualified access),
    - typecheck (`Ty::Tuple` index resolution),
    - fallback runtime evaluator (`RuntimeValue::Tuple` index resolution).
  - regression tests added for parser, typecheck, and runtime tuple index access.
- 2026-03-05 (session 164): Track C runtime bridge registry slice landed.
  - `crates/goby-wasm` fallback runtime now includes declarative bridge metadata
    (`module`, `symbol`, `kind`, `type_shape`, `intrinsic`) with duplicate
    metadata validation.
  - runtime bridge registry is now built from effective imports (explicit imports
    + implicit prelude when available).
  - fallback dispatch now resolves stdlib runtime bridges through registry lookup
    for:
    - prelude `read` / `read_line` bare paths,
    - `goby/env.fetch_env_var`,
    - `goby/int.parse`,
    - `goby/string.length`.
  - added wasm runtime regression coverage for selective `fetch_env_var` and
    module-receiver `string.length` bridge dispatch.
- 2026-03-05 (session 163): fallback runtime multi-arg effect-op dispatch generalized.
  - `Expr::Call` evaluation now dispatches multi-arg bare effect operations
    via handler method lookup (not intrinsic-only).
  - this unblocks source-level calls like `yield x state` for unified iterator
    contract usage in runtime evaluation path.
  - added regression test:
    `resolves_runtime_output_for_multi_arg_effect_op_call`.
- 2026-03-05 (session 162): stdlib migration progress toward unified `Iterator.yield`.
  - migrated to `effect Iterator a b / yield : a -> b -> (Bool, b)`:
    - `stdlib/goby/iterator.gb`,
    - `stdlib/goby/string.gb`,
    - `stdlib/goby/int.gb`.
  - runtime and typecheck remain backward-compatible with legacy iterator shapes
    during migration window (as planned in PR3 bridge policy).
  - `stdlib/goby/list.gb` migration remains pending mainly for source-level
    tuple-result consumption ergonomics (no tuple pattern destructuring yet);
    runtime multi-arg dispatch blocker is resolved in session 163.
- 2026-03-05 (session 161): runtime iterator contract bridge (PR3 slice) landed.
  - runtime intrinsic `__goby_string_each_grapheme` now supports unified iterator
    contract path:
    - `yield : String -> state -> (Bool, state)` for state-thread mode,
    - `(Bool, Unit)` path for 1-arg count mode with early-stop support.
  - compatibility bridge kept for migration:
    - legacy `yield : String -> Int` one-arg mode remains accepted,
    - legacy `yield_state` state-thread mode remains accepted.
  - runtime evaluator now supports non-empty tuple values (`RuntimeValue::Tuple`)
    required for `(Bool, state)` resume payloads.
  - handler dispatch core now supports multi-argument method binding, enabling
    intrinsic-driven two-arg `yield` calls.
  - regression coverage added:
    - unified iterator state-thread runtime path,
    - unified iterator early-stop runtime path,
    - stdlib-root typecheck acceptance for unified intrinsic usage shape.
- 2026-03-05 (session 160): handler-clause fresh instantiation + type-hole/type-param consistency fixes.
  - effect member type variables are now instantiated freshly per handler clause,
    and clause parameter locals + `resume` expected type share the same instantiated signature.
  - handler clause body checks now receive instantiated parameter types instead of all-`Unknown`.
  - fixed type-hole semantics: `_` in type position is now treated as an anonymous
    hole per occurrence (independent fresh vars), not a shared named type variable.
  - fixed effect-member type validation: unknown/free type variables not declared in
    effect header are now rejected.
  - regression tests added for:
    - unknown effect type parameter rejection,
    - independent `_` holes in multi-arg effect member signatures,
    - independent instantiation across multiple calls to same generic effect op.
- 2026-03-05 (session 159): effect-member generic unification core (PR2 slice) landed.
  - typecheck now uses substitution/unification for effect operation argument checks
    in handler-covered scopes, instead of strict equality-only matching.
  - multi-argument generic constraints are validated as one call-site constraint set
    (for example `a -> a -> Unit` rejects `Int` + `String` mix).
  - `resume` argument validation now accepts generic operation return signatures
    when they unify with actual resume values (and still rejects incompatible shapes).
  - regression tests added for:
    - generic effect op argument acceptance/rejection,
    - generic resume return acceptance/rejection.
- 2026-03-05 (session 158): generic effect header groundwork (PR1 slice) landed.
  - parser now accepts effect declarations with optional type parameters
    (`effect Iterator a b`), while preserving existing `effect Name` syntax.
  - parser now rejects invalid/duplicate effect type parameter names.
  - AST `EffectDecl` now carries `type_params`.
  - conflict-signature comparison for effect declarations now includes type-parameter
    list in addition to member signatures.
  - docs/examples synced:
    - `doc/LANGUAGE_SPEC.md` effect declaration syntax updated.
    - `doc/PLAN.md` Step 0/1 and PR1 checklist marked complete.
    - `examples/effect_generic.gb` added.
- 2026-03-05 (session 157): effect dependency cycle diagnostics hardened.
  - typecheck now rejects cycles in effect-member `can` dependencies
    (including self-cycles and multi-effect cycles).
  - added regression tests for `A -> B -> A` and `A -> A`.
  - docs synced:
    - `doc/LANGUAGE_SPEC.md` §5
    - `doc/PLAN.md` §2.3
- 2026-03-05 (session 156): operation-level effect dependency rules introduced.
  - effect member signatures now support dependency declarations via `can`
    (e.g. `trace : String -> Unit can Print`), validated in typecheck.
  - handler clause bodies now enforce dependency-constrained effect usage:
    allowed = currently covered effects + dependencies declared by the handled op.
  - `with` path now always validates inline handler bodies through the same
    unhandled-effect analysis path.
  - Wasm lowering plan now expands declaration required effects through
    operation-declared dependencies with deterministic topological ordering
    (source-order tie-break via DFS traversal order).
  - spec/planning docs synced:
    - `doc/LANGUAGE_SPEC.md` §5
    - `doc/PLAN.md` §2.3 notes
- 2026-03-05 (session 155): callable dispatch diagnostics hardened.
  - fallback runtime now reports deterministic runtime errors for unsupported
    callable argument shapes (`[E-CALLABLE-DISPATCH]`) in:
    - `goby/list.each` callback argument dispatch,
    - callable-parameter declaration calls.
  - added regression tests:
    `reports_callable_dispatch_error_for_list_each_non_callable_callback` and
    `reports_callable_dispatch_error_for_decl_callable_param_non_callable_arg`.
- 2026-03-05 (session 154): effectful callback coverage for
  `goby/list.each` import variants completed.
  - runtime regression tests added for effectful callback dispatch under
    plain/alias/selective imports:
    `resolves_runtime_output_for_effectful_callback_with_list_each_plain_import`,
    `resolves_runtime_output_for_effectful_callback_with_list_each_alias_import`,
    `resolves_runtime_output_for_effectful_callback_with_list_each_selective_import`.
- 2026-03-05 (session 153): lambda-as-function-argument runtime import-variant
  coverage completed for `list.each`.
  - fallback runtime now dispatches `goby/list.each` callbacks for plain import
    (`list.each`), alias import (`l.each`), and selective import (`each`).
  - added runtime regression tests:
    `resolves_runtime_output_for_list_each_with_plain_import`,
    `resolves_runtime_output_for_list_each_with_alias_import`,
    `resolves_runtime_output_for_list_each_with_selective_import`.
- 2026-03-05 (session 152): lambda-as-function-argument import-variant
  typecheck coverage completed.
  - added `goby/list.each` regression tests for import modes:
    plain (`import goby/list`), alias (`import goby/list as l`), selective
    (`import goby/list ( each )`).
- 2026-03-05 (session 151): lambda-as-function-argument runtime parity
  (list.each-style + captured closure slice) completed.
  - fallback runtime now executes curried declaration call chains with callable
    parameters in Unit side-effect position.
  - added runtime regression tests:
    `resolves_runtime_output_for_list_each_style_callback_dispatch` and
    `resolves_runtime_output_for_unit_callback_argument_inline_lambda_with_capture`.
  - `stdlib/goby/list.gb` now includes `iter`/`each` based on effect+handler
    structure (`ListYield` + `with handler ... in`).
- 2026-03-05 (session 150): lambda-as-function-argument runtime parity
  (Unit callback slice) completed.
  - fallback runtime now resolves declaration-local callable parameters for
    inline lambdas and named functions in side-effect position.
  - added regression tests:
    `resolves_runtime_output_for_unit_callback_argument_inline_lambda` and
    `resolves_runtime_output_for_unit_callback_argument_named_function`.
- 2026-03-05: effect-op argument type checking in handler scopes completed.
  - typecheck now rejects mismatched arg types for bare/qualified/method-style/pipeline effect-op calls.
  - regression tests added for qualified and method-style mismatches.
  - `examples/effect.gb` updated to keep example suite type-correct under stricter checks.
- 2026-03-05 (session 149): `Print` operation split finalized (`print`/`println`) with
  embedded default handler behavior and regression coverage.
- 2026-03-05 (sessions 148-146): control-flow typing/runtime parity improvements:
  - `if`/`case` branch type unification,
  - multiline `if`/`case` expression coverage in parser/runtime,
  - `case` arm block execution/typecheck support.
- 2026-03-05 (session 145): list-pattern semantics clarified to prefix matching and matcher refactor.
- 2026-03-04 (sessions 144-140): list-pattern implementation completion + import/type/effect
  selective import semantics stabilization.
- 2026-03-04 (sessions 139-138): Unit-arg call parsing fixes (`f ()`) and `int.parse`
  naming/behavior alignment.

Older milestone records are intentionally compressed out of this file to keep restart
context focused. Full chronology remains in git history.

## 5. Current Example Files

- `examples/hello.gb`
- `examples/basic_types.gb`
- `examples/function.gb`
- `examples/generic_types.gb`
- `examples/import.gb`
- `examples/control_flow.gb`
- `examples/type.gb`
- `examples/effect.gb`
- `examples/iterator.gb`

## 6. Immediate Next Steps (Execution Order)

Baseline gates before/after each implementation slice:

```
cargo fmt
cargo check
cargo test
cargo clippy -- -D warnings
```

Execution focus (aligned with `doc/PLAN.md`):

1. **Active**: Abortive handlers + multi-resume progression (PLAN.md §4.7).
   - implement immediate abort when handler clause finishes without `resume`.
   - implement continuation progression for multiple `resume` calls in one invocation.
   - relax conservative static rejection of multiple syntactic `resume`.
2. Stdlib runtime bridge generalization (reduce symbol-specific fallback branches).
3. Tooling foundation (`fmt`/`lint`/`lsp`) with stable diagnostics surface.
4. Native lowering coverage expansion for remaining unsupported expression/effect paths.

## 7. Resume Commands

```
cargo check
cargo test
cargo clippy -- -D warnings
cargo run -p goby-cli -- run examples/function.gb
```

## 8. Deferred TODO

- Declaration-side generic parameter binders (design memo only).
- `HandlerMethod` body type-checking (currently stored as raw `String` alongside `parsed_body: Option<Vec<Stmt>>`; future handler type-checking would benefit from full inference).
- Effect-safety / unhandled-effect diagnostics (out of scope for MVP).
- Record update syntax and pattern matching on record fields.
- `else if` chaining in `if` expressions (not supported in MVP; documented).
- Real Wasm next milestone: extend native coverage beyond current subset (lambda/HOF + effect runtime and remaining unsupported expression forms), then retire remaining fallback execution paths.
- REPL / interactive mode.
