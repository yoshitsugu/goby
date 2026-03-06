# Goby Project State Snapshot

Last updated: 2026-03-06 (session 191)

This file is a restart-safe snapshot for resuming work after context reset.

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
