# Development Plan

## Requirement Summary

- Scope:
  - Implement `doc/PLAN.md` section 4.7 from the start, beginning with Step 1
    semantic alignment audit and Step 2 runtime abort contract work.
  - Align fallback runtime and typed-continuation bridge behavior with the
    current language contract for abortive handlers and `resume`.
  - Keep `doc/PLAN.md`, `doc/LANGUAGE_SPEC.md`, examples, tests, and
    `doc/STATE.md` synchronized when language-facing behavior changes.
- Out of scope:
  - New syntax or type-system extensions beyond the existing 4.7 scope.
  - Post-MVP lowering redesigns outside the current runtime/effect execution
    paths.
  - Unrelated fallback reduction work such as list-pattern native lowering.
- Constraints:
  - Follow `codex-dev-flow` phase order: plan, review, plan commit, stepwise
    implementation, final report.
  - Prefer incremental changes that keep fallback mode and typed mode in parity
    after each landed step.
  - Run at least `cargo fmt`, `cargo check`, `cargo test`, and
    `cargo clippy -- -D warnings` before considering the track complete.

## Done Criteria

- [ ] Step 1 audit documents all effect-operation runtime entrypoints relevant to
      abort/resume behavior.
- [ ] Runtime dispatch distinguishes resumed completion, abortive no-`resume`
      completion, and invalid continuation errors explicitly.
- [ ] No-`resume` handler completion aborts at the handled operation boundary in
      both value-position and unit-position operation calls.
- [ ] Typed-continuation mode matches fallback runtime behavior for abort/resume
      cases covered by Track 4.7.
- [ ] Conservative typecheck rejection of multiple syntactic `resume` is removed
      only when runtime progression support is ready.
- [ ] Tests cover no-`resume` abort, successful `resume`, multi-resume
      progression, and resume-after-consumption error paths.
- [ ] Language-facing docs/examples/state snapshot are updated to reflect the
      final behavior.

## Quality Gate Commands

- Format: `cargo fmt`
- Lint: `cargo clippy -- -D warnings`
- Test: `cargo test`
- Additional verification during implementation: `cargo check` and targeted
  effect-runtime tests before full-suite reruns.

## Anti-Adhoc Harness

- Primary test coverage:
  - `crates/goby-wasm/src/lib.rs` runtime tests for fallback behavior and
    typed-mode parity.
  - `crates/goby-core/src/typecheck.rs` tests for `resume` legality and type
    compatibility.
- Regression guard:
  - keep existing `resume` success/error parity tests green while each step is
    implemented.
  - add targeted cases before refactors that change continuation bridge
    semantics.
- Failure-case tests:
  - `resume` outside handler,
  - resume-after-consumption runtime error,
  - no-`resume` abort in value position,
  - no-`resume` abort in unit position,
  - nested handler abort propagation,
  - multi-resume progression exhaustion path.

## Incremental Steps

1. Step 1: semantic alignment audit
   - enumerate all runtime paths that evaluate handled operations.
   - confirm current behavior for value-position and unit-position calls in both
     fallback and typed mode.
   - record findings in `doc/PLAN.md` / implementation notes before changing
     semantics.
2. Step 2: runtime abort contract implementation
   - detail is tracked in `doc/PLAN.md` 4.7 Step 2.1-2.12.
   - introduce explicit abort outcome and propagate it across dispatch paths.
   - lock value-position and unit-position abort behavior with tests.
3. Step 3: multi-resume progression support
   - replace current one-shot completion model with progression-aware
     continuation state per handler invocation.
   - keep deterministic runtime errors after continuation exhaustion.
4. Step 4: typecheck rule update
   - remove conservative multiple-`resume` rejection only after Step 3 is in
     place.
   - preserve `resume`-outside-handler and type-compatibility checks.
5. Step 5: docs/examples/state sync and full quality gate
   - update `doc/LANGUAGE_SPEC.md`, `doc/PLAN.md`, examples, and `doc/STATE.md`.
   - rerun full quality gates and record review notes.

## Plan Revision Policy

- If implementation reveals new runtime paths, semantic gaps, or test gaps,
  update this file and the relevant `doc/PLAN.md` checklist before continuing.
- Any scope change affecting syntax, semantics, or user-visible diagnostics must
  also update `doc/LANGUAGE_SPEC.md` and be noted in `doc/STATE.md`.
- Each implementation step must record the verification run and review outcome
  before moving to the next step.

## Plan Revision Log

- 2026-03-06: Initial Track 4.7 plan created for abortive handlers and
  multi-resume progression.
- 2026-03-06: Step 2 was split into a landed core slice (value/unit abort
  contract) and a remaining nested-handler follow-up under `doc/PLAN.md`
  Step 2.10 / 2.11.
- 2026-03-06: Nested-handler abort propagation required a parser fix because
  handler clause bodies were stripping relative indentation and hiding valid
  nested `with` / multi-branch `resume` forms from AST/typecheck/runtime paths.
- 2026-03-06: legacy syntax cleanup removed remaining `with_handler` and
  expression-form `Unit` value compatibility paths while keeping `Unit` as the
  type name.
- 2026-03-06: Step 3 gained a first runtime slice limited to AST-backed
  unit-position statement-tail replay; value-position continuation checkpoints
  remain deferred and are tracked explicitly in `doc/PLAN.md`.
- 2026-03-06: Step 3 follow-up refactored `eval_expr_ast_outcome` into a
  recursive evaluator for composite AST forms so future `Suspended(...)`
  checkpoints can propagate through value-position constructs without another
  wrapper-only rewrite.
- 2026-03-06: Step 3 added a narrow value-position replay slice for direct
  binding / assignment RHS handled operations in AST statement sequences while
  explicitly keeping deeper expression-tree checkpoints deferred.
- 2026-03-06: Step 3 kept `iterator_unified` on the AST runtime path by adding
  value-position `with` support and AST-backed general declaration value calls
  (including `f ()` and flattened multi-arg calls).
- 2026-03-06: Step 3 added the first true nested expression replay slice by
  carrying a single-arg named-call value continuation through resume tokens.
- 2026-03-06: Step 3 extended value continuations to direct `BinOp` operand
  replay for both left and right handled subexpressions.
- 2026-03-06: Step 3 strategy pivoted away from accumulating more ad hoc
  continuation shapes; the next implementation target is a unified
  `AstEvalOutcome::Suspended(Box<...>)` frame model that can replace the current
  exploratory token-side replay slices over time.
