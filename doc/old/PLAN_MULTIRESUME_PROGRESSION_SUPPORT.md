# Old Step 3 Plan: Multi-Resume Progression Support

This file archives the detailed Step 3 execution history from `doc/PLAN.md` (section 4.7)
as of session 231. The Step 3 architecture has been redesigned from scratch; this content
is retained for reference only.

---

## Original section 4.7: Active Language Task — Abortive Handlers and Multi-Resume Progression

Goal: align runtime/typecheck behavior with the current `LANGUAGE_SPEC` contract:

- handler clause without `resume` aborts immediately at the handled operation boundary,
- `resume` returns a value to the operation call site,
- repeated `resume` in one handler invocation progresses continuation to next resumable point,
  and raises runtime error only after continuation is consumed.

Step-by-step checklist:

- Completed summary:
  - [x] Step 1: semantic alignment audit
    - runtime handler dispatch entrypoints were audited across fallback and typed mode.
    - the audit confirmed the pre-fix gap was unit-position no-`resume` continuation.
  - [x] Step 2: runtime abort contract implementation
    - runtime dispatch now carries explicit abort/completion state instead of overloading nested `Option` meanings.
    - no-`resume` handler completion now aborts at the handled operation boundary in both value and unit positions.
    - nested-handler propagation and fallback/typed parity coverage were added.
- [ ] Step 3: multi-resume progression support
  - replace one-shot token consumption model with resumable progression model for one handler invocation.
  - each `resume` continues from the next resumable point; exhausted continuation raises runtime error.
  - keep guardrails for clearly invalid continuation state transitions.
  - quick view:
    - target design:
      - real `AstEvalOutcome::Suspended(Box<...>)`
      - unified continuation frames
      - compact migration slices (`define frame` -> `migrate one shape` -> `remove old path`)
    - next concrete action:
      - Step 3.2a: define the minimal unified continuation-frame interface in
        `crates/goby-wasm/src/lib.rs`
    - first migration target after that:
      - `single-arg call`
    - do not do next:
      - add another new ad hoc `AstValueContinuationKind::*` shape
  - implementation status (2026-03-06):
    - locked premise:
      - this step is not a token-only change; the runtime must be continuation-aware at the AST
        evaluator boundary.
      - a direct one-line change from one-shot to multi-shot would be incorrect because `resume`
        currently only returns a value to the handler body and does not itself reify the caller
        continuation.
    - strategy pivot (locked 2026-03-06):
      - Step 3 should no longer be completed by extending ad hoc continuation shapes one AST form
        at a time.
      - the recent replay slices proved the semantic direction, but they also showed that
        token-side `AstStmtContinuation` / `AstValueContinuationKind::*` growth will scatter future
        language changes across many shape-specific branches.
      - because Goby is still in a personal concept-validation phase and breaking changes are
        acceptable, the preferred direction is now a cleaner destructive refactor:
        - make `AstEvalOutcome::Suspended(...)` a real evaluator result,
        - represent suspension as unified continuation frames for "what to do next",
        - converge statement-position and value-position replay on the same resume contract.
      - implication:
        - the current replay slices are kept as working exploration/proof of semantics, not as the
          final Step 3 architecture.
        - future Step 3 work may replace or delete parts of the current token replay machinery if
          that yields a smaller long-term change surface.
    - completed slices:
      - AST runtime outcome groundwork:
        - introduced `AstEvalOutcome<T>` as the Step 3 runtime-shape carrier.
        - handler-dispatch statement execution now branches on explicit AST outcomes instead of
          relying only on `Option` + token-state probing.
      - frame-entry groundwork:
        - resume tokens now carry one transport field for AST continuation replay
          (`AstContinuationFrame`) instead of separate statement/value fields.
        - `resume` now routes through a single AST continuation entrypoint before returning to the
          handler body.
        - this does not emit `Suspended(...)` yet, but it establishes the minimal frame boundary
          required for Step 3.2a.
      - first real suspended-frame slice:
        - `eval_expr_ast_outcome` now handles `single-arg named call` through an outcome-aware path.
        - when `resume` sees a value-only `SingleArgNamedCall` frame, it now returns
          `AstEvalOutcome::Suspended(...)` instead of immediately replaying that frame inside the
          token bridge.
        - handler dispatch can surface that suspension back through
          `dispatch_handler_method_as_value_outcome`.
        - the slice is intentionally narrow: statement continuations and broader value shapes still
          complete through the old replay path.
      - unit-position replay:
        - resume tokens can carry an AST statement-tail continuation snapshot.
        - top-level `with` bodies and unit-position AST statement sequences register checkpoints
          for remaining statements.
        - `resume` can replay those remaining unit statements before handler-body execution
          continues.
      - recursive value-expression groundwork:
        - `eval_expr_ast_outcome` now evaluates composite AST forms recursively instead of only
          wrapping `eval_expr_ast`.
        - covered composite forms: `InterpolatedString`, `BinOp`, `ListLit`, `TupleLit`, `Block`,
          `Case`, `If`.
      - direct statement-RHS replay:
        - resume tokens can also carry binding / assignment continuation checkpoints for AST
          statement sequences.
        - when a direct handled operation is used as the RHS of `x = op ...` or assignment,
          `resume` restores the resumed value into that local before running later statements.
        - top-level / `with`-body direct RHS bindings are covered in both fallback and typed mode.
      - AST value-path retention:
        - AST value evaluation now supports `with ... in <expr>` in value position.
        - AST value evaluation now supports general declaration calls as values, including
          zero-arity `f ()` and flattened multi-arg named calls.
        - `examples/iterator_unified.gb` now resolves through the fallback runtime and matches
          typed-mode parity for its locked progression shape.
      - first nested expression checkpoint:
        - resume tokens can now carry a value continuation for single-argument named call sites.
        - this allows value-position replay through shapes like `print (id (next 0))` instead of
          returning the raw resumed value directly to the surrounding statement.
        - fallback and typed mode parity are covered for this first nested call-argument slice.
      - binop operand replay:
        - value continuations now also cover `BinOp` operand progression.
        - both left-operand and right-operand handled calls replay through the interrupted
          arithmetic/equality expression before returning to outer statement flow.
        - fallback and typed mode parity are covered for the first binop slice.
    - remaining gaps (at time of archive):
      - no explicit `Suspended(...)` result is emitted yet; progression is still modeled through
        captured statement/declaration replay.
      - nested intermediate checkpoints inside arbitrary expression trees are still open.
      - `dispatch_handler_method_core` TODO sites unresolved.
      - `Expr::With` early-pop bug: handler stack is popped before continuation is replayed.
    - confirmed investigation findings:
      - current runtime anchor points:
        - `crates/goby-wasm/src/lib.rs`: `dispatch_handler_method_core`
        - `crates/goby-wasm/src/lib.rs`: `resume_through_active_continuation_fallback`
        - `crates/goby-wasm/src/lib.rs`: `resume_through_active_continuation_optimized`
        - `crates/goby-wasm/src/lib.rs`: `eval_expr_ast`
        - `crates/goby-wasm/src/lib.rs`: `execute_unit_expr_ast`
        - `crates/goby-wasm/src/lib.rs`: `execute_unit_ast_stmt`
      - current token model:
        - fallback mode stores `ResumeToken { continuation: Continuation { consumed }, state }`.
        - typed mode stores `OptimizedResumeToken { consumed, state }`.
        - both modes mark the token consumed at the first `resume`.
        - `dispatch_handler_method_core` breaks the handler-body loop as soon as a resumed value is observed.
      - semantic gap:
        - the current bridge can express single `resume` success, no-`resume` abort, and
          deterministic runtime error on second `resume`.
        - the current bridge cannot express "resume caller continuation until the next resumable
          point, then return control to the same handler invocation".
      - ad hoc continuation types accumulated:
        - `AstContinuation`, `AstContinuationFrame`
        - `AstValueContinuation` / `AstValueContinuationKind`
        - `AstStmtContinuation` / `AstStmtContinuationKind`
        - `AstEvalOutcome<T>`
        - `HandlerCompletion`, `HandlerContinuationState`
        - Fields on `RuntimeOutputResolver`: `pending_stmt_continuations`,
          `pending_value_continuations`, `active_stmt_sequence_ids`, `next_stmt_sequence_id`,
          `consumed_stmt_sequence_id`, `replaying_stmt_continuation_depth`.
  - staged execution plan (archived, superseded by overhaul):
    - [x] Step 3.1: introduce continuation-aware runtime result types
    - [~] Step 3.2: model resumable caller checkpoints for AST execution (partial)
    - [~] Step 3.3: implement progression in fallback mode first (partial)
    - [~] Step 3.4: mirror the same contract in typed-continuation mode (partial)
    - [~] Step 3.5: cover the progression matrix with tests (partial)
  - test coverage accumulated (209 tests as of session 231):
    - fallback + typed parity regressions for: unit-position replay, direct binding-value replay,
      declaration-call progression, single-arg call-argument replay, multi-arg named call-argument
      replay, nested resume-value replay, one-arg receiver/method call replay, pipeline value
      replay, direct binop operand replay, `if` condition replay, `case` scrutinee replay,
      legacy list-literal replay, legacy record-constructor field replay, positional
      single-field constructor replay.
    - Shapes E-K added in sessions 226–230.
    - Parity tests added in session 231: `with_captures_lexical_local`,
      `nested_with_nearest_handler_wins`, `qualified_effect_call_dispatch`,
      `pipeline_effect_call_dispatch`, `basic_with_inline_handler_dispatch`,
      `resume_outside_handler_error`, `multi_arg_effect_op_call`,
      `qualified_call_without_active_handler`, `lambda_closure_capture`,
      `list_each_style_callback_dispatch`.
- [x] Step 4: typecheck rule update
  - conservative syntactic multi-`resume` rejection was removed.
  - retained checks are `resume` placement, resumed-value type compatibility, and unresolved generic diagnostics.
- [ ] Step 5: tests and parity locks (superseded by overhaul)
- [ ] Step 6: docs sync (superseded by overhaul)
- [ ] Step 7: quality gate (superseded by overhaul)
