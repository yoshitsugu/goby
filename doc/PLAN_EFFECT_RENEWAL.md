# Effect Syntax Renewal Plan: `handler`/`using` -> `with`

Status: Draft  
Owner: Goby core/runtime track  
Last updated: 2026-03-03

## 1. Purpose

This document defines a staged migration from current `handler` + `using` effect
syntax to an expression-scoped `with` model (Ante-like style), so effect handling
can capture lexical scope values (variables/functions) naturally.

Target direction:

- Keep current programs working during migration.
- Add `with` first, then migrate stdlib and examples.
- Retire old `handler` and `using` syntax only after parity and diagnostics are stable.

## 2. Problem Statement

Current `handler` declarations are top-level and cannot naturally capture local scope.
This blocks idioms like iterator combinators (`foreach`, `take_while`, `collect`) where
the handler logic needs direct access to local function arguments and locals.

Desired style:

```goby
each : List a -> (a -> Unit) -> Unit can Iterate
each xs f =
  with
    emit x =
      f x
      resume True
  iter xs
```

## 3. Non-Goals (for this migration)

- Full language redesign beyond effect syntax/runtime plumbing.
- New optimization backend beyond what is needed for semantic parity.
- Immediate removal of all old runtime code paths in one step.

## 4. Migration Phases

Each phase includes explicit completion criteria.
If a phase is too large or risky, split it into substeps (`Px.a`, `Px.b`, ...),
and add those substeps directly under the phase before implementation starts.

### P0. Semantic Lock for `with`

Define and lock the core `with` semantics before coding.

Scope to lock:

- `with` is expression-scoped and lexically captures outer bindings.
- Handler clauses are operation clauses (`op arg = ...`) with `resume`.
- `with` body result type and effect behavior.
- Interaction with nested `with` blocks and existing `using`/`handler`.
- Policy for coexistence with old `handler`/`using` declarations during migration.

P0 required semantic decisions (must be explicit and testable):

- `resume` policy:
  - one-shot per operation handling (same as current runtime contract),
  - second `resume` in the same clause is rejected (typecheck error).
- Abortive policy:
  - if a clause does not call `resume`, the captured continuation is discarded.
- Dispatch policy:
  - nearest lexically-scoped `with` clause wins first,
  - if not found, fall back to active `using` handlers,
  - if still not found, report existing unhandled-effect diagnostic.
- Partial handling policy:
  - `with` may handle only a subset of operations; unhandled ops are propagated.
- Type/effect policy:
  - `with` removes handled effect operations from the body requirement set,
  - non-handled effects remain in the resulting `can` obligations.
- Capture policy:
  - clause bodies see all lexical bindings visible at `with` site,
  - clause parameter names shadow outer bindings in clause scope only.

Static resolution table (must be mirrored by runtime):

| Situation | Resolution |
| --- | --- |
| operation handled by nearest `with` and `using` | nearest `with` wins |
| operation not handled by nearest `with` but handled by outer `with` | nearest matching outer `with` wins |
| operation not handled by any `with`, handled by `using` | `using` handles |
| operation handled by neither | unhandled effect diagnostic |

Completion criteria:

- Semantics written and approved in `doc/PLAN.md` (or linked section).
- At least 5 executable examples documented (capture, nested `with`, abortive path,
  resume path, multiple operations).

### P1. Parser + AST Support (`with`)

Add syntax and AST representation for `with` expressions.

Minimum deliverables:

- Parse canonical `with` block form:
  - `with` on its own line,
  - one or more operation clauses (`op arg =` + indented body),
  - followed by exactly one handled body expression/block.
- Represent handler clauses in AST as expression-level constructs.
- Preserve existing `handler` parsing unchanged.
- Reserve concrete error messages for malformed `with`:
  - missing clause,
  - missing handled body,
  - malformed clause header.

Grammar contract to lock in P1:

```text
WithExpr :=
  "with" NEWLINE
    INDENT Clause+ DEDENT
    INDENT Body DEDENT

Clause :=
  Ident ParamList? "=" NEWLINE INDENT Stmt* Expr? DEDENT

ParamList :=
  Ident*
```

Notes:

- Final grammar shape may adjust for existing parser architecture, but must preserve:
  - unambiguous clause/body boundary,
  - indentation-based structure,
  - no parsing conflict with `if/case/using`.
  - explicit policy for multi-argument operations:
    - either curried single-arg operations only, or
    - direct multi-arg clause headers (`op a b =`), with matching operation signature rules.

### P1.5 Tooling + CLI + Formatter Integration

Add first-class tooling support for `with` syntax before broad migration.

Minimum deliverables:

- CLI diagnostics for malformed `with` match existing quality bar (line/col/snippet).
- `check`/`run` surface deprecation warnings consistently (when enabled).
- Formatter support for canonical `with` indentation and clause layout.

Completion criteria:

- CLI integration tests for `with` parse/type errors.
- Formatter golden tests for `with` blocks.
- No regression in existing CLI/format tests.

Completion criteria:

- Parser tests for valid/invalid `with` syntax (including indentation errors).
- Existing parser tests remain green.

### P2. Typechecker Support (`with`)

Typecheck `with` using lexical environment capture.

Minimum deliverables:

- Clause argument typing from effect operation signatures.
- `resume` typing inside `with` clauses.
- Effect coverage checking for `with` body.
- Coexistence rules with `using` and old `handler`.

Typechecking contract to lock in P2:

- Clause operation lookup:
  - clause name must resolve to a known effect operation in the current effect context.
- Clause parameter typing:
  - first clause parameter type must match operation input type.
- `resume` typing:
  - `resume <expr>` argument type must match operation result type.
- Clause result typing:
  - clause body type must be compatible with operation resume-return expectation.
- Handled-body typing:
  - `with` expression type is the handled body result type.
- Effect reduction:
  - handled operations are subtracted from required effects of handled body.
  - resolution order must follow P0 static resolution table exactly.

Completion criteria:

- New `with` typecheck tests for:
  - lexical capture (`f` and local vars),
  - correct/incorrect `resume` argument,
  - nested `with`,
  - unknown operation/unknown effect diagnostics.
- No regression in existing effect tests.

### P3. Runtime Execution Model for `with`

Implement runtime dispatch for expression-scoped handlers with lexical capture.

Minimum deliverables:

- Runtime representation of active `with` frames.
- Operation dispatch selects nearest matching `with` clause.
- `resume` returns into captured continuation correctly.
- Abortive behavior when `resume` is not called remains well-defined.

Runtime contract to lock in P3:

- Runtime frame model:
  - push expression-scoped `with` frames with captured lexical environment snapshot.
- Dispatch order:
  - search active `with` frames from innermost to outermost,
  - then existing `using`/legacy handler path.
- Continuation bridge:
  - preserve existing one-shot token semantics and error IDs.
- Error stability:
  - retain deterministic runtime error identifiers for misuse paths.

Completion criteria:

- Runtime tests covering:
  - local capture in clause body,
  - resume success path,
  - no-resume abortive path,
  - nested handler precedence.
- Parity tests show expected behavior in both fallback and optimized modes.
- Parity gate is strict:
  - same output text,
  - same runtime error ID/hint category,
  - same one-shot consume behavior.

### P4. Stdlib Iterator Rewrite on `with`

Migrate iterator-related stdlib logic from top-level `handler` dependence to local `with`.

Minimum deliverables:

- Rewrite current iterator-driven string pipeline to `with` style.
- Implement `foreach`/`take_while`/`collect`-like patterns as reference examples.
- Remove temporary workarounds that existed only due to top-level handler limits.

Preconditions for P4:

- P2 completed with lexical-capture typecheck support.
- P3 completed with expression-scoped runtime handler frames.
- Generic/HOF feature subset needed by iterator examples is available (or examples are scoped
  to current MVP-supported subset and marked accordingly).

Completion criteria:

- `stdlib` iterator/string implementation uses `with` for local capture cases.
- `examples/iterator.gb` (and new iterator examples) demonstrate local capture.
- C4 split path no longer depends on top-level-handler-only workarounds.

### P5. Compatibility + Deprecation Phase

Keep old syntax available, but begin deprecation path.

Minimum deliverables:

- Warnings (or docs-level deprecation note) for legacy `handler` and `using` syntax.
- Migration docs: old -> new syntax mapping.
- Tooling diagnostics point to `with` alternatives.

Deprecation schedule (initial lock):

- Release N:
  - old `handler`/`using` accepted, no warning by default.
- Release N+1:
  - old `handler`/`using` accepted with warning (`effect_legacy_handler_syntax`).
- Release N+2:
  - old `handler`/`using` rejected by default (opt-in compatibility flag allowed temporarily).

Completion criteria:

- Deprecation policy written in docs with timeline.
- Users can migrate examples 1:1 using provided guide.
- Migration mapping appendix exists with at least:
  - top-level `handler` + `using` -> `with`,
  - nested handlers -> nested `with`,
  - mixed qualified/unqualified operation clauses.

### P6. Removal Phase (Old `handler`/`using` Syntax)

Remove old top-level `handler` and `using` syntax only after migration confidence is high.

Minimum deliverables:

- Parser/typechecker/runtime cleanup for retired syntax.
- Remove dead code and compatibility shims.

Completion criteria:

- All stdlib/examples/tests are `with`-based.
- No `handler`/`using` syntax remains in active docs/examples.
- `cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings` all pass.

## 5. Governance + Delivery Rules

Implementation discipline:

- Each phase must produce:
  - spec delta (docs),
  - tests,
  - implementation,
  - migration notes (if externally visible behavior changed).
- Each phase PR must state:
  - scope in/out,
  - rollback strategy,
  - compatibility impact.
- No phase is marked complete without explicit checklist update in this document.
- Each phase PR must link:
  - one tracked issue/milestone,
  - one status update entry in `doc/STATE.md`.

Suggested ownership split:

- Parser/AST: `goby-core` parser owner.
- Type system/effects: `goby-core` typecheck owner.
- Runtime dispatch/continuation: `goby-wasm` runtime owner.
- Stdlib/examples/docs migration: language/library owner.

## 6. Risk Tracking

Key risks:

- Semantic mismatch between existing `resume` runtime model and expression-scoped capture.
- Performance regressions in nested handler-heavy programs.
- Ambiguous migration period when `using`, `handler`, and `with` all coexist.

Mitigations:

- Keep phases small with explicit parity tests.
- Add snapshot tests for diagnostics and runtime behavior.
- Do not remove old paths until `with` parity is proven.
- Add performance gate before syntax removal:
  - resume-heavy benchmark suite,
  - nested-handler benchmark suite,
  - allowed regression threshold (initial lock: <= 10% on median runtime for benchmark set).

## 7. Immediate Next Step

Start **P0**: lock exact `with` semantics and add approved examples before parser changes.
