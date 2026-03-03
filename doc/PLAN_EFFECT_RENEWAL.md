# Effect Syntax Renewal Plan: `handler` -> `with`

Status: Draft  
Owner: Goby core/runtime track  
Last updated: 2026-03-03

## 1. Purpose

This document defines a staged migration from the current top-level `handler` model
to an expression-scoped `with` model (Ante-like style), so effect handlers can
capture lexical scope values (variables/functions) naturally.

Target direction:

- Keep current programs working during migration.
- Add `with` first, then migrate stdlib and examples.
- Retire old `handler` syntax only after parity and diagnostics are stable.

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
- Interaction with nested `with` blocks and existing `using`.
- Policy for coexistence with old `handler` declarations during migration.

Completion criteria:

- Semantics written and approved in `doc/PLAN.md` (or linked section).
- At least 5 executable examples documented (capture, nested `with`, abortive path,
  resume path, multiple operations).

### P1. Parser + AST Support (`with`)

Add syntax and AST representation for `with` expressions.

Minimum deliverables:

- Parse `with ... <expr>` block form.
- Represent handler clauses in AST as expression-level constructs.
- Preserve existing `handler` parsing unchanged.

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

Completion criteria:

- Runtime tests covering:
  - local capture in clause body,
  - resume success path,
  - no-resume abortive path,
  - nested handler precedence.
- Parity tests show expected behavior in both fallback and optimized modes.

### P4. Stdlib Iterator Rewrite on `with`

Migrate iterator-related stdlib logic from top-level `handler` dependence to local `with`.

Minimum deliverables:

- Rewrite current iterator-driven string pipeline to `with` style.
- Implement `foreach`/`take_while`/`collect`-like patterns as reference examples.
- Remove temporary workarounds that existed only due to top-level handler limits.

Completion criteria:

- `stdlib` iterator/string implementation uses `with` for local capture cases.
- `examples/iterator.gb` (and new iterator examples) demonstrate local capture.
- C4 split path no longer depends on top-level-handler-only workarounds.

### P5. Compatibility + Deprecation Phase

Keep old syntax available, but begin deprecation path.

Minimum deliverables:

- Warnings (or docs-level deprecation note) for legacy `handler` style.
- Migration docs: old -> new syntax mapping.
- Tooling diagnostics point to `with` alternatives.

Completion criteria:

- Deprecation policy written in docs with timeline.
- Users can migrate examples 1:1 using provided guide.

### P6. Removal Phase (Old `handler` Syntax)

Remove old top-level `handler` syntax only after migration confidence is high.

Minimum deliverables:

- Parser/typechecker/runtime cleanup for retired syntax.
- Remove dead code and compatibility shims.

Completion criteria:

- All stdlib/examples/tests are `with`-based.
- No `handler` syntax remains in active docs/examples.
- `cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings` all pass.

## 5. Risk Tracking

Key risks:

- Semantic mismatch between existing `resume` runtime model and expression-scoped capture.
- Performance regressions in nested handler-heavy programs.
- Ambiguous migration period when `using`, `handler`, and `with` all coexist.

Mitigations:

- Keep phases small with explicit parity tests.
- Add snapshot tests for diagnostics and runtime behavior.
- Do not remove old paths until `with` parity is proven.

## 6. Immediate Next Step

Start **P0**: lock exact `with` semantics and add approved examples before parser changes.
