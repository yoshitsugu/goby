# Goby Error Reporting Plan

Last updated: 2026-03-25

This document tracks follow-up work for richer compiler diagnostics, with the
current immediate focus on unresolved-name and name-resolution errors carrying
precise source spans into CLI and LSP output.

Representative target:

```goby
import goby/list ( each )
import goby/string ( split, graphemes )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  rolls = map lines graphemes
  each (rolls[2]) println
```

If `map` is not in scope, the desired behavior is:

- CLI points at the `map` token with a caret/underline.
- LSP publishes a diagnostic range covering just `map`, so editors can render a
  wave line under the unresolved name.

## 1. Existing Base

The repository already has most of the presentation-side machinery:

- unified diagnostics in `crates/goby-core/src/diagnostic.rs`,
- CLI caret rendering in `crates/goby-cli/src/main.rs`,
- Goby `Span` -> LSP `Range` conversion plus diagnostic publication in
  `crates/goby-lsp/src/main.rs`,
- parser/body-relative span work for declarations, statements, handler clauses,
  and case arms.

The missing work is mainly in the middle:

- expression-level spans are not yet used consistently at unresolved-name
  failure sites,
- many typecheck/name-resolution errors still return `span: None` with comments
  like `expr span not yet available`,
- import declarations do not yet carry spans, so import-resolution diagnostics
  cannot be precise.

## 2. Problem Statement

Current unresolved-name diagnostics are structurally weak.

Examples:

- `ensure_known_call_targets_in_expr` in
  `crates/goby-core/src/typecheck_stmt.rs` can produce
  `unknown function or constructor`, but currently often returns `span: None`.
- ambiguity/name-resolution checks in
  `crates/goby-core/src/typecheck_ambiguity.rs` also commonly return
  `span: None`.
- import validation in `crates/goby-core/src/typecheck_validate.rs` cannot
  point at specific import clauses because `ImportDecl` has no span field yet.

As a result:

- CLI often falls back to a declaration-level error without a useful underline.
- LSP can end up with a `0:0` diagnostic range, so the editor cannot mark the
  actual unresolved token.

## 3. Goals

1. Unresolved-name diagnostics should point at the exact offending token.
2. CLI and LSP should consume the same core diagnostic and differ only in
   presentation.
3. The first implementation slice should improve unresolved names and
   ambiguity-related diagnostics without redesigning the full typechecker.
4. Expression-span propagation should be mechanical and generic, not a special
   case for `map`.
5. Import-resolution diagnostics should join the same track after the
   expression-path work is in place.

## 4. Non-goals

- global wording polish for every diagnostic in one pass,
- typo suggestions / "did you mean" search in the first slice,
- notes / secondary labels / multi-span diagnostics,
- syntax or name-resolution semantic changes,
- editor-only bespoke error generation outside `goby-core`.

## 5. Design Direction

### 5.1 Syntax-bearing nodes own source locations

For unresolved names, the source of truth should be the AST node that introduced
the name:

- `Expr::Var { span, .. }`
- `Expr::Qualified { span, .. }`
- pipeline callee location once represented explicitly

Do not reconstruct identifier columns from rendered strings during typecheck.

### 5.2 `goby-core` owns precision

`goby-cli` and `goby-lsp` should remain thin presentation layers. They should
not re-run name lookup or guess an underline range.

### 5.3 Improve one family of errors at a time

The first high-value family is:

- unresolved bare names,
- unresolved qualified names,
- use-site ambiguity diagnostics.

After that, extend the same approach to import diagnostics and then other
expression-level type errors that still use `span: None`.

## 6. Existing Related Work To Reuse

This plan should build on the diagnostic groundwork already visible in code and
comments:

- parser/body-relative span work and related D1 comments/tests,
- LSP collection/publishing path and D2b comments in `crates/goby-lsp`,
- the existing `Diagnostic` unification layer,
- existing tests proving CLI/LSP can render spans when they exist.

This should be treated as continuation work, not a fresh diagnostics system.

## 7. Phased Plan

### Phase ER1: Precise unresolved-name spans in ordinary expressions

Goal: unresolved-name diagnostics point at the exact identifier token.

Work:

- audit parser construction of `Expr::Var`, `Expr::Qualified`, and `Expr::Call`
  spans and fill obvious missing cases,
- add helper accessors for extracting the best available span from an
  expression node,
- update unresolved-name checks in `typecheck_stmt.rs` and
  `typecheck_ambiguity.rs` to use expression spans instead of `None`.

Representative done conditions:

- `rolls = map lines graphemes` without importing `map` yields a
  `TypecheckError` whose span covers `map`,
- CLI renders a caret/underline under `map`,
- LSP publishes a non-zero-width range for the same error.

### Phase ER2: Use-site ambiguity diagnostics use token spans

Goal: ambiguity errors underline the ambiguous token rather than falling back to
declaration-level context.

Work:

- thread the relevant expression span into ambiguity checks for:
  - bare names,
  - qualified names,
  - tuple-member-style qualified accesses where applicable,
- keep current semantics and wording unless a wording change is required for
  clarity.

Representative done conditions:

- ambiguous imported symbol errors point at the ambiguous use site,
- no regression in existing ambiguity behavior.

### Phase ER3: Import declaration spans

Goal: import-related resolution failures can point at the exact import clause.

Work:

- extend `ImportDecl` with span metadata,
- populate spans in top-level parser code,
- use those spans in:
  - unknown module diagnostics,
  - unknown selective-import symbol diagnostics,
  - conflicting imported embedded-default diagnostics where a single import site
    is the actionable location.

Representative done conditions:

- `import goby/list ( each, maap )` underlines `maap`,
- unknown-module diagnostics underline the module path token or a clearly
  documented minimal actionable span.

### Phase ER4: Shared helper cleanup

Goal: eliminate repeated ad-hoc `span: None` sites where the span is already
available in the AST.

Work:

- add small shared helpers for:
  - best-effort span extraction from expressions,
  - body-relative -> file-relative span conversion where needed,
  - constructing unresolved-name diagnostics consistently,
- migrate the highest-value remaining unresolved-name / ambiguity call sites
  from `span: None` to real spans.

Representative done conditions:

- common unresolved-name paths no longer open-code `TypecheckError { span: None
  }`,
- new diagnostics code uses helpers rather than duplicating span extraction.

### Phase ER5: Quality lock across core, CLI, and LSP

Goal: freeze behavior with focused regression coverage.

Required tests:

- `goby-core`
  - unresolved bare name returns precise span,
  - unresolved qualified name returns precise span,
  - ambiguous name use returns precise span,
  - import selective-symbol failure returns precise span.
- `goby-cli`
  - rendered diagnostic snippet points at the unresolved identifier.
- `goby-lsp`
  - `analyze(...)` returns a diagnostic with the expected LSP range for an
    unresolved identifier.

Acceptance examples to lock:

1. `rolls = map lines graphemes` without importing `map`
2. a qualified unresolved-name case
3. a selective-import typo
4. an ambiguous imported-name collision

## 8. Implementation Notes

### 8.1 Start with AST-based typecheck paths

The unresolved-name diagnostics in question are currently produced while
walking AST expressions during validation/typechecking. Improve that path first.
Do not block this work on a full resolved-form span redesign.

### 8.2 Keep spans narrow

Preferred underline targets:

- bare identifier: underline the identifier only,
- qualified name: start by underlining the full `receiver.member` token,
- selective import typo: underline the typo token, not the whole line, when the
  parser span model supports it.

### 8.3 Keep the diagnostic contract simple

`Diagnostic { span, message, declaration, severity }` remains the interchange
format. Do not introduce an LSP-only diagnostic model for this slice.

## 9. Open Questions

1. For `Expr::Qualified`, should unresolved-name diagnostics underline the full
   `receiver.member` token or only the missing member portion?
   Current preference: start with the full qualified token.
2. For import errors, should the preferred highlight range be:
   - the whole import line,
   - the module path,
   - or the individual missing symbol?
   Current preference: the individual missing symbol when available.
3. Should unresolved-name diagnostics stay under typechecking, or should Goby
   later grow an explicit earlier name-resolution diagnostics phase?
   Current preference: improve precision first without splitting phases.

## 10. Exit Criteria

This plan is complete when all of the following are true:

- unresolved-name diagnostics in ordinary expressions carry precise spans,
- ambiguity diagnostics at use sites carry precise spans,
- import-resolution diagnostics carry precise spans,
- CLI caret rendering and LSP underline ranges are locked by tests,
- the common unresolved-name path no longer depends on `span: None` in the
  cases users hit first.
