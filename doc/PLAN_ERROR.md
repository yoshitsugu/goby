# Goby Error Reporting Plan

Last updated: 2026-03-28

This document is the active development plan for richer compiler diagnostics.
Its first concrete target is precise unresolved-name / name-resolution error
reporting in both:

- CLI output (`goby check`, `goby run`, `goby lint`)
- LSP diagnostics (editor wave underline)

The plan is intentionally architecture-first:

- no `map`-specific or symbol-specific shortcuts,
- no duplicate error-generation logic in CLI vs LSP,
- no string-based reconstruction of source positions during typechecking,
- no one-off fixes that make future diagnostic work harder.

The end state should be modular, restart-safe, and incrementally shippable.

---

## 1. Representative User-Facing Goal

Given:

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

When `map` is not in scope, the desired result is:

- CLI:

  ```text
  sample.gb:8:11: error: unknown function or constructor `map` in 'main'
   8 |   rolls = map lines graphemes
     |           ^^^
  ```

- LSP:
  - diagnostic range covers the `map` token only,
  - editor renders the underline directly under `map`.

This same mechanism must generalize to:

- unresolved bare names,
- unresolved qualified names,
- ambiguity-at-use-site diagnostics,
- import symbol resolution errors.

---

## 2. Existing Base To Reuse

The repository already has the outer presentation layers needed for rich
diagnostics.

### 2.1 Present today

- Unified diagnostics:
  `crates/goby-core/src/diagnostic.rs`
- CLI diagnostic header + snippet rendering:
  `crates/goby-cli/src/main.rs`
- LSP diagnostic publication and `Span -> Range` conversion:
  `crates/goby-lsp/src/main.rs`
- Parser/body-relative span work for:
  - declarations,
  - statements,
  - handler clauses,
  - case arms
- Existing comments marking known span gaps:
  `expr span not yet available`

### 2.2 Current bottleneck

The missing piece is not rendering. The missing piece is precise span
production at the point where name resolution / typechecking decides an error
occurred.

Right now many relevant errors still use:

- `TypecheckError { span: None, ... }`

That prevents:

- CLI from showing a useful underline,
- LSP from publishing a useful range.

---

## 3. Problem Breakdown

There are four distinct but related problems.

### 3.1 Expression use-site span loss

Errors in these areas often know *what* failed but not *where*:

- `typecheck_stmt.rs`
- `typecheck_ambiguity.rs`
- `typecheck_check.rs`
- `typecheck_effect_usage.rs`
- `typecheck_resume.rs`
- `typecheck_branch.rs`

Many sites explicitly say `expr span not yet available`.

### 3.2 Import declaration span absence

`ImportDecl` currently does not carry span metadata. That blocks precise
diagnostics for:

- unknown module path,
- unknown selective-imported symbol,
- some import-related conflict diagnostics.

### 3.3 Data-model fragmentation risk

If each checker starts inventing custom span extraction rules, the codebase will
drift toward ad-hoc behavior. That would make future diagnostics inconsistent
and hard to maintain.

### 3.4 Presentation split risk

If CLI and LSP diverge in how they interpret diagnostics, the project will end
up maintaining two parallel UX contracts.

---

## 4. Design Principles

1. **`goby-core` owns diagnosis; frontends own rendering.**
   CLI and LSP must not perform extra name analysis to guess highlight ranges.
2. **Syntax-bearing nodes own source locations.**
   Spans should come from AST nodes, not reconstructed strings.
3. **One shared diagnostic contract.**
   `Diagnostic { span, message, declaration, severity }` remains the only
   presentation boundary in this slice.
4. **Narrowest actionable span wins.**
   Prefer the exact unresolved token over an enclosing line or declaration span.
5. **Generic span propagation over per-symbol hacks.**
   Fix the infrastructure once; do not special-case `map`, `print`, etc.
6. **Milestones must be shippable.**
   Each phase should leave the repository in a coherent, releasable state.
7. **Design-affecting uncertainty is paid down early.**
   If a question changes AST shape, span ownership, or diagnostic architecture,
   prefer resolving it in this track rather than postponing it behind local UX
   tweaks.
8. **Purely local presentation choices can be deferred.**
   If a choice does not materially affect the long-term data model or ownership
   split, it may be handled later once the structural path is locked.

---

## 5. Target Architecture

### 5.1 Responsibility split

`goby-core`

- owns AST span capture,
- owns span propagation through validation/typechecking,
- owns the final `Diagnostic` span/message/declaration payload.

`goby-cli`

- renders `Diagnostic` as:
  - header,
  - source snippet,
  - multi-character underline when `Span.end_col > Span.col`.

`goby-lsp`

- converts `Diagnostic.span` into LSP `Range`,
- publishes diagnostics,
- does not reinterpret Goby semantics to guess spans.

### 5.2 Data model direction

Short-to-medium term:

- keep using existing `Span`,
- enrich AST/import nodes with missing span fields,
- add small shared helpers for extracting "best available error span".

Longer-term compatible direction:

- a dedicated diagnostic helper layer in `goby-core` may centralize common
  span-selection policies,
- but this plan does not require redesigning the whole typechecker around a new
  error type.

### 5.3 Explicitly avoided directions

- backend-/frontend-specific diagnostic generation,
- string search over source text to locate missing identifiers,
- per-checker bespoke span rules with no shared helper layer,
- speculative suggestion engines in the same slice.

---

## 6. Scope and Non-goals

### 6.1 In scope

- unresolved bare names,
- unresolved qualified names,
- name ambiguity at use sites,
- import module / selective symbol resolution errors,
- precise CLI underline rendering for those errors,
- precise LSP ranges for those errors.

### 6.2 Out of scope for this plan

- typo suggestions / "did you mean",
- notes / related spans / secondary labels,
- general wording refresh for all diagnostics,
- full parser-span coverage for every syntax form not needed by this plan,
- new compiler phases that split resolution from typechecking unless clearly
  required later.

---

## 7. Milestones

The milestones below are intended to be implemented in order. Each one is small
enough to complete and verify independently.

### Milestone ER0: Plan and boundary lock

- [x] ER0.1 Lock the diagnostic boundary and ownership model in this document.
- [x] ER0.2 Confirm that CLI and LSP remain pure renderers of `goby_core::Diagnostic`.
- [x] ER0.3 Enumerate the first unresolved-name call sites to migrate.

ER0 decisions locked on 2026-03-28:

- `goby-core` remains the only layer allowed to decide compiler-diagnostic
  meaning, message text, declaration context, and source span in this track.
- `goby-cli` remains a renderer only:
  - it calls `goby_core::parse_module` and
    `goby_core::typecheck_module_collect_with_context`,
  - converts returned errors via `goby_core::Diagnostic::from(...)`,
  - renders the resulting payload without re-running name analysis.
- `goby-lsp` remains a renderer/transport layer only:
  - it calls `goby_core::parse_module` and
    `goby_core::typecheck_module_collect_with_context`,
  - converts returned diagnostics through `to_lsp_diagnostic(...)`,
  - it does not run a second name-resolution or span-guessing pass.
- The current synthetic LSP-only `"stdlib root not found"` environment error is
  outside the unresolved-name/import compiler-diagnostic family and is not a
  precedent for duplicating compiler diagnosis in frontends.

Current code evidence for the ownership split:

- `crates/goby-cli/src/main.rs`
  - `run()` converts parse/typecheck errors to `goby_core::Diagnostic` and
    passes them to `render_diag(...)`.
- `crates/goby-lsp/src/main.rs`
  - `analyze()` converts parse/typecheck errors to `goby_core::Diagnostic` and
    then to LSP diagnostics with `to_lsp_diagnostic(...)`.
- `crates/goby-core/src/diagnostic.rs`
  - already defines the shared `Diagnostic { span, message, declaration,
    severity }` boundary consumed by both frontends.

First migration inventory locked for ER1/ER2/ER3:

- ER2 unresolved bare-name sites
  - `crates/goby-core/src/typecheck_stmt.rs`
    - `ensure_known_call_targets_in_expr`: bare call callee
    - `ensure_known_call_targets_in_expr`: pipeline callee
  - `crates/goby-core/src/typecheck_call.rs`
    - higher-order callback validation path when
      `unresolved_callable_name(...)` succeeds
  - `crates/goby-core/src/typecheck_check.rs`
    - ordinary use-site errors still marked `expr span not yet available`
  - `crates/goby-core/src/typecheck_effect_usage.rs`
    - unresolved handler/effect-operation use sites currently returning
      `span: None`
  - `crates/goby-core/src/typecheck_resume.rs`
    - unresolved / mismatched `resume` operand sites currently returning
      `span: None`
  - `crates/goby-core/src/typecheck_branch.rs`
    - branch consistency errors still marked `expr span not yet available`
- ER3 ambiguity / qualified-name sites
  - `crates/goby-core/src/typecheck_ambiguity.rs`
    - `ensure_name_not_ambiguous(...)`
    - expression walkers currently returning `span: None` for ambiguous vars,
      constructors, qualified names, and pipeline callees
- ER4 import-resolution sites
  - `crates/goby-core/src/typecheck_validate.rs`
    - unknown module
    - unknown selective import symbol
    - import conflict/error paths blocked on `ImportDecl` lacking span metadata

Explicit ER0 boundaries:

- `typecheck_validate.rs` import diagnostics stay out of ER1/ER2 because they
  require `ImportDecl` span ownership work first.
- `ir_lower.rs`, runtime/backend limitation diagnostics, and unrelated
  type-mismatch cleanup are not part of the first unresolved-name migration
  slice.
- Pipeline-callee span ownership remains an ER1 design decision:
  - if existing AST spans already identify the callee token precisely, use that
    path,
  - otherwise record the required AST/data-model change before ER2.

Done when:

- this plan is accepted as the working roadmap,
- the first implementation slice can proceed without open architectural
  uncertainty.

### Milestone ER1: Expression-span extraction foundation

- [x] ER1.1 Audit parser construction of `Expr::Var`, `Expr::Qualified`,
  `Expr::Call`, and pipeline-related forms for usable spans.
- [x] ER1.2 Add shared helpers in `goby-core` for:
  - extracting the narrowest available expression span,
  - extracting identifier/qualified-name spans where available.
- [x] ER1.3 Add unit tests for those helpers before migrating error sites.
- [x] ER1.4 Lock the pipeline-callee span ownership decision:
  - if pipeline callee spans require AST/data-model changes that affect the
    long-term span architecture, include that structural work in this track,
  - otherwise record pipeline callee spans as an explicitly deferred local
    follow-up after the core unresolved-name path is stable.

ER1 status locked on 2026-03-28:

- Shared helper layer added in `crates/goby-core/src/typecheck_span.rs`:
  - `direct_expr_span(...)`
  - `best_available_name_use_span(...)`
  - `best_available_expr_span(...)`
- Focused tests now lock the current parser/span reality:
  - manually-constructed `Expr::Var` / `Expr::Qualified` / `Expr::Call` spans are
    consumed by the helper layer when present,
  - `parse_body_stmts("map xs")` and `parse_body_stmts("list.map xs")` keep the
    outer statement span but still leave nested callee spans as `None`.
- Current parser audit result:
  - `parser_expr.rs` can construct `Expr::Var`, `Expr::Qualified`, and
    `Expr::Call` with span fields, but ordinary string-level parsing currently
    populates them as `None`,
  - `parser_stmt.rs` records statement spans and some outer multiline-call spans,
    but does not yet attach token-precise spans to nested identifier/qualified
    callee nodes produced inside statement parsing,
  - `Expr::Pipeline` stores `callee: String` with no span field.
- Pipeline-callee decision:
  - precise pipeline-callee underlining requires an AST/data-model change,
  - that structural work is explicitly deferred until after the non-pipeline
    unresolved-name path is stable,
  - ER2 may use the shared helper for pipeline diagnostics, but `span: None` is
    an expected temporary outcome there until pipeline callee span ownership is added.

Constraints:

- no behavior change yet beyond helper introduction,
- no import parser changes yet.
- design-affecting uncertainty around pipeline span ownership must not remain
  implicit after ER1.

Done when:

- there is one shared helper path for expression-span lookup,
- new checker code can depend on helpers instead of custom span logic,
- the pipeline-callee span decision is explicitly recorded as either
  in-track work or deferred work.

### Milestone ER2: Unresolved bare-name diagnostics at use sites

- [x] ER2.1 Migrate unresolved bare-name checks in
  `crates/goby-core/src/typecheck_stmt.rs` to use expression spans.
- [x] ER2.2 Migrate unresolved bare-name diagnostics in the checker modules
  covered by this track:
  - `typecheck_stmt.rs`
  - `typecheck_check.rs`
  - `typecheck_effect_usage.rs`
  - `typecheck_resume.rs`
  - `typecheck_branch.rs`
- [x] ER2.3 For each covered module above, classify every remaining
  unresolved-name-related `span: None` site as one of:
  - completed in ER2,
  - intentionally deferred outside this track with a recorded reason.
- [x] ER2.4 Add focused tests proving that:
  - `map`-not-imported points at `map`,
  - a missing local/decl reference points at the unresolved token.

Constraints:

- no ad-hoc `map` special case,
- use the shared helper added in ER1.
- ER2 must not close with vague “high-value remaining” wording; the covered
  modules and deferred items must be explicit.

Done when:

- CLI can underline `map` exactly in the representative example,
- LSP range for that error is non-zero-width and token-aligned,
- the covered unresolved bare-name producers are explicitly partitioned into
  done vs deferred rather than left implicit.

ER2 completion partition:

- done in ER2
  - `typecheck_stmt.rs`
    - bare call callee uses `best_available_name_use_span(...)`
    - pipeline callee uses the shared helper and returns a token span only when
      the AST already owns one
  - `typecheck_call.rs`
    - unresolved higher-order callback names use
      `best_available_name_use_span(...)`
  - `typecheck_check.rs`
    - expression-owned mismatch sites now use `best_available_expr_span(...)`
  - `typecheck_effect_usage.rs`
    - `with` non-handler values, ordinary effect-op calls, qualified effect-op
      calls, method-style effect-op calls, and effect-op argument mismatches now
      use shared expression/name-span helpers
  - `typecheck_resume.rs`
    - `resume` operand mismatch / unresolved-generic sites now use
      `best_available_expr_span(...)`
  - `typecheck_branch.rs`
    - branch body mismatch sites now use `best_available_expr_span(...)`
- deferred outside ER2 with explicit reason
  - `typecheck_effect_usage.rs`
    - `resolve_handler_clause_name(...)` unknown/ambiguous handler clause names
      still return `span: None` because handler clause names are stored as raw
      strings and the AST does not yet own a clause-name token span
    - duplicate handler-clause diagnostics still return `span: None` for the
      same reason: no owned source span for the repeated clause name token
    - pipeline unhandled-effect diagnostics still return `span: None` because
      ER1 explicitly deferred pipeline-callee token ownership on the AST
  - `typecheck_stmt.rs`
    - declared-return/body mismatch still returns `span: None`; it is not an
      unresolved-name diagnostic and remains outside ER2
  - `typecheck_effect_usage.rs`
    - required-effect coverage failures in `check_callee_required_effects(...)`
      still return `span: None`; this is an effect-coverage diagnostic, not an
      unresolved bare-name use-site failure

### Milestone ER3: Qualified-name and ambiguity diagnostics

- [x] ER3.1 Migrate qualified unresolved-name errors to use precise spans.
- [x] ER3.2 Migrate use-site ambiguity diagnostics in
  `crates/goby-core/src/typecheck_ambiguity.rs` to use precise spans.
- [x] ER3.3 Decide and lock the initial underline policy for qualified names:
  full token (`receiver.member`) vs member-only.
- [x] ER3.4 Add regression tests for:
  - unresolved qualified name,
  - ambiguous imported name collision at use site,
  - tuple-member-related ambiguity/error paths that overlap this machinery.

ER3 partition (done vs deferred):
- Done: `Expr::Var` ambiguity span, `Expr::Qualified` ambiguity span,
  `Expr::Qualified` tuple-member-access 3 error sites.
- Deferred (no AST span field): `Expr::RecordConstruct` 5 error sites,
  `Expr::MethodCall` (no span field on node).
- Deferred by policy: `Expr::Pipeline` callee (callee is `String`,
  consistent with ER2); `Expr::Block` structural error (not a name-use error).

Constraints:

- keep semantics unchanged,
- do not mix in import span work yet.

Done when:

- ambiguity errors no longer fall back to declaration-only context in common
  use-site cases,
- qualified unresolved names are underlined consistently in CLI and LSP.

### Milestone ER4: Import declaration spans

- [ ] ER4.1 Extend `ImportDecl` with span metadata sufficient for precise
  import diagnostics.
- [ ] ER4.2 Populate import spans in parser/top-level parsing code.
- [ ] ER4.3 Update import validation paths in
  `crates/goby-core/src/typecheck_validate.rs` to use import spans.
- [ ] ER4.4 Add regression tests for:
  - unknown module path,
  - unknown selective-import symbol,
  - conflicting import-related diagnostics where a single import site should be
    highlighted.

Constraints:

- choose the narrowest actionable import span practical for each error kind,
- avoid reworking unrelated parser structures.

Done when:

- `import goby/list ( each, maap )` can underline `maap`,
- import-resolution diagnostics have useful spans in both CLI and LSP.

### Milestone ER5: Shared diagnostic-construction cleanup

- [ ] ER5.1 Add common constructors/helpers for frequent diagnostic families:
  - unresolved bare name,
  - unresolved qualified name,
  - ambiguity at use site,
  - import symbol resolution error.
- [ ] ER5.2 Replace duplicated `TypecheckError { span: None, ... }` sites in
  the covered diagnostic families with helper-backed construction.
- [ ] ER5.3 Document the preferred span-selection policy near the helper layer.

Constraints:

- do not refactor unrelated error families in the same slice,
- keep helpers small and domain-specific rather than introducing a giant
  diagnostic framework prematurely.

Done when:

- the covered error families no longer rely on open-coded ad-hoc span logic,
- adding a new unresolved-name diagnostic follows one established pattern.

### Milestone ER6: CLI rendering quality lock

- [ ] ER6.1 Lock snippet rendering tests for multi-character underline width on
  unresolved-name spans.
- [ ] ER6.2 Lock at least one `goby check` rendered output fixture or focused
  assertion for the representative `map` case.
- [ ] ER6.3 Lock equivalent CLI rendering for an import typo case.

Constraints:

- CLI remains a renderer only,
- no CLI-only error heuristics.

Done when:

- `goby check` and `goby run` both show useful underline-rich errors for the
  covered families,
- range spans render as whole-token underlines, not single-caret fallbacks.

### Milestone ER7: LSP range parity lock

- [ ] ER7.1 Add or extend `goby-lsp` tests to verify expected range for the
  unresolved `map` example.
- [ ] ER7.2 Add LSP tests for:
  - unresolved qualified name,
  - import typo,
  - ambiguity use site.
- [ ] ER7.3 Verify UTF-8 / UTF-16 conversion behavior remains correct when
  diagnostics appear on lines containing multi-byte text nearby.

Constraints:

- LSP still consumes `Diagnostic` only,
- no separate name-resolution layer inside the LSP server.

Done when:

- covered CLI and LSP diagnostics are span-parity-locked,
- editor underline behavior is predictable from `goby-core` output.

### Milestone ER8: Track closure and follow-up boundary

- [ ] ER8.1 Review remaining `expr span not yet available` sites and separate:
  - done by this track,
  - intentionally deferred.
- [ ] ER8.2 Record which remaining diagnostics still lack precise spans and why.
- [ ] ER8.3 Update `doc/STATE.md` and, if needed, `doc/PLAN.md` with closure or
  next-slice notes.

Done when:

- the track has a clear closure report,
- remaining work is explicitly partitioned rather than hidden in comments.

---

## 8. Execution Order

Strict order:

1. ER0 before implementation
2. ER1 before any checker migration
3. ER2 before ER3
4. ER3 before ER4
5. ER4 before ER5
6. ER5 before ER6/ER7
7. ER6 and ER7 before ER8

Reason:

- expression use-site spans are the highest-value first win,
- import spans are structurally separate and should not block early progress,
- cleanup should happen only after the first precision path is proven,
- presentation locks come after `goby-core` output is stable.

---

## 9. Module Ownership

To keep the work modular, ownership should be split like this.

### 9.1 `crates/goby-core`

Primary owner of:

- AST/import span capture,
- span helpers,
- unresolved-name diagnostic construction,
- ambiguity diagnostic construction,
- import-resolution diagnostic construction.

Likely touch points:

- `ast.rs`
- `parser_top.rs`
- `parser_expr.rs`
- `typecheck_stmt.rs`
- `typecheck_ambiguity.rs`
- `typecheck_validate.rs`
- `diagnostic.rs`

### 9.2 `crates/goby-cli`

Primary owner of:

- snippet rendering behavior tests,
- header/snippet formatting quality lock.

Not owner of:

- deciding where an unresolved name is.

### 9.3 `crates/goby-lsp`

Primary owner of:

- `Diagnostic.span` -> LSP `Range` tests,
- publication/parity checks.

Not owner of:

- name-resolution heuristics.

---

## 10. Testing Strategy

Each milestone should add focused tests closest to the owned behavior.

### 10.1 `goby-core`

Must cover:

- unresolved bare names,
- unresolved qualified names,
- ambiguity at use site,
- import typo/module errors,
- helper span selection behavior.

### 10.2 `goby-cli`

Must cover:

- whole-token underline width,
- single-line snippet alignment,
- representative unresolved-name rendering.

### 10.3 `goby-lsp`

Must cover:

- precise range for `map` unresolved case,
- parity with CLI token selection,
- UTF-16 conversion stability around multibyte text.

### 10.4 Quality gates

At minimum for each non-trivial slice:

- `cargo fmt`
- `cargo check`
- focused crate tests
- `cargo test`

---

## 11. Risks and Mitigations

### Risk A: Span plumbing spreads ad-hoc through many checkers

Mitigation:

- require ER1 helper layer first,
- require ER5 helper-backed cleanup before closing the track.

### Risk B: Import span work balloons parser churn

Mitigation:

- isolate import-span work in ER4,
- do not mix it into early unresolved-name slices.

### Risk C: CLI and LSP drift apart

Mitigation:

- keep one `Diagnostic` contract,
- lock both renderers with parity-oriented tests.

### Risk D: Wide-scope diagnostic cleanup stalls progress

Mitigation:

- only migrate the covered families,
- explicitly defer unrelated `span: None` sites in ER8.

---

## 12. Current Decisions

1. Qualified unresolved names should initially underline the full
   `receiver.member` token.
   Rationale:
   - this keeps the first shipped rule simple and consistent,
   - it avoids prematurely splitting receiver/member responsibility in the
     diagnostic model,
   - it is compatible with later refinement if member-only highlighting proves
     clearly better.
   Status: locked in ER3. Implemented via `best_available_name_use_span(expr)`
   at `Expr::Qualified` call sites in `typecheck_ambiguity.rs`.
2. Unknown module diagnostics should initially target the module path, not the
   full import clause.
   Rationale:
   - the module path is the smallest clearly actionable region in the common
     case,
   - this keeps import diagnostics precise without forcing path-segment-level
     metadata in the first slice.
3. Pipeline callee spans should be handled in this track if, during ER1, they
   are found to affect AST shape or span ownership in a way that changes the
   long-term design. If they turn out to be a purely local extension with no
   architectural consequence, they can be deferred until after the core
   unresolved-name path is stable.
   Rationale:
   - design-affecting uncertainty should be reduced early,
   - purely local rendering/value improvements do not need to block the main
     span-propagation path.
   Status hook:
   - this decision must be recorded by ER1.4 and must not remain an informal
     note by the time implementation leaves ER1.

---

## 13. Exit Criteria

This plan is complete when all of the following are true:

- unresolved-name diagnostics in covered ordinary-expression cases carry precise
  spans,
- ambiguity diagnostics at use sites carry precise spans,
- import-resolution diagnostics carry precise spans,
- CLI renders whole-token underline snippets for covered range-span cases,
- LSP publishes matching token-aligned ranges for the same cases,
- the covered families no longer rely on ad-hoc `span: None` construction,
- the ER2 covered-module set is explicitly marked done or deferred on a
  per-module basis,
- the pipeline-callee span decision is explicitly recorded rather than implied,
- remaining unaddressed span gaps are explicitly cataloged rather than hidden in
  comments.
