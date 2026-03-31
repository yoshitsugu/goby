# Goby Error Reporting Plan

Last updated: 2026-03-29

This document is the active planning note for compiler-diagnostic quality.
It is intentionally kept compact:

- active and follow-on tracks stay here,
- completed tracks are summarized only at closure level,
- implementation-by-implementation history belongs in git and `doc/STATE.md`,
  not in this plan.

The shared architectural rule remains:

- `goby-core` owns diagnostic meaning, message text, and source span,
- `goby-cli` and `goby-lsp` render the shared `Diagnostic` payload,
- frontends must not guess spans by reinterpreting Goby semantics.

Plan-label hygiene:

- labels in this document such as track names or roadmap shorthand are planning-only metadata
- do not copy those labels into code comments, test names, diagnostics, or user-visible strings
- when implementation notes need context, describe the technical purpose directly

---

## 1. Shared Design Rules

1. **`goby-core` owns diagnosis; frontends own rendering.**
2. **Syntax-bearing nodes own source locations.**
3. **One shared diagnostic contract.**
   `Diagnostic { span, message, declaration, severity }` remains the boundary.
4. **Narrowest actionable span wins.**
   Prefer the blamed token or argument expression over enclosing lines when the
   AST owns that location.
5. **Generic rules over symbol-specific branches.**
   Do not special-case `map`, `print`, `println`, or any one fixture.
6. **Closure requires explicit deferred-gap reporting.**
   If a diagnostic family still lacks precise spans, record it explicitly rather
   than leaving open `span: None` comments to imply future work.

---

## 2. Completed Track ER: Name-Resolution Rendering Parity

Track ER is complete.

What ER closed:

- unresolved bare-name diagnostics at ordinary use sites,
- unresolved qualified-name diagnostics,
- ambiguity-at-use-site diagnostics,
- import module / selective-symbol diagnostics,
- CLI snippet rendering parity for those families,
- LSP range parity for those families.

Representative result:

```text
hoge.gb:7:3: error: unknown function or constructor `prntln` in 'main'
7 |   prntln "test"
  |   ^^^^^^
```

ER closure summary:

- `goby-core` now produces token-precise spans for the covered unresolved-name /
  ambiguity / import-resolution families.
- CLI and LSP parity is regression-locked for the covered cases.
- ER did not attempt to solve every remaining `span: None` in the compiler.

Deferred outside ER:

- `crates/goby-core/src/typecheck_ambiguity.rs`
  - block-structure validation (`block expression must end with an expression`)
- `crates/goby-core/src/typecheck_stmt.rs`
  - declaration body vs declared return-type mismatch
- `crates/goby-core/src/typecheck_effect_usage.rs`
  - required-effect coverage failures
  - pipeline unhandled-effect diagnostics
- `crates/goby-core/src/typecheck_validate.rs`
  - conflicting effect declarations across imports/local declarations
  - `@embed` source-path validation
- `crates/goby-core/src/typecheck_types.rs`
  - type-declaration validation spans

These remain follow-up work because they need a separate blame-site policy,
body-relative-to-file-relative wiring, or additional syntax/span ownership.

---

## 3. Active Follow-on Track TD: Typed Diagnostic Rendering Parity

Track ER solved unresolved-name-style diagnostics. The next slice is to give
type errors the same CLI/LSP rendering quality.

Representative goal:

Given:

```goby
f : Int -> Int
f a = a + 10

main : Unit -> Unit can Print
main =
  b = f "a"
  println "test"
```

The desired CLI result is:

```text
hoge.gb:6:9: error: `f` expects argument of type `Int` but got `String` in 'main'
6 |   b = f "a"
  |         ^^^
```

And the corresponding LSP diagnostic should:

- highlight only the blamed argument expression according to the span policy
  below,
- reuse the same `Diagnostic` contract already used by CLI,
- avoid frontend-side guessing.

Locked acceptance fixture for TD:

- program: `crates/goby-cli/tests/fixtures/type_mismatch_arg_input.gb`
- expected CLI stderr: `crates/goby-cli/tests/fixtures/type_mismatch_arg_expected.txt`
- canonical `check` command:
  - `cargo run -p goby-cli -- check crates/goby-cli/tests/fixtures/type_mismatch_arg_input.gb`
- canonical `run` parity command:
  - `cargo run -p goby-cli -- run crates/goby-cli/tests/fixtures/type_mismatch_arg_input.gb`
- `check` and `run` must agree on the typecheck diagnostic family and rendered
  message/snippet for this fixture; `run` must fail before lowering/codegen.

### 3.1 Scope

- ordinary first-order argument type mismatches,
- qualified ordinary-call argument mismatches,
- later-argument mismatches in curried ordinary calls,
- local-bound / partially-applied ordinary-call remainder mismatches,
- effect-op argument mismatches where the wrong argument expression is already
  known,
- `resume` argument mismatches where the wrong argument expression is already
  known.

### 3.2 Non-goals

- suggestion text (`did you mean`),
- secondary labels / notes,
- wide wording refresh for every typecheck diagnostic family,
- diagnostics whose blamed syntax is still not owned precisely enough to choose
  an honest span.

### 3.3 Constraints

- keep `goby-core` as the sole owner of typed diagnostic meaning and span,
- reuse the existing ER helper/renderer contract rather than inventing a second
  type-error presentation path,
- do not special-case one function such as `f`, `print`, or `println`,
- if a typed diagnostic still lacks a precise span, record that explicitly
  instead of letting the invalid program drift into lowering/runtime/codegen.

### 3.4 Typed Span Policy

- The default TD policy is: highlight the whole blamed argument expression.
- If the AST already owns a narrower token that is also the honest semantic
  blame site, that narrower token may be used instead.
- Do not let checker-specific convenience choose different span widths for
  equivalent mismatch families.
- If a typed diagnostic cannot yet identify an honest blamed expression under
  this policy, defer it explicitly instead of inventing a pseudo-precise span.

Track TD is complete when all of the following are true:

- the locked fixture and commands above are implemented and parity-locked,
- the covered typed mismatch families use the typed span policy consistently,
- CLI and LSP parity is regression-locked for the covered families,
- remaining typed diagnostics without precise spans are explicitly cataloged as
  deferred.

### 3.5 Milestones

#### Milestone TD0: Boundary Lock and Inventory

- [ ] TD0.1 Enumerate typed diagnostic families that already know the blamed
  expression but still return `span: None` or an overly wide span.
- [ ] TD0.2 Partition those families into:
  - can become token-precise with existing AST ownership,
  - require new span ownership or a separate follow-up track.
- [ ] TD0.3 Lock the initial family set for TD so the slice stays bounded.

Done when:

- the first typed-diagnostic migration set is explicit rather than open-ended.

#### Milestone TD1: Ordinary Call Argument Mismatch Spans

- [ ] TD1.1 Ensure ordinary first-order argument mismatch diagnostics point at
  the mismatched argument expression, not only declaration context.
- [ ] TD1.2 Add focused regressions for:
  - bare ordinary call mismatch,
  - qualified ordinary call mismatch,
  - later-argument mismatch,
  - partially-applied remainder mismatch.
- [ ] TD1.3 Record any remaining ordinary-call mismatch sites that still cannot
  become precise and why.

Done when:

- the covered ordinary-call mismatch family renders with source snippets in CLI
  and token-aligned ranges in LSP.

#### Milestone TD2: Existing Typed-Argument Families Parity

- [ ] TD2.1 Audit and tighten effect-op argument mismatch spans where the wrong
  argument expression is already known.
- [ ] TD2.2 Audit and tighten `resume` argument mismatch spans for the same
  reason.
- [ ] TD2.3 Add focused regressions proving these families keep their current
  ownership/messages while gaining precise spans.

Done when:

- typed argument-mismatch families that already know the blamed argument gain
  snippet/range parity without moving diagnosis out of `goby-core`.

#### Milestone TD3: CLI Rendering Lock for Typed Diagnostics

- [ ] TD3.1 Add a representative `goby check` fixture for the ordinary mismatch
  example above.
- [ ] TD3.2 Add a `goby run` regression proving the same invalid program fails
  during typecheck with the same rendered diagnostic rather than drifting into
  lowering/codegen.
- [ ] TD3.3 Lock at least one later-argument or qualified-call typed mismatch
  rendering case.

Done when:

- typed diagnostics render with the same snippet/header quality now locked for
  unresolved-name diagnostics.

#### Milestone TD4: LSP Range Parity for Typed Diagnostics

- [ ] TD4.1 Add LSP tests for ordinary argument mismatch range selection.
- [ ] TD4.2 Add at least one parity test for a qualified or later-argument
  typed mismatch.
- [ ] TD4.3 Confirm UTF-16 conversion remains correct for typed diagnostic
  ranges on multibyte-adjacent lines.

Done when:

- editor underline behavior for the covered typed families is predictable from
  `goby-core` spans alone.

#### Milestone TD5: Closure and Deferred-Gap Report

- [ ] TD5.1 Record which typed diagnostic families now have snippet/range parity.
- [ ] TD5.2 Explicitly catalog the remaining typed diagnostics that still lack
  precise spans and the ownership reason for each.
- [ ] TD5.3 Update `doc/STATE.md` with closure or next-slice notes.

Done when:

- typed-diagnostic rendering coverage is explicitly partitioned into done vs
  deferred, matching the closure standard used in Track ER.

---

## 4. Module Ownership

### 4.1 `crates/goby-core`

Primary owner of:

- span capture and span propagation,
- typed and unresolved-name diagnostic construction,
- helper policies for span selection.

### 4.2 `crates/goby-cli`

Primary owner of:

- snippet rendering behavior tests,
- header/snippet rendering quality locks.

Not owner of:

- deciding where diagnostics should point.

### 4.3 `crates/goby-lsp`

Primary owner of:

- `Diagnostic.span -> LSP Range` conversion tests,
- publication/parity checks.

Not owner of:

- compiler-semantic reinterpretation for range guessing.

---

## 5. Testing Strategy

### 5.1 `goby-core`

Must cover:

- the active typed diagnostic families in TD,
- any helper span-selection behavior introduced for those families,
- explicit done-vs-deferred partitioning when a family cannot yet become
  precise.

### 5.2 `goby-cli`

Must cover:

- whole-token / whole-argument underline width,
- representative rendered output fixtures for active TD families,
- `goby check` / `goby run` parity for invalid programs that should fail during
  typecheck.

### 5.3 `goby-lsp`

Must cover:

- token-aligned typed diagnostic ranges for active TD families,
- parity with CLI token selection,
- UTF-16 conversion stability near multibyte text.

### 5.4 Quality Gates

At minimum for each non-trivial slice:

- `cargo fmt`
- `cargo check`
- focused crate tests
- `cargo test`

---

## 6. Exit Criteria for This Plan

This plan is in good shape when all of the following are true:

- completed tracks are summarized only at closure level,
- the active typed-diagnostic rendering track is explicit and bounded,
- ownership between `goby-core`, CLI, and LSP remains unambiguous,
- remaining deferred span gaps are cataloged rather than implied.
