# Goby IR Lowering Plan

This document is the active execution plan for bringing Goby's AST-to-IR
lowering to durable, long-term-complete coverage.

Purpose:

- eliminate the current "pure IR subset" ceiling as the main architecture limit,
- stop growing backend support around AST-shape fallbacks and recognizers,
- make IR the normal semantic handoff between the front-end and backends,
- prefer principled IR evolution over local rewrites that only unblock one syntax form.

Relationship to other documents:

- `doc/LANGUAGE_SPEC.md` remains the source of truth for current language syntax and semantics.
- `doc/PLAN.md` remains the top-level roadmap.
- `doc/PLAN_IR.md` is the detailed plan for the IR-lowering work that now takes top priority.

## 1. Why This Is Top Priority

Current backend friction is not primarily a Wasm problem. It is an IR-boundary
problem.

Examples:

- `Read.read` / `Print.println` work better than bare prelude names because the current
  lowerer recognizes only some effect-call shapes.
- `list.get lines 1` is lowerable while `lines[1]` is not, even though they express the
  same semantics.
- runtime support keeps needing fallback logic because semantically ordinary programs do
  not reach the shared IR in a stable, backend-friendly form.

If this is addressed with one-off rewrites at the backend boundary, Goby will accumulate
architecture debt in exactly the place where it most needs one semantic source of truth.

Therefore:

- IR lowering completeness takes precedence over adding more backend-side recognizers.
- when a supported language construct fails to lower, default action is to improve IR
  design/lowering, not to special-case that construct in Wasm or interpreter plumbing.
- temporary breakage is acceptable while converging on the better IR architecture.

## 2. Problem Statement

The current IR lowering is still organized around a "pure IR subset" model. That model was
useful as a bootstrap phase, but it is now the main blocker.

Today, the lowerer still rejects or incompletely models major AST forms, including:

- lambda expressions,
- method calls,
- pipelines,
- case expressions,
- list literals,
- list indexing,
- non-unit tuples,
- record construction,
- mutable binding / assignment forms,
- some effect-call forms that depend on AST surface spelling rather than semantic identity.

This creates three bad outcomes:

1. semantically equivalent programs lower differently depending on syntax spelling,
2. backends need fallback classification or handwritten recognizers,
3. future features will keep reopening the same boundary instead of extending one durable IR.

## 3. Architectural Direction

### 3.1 Core Direction

Goby should move from a "pure subset IR" mindset to a "language-semantic IR" mindset.

That means:

- the shared IR should be able to represent the language constructs that Goby intends to run,
  even when those constructs are not yet optimized in every backend,
- AST-to-IR lowering should preserve semantics, not enforce an artificial purity gate,
- backend-specific simplification or fusion should happen after IR construction, not by making
  IR artificially too small to express the source program.

### 3.2 Design Rules

1. Do not add backend-specific ad-hoc AST recognizers when the construct belongs in IR.
2. Do not encode source-syntax accidents into semantic lowering behavior.
3. Prefer explicit IR nodes over hidden rewrite conventions when the semantics are first-class.
4. Allow desugaring only when it is semantics-preserving and stable across backends.
5. Treat IR as the shared semantic contract for:
   - runtime/interpreter behavior,
   - Wasm lowering,
   - future tooling/analysis consumers.

### 3.3 Preferred Shape

Preferred long-term shape:

`AST -> resolved AST / symbol-resolved form -> shared IR -> backend IR / analysis IR -> executable backend`

Not preferred:

- `AST -> special-case backend matcher -> Wasm`
- `AST -> interpreter-only behavior` for ordinary language constructs
- "support" implemented primarily by recognizing a small catalog of source shapes

### 3.4 Resolution Boundary

IR lowering should not infer semantic identity from raw surface spelling when that
identity can be resolved earlier.

Locked direction:

- shared IR should be constructed from a name-resolved / symbol-resolved front-end form,
  not directly from raw AST spelling alone,
- the project should use a distinct resolved front-end form as the lowering input to shared IR,
  rather than incrementally attaching more ad-hoc resolution fields onto the raw/typed AST,
- effect operations should lower from resolved operation identity, not from textual patterns
  like "qualified name happens to look like `Read.read`",
- ordinary callable references should already distinguish at least:
  - local bindings,
  - top-level declarations,
  - stdlib/helper references,
  - effect operation references,
- semantically equivalent spellings such as bare prelude names, imported names, and
  qualified names should converge before or at the resolved-to-IR boundary, not later in
  backend-specific code.

Implication:

- effect-call normalization is not just an `ir_lower.rs` rewrite. It depends on introducing
  or exposing resolved symbol information as a first-class lowering input.
- `ir_lower` should ultimately consume the resolved front-end form, not raw AST plus local
  name-pattern heuristics.

### 3.5 Locked Resolved-Input Strategy

The chosen direction is:

- introduce a distinct resolved front-end form between AST/typecheck and shared IR,
- do not treat the existing AST or typed AST as the long-term lowering input,
- do not continue the pattern of attaching more one-off resolution-only fields directly to
  raw AST nodes in order to unblock one lowering gap.

Planned shape:

`Parsed AST -> typecheck/name resolution -> ResolvedModule / ResolvedDecl / ResolvedExpr -> shared IR`

Minimum semantic identities that the resolved form must carry:

- `LocalRef`
  - for lexical local bindings and parameters
- `DeclRef`
  - for top-level function or value declarations
- `HelperRef`
  - for stdlib/runtime helper calls such as `goby/string.split` and `goby/list.get`
- `EffectOpRef`
  - for effect operations such as `Read.read`, `Read.read_line`, `Print.print`, `Print.println`
- `TypeCtorRef` or equivalent product-data constructor identity
  - if tuples/records/constructors continue to need distinct lowering-time meaning

Ownership boundary:

- parser continues to own syntax only,
- typecheck / symbol-resolution layer owns semantic identity,
- resolved form owns the normalized callable/effect references consumed by `ir_lower`,
- shared IR owns semantic execution structure, not raw source spelling.

Initial implementation note:

- the first version of the resolved form does not need to cover every AST family at once,
  but for any family it does cover, `ir_lower` should consume the resolved form rather than
  re-deriving meaning from raw syntax.
- the first families to move should be:
  - effect operations,
  - helper calls,
  - local / top-level callable references.

## 4. Scope of "Complete IR Lowering"

For this track, "complete" does not mean every backend can execute every feature immediately.
It means the front-end can lower the language's supported constructs into shared IR without
depending on syntax-shaped escape hatches.

Completion target:

- all currently supported source constructs have a defined AST-to-IR mapping,
- semantically equivalent surface forms lower to semantically equivalent IR,
- unsupported execution in a backend is expressed as a backend limitation, not an IR-construction failure,
- new language work must extend IR intentionally rather than reintroducing subset-only lowering.

For avoidance of doubt:

- mutable local forms already present in the language surface are in scope for this track.
- "complete IR lowering" does not permit leaving shipped mutable forms outside shared IR as a
  permanent exception.

## 5. Work Breakdown

### IR0. Architecture Lock

Goal: replace the "pure IR subset" framing with an explicit long-term IR charter.

Deliverables:

- document the role of shared IR in `goby-core`,
- define which constructs must exist directly in IR vs which may desugar,
- define lowering invariants and error policy.

Required decisions:

- whether list index lowers to a dedicated IR node or a canonical helper-call form,
- whether pipelines lower to ordinary calls at AST-to-IR time or stay explicit in IR,
- whether method call syntax is only sugar over qualified/global calls or has distinct semantics,
- what resolved front-end representation feeds shared IR and which symbol identities it must carry,
- how mutable locals and assignment are represented in shared IR.

Done when:

- this plan is accepted as the active direction,
- `doc/PLAN.md` points to this plan as the top-priority track,
- later implementation work can follow one coherent rule set instead of local choices.

### IR1. Inventory and Mapping Table

Goal: enumerate every AST construct and define its lowering status.

Deliverables:

- a construct inventory table in this document or a linked follow-up note,
- for each construct:
  - current status,
  - canonical semantic form,
  - normalization boundary,
  - target IR form,
  - whether desugaring is allowed,
  - blockers,
  - representative tests.

Minimum inventory categories:

- literals and operators,
- local bindings and sequencing,
- function calls,
- effect operations,
- handlers / with / resume,
- lambda / higher-order values,
- if / case,
- list literals / spread / indexing,
- tuples / tuple index,
- records,
- mutable forms,
- import/prelude-driven name resolution interactions.

Done when:

- there is no "unknown unsupported set" left,
- each lowering gap is attached to an owned milestone.

Additional requirement:

- the inventory must distinguish three states instead of only "supported/unsupported":
  - lowers cleanly to shared IR,
  - lowers to shared IR but not yet executable in some backend,
  - cannot yet lower to shared IR.
- the inventory must make it impossible for semantically equivalent spellings to look
  "covered" while still lowering through different rules.
  At minimum, each row must identify:
  - the canonical semantic form,
  - the stage where normalization occurs
    (`parser`, `resolved form`, `IR lowering`, or `not yet normalized`).

Initial inventory table:

| Construct family | Current status | Canonical semantic form | Normalization boundary | Canonical IR target | Desugaring allowed | Owner milestone | Representative tests / notes |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Bare / imported / qualified effect ops (`read`, `Read.read`, imported aliases) | partially lowerable; some spellings still depend on ad-hoc rules | `EffectOpRef(effect, op)` | resolved form | `PerformEffect` | no ad-hoc spelling-based desugar after resolved form | IR3, IR4 | `crates/goby-core/src/ir_lower.rs` effect-call tests; runtime-I/O parity tests |
| Direct helper calls (`string.split`, `list.get`, `list.each`) | partially lowerable; some sugar converges only late | `HelperRef(module, name)` | resolved form | `Call(GlobalRef/HelperRef canonical target)` or dedicated IR decided by IR2 | yes, if one canonical helper-call form is chosen | IR3, IR4, IR5 | split/index lowering tests in `goby-wasm` and `goby-core` |
| List index sugar (`lines[1]`) | lowerable today only via canonical rewrite, not yet governed by resolved-form architecture | same semantic family as `HelperRef(list.get)` or explicit index IR, to be locked in IR0 | IR lowering today; target is resolved form + canonical IR strategy | helper-call canonical form or dedicated IR node | yes, but only via the IR0-locked canonical strategy | IR0, IR5 | `lower_list_index_to_list_get_call` and split-index runtime tests |
| Pipelines | not yet lowered cleanly | ordinary call chain or explicit pipeline IR, to be locked in IR0 | not yet normalized | TBD in IR0 / IR2 | TBD in IR0 | IR0, IR2 | parser and runtime pipeline tests already exist |
| Method calls | not yet lowered cleanly | qualified/member call canonical form, to be locked in IR0 | not yet normalized | TBD in IR0 / IR2 | TBD in IR0 | IR0, IR2 | parser method-call tests |
| List literals / spread | not yet lowered cleanly to shared IR | collection construction semantic form | not yet normalized | list literal/spread IR or canonical constructor/helper form | yes, if semantics remain backend-independent | IR2, IR5 | parser list/spread tests; runtime list tests |
| `case` | not yet lowered cleanly to shared IR | branch/match semantic form | not yet normalized | explicit `case`/match IR or equivalent canonical control-flow form | limited; only if semantics stay explicit | IR2, IR6 | parser/type/runtime `case` tests |
| Lambda / higher-order values | not yet lowered cleanly to shared IR | function-value / closure semantic form | not yet normalized | closure IR or canonical lifted form | yes, only with explicit capture semantics | IR2, IR7 | runtime higher-order/lambda parity tests |
| Non-unit tuples | not yet lowered cleanly to shared IR | tuple value semantic form | not yet normalized | tuple IR / product-data IR | yes, if canonical and backend-independent | IR2, IR8 | parser/runtime tuple tests |
| Record construction | not yet lowered cleanly to shared IR | record value semantic form | not yet normalized | record/product-data IR | yes, if canonical and backend-independent | IR2, IR8 | parser/runtime record tests |
| Mutable locals / assignment | intentionally still outside shared IR | mutable local semantic form | not yet normalized | mutable binding / assignment IR | no permanent exception outside IR | IR0, IR2, IR9 | parser/runtime mutation tests |

### IR2. Shared IR Expansion

Goal: add or normalize shared IR forms so supported source constructs can lower cleanly.

Entry condition:

- IR3 is complete for the semantic family being expanded.
- shared IR expansion must not proceed by teaching raw AST-based heuristics to compensate for
  an unresolved front-end boundary.

Likely work items:

- list/index representation,
- collection literals and spread representation,
- tuple values beyond unit,
- case representation,
- lambda/function-value representation,
- mutable local / assignment representation,
- canonical representation for effect operations independent of surface spelling.

Design bar:

- no new IR node should exist only to satisfy one current backend workaround,
- each addition must have a semantics explanation and a plausible path to multiple consumers.

Done when:

- the target IR can express the supported language surface without the current subset exclusions.

Required artifacts:

- IR design notes or inline docs for each newly introduced semantic family,
- one focused test or snapshot that demonstrates each new IR form directly,
- a short rejected-alternatives note when choosing between "new IR node" and "desugar to existing IR".

### IR3. Lowering Implementation by Semantic Family

Goal: implement lowering in coherent ownership slices instead of syntax-by-syntax patching.

Recommended sequence:

1. Resolution boundary and effect-call normalization
2. Collections: list literal, spread, indexing
3. Control flow: case and related branch lowering
4. Function values: lambda and higher-order capture model
5. Product data: tuples and records
6. Mutable forms: mutable locals and assignment

For each slice:

- update IR definitions first when needed,
- lower all related AST forms together,
- add focused snapshots / unit tests,
- remove now-obsolete fallback assumptions in downstream code where practical.

### IR4. Backend Boundary Convergence

Goal: make backends consume IR as the normal path.

Required outcomes:

- Wasm lowering should not depend on source-syntax distinctions like `lines[1]` vs `list.get lines 1`,
- Wasm/runtime behavior should not depend on source-syntax distinctions like `read`, imported `read`,
  and `Read.read`,
- runtime-I/O support should explain failures in backend terms, not in "this syntax failed IR lowering" terms,
- fallback/interpreter paths should consume equivalent semantics from IR or a clearly aligned runtime representation.

Done when:

- `runtime_io_plan.rs` and similar layers no longer need to compensate for ordinary IR-lowering gaps,
- backend unsupported cases are mostly about execution capability, not front-end shape mismatch.

### IR5. Deletion of the "Pure IR Subset" Model

Goal: remove the old conceptual model from docs, comments, and code paths.

Required work:

- rename/rewrite comments that describe the lowerer as a subset-only path,
- remove stale unsupported diagnostics that are only true because of the old architecture,
- ensure new unsupported errors mean either:
  - genuinely unsupported language feature, or
  - intentionally deferred backend execution support.

Done when:

- "pure IR subset" is no longer the governing architecture concept,
- the codebase communicates one stable lowering model.

## 6. Invariants

All implementation under this plan must preserve:

1. Panic-free lowering for user-controlled source in normal compiler flows.
2. Explicit, readable lowering errors when a construct is truly unsupported.
3. One semantic source of truth across front-end, fallback runtime, and Wasm lowering.
4. No new backend-specific AST shape recognizers for constructs that belong in shared IR.
5. Semantically equivalent source forms must not diverge purely because of syntactic spelling.
6. Tests must lock semantics at the IR boundary, not only end-to-end runtime behavior.
7. Shipped mutable forms must not bypass shared IR as a permanent architecture exception.
8. Name/effect resolution responsibilities must be explicit; the lowerer must not silently
   recreate ad-hoc resolution logic from raw syntax when resolved identity should already exist.

## 7. Milestones

- [x] IR0. Architecture lock
  - lock the resolved-front-end -> shared IR pipeline shape
  - lock mutable-form inclusion in shared IR scope
  - lock initial decisions for list index, pipelines, method calls, and effect-op identity
  - implementation checklist:
    - explicitly lock the use of a distinct resolved front-end form
    - name the owning input type that will feed `ir_lower`
    - write the construct-to-IR decision table for:
      - list index,
      - pipelines,
      - method calls,
      - mutable locals / assignment,
      - effect operation references
    - identify the concrete owning modules for:
      - resolved input representation,
      - shared IR definitions,
      - AST-to-IR lowering,
      - lowering tests
    - add a short "first implementation slice" note naming the exact files expected to change first
  - validation checklist:
    - `doc/PLAN_IR.md` contains the locked decisions and ownership notes
    - the document explicitly rejects "keep extending typed AST with ad-hoc resolution-only fields"
      as the default architecture
    - any conflicting comments in `goby-core` are updated in the same slice
- [x] IR1. Construct inventory and mapping table
  - enumerate all AST constructs and their lowering state
  - mark each gap as IR gap vs backend gap
  - implementation checklist:
    - add the inventory table to this document
    - include one row per user-visible AST family, not only per parser enum
    - mark each row with:
      - lowering status,
      - canonical semantic form,
      - normalization boundary,
      - canonical IR target,
      - owner milestone,
      - representative test file or test name
  - validation checklist:
    - there is no remaining "misc unsupported" bucket in the document
    - each unsupported construct names a next milestone or explicit non-goal
    - semantically equivalent spellings share the same canonical semantic form entry
- [ ] IR2. Shared IR expansion
  - add or normalize IR forms needed to represent the currently supported language surface
  - implementation checklist:
    - update `crates/goby-core/src/ir.rs`
    - update IR printers / validators alongside any new node or invariant
    - add direct IR construction tests for each new semantic family
    - document whether each family is:
      - a new IR node,
      - a canonical desugaring into existing IR
  - validation checklist:
    - every new IR form has at least one printer or snapshot assertion
    - IR validation errors are explicit and user-comprehensible where relevant
- [x] IR3. Resolved lowering input
  - introduce or expose the resolved/symbol-bound lowering input needed for stable effect/call lowering
  - implementation checklist:
    - introduce the distinct resolved front-end form chosen in IR0
    - make symbol identity available to `ir_lower`
    - ensure effect operations, helpers, top-level declarations, and locals are distinguishable without textual guessing
    - remove now-obsolete raw-name heuristics where the resolved layer replaces them
  - validation checklist:
    - tests cover bare/imported/qualified spellings lowering through the same resolved identity
    - `ir_lower` input type is the resolved front-end form rather than raw AST
    - `ir_lower` no longer needs ad-hoc name-pattern logic for the covered forms
- [ ] IR4. Calls and effect-call normalization
  - converge bare/imported/qualified effect-call spellings before or at the IR boundary
  - implementation checklist:
    - normalize ordinary call targets and effect-op targets using resolved identity
    - cover aliasing cases that should remain semantically equivalent
    - update runtime-I/O/general-lowering tests that currently exist only because of spelling differences
  - validation checklist:
    - semantically equivalent effect-call spellings produce equivalent IR snapshots
    - backend/runtime tests that previously depended on spelling differences are simplified or deleted
- [ ] IR5. Collections lowering
  - lower list literal, spread, and indexing through the new architecture
  - implementation checklist:
    - lower list literals and spread in the same slice as their canonical IR representation
    - lower indexing via the locked IR strategy from IR0
    - update collection-related backend assumptions that currently recognize syntax-shaped forms
  - validation checklist:
    - AST-to-IR tests cover literal/spread/index combinations
    - at least one backend parity test proves syntax sugar and canonical helper spellings converge
- [ ] IR6. Control-flow lowering
  - lower `case` and related branch forms into shared IR
  - implementation checklist:
    - lower `case` with all currently supported pattern families
    - ensure effectful scrutinees and branch bodies survive lowering without AST fallback
    - update any runtime path that still assumes `case` is interpreter-only
  - validation checklist:
    - IR snapshots exist for both pure and effectful `case`
    - branch semantics match existing runtime behavior
- [ ] IR7. Function-value lowering
  - lower lambda and higher-order function-value forms into shared IR
  - implementation checklist:
    - define closure/capture representation at shared-IR level or explicit desugaring boundary
    - lower lambda syntax and named higher-order references in one semantic family
    - update call lowering assumptions that currently require only direct/global callees
  - validation checklist:
    - direct IR tests cover captures and call sites
    - backend unsupported cases, if any, are reported as backend limitations
- [ ] IR8. Product-data lowering
  - lower non-unit tuples and record construction into shared IR
  - implementation checklist:
    - lower tuples and records in one product-data slice
    - update equality / access assumptions where they currently rely on AST/runtime-only handling
  - validation checklist:
    - IR tests cover construction and representative consumption sites
    - any remaining backend limitation is documented as such rather than left as lowering failure
- [ ] IR9. Mutable-form lowering
  - lower mutable locals and assignment into shared IR
  - implementation checklist:
    - choose the shared-IR representation for mutable locals
    - lower `mut` binding and assignment in the same slice
    - update resume/handler/runtime assumptions if mutable state can cross those boundaries
  - validation checklist:
    - mutation semantics are covered by IR tests and existing runtime parity tests
    - mutable forms no longer fail at AST-to-IR time
- [ ] IR10. Backend boundary convergence
  - make backend limitations show up as backend limitations rather than IR-construction failures
  - implementation checklist:
    - remove backend workarounds that only exist for pre-convergence AST/IR mismatch
    - rewrite diagnostics that still imply "syntax not lowerable" when the real issue is backend support
    - shrink fallback classification that compensates for now-fixed IR gaps
  - validation checklist:
    - each landing slice names the specific fallback/recognizer removed or narrowed
    - if a fallback remains, the slice states why it remains and why it is an optimization rather than a semantic dependency
    - representative unsupported cases fail with backend-oriented errors
- [ ] IR11. Pure-subset deletion
  - remove "pure IR subset" as the governing model in docs/comments/code paths
  - implementation checklist:
    - rewrite stale comments, test names, and diagnostics
    - remove dead code branches that only exist for the old subset framing
    - update surrounding docs to describe one stable lowering model
  - validation checklist:
    - repo search no longer shows the old model as active architecture
    - `doc/STATE.md` restart notes reference the new model only

Milestone update rule:

- when a milestone is reached, update this checklist in `doc/PLAN_IR.md` by changing its checkbox
  from `[ ]` to `[x]` in the same change that lands the milestone outcome.
- do not check a milestone based only on discussion or partial implementation; the representative
  code, tests, and document updates for that milestone must land together.
- if a milestone grows too large or ambiguous, split it into child milestones in this document
  before continuing implementation.

## 8. Testing Strategy

For each milestone, prefer this pyramid:

- AST-to-IR unit tests,
- IR snapshot tests,
- backend lowering tests from shared IR,
- runtime/Wasm parity tests for representative programs.

Representative parity cases must include:

- bare and qualified effect-call spellings,
- direct helper-call spelling vs sugared syntax spelling,
- collection/indexing programs such as `lines[1]`,
- control-flow forms whose branches contain effects,
- higher-order forms once lambda lowering is introduced.

## 9. Immediate First Implementation Targets

These are the first targets after the planning slice lands:

1. lock the resolved-lowering-input strategy and symbol identity boundaries,
2. lock the IR representation strategy for list indexing and related collection forms,
3. implement effect-call normalization so bare prelude forms and qualified forms do not diverge semantically,
4. implement lowering for list indexing without introducing backend-specific shortcuts,
5. use those changes to remove at least one existing Wasm/runtime fallback distinction.

Reason for this order:

- it directly addresses the current `Read.read` / `read`, `list.get` / `[]` inconsistency,
- it fixes the semantic-identity boundary first instead of teaching `ir_lower` more spelling-specific tricks,
- it tests the plan on a real backend pain point,
- it improves architecture quality without committing to a throwaway workaround.

## 10. Development Process Notes

- treat this file as the active progress tracker for the IR-lowering program.
- every implementation slice under this program should mention which IR milestone it advances.
- when a slice completes a milestone, mark the checkbox in this file in the same change.
- if implementation learns that a locked architectural decision is wrong, update this file first
  with the new decision and rationale before continuing to spread code changes.
- each implementation slice should follow this order unless there is a documented reason not to:
  1. update `doc/PLAN_IR.md` if the slice locks or changes a design decision,
  2. update shared IR definitions,
  3. update lowering implementation,
  4. update focused AST-to-IR / IR tests,
  5. update downstream backend/runtime assumptions,
  6. run `cargo fmt` and `cargo test`.
- prefer slices that complete one semantic family end-to-end rather than touching many families shallowly.
- when deleting fallback logic, do it in the same slice that proves the shared-IR path covers the replaced behavior.
- if a slice cannot finish a milestone, leave the checkbox unchecked and add a short note in
  `doc/STATE.md` naming the exact unfinished sub-steps.
- development rule for this phase:
  - this project is currently used by one person, so temporary breakage during development is acceptable.
  - optimize for the long-term design from the start, even when that means the codebase stays broken for a while mid-slice.
  - do not accept architectural compromises just because the principled design is harder to land.
  - avoid "small safe" steps that accumulate ad-hoc structure and make the long-term design worse.
  - if an implementation approach fails repeatedly, do not gradually degrade the design to force it through.
  - after 5 serious implementation attempts on the same design path without a clean result, stop the work,
    summarize the blocker, and explicitly ask for a direction check before continuing.

## 11. Suggested Next Slice

The recommended next implementation slice is:

1. advance IR2 by deciding which remaining semantic families need new shared-IR nodes versus canonical desugaring,
2. advance IR4 by adding IR snapshots that prove bare/imported/qualified effect-call spellings converge through the resolved boundary,
3. start IR5 by locking the collection strategy for list literals/spread/indexing against the now-landed resolved form,
4. remove any stale comments or helper code that still describe `ir_lower` as raw-AST name-pattern matching.

Recommended file entry points for that slice:

- `doc/PLAN_IR.md`
- `crates/goby-core/src/resolved.rs`
- `crates/goby-core/src/ir_lower.rs`
- `crates/goby-core/src/ir.rs`
- the backend/runtime tests that currently encode spelling-specific equivalence cases

Definition of done for the next slice:

- `ir.rs` documents the canonical strategy chosen for the next semantic family,
- `ir_lower` continues to lower through `Resolved*` inputs without reintroducing raw-name heuristics,
- new AST-to-IR / IR snapshot tests prove semantic convergence for covered call/effect families,
- `doc/STATE.md` names only the active next milestone and any exact unfinished sub-steps.

## 12. Non-Goals for This Document

This plan does not itself lock:

- final semantics for every future language feature,
- backend optimization strategy,
- interpreter deletion timing,
- exact commit slicing.

Those decisions can remain incremental as long as they obey the architecture and invariants above.
