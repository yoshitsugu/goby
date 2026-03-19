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

### IR2. Shared IR Expansion

Goal: add or normalize shared IR forms so supported source constructs can lower cleanly.

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

- [ ] IR0. Architecture lock
  - lock the resolved-front-end -> shared IR pipeline shape
  - lock mutable-form inclusion in shared IR scope
  - lock initial decisions for list index, pipelines, method calls, and effect-op identity
  - implementation checklist:
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
    - any conflicting comments in `goby-core` are updated in the same slice
- [ ] IR1. Construct inventory and mapping table
  - enumerate all AST constructs and their lowering state
  - mark each gap as IR gap vs backend gap
  - implementation checklist:
    - add the inventory table to this document
    - include one row per user-visible AST family, not only per parser enum
    - mark each row with:
      - lowering status,
      - canonical IR target,
      - owner milestone,
      - representative test file or test name
  - validation checklist:
    - there is no remaining "misc unsupported" bucket in the document
    - each unsupported construct names a next milestone or explicit non-goal
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
- [ ] IR3. Resolved lowering input
  - introduce or expose the resolved/symbol-bound lowering input needed for stable effect/call lowering
  - implementation checklist:
    - choose whether to extend the typed AST or add a new resolved front-end layer
    - make symbol identity available to `ir_lower`
    - ensure effect operations, helpers, top-level declarations, and locals are distinguishable without textual guessing
    - remove now-obsolete raw-name heuristics where the resolved layer replaces them
  - validation checklist:
    - tests cover bare/imported/qualified spellings lowering through the same resolved identity
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
    - at least one deleted fallback/recognizer is mentioned in the landing slice
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

## 11. Suggested Next Slice

The recommended next implementation slice is:

1. finish IR0 by locking the concrete resolved-input strategy,
2. start IR1 by adding the construct inventory table,
3. begin IR3/IR4 only after those two pieces are written down in this file.

Recommended file entry points for that slice:

- `doc/PLAN_IR.md`
- `crates/goby-core/src/ir.rs`
- `crates/goby-core/src/ir_lower.rs`
- the front-end resolution/typecheck modules that already own symbol identity

Definition of done for the next slice:

- `PLAN_IR` names the resolved lowering input and ownership boundary explicitly,
- the construct inventory table exists with milestone ownership,
- `doc/STATE.md` can be reduced to a short note pointing back to this plan and the next IR milestone.

## 12. Non-Goals for This Document

This plan does not itself lock:

- final semantics for every future language feature,
- backend optimization strategy,
- interpreter deletion timing,
- exact commit slicing.

Those decisions can remain incremental as long as they obey the architecture and invariants above.
