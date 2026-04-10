# Sequence-Backed List Plan

This document defines the long-term plan for replacing Goby's current
list-oriented runtime model with a sequence-backed collection model that keeps
the user-facing `List` surface simple while making indexing, updates, and
iteration substantially more practical.

The goal of this plan is not to preserve the current linked-list-flavored
implementation details. The goal is to keep the language surface pleasant and
list-like while giving the compiler and runtime enough freedom to use stronger
internal representations and optimized execution paths.

For this plan, **sequence-backed `List`** means:

- users continue to write and reason about `List` as Goby's default ordered
  collection;
- `List` is no longer specified or implemented as a simple linked list;
- the language/runtime may use one or more sequence-oriented internal
  representations to improve practical indexing, immutable updates,
  iteration, slicing, and construction;
- list-style syntax and pattern matching remain part of the intended user
  experience, even if they become views over a richer internal structure.

This plan does **not** require the public language surface to introduce
separate everyday collection types such as `Vec` or `Array` for the common
case. The design target is one pleasant primary collection surface with better
internals.

## 1. Product Goal

Goby should let users treat `List` as the obvious default ordered collection
without forcing them to choose between:

- pleasant list-like programming style,
- practical indexed access and update,
- practical iteration on non-trivial input sizes,
- future effect-oriented iterator design.

In practical terms, a user should be able to:

- construct `List` values with today's syntax;
- pattern-match on `List` values with list-shaped patterns;
- iterate over `List` values through stdlib helpers and iterator-style APIs;
- index and update `List` values often enough for ordinary application code,
  scripting, and AoC-scale workloads without immediately falling off a
  performance cliff caused by a naïve linked-list runtime model.

## 2. Completion Goal

This plan is complete only when all of the following are true:

- Goby's docs state clearly that `List` is the default ordered collection
  surface and no longer imply a simple linked-list implementation contract.
- The runtime representation of `List` is sequence-backed and chosen for
  practical whole-program behavior rather than for preserving current
  implementation simplicity.
- List indexing and immutable update meet the locked benchmark/workload gates
  for ordinary Goby scripts rather than merely improving qualitatively over a
  simple linear linked-list walk.
- "Practical for ordinary scripts" is backed by named benchmark/workload gates
  rather than by qualitative wording alone.
- Stdlib iteration and collection helpers run through explicit optimized
  compiler/runtime boundaries rather than through hidden symbol-name hacks.
- List pattern matching (`[]`, `[x, ..rest]`, exact-length forms, and related
  list syntax) still works as a first-class language feature.
- Iterator/effect-based traversal remains a supported design direction rather
  than being discarded in favor of a loop-only surface.
- The implementation has representative compile/runtime coverage for:
  - indexing-heavy programs,
  - update-heavy programs,
  - pattern-matching-heavy programs,
  - iterator/effect-driven traversal over large collections.
- The docs state clearly which `List` operations are intended to be practical
  and which operations remain less ideal even after the redesign.

Until these are true, Goby may improve list internals incrementally, but should
not claim that `List` has been fully re-founded as the language's practical
default sequence collection.

## 3. Non-Goals

The following are explicitly out of scope for this plan:

- preserving the current linked-list implementation because current examples
  happen to rely on it;
- introducing a user-facing requirement to choose between `List`, `Array`,
  `Vec`, or several other primary collection types for ordinary code;
- silently changing performance-sensitive behavior without updating the docs and
  language positioning;
- treating stdlib names as magical compiler hooks without an explicit intrinsic,
  lowerer-owned form, or similarly visible boundary;
- requiring Wasm-specific low-level details to leak into the language design;
- solving every future data-structure use case with one collection type.

The project may still add lower-level specialized collections later, but that
must not become an excuse to leave the default `List` surface impractical, and
it must not become the default answer if the sequence-backed `List` plan fails
to reach its stated target.

## 4. Design Principles

### 4.1 Keep One Pleasant Default Collection Surface

Users should continue to experience `List` as Goby's ordinary ordered
collection. The language should not push routine code toward "pick the correct
container family first" ergonomics.

That implies:

- list literals remain first-class;
- list indexing and update remain available on the default collection surface;
- list pattern matching remains available;
- ordinary stdlib traversal should continue to target `List`.

### 4.2 Separate Surface Semantics From Runtime Representation

The language should specify what users can do with `List`, not which node
layout or pointer structure the runtime must use.

That implies:

- `[x, ..rest]` should be treated as a sequence view, not as proof that `List`
  is physically represented as a cons cell chain;
- `[]`/head-tail/exact-length list patterns are part of the surface contract;
- runtime representation should remain free to evolve.

### 4.3 Make Optimized Boundaries Explicit

If stdlib iteration, indexing, update, or sequence construction runs through a
special compiler/runtime path, that boundary should be explicit.

Preferred forms include:

- intrinsic functions such as `__goby_*`,
- explicit lowerer-owned IR forms,
- future dedicated syntax when it is clearly justified.

The project should reject hidden "special-case `goby/list.each` by name"
architecture.

### 4.4 Preserve The Effect Story

Goby already cares about effects and iterator-style control flow. The sequence
plan should not flatten that design into a purely imperative loop surface.

That implies:

- iterator/effect-based traversal remains a supported abstraction;
- optimized traversal may lower effect-driven iterator execution to loops or
  state machines internally;
- stdlib APIs should still be able to look effect-oriented and Goby-like, even
  when their execution path is optimized.

### 4.5 Prefer Practical Sequence Performance Over Purist Linked-List Identity

If there is tension between "this feels like a classic linked list" and "this
is the right default collection for real programs", prefer the latter.

In practice, Goby should bias toward a sequence representation that improves:

- indexing,
- immutable point updates,
- iteration,
- concatenation/slicing opportunities,
- large-input behavior.

It is acceptable if some list-like operations are no longer as naturally cheap
as they would be on a pure cons list, as long as the user-facing model remains
coherent and elegant.

### 4.6 Lock Claims Behind Benchmarkable Gates

This plan must not rely on vague success language such as "materially better"
or "practical enough" without concrete workload gates.

That implies:

- the plan must define named benchmark/workload suites before the
  representation direction is locked;
- milestone completion should reference those suites explicitly;
- if the chosen design misses the locked practical target, the plan must pause
  and be revised rather than silently weakening the promise.

### 4.7 Distinguish Semantic Validity From Performance Promise

List-pattern views such as `[]` and `[x, ..rest]` are semantically first-class.
Their performance story must be documented honestly and separately.

That implies:

- the language guarantees that list-pattern forms remain available and pleasant
  to use;
- the plan must later decide, with evidence, what performance language is
  honest for repeated list-pattern view extraction;
- the implementation must not quietly degrade list patterns into a "valid but
  impractical" feature while still describing them as ordinary-use forms.

### 4.8 Prefer The Smallest Explicit Machinery That Reaches The Goal

When there is tension between readable Goby stdlib code and optimized runtime
execution, Goby should prefer the smallest explicit lowerer/runtime surface
that still reaches the locked practical target.

That implies:

- first prefer shared lowerer-owned forms over many symbol-specific intrinsics;
- if shared lowerer-owned forms are insufficient, prefer shared
  handler/lowering specialization before adding explicit intrinsics;
- add explicit intrinsics only when a shared IR/lowering boundary is not enough
  or would make the design less honest;
- reject growth in special machinery that is not justified by benchmarked user
  benefit.

## 5. Target Architecture

The preferred end-state architecture is:

1. `List` remains the user-visible ordered collection type in the language
   surface.
2. The runtime stores `List` values in a sequence-oriented representation rather
   than a simple linked-list chain.
3. After the representation family is chosen and benchmark gates are defined,
   the compiler/runtime defines explicit optimized boundaries for core sequence
   operations such as:
   - indexed read,
   - immutable indexed update,
   - iteration/consumption,
   - map/fold/length-style stdlib helpers,
   - list-pattern view extraction.
4. List patterns operate through a shared "sequence view" boundary so source
   code can keep using list-shaped matching without coupling semantics to one
   node layout.
5. Iterator/effect-based traversal lowers through explicit sequence/iterator
   runtime support, potentially using loop/state-machine lowering internally.
6. The runtime remains free to use chunked, tree-based, persistent-vector-like,
   or hybrid sequence representations as long as the language contract stays
   honest and the benchmark/workload gates remain the deciding standard.

In other words, the project should move toward:

- surface `List` as one pleasant collection,
- sequence-backed runtime internals,
- explicit optimized boundaries for hot operations,
- list-pattern views instead of linked-list identity.

## 6. Candidate Internal Directions

This plan does not lock one exact representation up front, but the leading
options should be evaluated against Goby's language goals.

### 6.1 Chunked Sequence

Store `List` as a persistent sequence of small contiguous chunks.

Advantages:

- materially better indexing than a node-per-element linked list;
- can support efficient iteration and chunk-oriented lowering;
- easier to keep list-pattern views than a pure flat array story;
- lower implementation complexity than some richer balanced-tree designs.

Risks:

- may still need extra machinery for good concatenation/slicing;
- pattern-view performance semantics must be documented carefully.

### 6.2 Persistent Vector / RRB-Style Sequence

Store `List` as a tree-backed persistent vector or related sequence structure.

Advantages:

- strong practical indexing and immutable update;
- good long-term fit for one default collection with broad utility;
- good platform for future slicing/concatenation improvements.

Risks:

- list-style head/tail views become more obviously virtual;
- implementation complexity is higher;
- exact performance of list patterns must be treated carefully.

### 6.3 Finger-Tree-Like Sequence

Use a sequence structure optimized for views at the ends plus broader sequence
operations.

Advantages:

- strong support for list-like decomposition ergonomics;
- good concatenation/splitting story;
- philosophically close to keeping list-like source style elegant.

Risks:

- indexing/update may still be less attractive than a vector-first design;
- implementation complexity remains non-trivial.

### 6.4 Evaluation Standard

The chosen representation should be selected based on:

- whether `List` remains pleasant as the default collection;
- whether indexed access and update become practical enough for ordinary code;
- whether list-pattern views remain elegant;
- whether iterator/effect lowering can target the representation cleanly;
- whether the implementation stays maintainable for Goby's small-project scale;
- whether it reaches the locked benchmark/workload gates without requiring a
  large amount of explicit special machinery.

### 6.5 Required Benchmark / Workload Suite

Before the representation direction is locked, the project must define and keep
using a named workload suite that includes at least:

- indexed-read-heavy scripts over large `List` values;
- immutable point-update-heavy scripts;
- repeated `[x, ..rest]` / exact-length / prefix-tail list-pattern workloads;
- iterator/effect-driven traversal workloads;
- nested `List (List a)` workloads, including AoC-style grid transforms;
- stdlib-heavy scripts using `length`, `each`, `map`, and `fold`.

The suite does not need to promise parity with mutable-array-heavy languages.
It does need to define a stable success bar for saying that `List` is practical
for ordinary Goby scripts.

## 7. Acceptance Standard For Saying "Goby List Has Been Re-Founded"

The project may say that `List` has been successfully re-founded as Goby's
practical default ordered collection only when all of the following are true:

- `doc/LANGUAGE_SPEC.md` and user-facing docs describe `List` as the main
  ordered collection without implying a simple linked-list runtime contract;
- the runtime no longer depends on naïve linear linked-list behavior for core
  indexed/update workloads;
- list patterns still work and are documented honestly;
- stdlib traversal runs through explicit optimized boundaries;
- iterator/effect traversal remains part of the story;
- representative practical workloads no longer fail merely because the default
  collection still behaves like a raw linked list internally.

## 8. Locked Direction

The following product-direction decisions are already locked for this plan:

- Goby keeps the public surface name `List`.
  - The language should not force routine users to switch their mental model to
    `Sequence`, `Vec`, or `Array` for everyday collection work.
  - `doc/LANGUAGE_SPEC.md` should explicitly explain that `List` is the
    user-facing collection surface while the backend/runtime is free to use a
    sequence-oriented internal representation for practical execution.
- The target usability level is ordinary practical scripting, not merely
  theoretical improvement over a naïve linked list.
  - Indexed reads, immutable updates, and traversal should become practical for
    normal scripts and Advent-of-Code-scale workloads.
  - If the implementation investigation shows that this target is not honestly
    achievable under the current language constraints, the plan should stop and
    be revised rather than quietly lowering the promise.
- List-pattern ergonomics remain a first-class requirement.
  - `[]`, `[x, ..rest]`, exact-length patterns, and related list-pattern forms
    are intended for ordinary use, not for rare or discouraged use.
  - The implementation must therefore search for a representation and runtime
    boundary that preserve both practical performance and pleasant list-shaped
    decomposition.
- Stdlib should remain as close as possible to straightforward Goby code.
  - Explicit intrinsic or compiler-owned lowerer boundaries are allowed, but
    they should be kept minimal and justified.
  - The preferred design is to preserve a readable Goby-level stdlib surface
    and move only the truly hot or semantically fundamental runtime boundaries
    into explicit compiler/runtime ownership.
- The optimization-boundary policy (§4.3, §4.8) is adopted as a guiding
  principle starting at M0: prefer shared lowerer-owned forms over
  symbol-specific intrinsics; add explicit intrinsics only where benchmarked
  workloads (§6.5) justify them. This principle may be refined once workload
  evidence from M2/M3 is available.

## 9. Milestones

- [x] **M0: Lock the product contract** (complete, 2026-04-10)
  - Define the long-term user-facing position of `List` as Goby's default
    ordered collection.
  - State explicitly that `List` remains the public surface name while the
    backend/runtime may use a sequence-backed representation for practical
    execution.
  - State explicitly that Goby does not guarantee a simple linked-list runtime
    representation.
  - Lock the success bar as "practical for ordinary scripts" rather than "less
    bad than today's list internals".
  - Lock the explicit optimization-boundary policy for stdlib operations,
    with a bias toward minimizing intrinsic surface area (recorded in §8).
  - Define the initial benchmark/workload suite (see §6.5) and the plan-revision
    rule if that suite cannot be met honestly.
  - Note: the spec-wording update is deferred to M1. This milestone locks the
    product contract and design principles in this plan; `doc/LANGUAGE_SPEC.md`
    will be updated in M1 to reflect them.

- [x] **M1: Rewrite the language/spec wording around `List`** (complete, 2026-04-10)
  - Update `doc/LANGUAGE_SPEC.md` so `List` is described in surface-semantic
    terms rather than as a linked-list identity.
  - Document list patterns as sequence views.
  - Clarify the intended practicality of indexing and update on `List`.
  - Explain that list-pattern forms remain part of ordinary user code while
    leaving their exact performance promise to the later benchmarked design
    decision.
  - Audit README/examples wording so user-facing docs do not over-teach a
    linked-list mental model. (Audited 2026-04-10: no "linked list", "cons",
    or head/tail-as-idiomatic framing found in README.md or examples/README.md;
    no changes required.)

- [ ] **M2: Prototype candidate internal representations**
  - Evaluate at least two viable sequence-backed `List` representation
    directions, with enough prototype or implementation evidence to compare
    them honestly.
  - Evaluate them on:
    - indexed reads,
    - indexed updates,
    - head/tail pattern extraction,
    - iteration-heavy workloads,
    - update-heavy workloads,
    - AoC-style two-dimensional list workloads.
  - Use current literature and practical implementations as inputs when
    selecting the candidate family and final direction.
  - Identify what each candidate would require from explicit lowerer/runtime
    boundaries before locking those boundaries in the next milestone.
  - Lock the representation direction only after shared benchmarks and
    implementation-complexity review.

- [ ] **M3: Introduce an explicit sequence runtime boundary**
  - Add compiler/runtime-owned sequence operation boundaries for:
    - index read,
    - immutable update,
    - length/iteration/map/fold-style operations,
    - list-pattern view extraction.
  - Ensure these boundaries are explicit in implementation ownership rather
    than hidden stdlib-name magic.
  - Prefer shared lowerer-owned boundaries first and add explicit intrinsics
    only where benchmarked needs justify them.
  - Keep room for effect-aware iterator lowering on top of the same boundary.

- [ ] **M4: Re-found list pattern matching on sequence views**
  - Route `[]`, `[x, ..rest]`, exact-length patterns, and prefix/tail variants
    through one shared sequence-view boundary.
  - Ensure parser/typechecker semantics remain elegant while runtime extraction
    no longer assumes a cons-list node layout.
  - Decide and document the honest performance language for ordinary repeated
    list-pattern use based on benchmark evidence.
  - Add regression coverage for list-pattern-heavy programs on the new runtime
    model.

- [ ] **M5: Rebuild stdlib traversal on explicit sequence/iterator boundaries**
  - Rework `length`, `each`, `map`, `fold`, and related stdlib helpers so their
    optimized execution path is explicit and maintainable.
  - Preserve Goby's effect/iterator story where it improves language clarity.
  - Prefer designs where the readable stdlib surface remains ordinary Goby code
    and only the minimum hot-path execution boundary becomes compiler/runtime
    owned.
  - Compare:
    - sequence intrinsics,
    - iterator intrinsics,
    - handler specialization,
    - or a layered combination of those,
    and choose the smallest explicit machinery that still reaches the practical
    scripting target.
  - Reject solutions that meet the benchmark target only by turning most of the
    stdlib into opaque runtime stubs.

- [ ] **M6: Make index/update workloads practical**
  - Ensure `xs[i]` and `xs[i] := v` no longer behave like thin syntax over naïve
    linear linked-list rebuilding.
  - Add representative tests for:
    - large indexed reads,
    - many immutable point updates,
    - nested `List (List a)` workloads,
    - realistic Advent-of-Code-style transforms.
  - Evaluate nested-update and rebuild-heavy workloads for allocation pressure
    and intermediate-structure costs, not only for syntax-level correctness.
  - Check the locked benchmark/workload suite and stop for plan revision if the
    target cannot be reached honestly.
  - Keep diagnostics honest if some operations remain intentionally non-ideal.

- [ ] **M7: Integrate effect-oriented iterator execution**
  - Define how Iterator/effect-based collection traversal lowers onto the new
    sequence model.
  - Decide which explicit runtime/compiler forms own:
    - stepping,
    - yielding,
    - consumption,
    - specialization of known callbacks.
  - Prove that Goby can keep an effect-oriented traversal story without giving
    up practical runtime behavior.

- [ ] **M8: Publish the new `List` contract**
  - Update README/examples/spec/plan documents with the final language-facing
    story.
  - Add representative examples showing:
    - ordinary indexed access on `List`,
    - list-pattern matching,
    - iterator/effect-based traversal,
    - workloads that previously exposed the linked-list runtime boundary.
  - Lock the wording for what Goby means by "default collection" and by
    "practical indexed/update behavior" for `List`.

## 10. Remaining Open Questions

- Which current research-informed family offers the best balance for Goby in
  practice:
  - chunked sequence,
  - persistent-vector/RRB-style sequence,
  - finger-tree-like sequence,
  - or a hybrid,
  while still satisfying:
  - indexing practicality,
  - pattern-view elegance,
  - implementation complexity,
  - compatibility with effect-oriented traversal?
- How cheap can Goby honestly make `[x, ..rest]`-style sequence views under the
  chosen representation, and what performance language should the docs promise?
- How much should stdlib traversal optimization be expressed in sequence
  intrinsics versus iterator/effect lowering?
- What is the minimum explicit intrinsic/lowerer surface needed to keep stdlib
  readable while still making `List` practical for ordinary scripts?

## 11. Working Conclusion

The current linked-list-flavored runtime model is too limiting for Goby's goal
of one pleasant default collection. The long-term direction should be to keep
`List` as the surface collection while moving its internals and optimized
execution model toward a sequence-backed design with explicit compiler/runtime
bounds, honest docs, preserved list-like elegance, and the smallest possible
amount of special runtime machinery.
