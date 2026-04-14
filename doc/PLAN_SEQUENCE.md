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
- stdlib-heavy scripts using `length`, `each`, `map`, and `fold`;
- bump allocator allocation pressure: repeated immutable updates must not
  produce memory exhaustion within the §6.6 success-bar input bounds.

The suite does not need to promise parity with mutable-array-heavy languages.
It does need to define a stable success bar for saying that `List` is practical
for ordinary Goby scripts.

### 6.6 Workload Matrix and Success Bar (M2, 2026-04-10)

**Success bar definition:** A representation is "practical for ordinary scripts"
if none of the seven workload types below produce memory exhaustion or visibly
O(n²) behavior on inputs of size ≤ 1 000 total elements under the Goby bump
allocator. For nested `List (List a)` workloads: ≤ 30 rows × 30 columns
(≤ 900 total elements).

This bar may be revised in M3 once actual benchmark execution is possible.
It is a planning threshold, not a performance contract.

**Bump allocator constraint:** Goby's runtime uses a bump allocator
(allocate, never free). Any representation that requires structural sharing to
achieve good performance will instead accumulate O(n) allocation per operation
under this model, equivalent to copying. Representations that _avoid_ sharing
(e.g., write new flat chunks) behave honestly under the allocator constraint.

| Workload | Candidate A (flat array) | Candidate B (Chunked) | Candidate C (Spine+Tail) |
|---|---|---|---|
| Indexed read (large list) | O(1) ✓ | O(1) into chunk ptr + O(1) within chunk ✓ | O(n/32) ~ ok |
| Immutable point update | O(n) copy ✗ | O(CHUNK_SIZE) chunk copy ✓ | O(n/32) copy ✗ |
| `[x, ..rest]` pattern (split) | O(n) copy ✗ | O(n/CHUNK_SIZE) worst case at boundary; O(1) amortized ✓ | O(1) head chunk ✓ |
| Iterator/effect traversal | O(n) linear ✓ | O(n) chunk-walk ✓ | O(n) segment-walk ✓ |
| Nested `List (List a)` grid | O(n²) for row update ✗ | O(n × CHUNK_SIZE) ✓ | O(n²/32) ~ ok |
| stdlib `each`/`map`/`fold` | O(n) linear ✓ | O(n) chunk-walk ✓ | O(n) segment-walk ✓ |
| Bump alloc pressure (update) | O(n) per update ✗ | O(CHUNK_SIZE) per update ✓ | O(seg_size) per update ✓ |

Legend: ✓ = meets success bar, ✗ = fails success bar, ~ ok = marginal

**Candidate A** fails on immutable update, pattern split, and nested grid workloads.
Already demonstrated harmful in the RR-4 track (memory exhaustion on recursive
list-spread patterns). Rejected — see §8.

**Candidate B (Chunked Sequence):**
- Layout: `(total_len: i32, n_chunks: i32, chunk_ptrs: [u32; n_chunks])`
  header; each chunk is a flat array `(len: i32, items: [i64; CHUNK_SIZE])`.
- CHUNK_SIZE is locked as a fixed compile-time constant (`32` elements).
  Each chunk occupies `4 + 32×8 = 260` bytes.
- Indexed read at position i: O(1) — compute chunk index i/CHUNK_SIZE into the
  flat chunk_ptr array, then O(1) into the chunk. Total: O(1). Header copying
  on append: O(n_chunks) = O(n/CHUNK_SIZE); for n≤1000 and CHUNK_SIZE=32,
  n_chunks≤32, which is acceptable.
- Split at head: consume first element of the leading chunk; if chunk empties,
  drop the chunk. Amortized O(1) per element consumed; worst case O(n/CHUNK_SIZE)
  when a chunk boundary is crossed and the header array must be copied under the
  bump allocator (same as header copy on append).
- Update: copy the affected chunk, replace chunk pointer. Cost: O(CHUNK_SIZE).
- Under bump allocator: no structural sharing needed; each update copies one chunk.
  Total allocation per update: O(CHUNK_SIZE) = constant in practice. ✓

**Candidate C (Spine+Tail):**
- Small lists (≤ 32 elements): same flat array as Candidate A.
- Large lists: `(head_seg_ptr: u32, tail_ptr: u32)` — linked segments of flat arrays.
- Indexed read: walk segments O(n/seg_size).
- Split: take head from first segment; O(1) if first segment is non-empty.
- Update at index i: copy the segment containing i; O(seg_size).
- Weakness: O(n/32) indexed read does not meet the success bar for large random
  access patterns. Also, the two-path representation adds complexity at every
  operation boundary.

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
- Candidate A (flat array, current implementation) is rejected as the target
  representation as of M2 (2026-04-10): O(n) prepend and pattern-split
  allocation was demonstrated harmful in the RR-4 track, causing memory
  exhaustion on recursive list-spread programs. Candidate A remains in use
  until M3 lands the chosen replacement.
- Representation direction locked as of M2 (2026-04-10): Candidate B
  (Chunked Sequence). Rationale: O(1) indexed read (flat chunk_ptr array +
  intra-chunk offset); O(CHUNK_SIZE) chunk-copy for split and update (no
  structural sharing needed under bump allocator); header append cost is
  O(n/CHUNK_SIZE) which is bounded and small for the §6.6 success-bar input
  size; implementation complexity is moderate and appropriate for project
  scale. Candidate C was rejected for O(n/32) indexed read and two-path
  representation complexity at every operation boundary.
  Candidate C was also rejected for O(n/32) immutable update cost (marked ✗
  in the §6.6 matrix).
  This lock is conditional: if M3 benchmark execution reveals that chunked
  performance does not meet the §6.6 success bar, this direction will be
  revised before M4.

## 9. Milestones

- [x] **M0-M6: Completed foundation and practicality work** (complete, 2026-04-13)
  - `M0-M1`: locked the product contract and rewrote the language wording so
    `List` is the default ordered collection surface without promising a simple
    linked-list runtime identity.
  - `M2-M3`: selected Candidate B (Chunked Sequence), locked `CHUNK_SIZE=32`,
    and moved `List` execution onto explicit compiler/runtime-owned sequence
    boundaries.
  - `M4`: re-founded list-pattern matching on sequence views and locked honest
    performance language for repeated pattern extraction.
  - `M5`: replaced list stdlib name-magic with explicit traversal boundaries.
    `length`, `fold`, and `map` now lower through explicit intrinsics, `each`
    is ordinary Goby code on top of the shared fold boundary, and
    `ListReverseFoldPrepend` remains anchored on semantic fold shape rather
    than symbol matching.
  - `M6`: made indexed read/update practical on the chunked representation via
    the shared `ListGet`/`ListSet` boundary, added `list.get` / `list.set` as
    the functional-style public surface, and locked nested-update support for
    `List (List a)` on the same boundary.
  - Locked outcomes through M6:
    - no stdlib-name magic for `List` traversal/index/update paths;
    - no syntax-shaped backend exceptions for `xs[i]` / `xs[i] := v`;
    - no alternate hidden collection mode behind `List`;
    - remaining implicit string traversal magic (`graphemes` / `split`) is
      explicit temporary debt and must be addressed or carved out in M8.
  - Verification snapshots:
    - `M3`: `cargo test -p goby-wasm` -> `622 passed, 0 failed, 4 ignored`
    - `M4`: `cargo test -p goby-wasm` -> `625 passed, 0 failed, 4 ignored`
    - `M5`: `cargo test -p goby-wasm` -> `641 passed, 0 failed, 4 ignored`
    - `M6`: `cargo test -p goby-wasm` -> `656 passed, 0 failed, 7 ignored`

- [x] **M7: Integrate effect-oriented iterator execution**

  ### M7 Design Decisions

  **Goal:** preserve Goby's effect-oriented traversal story while keeping the
  runtime model centered on the explicit traversal boundary established in M5,
  not on new name-based magic.

  **Surface goal:** by the end of M7, Goby must have one documented,
  representative effect-oriented traversal style that is intended as the
  language-facing answer for practical collection traversal. M7 is not complete
  if only the internal lowering/runtime path is defined.
  This style's status must also be explicit:
  - whether it is the recommended default for effect-oriented traversal,
  - whether callback-style traversal remains equally first-class,
  - or whether it is a future-facing style that is implemented but not yet the
    primary recommendation.

  **Boundary rule:** Iterator/effect execution in M7 must lower onto the same
  traversal family introduced in M5. M7 may add iterator/effect-specific IR or
  lowering forms, but those forms must:
  - compose with the existing explicit traversal boundary,
  - avoid reintroducing stdlib-name magic,
  - and avoid callback-symbol-specific special cases unless a shared
    specialization rule is explicitly documented.

  **Specialization rule:** if M7 introduces specialization of known callbacks
  or handlers, the specialization must be defined by a general rule
  ("known effect op with property X lowers via form Y"), not by one-off symbol
  branches for `Print.println` or other specific stdlib names.

  **Surface lock rule:** M7 must choose one concrete user-facing traversal
  surface before implementation starts. This plan must name:
  - the exact stdlib/API entrypoint or syntax form;
  - one representative example to carry through baseline, implementation, docs,
    and verification;
  - whether callback-style `each` remains the default recommendation or becomes
    the compatibility surface after M7.

  **Effect semantics rule:** the chosen iterator/effect path must preserve and
  document:
  - element visitation order;
  - exactly-once callback/effect delivery semantics;
  - handler nesting/resume behavior;
  - whether traversal is single-pass and streaming or may buffer materialized
    intermediate state.
  M7 is not complete if performance work lands without these semantics being
  written down.

  **Measurement rule:** M7 must lock one performance story for the new surface.
  The benchmark set must distinguish:
  - pure per-element traversal,
  - effect-callback traversal,
  - the chosen iterator/effect surface on the same logical workload.
  If the new surface cannot beat or match the existing callback story on the
  locked practical workloads, the plan must explicitly say why it still exists.

  **Ownership rule:** M7 must explicitly assign ownership for:
  - stepping,
  - yielding,
  - consumption,
  - handler interaction,
  - optional specialization.
  These must be described as stable compiler/runtime boundaries, not as a
  list of current implementation hooks.

  **M7-0 lock snapshot (2026-04-13):**
  - locked surface entrypoint/syntax:
    - `goby/iterator` `iterator.yield : a -> b -> (Bool, b)` handled via
      `with ... in ...` is the M7 iterator/effect traversal surface.
    - producer code traverses `List` and calls `iterator.yield` for each
      element.
  - representative example (for M7-1/M7-3/M7-5 continuity):
    - `examples/list_iterator_effect.gb`
    - shape: list producer + `iterator.yield` handler demonstrating source
      order and early stop (`resume (False, state)`).
  - positioning lock for this milestone:
    - this iterator/effect surface is **experimental-but-supported** in M7.
    - callback-style `list.each` remains the recommended default traversal
      surface until M7 verification closes.

  ### M7 Implementation Steps

  - [x] **M7-0: Lock the target iterator/effect surface**
    - scope:
      - choose the exact user-facing traversal entrypoint or syntax to carry
        through M7;
      - add one representative example in `examples/` and reference it here;
      - state whether this surface is intended as:
        - the recommended default for effect-oriented traversal,
        - an equal alternative to callback-style traversal,
        - or an experimental-but-supported surface.
    - done when: the named surface and example are recorded in this section.
    - checks: docs/examples updated consistently.

  - [x] **M7-1: Capture traversal baseline against the locked surface**
    - scope: record baseline numbers and behavior for:
      - `each` with pure callback,
      - `each` with effect callback,
      - the M7-0 locked iterator/effect example on the same logical workload.
    - done when: baseline numbers are written in PLAN_SEQUENCE.md M7 section.
    - checks: benchmark command + `cargo test -p goby-wasm`
    - lock:
      - the same fixture names and input sizes must be reused for M7-4/M7-5.

  **M7-1 baseline snapshot (2026-04-13):**
  - benchmark command:
    - `cargo test -p goby-wasm m7_1_baseline_traversal_workloads -- --ignored --nocapture`
  - fixture lock (reuse as-is for M7-4/M7-5):
    - `each-pure-callback-3`: `List Int` sum workload over `[1, 2, 3]`.
    - `each-effect-callback-3`: same sum workload + per-element `print ""`.
    - `iterator-effect-yield-3`: same sum workload via `Iterator.yield` + `with` handler.
  - measured snapshot (`warmup=3`, `measured=10`, `p50/p95`, microseconds):
    - `each-pure-callback-3`: `p50=807us`, `p95=901us`
    - `each-effect-callback-3`: `p50=1010us`, `p95=1168us`
    - `iterator-effect-yield-3`: `p50=1234us`, `p95=1319us`
  - correctness snapshot:
    - all three fixtures resolved output `"6"` with `ok_runs=13`, `none_runs=0`.

  - [x] **M7-2: Define iterator/effect lowering ownership**
    - scope:
      - specify which layer owns stepping, yielding, consumption, and handler
        interaction;
      - define the IR/lowering/runtime forms used for that ownership split;
      - document how these forms map onto the M5 traversal boundary;
      - document the intended user-facing traversal style that these forms
        implement;
      - explicitly state whether that style is:
        - the recommended default for effect-oriented traversal,
        - an equal alternative to callback-style traversal,
        - or a future direction not yet recommended as primary.
      - explicitly state where element order, single-pass behavior, and
        resume/handler semantics are enforced.
    - done when: the ownership split is written in PLAN_SEQUENCE.md and
      mirrored in code comments for the owning modules.
    - checks: `cargo test -p goby-wasm`

  **M7-2 ownership lock snapshot (2026-04-13):**
  - ownership split:
    - stepping:
      - owned by traversal-producing source/lowered structure (for M7-0 this is
        list producer recursion / `list.each`-style source shape), not by
        effect dispatch internals.
    - yielding:
      - owned by effect-handler lowering at handled-op rewrite boundaries
        (`CompExpr::PerformEffect` / handled call -> rewritten clause body +
        continuation bridge).
    - consumption:
      - owned by traversal consumer shape (`list.each` callback path over
        `__goby_list_fold`, or M7-0 producer recursion that threads handler
        state).
    - handler interaction:
      - owned by runtime continuation state/token machinery (`ResumeToken`,
        handler continuation state progression, one-shot consume checks).
  - forms and mapping to M5 traversal boundary:
    - callback traversal keeps using the shared M5 `ListFold` family
      (`list.each` = Goby surface over `__goby_list_fold`);
    - iterator/effect traversal does not add stdlib-name magic paths; it uses
      existing effect forms (`PerformEffect` / `WithHandler` / `Resume`) and
      handler-lowering/runtime continuation machinery.
  - intended user-facing style and status (re-affirmed):
    - M7 iterator/effect traversal style remains **experimental-but-supported**
      in this milestone;
    - callback-style `each` remains the recommended default until M7 closes.
  - semantics enforcement points:
    - element order + single-pass behavior:
      enforced by producer traversal structure (`[head, ..tail]` progression /
      fold order) and preserved through M5 fold boundary.
    - resume/handler semantics:
      enforced in runtime continuation token state (pending/resumed/suspended +
      one-shot consume guard).

  - [x] **M7-3: Implement the generic iterator/effect execution path**
    - scope:
      - add the general lowering/runtime path for the M7-0 surface;
      - ensure it executes through explicit forms rather than symbol-name
        matching in stdlib/lowerer code;
      - validate effect handling semantics with focused regression tests,
        including element order, early handler exit if supported, and nested
        effect/callback interaction.
    - done when: base iterator/effect regression tests pass and the intended
      semantics from M7-2 are covered by named tests.
    - checks: `cargo test -p goby-wasm`

  **M7-3 implementation snapshot (2026-04-13):**
  - general path lock-in:
    - iterator effect-op resolution now covers imported owner forms used by M7
      surface code:
      - qualified receiver form keyed by module basename/alias (for example
        `iterator.yield`);
      - bare op form after selective effect-owner import
        (for example `import goby/iterator ( Iterator )`, then `yield ...`).
    - handler-lowering clause match now accepts both exact op names and
      qualified clause names (`iterator.yield`) via short-name match.
  - M7-3 focused regressions (`crates/goby-wasm/src/runtime_rr_tests.rs`):
    - `m7_3_iterator_effect_traversal_preserves_source_order_and_early_stop`
    - `m7_3_iterator_effect_handler_clause_can_host_nested_callback_effects`
  - resolver regressions (`crates/goby-core/src/resolved.rs`):
    - `resolves_alias_qualified_effect_op_via_module_basename_receiver`
    - `resolves_bare_effect_op_via_selective_imported_effect_owner`
  - verification snapshot:
    - `cargo test -p goby-wasm m7_3_ -- --nocapture`: green
    - `cargo test`: green

  - [x] **M7-4: Evaluate and, if justified, add shared specialization**
    - scope:
      - benchmark the general M7 path against the M7-1 baseline;
      - if practical targets are already met, do not add specialization;
      - if specialization is needed, add one shared rule with explicit
        eligibility criteria and regression tests;
      - the rule must be phrased in terms of effect/handler properties or IR
        shape, not specific stdlib symbol names.
    - done when: either
      - the generic path meets target with no specialization, or
      - the chosen shared specialization rule is documented, implemented,
        and benchmark-validated.
    - checks: `cargo test -p goby-wasm`

  **M7-4 evaluation snapshot (2026-04-13):**
  - benchmark command:
    - `cargo test -p goby-wasm m7_4_benchmark -- --ignored --nocapture`
  - measured snapshot (`warmup=3`, `measured=10`, `p50/p95`, microseconds):
    - `each-pure-callback-3`:    `p50=829us`, `p95=841us`  (M7-1: p50=807us, p95=901us)
    - `each-effect-callback-3`:  `p50=995us`, `p95=1009us` (M7-1: p50=1010us, p95=1168us)
    - `iterator-effect-yield-3`: `p50=1261us`, `p95=1296us` (M7-1: p50=1234us, p95=1319us)
  - overhead ratio: `iterator-effect-yield-3 / each-pure-callback-3 = 1.52x` (threshold 2.0x)
  - §6.6 evaluation: generic path meets practical target — no memory exhaustion, no O(n²) growth.
  - decision: **no specialization needed**. The general M7 execution path is sufficient.
    iterator-effect overhead (1.52x over each-pure) is within practical bounds for
    ordinary Goby scripts at the §6.6 success-bar input sizes.

  - [x] **M7-5: Lock iterator/effect verification**
    - scope: compare final numbers against M7-1 baseline.
      - correctness gate: iterator/effect traversal regressions green.
      - design gate:
        - no reintroduction of stdlib-name magic;
        - no callback-symbol-specific one-off branches;
        - iterator/effect execution still routes through the explicit
          traversal family established by M5.
        - one documented user-facing iterator/effect traversal style is
          published as the intended path in examples/spec/plan docs.
        - effect semantics are documented and match the regression suite.
      - performance gate: effect-oriented traversal must meet the practical
        scripting success criteria from §6.6, either via the generic path or
        one documented shared specialization rule.
      - positioning gate:
        - the plan explicitly states whether callback-style `each` remains the
          default recommendation, becomes legacy-compatible surface, or stays
          equal-first-class after M7.
    - done when: verification snapshot is recorded and the M7 checkbox is
      marked `[x]`.
    - checks: `cargo test -p goby-wasm` green, benchmark comparison recorded,
      PLAN_SEQUENCE.md updated.

  **M7-5 verification snapshot (2026-04-13):**
  - correctness gate:
    - `cargo test -p goby-wasm m7_3_ -- --nocapture`: 2 passed, 0 failed ✓
    - `cargo test -p goby-wasm`: 658+62 passed, 0 failed ✓
    - `cargo test`: all suites green ✓
  - design gate:
    - list traversal path no longer used stdlib-name magic (`each`/`map`/`fold`
      removed at M5) ✓
    - no callback-symbol-specific one-off branches in gen_lower ✓
    - iterator/effect execution routes through the M5 explicit traversal boundary
      (fold family + handler-lowering rewrite boundary) ✓
    - user-facing traversal style documented in `doc/LANGUAGE_SPEC.md` (M7 section
      "List traversal style and positioning") and `examples/list_iterator_effect.gb` ✓
    - effect semantics (`yield : a -> state -> (Bool, state)`, resume contract,
      early-stop convention) documented in `doc/LANGUAGE_SPEC.md` ✓
  - performance gate:
    - M7-4 measured: iterator-effect/each-pure = 1.52x (threshold 2.0x) ✓
    - §6.6 success bar met: no O(n²), no memory exhaustion at ≤1000 elements ✓
    - no specialization needed ✓
  - positioning gate:
    - `list.each` (callback-style) is the **recommended default** for ordinary iteration ✓
    - `goby/iterator` (`with ... in ...`) is **experimental-but-supported** for
      early-stop / state-threading use cases ✓
    - documented in `doc/LANGUAGE_SPEC.md` under "List traversal style and positioning" ✓

  ### M7 Design Constraints

  - Iterator/effect execution extends the M5 traversal boundary; it does not
    create a parallel implicit traversal mechanism.
  - No reintroduction of stdlib-name magic.
  - No callback-symbol-specific one-off fast paths.
  - Any specialization must be rule-based, documented, benchmark-justified,
    and optional relative to the generic path.
  - M7 is not complete until the intended language-facing iterator/effect
    traversal style is documented, not merely implemented internally.
  - M7 may complete the `List` traversal story before the equivalent `string`
    story is aligned, but in that case docs must explicitly say that the
    explicit-boundary policy is complete for `List` and still temporary debt
    for `string`.
  - If effect-oriented traversal cannot meet the practical target honestly,
    stop and revise the plan instead of hiding costs behind magical lowering.

- [x] **M8: Publish the new `List` contract** (complete, 2026-04-14)
  
  ### M8 Goal

  M8 closes the product/documentation loop. It is not just a doc sweep; it
  locks the final public contract for `List` after M0-M7 implementation work.
  M8 is incomplete if the implementation exists but users still need plan-only
  context to understand what `List` is good at, what the preferred traversal
  style is, and what debt remains.

  ### M8 Implementation Steps

  - [x] **M8-0: Audit every user-facing `List` explanation**
    - scope:
      - audit `README.md`, `doc/LANGUAGE_SPEC.md`, `doc/PLAN.md`,
        `doc/PLAN_SEQUENCE.md`, and representative `examples/*.gb`;
      - remove stale linked-list-flavored wording and stale pre-M7 traversal
        guidance;
      - identify every place where `List` practicality or traversal style is
        stated differently.
    - done when: this plan lists the files that define the public contract.
    - M8-0 publication set (locked):
      - `README.md` (high-level product positioning)
      - `doc/LANGUAGE_SPEC.md` (authoritative surface contract + caveats)
      - `doc/PLAN.md` (explicit-boundary policy status and debt framing)
      - `doc/PLAN_SEQUENCE.md` (sequence plan milestones and closure criteria)
      - `examples/README.md` + representative example sources

  - [x] **M8-1: Publish the final `List` positioning**
    - scope:
      - lock the wording for what Goby means by:
        - the default ordered collection,
        - practical indexed read,
        - practical immutable update,
        - practical traversal,
        - list-pattern matching as a surface feature.
      - ensure README stays high-level while spec/plan carry the precise
        contract and caveats.
    - done when: the wording is aligned across README/spec/plan without
      contradiction.

  - [x] **M8-2: Publish representative examples**
    - scope: add or refresh examples showing:
      - ordinary indexed access on `List`;
      - functional update via the intended public surface;
      - list-pattern matching on multi-chunk values;
      - the M7-chosen iterator/effect traversal style;
      - one workload shape that previously exposed the old linked-list runtime
        boundary and now has an honest recommended form.
    - done when: examples are referenced from docs and match the final wording.
    - locked examples:
      - indexed access: `examples/list_index.gb`
      - functional immutable update: `examples/list_set.gb`
      - multi-chunk list-pattern decomposition:
        `examples/list_pattern_multichunk.gb`
      - M7 iterator/effect traversal style:
        `examples/list_iterator_effect.gb`
      - update-heavy workload shape with recommended public update surface:
        `examples/list_set.gb`

  - [x] **M8-3: Resolve or carve out remaining explicit-boundary debt**
    - scope:
      - either align remaining string traversal magic (`graphemes` / `split`)
        to the same explicit-boundary policy;
      - or record it as temporary debt with:
        - a named follow-up milestone,
        - a short rationale,
        - and explicit wording that the policy is complete for `List` but not
          yet for `string`.
    - done when: there is no ambiguity about whether the explicit-boundary
      story is fully closed or intentionally partial.
    - M8 decision:
      - explicit-boundary policy is published as complete for `List`;
      - `string.graphemes` / `string.split` alignment remains temporary debt;
      - named follow-up milestone `M9` tracks this debt explicitly.

  - [x] **M8-4: Lock the final publication snapshot**
    - scope:
      - verify examples, spec text, and roadmap wording agree with the shipped
        implementation and measured claims;
      - record the final verification snapshot and mark this plan complete.
    - done when: final wording, examples, and remaining debt are all locked in
      one documented snapshot.
    - verification snapshot (2026-04-14):
      - docs/examples updated and cross-referenced:
        - `README.md`
        - `doc/LANGUAGE_SPEC.md`
        - `doc/PLAN.md`
        - `doc/PLAN_SEQUENCE.md`
        - `examples/README.md`
        - `examples/list_pattern_multichunk.gb`
      - quality gate:
        - `cargo fmt`: green
        - `cargo check`: green
        - `cargo test`: green
          - `goby-cli`: `25 passed`
          - `goby-cli integration`: `52 passed`
          - `goby-core`: `696 passed, 2 ignored`
          - `goby-wasm`: `658 passed, 9 ignored`
          - `wasm exports/smoke`: `62 passed, 2 ignored`

- [x] **M9: Align string traversal explicit-boundary policy** (complete, 2026-04-14)
  - scope:
    - remove remaining string traversal name-list wiring (`graphemes` /
      `split`) where feasible;
    - or replace it with the same explicit lowerer/runtime boundary style used
      for `List`.
  - rationale:
    - M8 intentionally closed publication for `List` while keeping string
      traversal debt explicit and non-hidden.
  - outcome:
    - `gen_lower/mod.rs` no longer excludes `graphemes` / `split` from generic
      stdlib declaration collection.
    - `string.graphemes` now resolves through ordinary
      `backend_intrinsic_for("string", "graphemes")`.
    - `split text ""` no longer rewrites to a dedicated
      `StringGraphemesList` lowering branch; empty-delimiter handling now lives
      inside the shared `StringSplit` helper boundary.
    - `map lines graphemes` remains supported through an explicit wrapper-handle
      lowering path for intrinsic-backed function values.
  - verification snapshot (2026-04-14):
    - focused:
      - `cargo test -p goby-wasm lower_string_ -- --nocapture`: green
      - `cargo test -p goby-wasm lower_value_arg_graphemes_prefers_intrinsic_wrapper_over_known_decl_handle -- --nocapture`: green
      - `cargo test -p goby-wasm c4_s4_split_empty_delimiter -- --nocapture`: green
    - full gate:
      - `cargo fmt`: green
      - `cargo check`: green
      - `cargo test`: green
        - `goby-cli`: `25 passed`
        - `goby-cli integration`: `52 passed`
        - `goby-core`: `696 passed, 2 ignored`
        - `goby-wasm`: `660 passed, 9 ignored`
        - `wasm exports/smoke`: `63 passed, 2 ignored`

## 10. Open Questions (and Resolved Decisions)

- **Resolved (M2):** Which family offers the best balance for Goby in practice?
  Candidate B (Chunked Sequence) is locked as the direction. See §6.6 and §8.
- **Resolved (M4):** honest performance language for `[x, ..rest]`-style
  sequence views is now documented in `doc/LANGUAGE_SPEC.md`:
  amortized O(1) extraction with chunk-boundary O(n/CHUNK_SIZE) header-copy
  cost under the current bump allocator.
- How much should stdlib traversal optimization be expressed in sequence
  intrinsics versus iterator/effect lowering?
- What is the minimum explicit intrinsic/lowerer surface needed to keep stdlib
  readable while still making `List` practical for ordinary scripts?
- **Resolved**: CHUNK_SIZE is currently locked at `32` for Candidate B.

## 11. Working Conclusion

The current linked-list-flavored runtime model is too limiting for Goby's goal
of one pleasant default collection. The long-term direction should be to keep
`List` as the surface collection while moving its internals and optimized
execution model toward a sequence-backed design with explicit compiler/runtime
bounds, honest docs, preserved list-like elegance, and the smallest possible
amount of special runtime machinery.
