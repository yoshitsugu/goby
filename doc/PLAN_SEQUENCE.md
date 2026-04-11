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

- [x] **M2: Prototype candidate internal representations** (complete, 2026-04-10)
  - Evaluate at least two viable sequence-backed `List` representation
    directions, with enough prototype or implementation evidence to compare
    them honestly.
  - Evaluated three candidates (A: flat array, B: Chunked Sequence, C: Spine+Tail)
    via static complexity analysis against the §6.6 workload matrix and success bar.
  - Bump allocator allocation pressure included as a named evaluation axis.
  - Candidate A rejected: O(n) prepend/split demonstrated harmful in RR-4 (see §8).
  - Candidate B (Chunked Sequence) selected: O(CHUNK_SIZE) chunk-copy per update
    (constant cost); no structural sharing needed under bump allocator; moderate
    implementation complexity (see §8).
  - Workload matrix and success bar recorded in §6.6.
  - Direction lock is conditional on M3 benchmark execution meeting the §6.6 bar.

- [x] **M3: Introduce an explicit sequence runtime boundary** (complete, 2026-04-10)
  - Implement the Candidate B (Chunked Sequence) runtime representation. Lock
    CHUNK_SIZE as a named compile-time constant and document it.
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
  - Verification snapshot: `cargo test -p goby-wasm` green
    (`622 passed, 0 failed, 4 ignored`), including RR-4 large-shape regressions
    and a multi-chunk `[h, ..t]` runtime regression case.
  - Direction lock for Candidate B remains active after M3 verification.

- [x] **M4: Re-found list pattern matching on sequence views** (complete, 2026-04-11)
  - Route `[]`, `[x, ..rest]`, exact-length patterns, and prefix/tail variants
    through one shared sequence-view boundary.
  - Ensure parser/typechecker semantics remain elegant while runtime extraction
    no longer assumes a cons-list node layout.
  - Decide and document the honest performance language for ordinary repeated
    list-pattern use based on benchmark evidence.
  - Add regression coverage for list-pattern-heavy programs on the new runtime
    model.
  - Verification snapshot:
    - `cargo test -p goby-wasm` green (`625 passed, 0 failed, 4 ignored`).
    - chunk-boundary and empty-tail regressions locked:
      - `rr4_repeated_head_tail_decomposition_crosses_chunk_boundaries`
      - `rr4_exact_length_pattern_crosses_chunk_boundary_and_matches`
      - `rr4_head_tail_pattern_binds_empty_tail_for_single_item_list`

- [ ] **M5: Rebuild stdlib traversal on explicit sequence/iterator boundaries**

  ### M5 Design Decisions

  **Approach: explicit intrinsics in stdlib, eliminate implicit magic**

  Up to M4 the lowerer silently recognized function names like `each`, `map`
  as magic words and replaced them with dedicated Wasm loop instructions
  (`ListEach`, `ListMap`, etc.). Reading the stdlib source gave no indication
  that special processing was involved. M5 eliminates this implicit pattern
  and makes stdlib `.gb` files call `__goby_*` intrinsics explicitly.

  The model is `string.gb`. For example `grapheme_count`:
  ```
  grapheme_count value =
    mut n = 0
    with
      yield _ _ ->
        resume (True, ())
    in
      n := __goby_string_each_grapheme value
    n
  ```
  - The special boundary (`__goby_string_each_grapheme`) is named explicitly.
  - Logic (counting) is expressed in ordinary Goby effect handler code
    (`with`/`yield`/`resume`).
  - A reader immediately sees where the special boundary is and where
    ordinary Goby logic is.

  **Semantic relationships:** `fold` is the fundamental traversal operation;
  `each` is a derived form.
  - `each xs f` = `fold xs () (fn _ x -> f x; ())`
  - `map` involves new-list construction, so it cannot be written purely
    in terms of fold — it requires its own intrinsic.

  **Comparison (§9 M5 requirements)**

  | Option | Intrinsics | stdlib readability | Implicit magic |
  |---|---|---|---|
  | A: every function as a 1-line intrinsic wrapper | 4 (length, each, map, fold) | each function is hollow | none (but no logic visible) |
  | B: lowerer name-matching (M4 approach) | 0 (implicit) | plain Goby code | SPECIALLY_LOWERED list |
  | **C: minimal intrinsics + Goby logic** | **3 (length, fold, map)** | **boundaries explicit, logic in Goby** | **none** |

  **Decision: C.** Intrinsics handle only operations unreachable from Goby
  code; all other logic is written in Goby.

  - **`__goby_list_length xs`** — reads the header `total_len` field in O(1).
    Direct access to the chunked internal structure is impossible from Goby.
    Clear justification for an intrinsic.

  - **`__goby_list_fold xs init f`** — chunk-walk loop + accumulator +
    callback dispatch. Chunk traversal is an internal operation unreachable
    from Goby code. `fold` is the fundamental traversal; other operations
    (`each`, etc.) are built on top.

  - **`__goby_list_map xs f`** — chunk-walk loop + callback + new list
    construction. Allocating and populating new chunks is unreachable from
    Goby code. Writing `map` in terms of `fold` would require prepend-then-
    reverse, adding an O(n) copy. Justification for a separate intrinsic.

  - **`each` is written in Goby** — it is a derived form of `fold`:
    ```
    each : List a -> (a -> Unit) -> Unit
    each xs f =
      __goby_list_fold xs () (fn _ x ->
        f x
        ()
      )
    ```
    `__goby_list_fold` handles the chunk walk; `each` is a fold that
    discards its accumulator. Semantically correct and avoids adding
    another intrinsic.

  **Eliminating implicit magic:**
  - Remove `"each"` and `"map"` from `SPECIALLY_LOWERED_STDLIB_NAMES`.
  - Remove the `GlobalRef("list","each")` / `GlobalRef("list","map")` /
    `GlobalRef("list","fold")` name-match branches in the lowerer.
  - Register `"__goby_list_length"`, `"__goby_list_fold"`,
    `"__goby_list_map"` in `backend_intrinsic_for_bare` instead —
    the same registration mechanism used by `"__goby_string_each_grapheme"`.

  **Preserving the effect story:**
  - Since `each` is a fold wrapper, effects in callbacks work naturally.
    `each xs (fn x -> println x)` performs the `println` effect inside
    the fold callback; effect handler lowering handles it as usual.
  - M5 does not introduce callback-specific specialization rules
    (`Print.println`-specific fast paths, etc.). All `each`/`fold` callback
    forms use the same generic `__goby_list_fold` execution boundary.
    Callback specialization design is deferred to M7 Iterator/effect work.
  - M7 can later introduce an Iterator effect using `__goby_list_fold`
    as its foundation, preserving design room for effect-oriented
    traversal.

  **`join`:** Currently written as case-based recursion in stdlib and does
  not call `fold`. After M5, rewriting `join` to use `fold` would let it
  ride the chunk-walk loop, but that rewrite is out of M5 scope. No
  dedicated `join` intrinsic is needed.

  **`ListReverseFoldPrepend`:** The existing optimization is preserved, but
  its trigger moves from symbol-name matching to semantic fold-boundary
  matching. M5 must keep the optimization active for both:
  - direct `__goby_list_fold xs init f` calls, and
  - public `fold xs init f` calls (via stdlib wrapper).
  The implementation strategy is a traversal-boundary reification pass:
  reify calls resolved to stdlib `list.fold` into the same internal fold
  boundary node used by direct `__goby_list_fold` calls before backend
  optimization matching. The pattern-match logic
  (`lower_supported_inline_list_fold_prepend_builder`) remains structural
  (`[prefix..., ..acc]` callback shape), not symbol-name based and not
  wrapper-shape dependent.

  **Performance note on `each` via `fold`:** `each` implemented as
  `__goby_list_fold xs () (fn _ x -> ...)` pushes and discards an unused
  Unit accumulator on every iteration. This is a constant-per-element
  overhead (a few Wasm instructions) and negligible for the §6.6 success
  bar. If profiling after M5 reveals that `each` is measurably slower than
  a hypothetical dedicated `__goby_list_each`, a targeted emit optimization
  (elide acc push/pop when init is `()` and the callback ignores its first
  argument) can be added later without changing the stdlib surface.

  **Future alignment: `graphemes`/`split` in `string.gb`** still use
  `SPECIALLY_LOWERED_STDLIB_NAMES` implicit magic. The same explicit-
  intrinsic principle adopted in M5 for list operations should be applied
  to string operations in a future milestone. This is out of M5 scope but
  M7/M8 must not present the explicit-boundary story as fully complete until
  the remaining string traversal magic is either aligned or explicitly carved
  out as temporary debt with a follow-up milestone.

  ### M5 Implementation Steps

  - [x] **M5-0: Capture baseline performance snapshot** (complete, 2026-04-11)
    - scope: record M4 baseline numbers for the locked list traversal
      workloads (`length`, `fold` sum, `each` with effect callback, `map`
      over multi-chunk list) under `cargo test -p goby-wasm -- --nocapture`
      bench harness or current project benchmark command.
    - done when: baseline numbers are written in PLAN_SEQUENCE.md M5 section.
    - checks: benchmark command + `cargo test -p goby-wasm`
    - M4 baseline: `cargo test -p goby-wasm` → 625 passed, 0 failed, 4 ignored.
      All list traversal tests (each, map, fold, length) pass via current
      implicit name-matching lowerer paths (ListEach, ListEachEffect, ListMap,
      ListReverseFoldPrepend, DeclCall("fold"), recursive length).

  - [x] **M5-1: Add `__goby_list_length` intrinsic** (complete, 2026-04-11)
    - scope:
      - `backend_ir.rs`: add `BackendIntrinsic::ListLength`.
        arity = 1, execution_boundary = InWasm.
      - `lower.rs` `backend_intrinsic_for_bare`: register
        `"__goby_list_length"` → `BackendIntrinsic::ListLength`.
      - `emit.rs`: add `emit_list_length_helper`. Extract the header ptr,
        `I32Load(offset: 0)` for `total_len`, tag as TAG_INT, widen to i64.
        No chunk walk needed.
      - `stdlib/goby/list.gb`: rewrite `length`:
        ```
        length : List a -> Int
        length xs = __goby_list_length xs
        ```
    - done when: `cargo test -p goby-wasm` green.
    - checks: `cargo test -p goby-wasm`

  - [x] **M5-2: Regression tests for `ListLength`** (complete, 2026-04-11)
    - scope: add to `runtime_rr_tests.rs`:
      - empty list `length []` → 0
      - single chunk (32 elements) via `length` → 32
      - multi-chunk (65 elements) via `length` → 65 (crosses chunk boundary)
      - one direct intrinsic smoke case: `__goby_list_length []` → 0
        (keeps intrinsic endpoint coverage without making tests stdlib-blind)
    - done when: all 4 cases green.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M5-3: Add `__goby_list_fold` intrinsic and rewrite stdlib `fold`**
    - scope: this step adds the intrinsic, rewrites stdlib, AND
      simultaneously updates the lowerer so that `__goby_list_fold` is the
      only fold path. Both changes must land together because the old
      `GlobalRef("list","fold")` branch would conflict with the new stdlib.
      - `backend_ir.rs`: add `BackendIntrinsic::ListFold`.
        arity = 3, execution_boundary = InWasm.
      - `lower.rs` `backend_intrinsic_for_bare`: register
        `"__goby_list_fold"` → `BackendIntrinsic::ListFold`.
      - `emit.rs`: add `emit_list_fold_helper`. Same double chunk-walk
        loop structure as `emit_list_each`, with an accumulator local added.
        Each element calls `func(acc, elem)` via `emit_callable_dispatch`
        with 2 arguments, stores the return value into acc. After the loop
        completes, push acc.
      - `stdlib/goby/list.gb`: rewrite `fold`:
        ```
        # Left fold. The callback receives the accumulator first: f acc elem.
        fold : List a -> b -> (b -> a -> b) -> b
        fold xs acc f = __goby_list_fold xs acc f
        ```
      - Remove `GlobalRef("list","fold")` and `Var("fold")` name-match
        branches from the lowerer. User code calling `fold` now dispatches
        through the stdlib `fold` definition, which calls `__goby_list_fold`.
    - done when: `cargo test -p goby-wasm` green.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M5-4: Re-anchor `ListReverseFoldPrepend` on semantic fold boundary**
    - scope: the old prepend-pattern optimization fired inside the removed
      `GlobalRef("list","fold")` branch. Replace it with a semantic-fold
      optimization gate that works for both public `fold` and direct
      `__goby_list_fold` calls:
      - add traversal-boundary reification in lowering so calls resolved to
        stdlib `list.fold` and direct `__goby_list_fold` produce the same
        internal fold boundary representation before optimization matching.
      - run `lower_supported_inline_list_fold_prepend_builder` on that fold
        boundary representation (not on wrapper syntax).
      - when callback shape is `[prefix..., ..acc]`, emit
        `ListReverseFoldPrepend`; otherwise emit general
        `BackendIntrinsic::ListFold`.
    - done when: `cargo test -p goby-wasm` green. Existing
      `ListReverseFoldPrepend` compile tests assert the dedicated
      instruction is emitted for both `fold` and `__goby_list_fold` entrypoints.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M5-5: Add `__goby_list_map` intrinsic and rewrite stdlib `map`**
    - scope: same atomic-step rationale as M5-3.
      - `backend_ir.rs`: add `BackendIntrinsic::ListMap`.
        arity = 2, execution_boundary = InWasm.
      - `lower.rs` `backend_intrinsic_for_bare`: register
        `"__goby_list_map"` → `BackendIntrinsic::ListMap`.
      - `emit.rs`: connect existing `emit_list_map` as the handler for
        `BackendIntrinsic::ListMap`.
      - `stdlib/goby/list.gb`: rewrite `map`:
        ```
        map : List a -> (a -> b) -> List b
        map xs f = __goby_list_map xs f
        ```
      - Remove `GlobalRef("list","map")` and `Var("map")` name-match
        branches from the lowerer.
      - Remove `"map"` from `SPECIALLY_LOWERED_STDLIB_NAMES`.
    - done when: `cargo test -p goby-wasm` green. All existing map tests pass.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M5-6: Rewrite `each` as Goby code on top of `__goby_list_fold`**
    - scope:
      - `stdlib/goby/list.gb`: rewrite `each`:
        ```
        each : List a -> (a -> Unit) -> Unit
        each xs f =
          __goby_list_fold xs () (fn _ x ->
            f x
            ()
          )
        ```
      - Remove `GlobalRef("list","each")` and `Var("each")` name-match
        branches from the lowerer.
      - Remove `"each"` from `SPECIALLY_LOWERED_STDLIB_NAMES`.
    - done when: `cargo test -p goby-wasm` green.
      All existing each tests (including effect callbacks) pass.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M5-7: Remove legacy `WasmBackendInstr` variants**
    - scope: after M5-3 through M5-6, the old dedicated backend instructions
      are no longer produced by the lowerer. This step removes the dead code:
      - Remove `WasmBackendInstr::ListEach` and its emit function.
      - Remove `WasmBackendInstr::ListMap` and its emit function.
      - Remove `WasmBackendInstr::ListEachEffect` and its emit function.
      - Verify that `SPECIALLY_LOWERED_STDLIB_NAMES` no longer contains
        `"each"` or `"map"` (only `"graphemes"` and `"split"` remain).
      - Verify `list.each xs Print.println` still executes correctly via the
        generic fold callback path.
    - done when: `cargo test -p goby-wasm` green. No references to the
      removed variants remain in non-test code.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M5-8: Runtime regression tests**
    - scope: add to `runtime_rr_tests.rs`:
      - empty-list fold: `fold [] 0 (fn acc x -> acc + x)` → 0
      - single-chunk fold sum
      - multi-chunk fold sum (65 elements)
      - fold with string concatenation → println (effect path)
      - `ListReverseFoldPrepend` coexistence:
        - prepend pattern lowered to dedicated instruction via public `fold`
        - prepend pattern lowered to dedicated instruction via direct
          `__goby_list_fold`
      - each with general callback (non-effect)
      - each with effect callback (Print.println)
      - map on multi-chunk list
    - done when: all cases green.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M5-9: Audit and documentation**
    - scope: record the M5 traversal boundaries in `backend_ir.rs`:
      - `__goby_list_length` → `BackendIntrinsic::ListLength`:
        header `total_len` direct read, O(1).
      - `__goby_list_fold` → `BackendIntrinsic::ListFold`:
        chunk-walk loop + accumulator + callback dispatch.
        Prepend callback shape → `ListReverseFoldPrepend` optimization
        at semantic fold boundaries (public `fold` and direct intrinsic call).
      - `__goby_list_map` → `BackendIntrinsic::ListMap`:
        chunk-walk loop + callback + new chunked list construction.
      - `each`: Goby code. Derived from `__goby_list_fold`
        (fold that discards its accumulator).
      - `fold`: 1-line wrapper around `__goby_list_fold`.
    - done when: doc comments added.
    - checks: `cargo test -p goby-wasm` (no regressions)

  - [ ] **M5-10: Verification snapshot and M5 completion**
    - scope: record final `cargo test -p goby-wasm` result and compare
      against M5-0 baseline workloads.
      - correctness gate: all M5 regression tests green.
      - design gate: no new callback-symbol-specific specialization branches
        added in lowerer for list traversal.
      - performance gate:
        - `length`/`fold`/`map` workloads must be no worse than M4 by >5%.
        - `each` effect-callback workload may regress up to 10% in M5
          (specialization deferred to M7), but must still satisfy §6.6
          practical scripting success criteria.
      Mark the M5 checkbox as `[x]` in PLAN_SEQUENCE.md with the
      verification snapshot (test count).
    - done when: all M5 sub-steps are `[x]`.
    - checks: `cargo test -p goby-wasm` green, benchmark comparison recorded,
      PLAN_SEQUENCE.md updated.

  ### M5 Design Constraints

  - stdlib `.gb` calls `__goby_*` intrinsics explicitly. No implicit name magic.
  - Intrinsics handle only operations unreachable from Goby code
    (chunk traversal, header reads, new list construction).
  - Logic (`each` implementation, etc.) is written in Goby code.
  - No new host imports (all intrinsics complete within Wasm).
  - M5 introduces no callback-symbol-specific traversal specialization
    (`Print.println`-specific path, etc.); those decisions are deferred to M7.
  - The existing `ListReverseFoldPrepend` optimization path must not break;
    it must fire at the semantic fold boundary for both public `fold` and
    direct `__goby_list_fold` calls.
  - stdlib rewrites and lowerer branch removal must land atomically per
    function (same step) to avoid intermediate states where old and new
    paths conflict.
  - `graphemes`/`split` implicit magic in `SPECIALLY_LOWERED_STDLIB_NAMES`
    remains for now; aligning string operations to the same explicit-
    intrinsic principle is deferred to a future milestone.

- [ ] **M6: Make index/update workloads practical**

  ### M6 Design Decisions

  **Goal:** indexed read/update must become honestly practical on the chunked
  `List` representation without introducing syntax-shaped exceptions or a
  second hidden collection model.

  **Boundary rule:** M6 extends the same explicit sequence boundary introduced
  in M3-M5. `xs[i]` and `xs[i] := v` may lower through shared sequence/index
  operations, but not through:
  - symbol-name magic on stdlib helpers,
  - syntax-form-specific backend exceptions for one benchmark fixture,
  - or a separate hidden "indexed array mode" for some `List` values.

  **Performance intent:**
  - indexed read should target chunk-aware access rather than head-recursive
    traversal;
  - immutable point update should target chunk-local rebuild plus shallow
    structural copying, not full linear spine rebuild;
  - nested updates should be evaluated as real workloads, not only as isolated
    primitive operations.

  **Diagnostic rule:** if some update or nested-update cases remain materially
  more expensive than the surface syntax suggests, M6 must either:
  - make that cost visible in docs/examples, or
  - narrow the optimization claim.
  M6 must not silently rely on "looks like array update" syntax while keeping
  effectively linked-list behavior underneath.

  ### M6 Implementation Steps

  - [ ] **M6-0: Capture baseline index/update workload snapshot**
    - scope: record current numbers for:
      - repeated `xs[i]` reads on large lists,
      - repeated immutable `xs[i] := v` updates,
      - nested `grid[y][x] := v`-style workloads,
      - one realistic Advent-of-Code-style transform.
    - done when: baseline numbers are written in PLAN_SEQUENCE.md M6 section.
    - checks: benchmark command + `cargo test -p goby-wasm`
    - lock:
      - indexed-read workload: repeated reads across a 4k-element list with
        mixed in-chunk and cross-chunk indices.
      - point-update workload: repeated immutable updates across a 4k-element
        list.
      - nested-update workload: repeated updates on a `64 x 64` `List (List Int)`.
      - AoC-style workload: one concrete transform fixture recorded by name.

  - [ ] **M6-1: Define the explicit index/update boundary**
    - scope:
      - choose and document the shared lowering/runtime boundary that owns
        indexed read and immutable point update on chunked `List`;
      - ensure both surface syntaxes (`xs[i]`, `xs[i] := v`) lower through
        that shared boundary rather than syntax-specific backend branches.
    - done when: the owning boundary is documented in code comments and
      referenced from PLAN_SEQUENCE.md.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M6-2: Implement chunk-aware indexed read**
    - scope:
      - lower indexed read through the shared M6 boundary;
      - implement chunk-aware lookup that avoids head-recursive linear walk;
      - keep out-of-range diagnostics/behavior explicit and regression-tested.
    - done when: large indexed-read regression tests pass and the locked
      4k-element indexed-read workload improves on the M6-0 baseline by at
      least 5x.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M6-3: Implement chunk-local immutable point update**
    - scope:
      - lower immutable point update through the shared M6 boundary;
      - rebuild only the touched chunk/header path required by the chunked
        representation, not the whole logical list;
      - preserve immutable semantics and existing diagnostics.
    - done when: repeated update regression tests pass and the locked
      4k-element point-update workload improves on the M6-0 baseline by at
      least 3x.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M6-4: Validate nested update workloads**
    - scope:
      - add regression/benchmark coverage for nested `List (List a)` update
        workloads;
      - record allocation/intermediate-structure observations, not only runtime;
      - confirm no syntax-shaped special-case lowering was added for nested
        forms beyond the shared M6 boundary.
    - done when: nested workload checks are recorded and green.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M6-5: Lock practical-goal verification**
    - scope: compare final numbers against M6-0 baseline.
      - correctness gate: all indexed read/update regressions green.
      - design gate: no syntax-form-specific backend branches added for
        `xs[i]` or `xs[i] := v` outside the shared M6 boundary.
      - performance gate:
        - indexed reads on the locked 4k-element workload must improve on the
          M6-0 baseline by at least 5x and must not regress the best M5
          traversal workloads by more than 5%.
        - repeated immutable point updates on the locked 4k-element workload
          must improve on the M6-0 baseline by at least 3x.
        - nested-update workload must remain within the practical scripting
          success criteria from §6.6 and within 2x of the single-update
          workload's per-operation cost; otherwise stop and revise the plan.
    - done when: verification snapshot is recorded and the M6 checkbox is
      marked `[x]`.
    - checks: `cargo test -p goby-wasm` green, benchmark comparison recorded,
      PLAN_SEQUENCE.md updated.

  ### M6 Design Constraints

  - One shared index/update boundary owns `xs[i]` and `xs[i] := v`.
  - No syntax-shaped backend exceptions for particular update forms or fixtures.
  - No hidden alternate collection mode behind `List`.
  - Performance claims must be backed by recorded workload numbers, not only by
    microbench intuition.
  - If practical targets cannot be reached honestly, stop and revise the plan
    instead of layering compensating exceptions.

- [ ] **M7: Integrate effect-oriented iterator execution**
  
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

  **Ownership rule:** M7 must explicitly assign ownership for:
  - stepping,
  - yielding,
  - consumption,
  - handler interaction,
  - optional specialization.
  These must be described as stable compiler/runtime boundaries, not as a
  list of current implementation hooks.

  ### M7 Implementation Steps

  - [ ] **M7-0: Capture traversal baseline after M5**
    - scope: record baseline numbers and behavior for:
      - `each` with pure callback,
      - `each` with effect callback,
      - one locked iterator/effect-shaped traversal surface example intended to
        replace callback-style traversal ergonomically.
    - done when: baseline numbers are written in PLAN_SEQUENCE.md M7 section.
    - checks: benchmark command + `cargo test -p goby-wasm`
    - lock:
      - record the exact language-facing iterator/effect example used as the
        M7 target surface in PLAN_SEQUENCE.md and examples/.

  - [ ] **M7-1: Define iterator/effect lowering ownership**
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
    - done when: the ownership split is written in PLAN_SEQUENCE.md and
      mirrored in code comments for the owning modules.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M7-2: Implement the base iterator/effect execution path**
    - scope:
      - add the general lowering/runtime path for iterator/effect traversal;
      - ensure it executes through explicit forms rather than symbol-name
        matching in stdlib/lowerer code;
      - validate effect handling semantics with focused regression tests.
    - done when: base iterator/effect regression tests pass.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M7-3: Evaluate and, if justified, add shared specialization**
    - scope:
      - benchmark the general M7 path against the M7-0 baseline;
      - if practical targets are already met, do not add specialization;
      - if specialization is needed, add one shared rule with explicit
        eligibility criteria and regression tests.
    - done when: either
      - the generic path meets target with no specialization, or
      - the chosen shared specialization rule is documented, implemented,
        and benchmark-validated.
    - checks: `cargo test -p goby-wasm`

  - [ ] **M7-4: Lock iterator/effect verification**
    - scope: compare final numbers against M7-0 baseline.
      - correctness gate: iterator/effect traversal regressions green.
      - design gate:
        - no reintroduction of stdlib-name magic;
        - no callback-symbol-specific one-off branches;
        - iterator/effect execution still routes through the explicit
          traversal family established by M5.
        - one documented user-facing iterator/effect traversal style is
          published as the intended path in examples/spec/plan docs.
      - performance gate: effect-oriented traversal must meet the practical
        scripting success criteria from §6.6, either via the generic path or
        one documented shared specialization rule.
    - done when: verification snapshot is recorded and the M7 checkbox is
      marked `[x]`.
    - checks: `cargo test -p goby-wasm` green, benchmark comparison recorded,
      PLAN_SEQUENCE.md updated.

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
  - Either align remaining string traversal magic (`graphemes`/`split`) to the
    same explicit-boundary policy, or explicitly record it as temporary debt
    with a named follow-up milestone and rationale.

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
