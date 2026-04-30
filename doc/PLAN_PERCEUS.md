# Goby Memory Management Plan — Perceus Refcount with Reuse

Last updated: 2026-04-30

This document is the roadmap for Goby's memory management — **Perceus-style
reference counting with reuse analysis** (Reinking, Xie, de Moura, Leijen;
PLDI 2021, as used in Koka, Lean 4, and Roc).

**Status (2026-04-30 recheck): M10 IR boundary fixed; M11 host allocator
unification implemented for active reductions.**

- **M0 – M8 shipped.** The runtime layout, refcount + free-list discipline,
  static drop insertion, borrow inference, basic-block reuse, `mut`
  lowering through reuse primitives, and the M8 widening for the
  borrow-then-update driver shape are all in place. See §4 for the
  one-line acceptance summary of each.
- **M9 partially shipped (2026-04-29).** 3a (self-tail-call param drops)
  and 3c (TupleProject Owned propagation) landed and the M9 acceptance
  test (`perceus_real_world_driver_drops_intermediates_and_reuses_per_round`)
  is green. The widening exposed two design fault lines (DI-1 / DI-2)
  recorded in §4.99.
- **M10 focused fixes landed.** DI-2 is green, and local-shadowed field
  projections now distinguish immutable `Let` parents from `LetMut` parents.
  See §4.100.
- **M11 host-intrinsic allocator unification implemented for active
  reductions.** Host imports that return escaping Goby values allocate into
  the refcounted heap and update allocation stats. Both the 138x138
  `read_lines () -> list.map graphemes` reduction and the full BUGS.md
  real-world-driver shape run under 256 MiB. See §4.101.

Two long-deferred checkboxes are tracked but **won't-fix** under this
plan; the rationale and re-open conditions live in `doc/PLAN.md` §4.2:

- M4 residency test `perceus_loop_residency` — superseded by the M3
  proxy plus the M6 acceptance test.
- M6 Step 5 (`stdlib/goby/list.gb` `set` reuse rewrite) — not on the
  acceptance path; ABI changes required are mirrored in `doc/PLAN.md`.

Related documents:

- `doc/LANGUAGE_SPEC.md` — source of truth for language semantics. The
  current spec says captured `mut` bindings are **shared mutable cells**, not
  values. This plan treats that as a constraint, not something to quietly
  redefine; see §3.4 and Track B.
- `doc/PLAN.md` — top-level roadmap.
- `doc/PLAN_IR.md` — IR lowering pipeline. The new passes introduced by this
  plan insert between closure-env materialization and backend IR lowering;
  the exact slot is specified in §3.8.
- `doc/PLAN_LIST_FIX_M4_SITES.md` — in-flight memory64 migration; this plan
  begins only after that migration is complete (see §6 and M0).

Plan-label hygiene (inherits from `PLAN_IR.md`):

- milestone IDs below are planning metadata only
- do not copy them into code comments, diagnostics, or test names
- describe technical intent directly in code

---

## 1. Goal

Goby programs that update an aggregate value in a tail-recursive loop must run
**without per-iteration allocation growth** when the aggregate is uniquely
owned at that program point. Shared values fall back to copy-on-write. Dead
allocations are reclaimed when the last reference is dropped, not "never" as
under the previous bump-only allocator.

The success criterion is a self-contained program that lives **in this
repository** so the acceptance shape is reviewable long after this plan
closes. The program is fully specified in §1.1 to keep the target
reproducible.

- Terminate under `goby run --max-memory-mb 16` on the end-state
  implementation. Goby ships a single execution path: `goby run`
  compiles the program through the general-lowered Wasm backend and
  runs it under wasmtime. (`goby check` only parses and typechecks
  and is not a runtime backend.)
- Produce the checksum recorded by the M1 acceptance fixture under
  `goby run`. The checksum is committed into the integration test as a
  literal and is the only authoritative target.
- Allocate O(length) bytes total over the whole run, measured by the
  `--debug-alloc-stats` counter introduced in M2. Exact ceiling: 200 KiB
  for the listed program. Anything above that is a bug.
- Existing `examples/*.gb` continue to produce identical output to the
  pre-plan baseline. Divergence is a bug.

This is a **clean rewrite** of the allocation and liveness layer. Goby is
still a personal PoC language; backward compatibility with the current bump
layout is not a constraint. Older heap layouts are removed, not kept behind
flags.

### 1.1 Goal program (normative)

Authored in M1 as `crates/goby-wasm/tests/fixtures/alloc-baseline/refcount_reuse_loop.gb`. The following source is
normative — implementers must not "equivalent"-rewrite it, because the
acceptance checksum depends on the exact allocation and assignment pattern.

```goby
import goby/list (length)
import goby/stdio

step : List Int -> Int -> Int -> List Int
step xs iter iters_remaining =
  if iters_remaining == 0
    xs
  else
    n = length xs
    i = iter % n
    v = (iter * 2654435761) % 65536
    mut ys = xs
    ys[i] := v
    step ys (iter + 1) (iters_remaining - 1)

build : Int -> Int -> List Int -> List Int
build k len acc =
  if k == len
    acc
  else
    build (k + 1) len [k, ..acc]

xor_fold : List Int -> Int -> Int
xor_fold xs acc =
  case xs
    [] -> acc
    [x, ..rest] -> xor_fold rest (acc ^ x)

main : Unit -> Unit can Print
main =
  initial = build 0 4096 []
  final = step initial 0 5000
  checksum = xor_fold final 0
  println "${checksum}"
```

Constants: length 4096, iterations 5000.

Expected checksum: recorded by M1 from a `goby run` execution against
this exact program and committed into the integration test as a
literal. It becomes the authoritative value; subsequent runs must
reproduce it byte-for-byte.

---

## 2. Why Perceus, Not GC or Regions

Three mainstream strategies for functional languages with immutable
aggregates:

1. **Tracing GC** (generational, incremental) — OCaml, Haskell, Go, Java.
   Proven, but requires root-set walking, stack maps, and write barriers.
   Heavy for a language targeting Wasm with a custom layout.
2. **Region-based allocation** — MLKit, earlier Koka. Excellent fit for
   tail-recursive loops, but escape-aware region inference is invasive and
   forces a region calculus onto the type system. Koka itself moved off
   pure regions toward Perceus.
3. **Perceus refcount + reuse** — Koka, Lean 4, Roc. Static `dup`/`drop`
   insertion plus drop-guided reuse. No GC thread, no stack maps, no write
   barriers. Cycles are disallowed; see §3.6.

Perceus is chosen because:

- It gives **in-place update cost for immutable-style code** whenever the
  value is dynamically unique. This is the regime most Goby programs live
  in.
- It composes with tail calls: dropping the caller's owned bindings at the
  return-call boundary is a natural place to recycle allocations.
- The runtime delta from the current bump allocator is small: one refcount
  word per heap object plus a free list per size class. No moving
  collector.
- It plays well with Wasm's lack of stack introspection (no stack maps).

Linear / uniqueness types (Rust, Austral) would give stronger guarantees
but require ownership to surface in the type system. Out of scope.

---

## 3. End-State Design

### 3.1 Heap object layout

Every heap-allocated object carries a one-word header **before** the
existing payload:

```
[ object_ptr - PTR_BYTES ] : uN  refcount  (N = 64; Goby is memory64 only)
[ object_ptr + 0 .. ]      : existing payload (list header, tuple, closure, ...)
```

`object_ptr` (the value stored in a tagged pointer) still points at the
payload, so load/store offsets inside the object are unchanged.

**Existing code that must change.** The list layout is documented in
`crates/goby-wasm/src/gen_lower/emit.rs` lines ~102–160 (the "Chunked
Sequence" comment block). M2 rewrites that comment and adjusts every
`alloc`-emitting site to prepend the refcount word:

- `emit_list_*` (header and chunk allocations),
- `emit_helper_call` / `ListLit` / `TupleLit` / `RecordLit` in
  `emit_instrs`,
- `CreateClosure` lowering,
- `emit_string_split_helper` and any stdlib-intrinsic allocations.

**Static sentinel.** Refcount `!0u64` (all bits set, i.e. `u64::MAX`) marks
static values. `dup` and `drop` short-circuit on sentinel by a refcount
comparison, not by saturating arithmetic. Saturating was considered and
rejected: branch also short-circuits the child-drop cascade, which matters
for hoisted literals that contain heap children.

### 3.2 Size classes (normative table)

Size classes are statically enumerated. Each class has a dedicated free-list
head. No dynamic bucket discovery.

| Class        | Members                             | Size calculation                     |
|--------------|-------------------------------------|--------------------------------------|
| `chunk`      | list chunks                         | `CHUNK_SIZE * 8 + meta` (one size)   |
| `header[k]`  | list headers with `n_chunks == k`   | `k ∈ {1, 2, 4, 8, 16, 32, 64, 128}`  |
| `tuple[a]`   | tuples of arity `a`                 | `a ∈ {1..8}`                         |
| `record[a]`  | records of arity `a`                | `a ∈ {1..8}`                         |
| `closure[s]` | closure env with `s` slots          | `s ∈ {0..8}`                         |
| `cell`       | mutable cell (one boxed slot)       | one size                             |
| `string[b]`  | boxed strings                       | `b ∈ {8, 16, 32, 64, 128, 256, 512, large}` rounded up |
| `large`      | overflow bucket                     | exact-size, no free list              |

Allocations that do not fit any enumerated bucket fall into `large`. As
implemented today (`emit.rs::emit_helper_drop_large`), reaching refcount
zero on a `large` allocation **abandons** the memory: there is no
exact-size free range and the bytes are not recycled into any other
bucket. This is a deliberate trade-off — the goal program does not
exercise `large` — and is acceptable until a workload demands it. A
future "exact-size free range" implementation is allowed but is not part
of this plan.

Every size-class head is a reserved slot in linear memory. The slot layout
is declared as a struct in `emit.rs` adjacent to the existing
`host_bump_cursor` slot; M3 commits the struct layout into that file.

### 3.3 Core runtime operations

- `alloc(size_class) -> ptr` — pop the class's free list; on miss, bump
  fresh memory. Initializes refcount to 1. Returns the payload pointer
  (header word already written).
- `dup(ptr)` — branch on sentinel; else increment refcount.
- `drop(ptr)` — branch on sentinel; else decrement. On reaching zero: call
  the per-type child-drop helper, then push onto the free list. Free-list
  push is **intrusive**: the payload's first word is overwritten with the
  old free-list head, and the head is updated to this object.
- `is_unique(ptr) -> bool` — `refcount == 1`. A single `== 1` compare
  suffices because sentinel is `u64::MAX`.
- `drop_reuse(ptr) -> token` — if `is_unique(ptr)`, return `ptr` unchanged
  (no decrement, no free-list push; the object is about to be recycled).
  Otherwise call `drop(ptr)` and return `null`.
- `alloc_reuse(token, size_class) -> ptr` — if `token != null`, write
  refcount=1 to `token - PTR_BYTES` and return `token`. Else call
  `alloc(size_class)`. The size-class match is resolved statically by the
  reuse pass, so no runtime check is needed.

Reuse token ABI: single `i64` (payload pointer or null).

### 3.4 `mut` bindings and the shared-cell constraint

The current spec (`doc/LANGUAGE_SPEC.md`:170) defines a captured `mut`
binding as a **shared mutable cell**: multiple closures that capture the
same `mut` observe each other's writes. This cannot be modeled as a
consumed linear value.

This plan separates the memory-management concern from the `mut` semantics
concern via two tracks.

**Track A (primary, this plan).** Perceus applies uniformly to all heap
objects, including mutable cells. Mutable cells are the `cell` size class
in §3.2, with refcount identical to any other heap object. `xs[i] := v` on
a non-captured `mut xs` lowers to the reuse machinery iff the runtime
refcount is 1; otherwise it lowers to copy-on-write plus rebind, matching
current observable semantics with accurate freeing. Captured mutable cells
have refcount > 1 by construction and never reuse. The goal program in
§1.1 deliberately uses a non-captured `mut` to stay in the reuse-fast
path.

**Track B (out of scope).** Dropping shared-cell capture in favor of value
capture + explicit `Ref a` is a separate language-spec change with its own
plan. This plan must continue to function unchanged whether or not Track B
happens.

**M6 clarification.** M6 does *not* unify `mut` with reuse in a way that
changes `mut` semantics. M6 only routes the backend lowering of `:=`
through the same reuse primitives as immutable update.

### 3.5 Closures, effect handlers, and multi-resume

**Closures.** A closure environment is a heap object (size class
`closure[s]`) whose slots own their values. Drop insertion runs **after**
closure-env materialization (existing pass in
`crates/goby-core/src/closure_capture.rs`) so env ownership is a
first-class edge in the IR. This ordering is enforced by an assertion in
the pass pipeline; see §3.8.

**Effect handlers and `resume`.** `resume` can be invoked zero, one, or
more than one time by a handler clause (spec: multi-resume;
`doc/PLAN_IR.md`:111). A value captured by a suspended continuation must
not be dropped until the continuation is resumed-and-completed or
abandoned. Rules:

- Continuation reification (`WithHandler` / `PerformEffect` lowering)
  `dup`s every owned binding live across the effect call site. The
  continuation object owns those extra refcounts; when the continuation
  is dropped (abandoned or completed), its own child-drop releases them.
- Reuse analysis must not fire across a `PerformEffect`, a `WithHandler`
  boundary, or a `Call` whose IR-recorded effect row intersects an
  enclosing handler's row. These are **path breakpoints** in §3.7.

These rules are part of the M4 deliverable, not a future follow-up.
Without them, Perceus is unsound for Goby's effect system.

### 3.6 Cycles

Goby has no language-level facility for constructing cyclic heap values.
Closures capture by value or by shared cell; neither produces a cycle,
because cells hold values and values do not refer to their enclosing
scope. This plan forbids introducing cycle-capable features without first
extending this document.

### 3.7 Execution paths and basic blocks (normative)

Perceus's reuse pass requires a precise notion of "same execution path".
The current shared IR (`crates/goby-core/src/ir.rs`) is expression-tree
shaped; this plan imposes the following block structure on top of it
without materializing a new CFG representation.

A **basic block** is a maximal sequence of `CompExpr` nodes such that
none of the following appear except at the end:

- `CompExpr::If { ... }` — terminates the block; each arm starts a new
  block. Reuse does not cross `If`.
- `CompExpr::Case { ... }` — same treatment per arm. Reuse does not cross
  `Case`.
- `CompExpr::PerformEffect { ... }` — terminates. **Reuse does not cross.**
- `CompExpr::WithHandler { ... }` — terminates; the body is a nested
  region. **Reuse does not cross into or out of.**
- `CompExpr::Resume { ... }` — terminates. **Reuse does not cross.**
- `CompExpr::Call { ... }` whose IR effect row is non-empty and
  intersects any enclosing handler's row. **Reuse does not cross.**
- `CompExpr::Call { ... }` in tail position. Reuse may match a preceding
  `drop` into the callee's first allocation via the cross-call rule in
  §3.7.1.

A plain `CompExpr::Let` / `Seq` / `Assign` / `AssignIndex` / `Value` /
non-effectful `Call` does **not** end a block. Reuse matching is
restricted to within a single block unless the cross-call rule applies.

#### 3.7.1 Cross-call reuse at tail position

For the specific shape "drop X of size class S in the current block, then
immediately tail-call F", F's first allocation of size class S (as
determined by inspecting F's body IR) may receive the reuse token. This is
the tail-recursive-loop fast path that makes the goal program's
iterations allocation-free. The reuse token is passed as a hidden trailing
argument to tail-called functions when, and only when, the call-site
analysis proves a size-class match.

Other inter-procedural reuse (non-tail calls, arbitrary callees) is
explicitly out of scope for M5. If it becomes necessary, it goes into a
follow-up plan.

### 3.8 IR changes and pass ordering (normative)

**New IR variants** (added in M4 to `crates/goby-core/src/ir.rs`,
`CompExpr`):

```rust
Dup { value: ValueExpr },
// Evaluates `value`, increments the heap refcount if the value is a heap
// pointer; otherwise no-op. Produces Unit.

Drop { value: ValueExpr },
// Evaluates `value`, decrements refcount; on zero calls per-type
// child-drop. Produces Unit.

DropReuse { value: ValueExpr, bind: String },
// Evaluates `value`, runs drop_reuse (§3.3). Binds the resulting token
// (i64) to `bind`. Produces Unit.

AllocReuse { token: String, size_class: SizeClass, init: AllocInit },
// Allocates via alloc_reuse(token, size_class). AllocInit mirrors the
// initializer of the replaced literal form (ListLit / TupleLit /
// RecordLit / CreateClosure).
```

**Size-class encoding.** A new `SizeClass` enum mirrors the table in §3.2.
It lives in `goby-core` so IR can carry it and the Wasm backend can read
it. (Goby ships only the Wasm backend today; "both backends" wording in
older drafts referred to a planned interpreter that did not survive
the M0 precondition gate.)

**Pass pipeline** (required ordering, asserted at pipeline boot):

```
resolved
  → shared IR lowering (ir::from_resolved)
  → closure_capture::materialize_envs
  → ownership_classify          (M4; perceus_ownership.rs)
  → return_ownership            (M10; classify_decl_return_ownership;
                                 reads ownership_classify's output)
  → drop_insert                 (M4 / M10; perceus_drop.rs;
                                 consumes return_ownership)
  → reuse_pair                  (M5; perceus_reuse.rs)
  → backend_ir::lower (Wasm)
```

`return_ownership` (M10) is inserted between `ownership_classify` and
`drop_insert` because it consumes parameter facts and produces a
per-decl return fact that `drop_insert` reads at every Call use-site
(via `classify_owned_result`). The order is enforced by
`assert_perceus_pipeline_order` at pipeline boot.

File names use the `perceus_` prefix; the exported pass functions and
types do not carry that prefix (per label hygiene).

### 3.9 Drop specialization

**Current implementation (M0–M9, runtime-tag dispatch).**
Drop is a single `__goby_drop` helper emitted once per module. Given a
tagged i64 value, it switches on the runtime tag (`TAG_LIST`,
`TAG_TUPLE`, `TAG_RECORD`, `TAG_CLOSURE`, `TAG_CHUNK`, `TAG_CELL`,
`TAG_STRING`) and walks the appropriate object structure, recursing on
owning fields and finally returning the chunk to its size-class
free-list (or, for `large`, abandoning it; see §3.2).

This means drop dispatch does **not** rely on per-decl IR type
information — DI-1 in §4.99 documents that no such information reaches
IR today. The polymorphic shape of `List a` / `(a, b)` / `record { … }`
is recovered at runtime from the chunk-element tags rather than at
compile time from inferred element types. No monomorphization.

**Future (post-PLAN_PERCEUS).** A typechecker-fed pass that reflows
inferred ADT types into IR would let `__goby_drop` specialise per
type and skip the runtime tag switch on monomorphic call sites. This
is the long-term successor referenced in §4.99 DI-1 Option C and in
M10's "Out of scope" list. Until that lands, the current tag-based
helper remains the contract.

Implementation pointer: `crates/goby-wasm/src/gen_lower/emit.rs::emit_goby_drop_function`
emits the single tag-dispatching `__goby_drop` helper. Per-tag handling
lives inline as the cases of that function's switch rather than as
separate helpers.

### 3.10 Last-use rules (normative)

Drop insertion follows the Perceus paper §3, specifically rules *Rdrop*,
*Rsub*, *Rapp*, and the branch-balancing rule for `If`/`Case`.
Implementation summary:

- A binding is *live* at a program point if some subsequent use reads it.
- An owned binding's **last use** is the final program point where it is
  live. `Drop` is inserted immediately after the last use.
- At `If` / `Case`, a binding live in one arm but not the other gets a
  `Drop` inserted at the dead arm's entry (branch balancing).
- At a non-last use, `Dup` is inserted before the use. "Non-last"
  includes every read other than the last in textual order within a
  block and every read that occurs before a branch in which the same
  binding is also used later.
- Parameters classified `owned` receive the same treatment as `let`-
  bound values. Parameters classified `borrowed` receive neither `Dup`
  nor `Drop`; the caller retains the refcount.

Closure captures materialized into env slots count as ordinary heap
pointer reads from `env_slot[i]`; the env itself is dropped by the
closure-type drop helper when the closure value is dropped.

### 3.11 What is removed

- The current bump-only allocator is demoted. `host_bump_cursor` /
  `heap_floor` survive only as "free-list miss" backing pages.
- The "allocations live forever" assumption across `emit.rs` and
  `stdlib/goby/*.gb` is removed. Any site that relied on it is rewritten.
- Because Goby is memory64-only (`doc/STATE.md` M4.6), the W32 path for
  refcount and free-list code is not emitted. W32 remains only where
  prior code still produces it, and is not extended by this plan.

---

## 4. Milestones

M0 – M8 are shipped and listed compactly below — each as a one-line
acceptance summary plus the key code locations. M9 is partially shipped
(3a + 3c landed; M9 acceptance test green); the open work is rolled
forward into M10 in §4.100. M9's verbose original prescription has been
removed: it served its purpose during implementation and is now
superseded by the design notes in §4.99 and the M10 plan in §4.100.
The git history at commits `ece2e66` (M9 opening) and `6c87777` (M9
step-by-step rewrite) preserves the long form for archaeology.

### M0 – Precondition gate (shipped)

**Acceptance:** memory64 migration complete; no `BUGS.md` entries
touching allocator paths; `doc/STATE.md` records "Perceus plan
unblocked".

### M1 – Literal hoisting and goal harness (shipped)

**Acceptance:** closed list / tuple / record literals are hoisted to a
module-init static arena with the §3.1 sentinel refcount;
`refcount_reuse_loop_example_compiles` (un-ignored 2026-04-18) green.

Code: `crates/goby-core/src/literal_hoist.rs` (closed-literal detector);
`crates/goby-wasm/src/gen_lower/emit.rs` (static arena).

### M2 – Heap object header with refcount, allocation counter (shipped)

**Acceptance:** every heap allocation prepends a refcount word; the
shared allocator (`emit_alloc_from_top`) absorbs the prefix without
changing the payload-pointer contract; existing `examples/*.gb` outputs
byte-identical; `--debug-alloc-stats` emits the frozen
`alloc-stats: …` line.

Code: `crates/goby-wasm/src/gen_lower/emit.rs::emit_alloc_from_top`,
`emit_alloc_stats_line`.

### M3 – Free lists and `drop` runtime (shipped)

**Acceptance:** static-sentinel-aware refcount adjust + per-size-class
free-list helpers; `cargo test -p goby-wasm` green;
`free_list_hits` exposed in `--debug-alloc-stats`.

Code: `emit_refcount_dup_helper`, `emit_refcount_drop_helper`,
`emit_chunk_free_list_pop` / `_push` in
`crates/goby-wasm/src/gen_lower/emit.rs`.

### M4 – Static drop insertion, effect-aware (shipped)

**Acceptance (as-shipped):** all correctness tests pass; effect-aware
last-use rules from §3.10 implemented; `multi_resume_captured_list_state_preserved_across_resumes`
green. The residency test `perceus_loop_residency` is **won't-fix**
(superseded by M3 proxy + M6 acceptance — see header).

Code: `crates/goby-core/src/perceus.rs::drop_insert_module`,
`balance_if_branch_drops`, `balance_case_branch_drops`,
`insert_owned_let_drop`.

### M4.5 – Borrow inference for hot parameters (shipped)

**Acceptance (as-shipped):** `cargo test -p goby-wasm alloc_baseline`
green within the 10% regression gate; `Borrowed` parameters skip the
caller-side `Dup` / callee-side `Drop` pair.

Code: `ownership_classify_module`, `ownership_classify_decl`,
`collect_scalar_param_evidence` in
`crates/goby-core/src/perceus.rs`. Note: ownership is decided by a
fix-point over **syntactic** scalar-usage evidence, not by IR types,
which is why M4.5 is unaffected by DI-1 (see §4.99).

### M5 – Reuse analysis (shipped)

**Acceptance:** the M1 goal program runs with `total_bytes < 200 KiB`
under `refcount_reuse_loop` and `reuse_hits == 5000`. Tail-call
cross-call reuse (§3.7.1) included.

Code: `crates/goby-core/src/perceus_reuse.rs` (`SizeEnv`,
`peek_binding_shape`, `insert_reuse_comp`,
`insert_tail_reuse_module`); IR ops `AllocReuse`, `RefCountDropReuse`,
`reuse_token`.

### M6 – `mut` lowering through reuse primitives (shipped)

**Acceptance:** `cargo test -p goby-wasm` green; the four
`mut` / `closure_mut*` example outputs byte-identical;
`refcount_reuse_loop.gb` `total_bytes < 200 KiB` and the §1.1
checksum unchanged. M6 Step 5 (stdlib `list.set` rewrite) is
**won't-fix** (see header).

Code: `lower_list_each_mutating_assign`,
`lower_supported_inline_list_fold_mutating_each`,
`lower_assign_index_reuse` in
`crates/goby-wasm/src/gen_lower/lower.rs`.

### M7 – Remove the bump-only fallback; document (shipped)

**Acceptance (2026-04-26):** no live documentation reference to "bump-only
allocator, no free" remains; goal program runs under the 200 KiB budget;
`cargo test --workspace` green; `alloc_baseline.txt` unchanged
(comment-only diff).

### M8 – Reuse and drop on real-world driver shapes (shipped)

**Acceptance:** `perceus_real_world_driver_borrow_then_update_reuses_and_frees`
green; `reuse_hits >= 200` and `total_bytes < 64 KiB` for the
borrow-then-update driver. The original strict-equality bound was
loosened to `>= 200` after M9's per-index reuse fired (DI-4).

Code: widened Owned-parameter classification through `mut ys = xs`
seeds in `crates/goby-core/src/perceus_reuse.rs`; reuse-token
propagation into the `each` callback in
`crates/goby-wasm/src/gen_lower/lower.rs::lower_list_each_mutating_assign`;
imported-Var callee consultation in
`crates/goby-core/src/perceus.rs::classify_call_args`.

### M9 – Drop on tail-recursive self-calls and HOF-laundered last-use (partial)

**Status:** acceptance test
`perceus_real_world_driver_drops_intermediates_and_reuses_per_round`
is green at commit `a4c3d7a`. The implementation that landed:

| Sub-fix | Status | Where |
|---|---|---|
| 3c TupleProject Owned propagation | shipped | `classify_return_value`, `classify_consumed_value` in `perceus.rs` |
| 3a Self-tail-call param drop | shipped | `insert_self_tail_call_param_drops_comp` in `perceus.rs` |
| 3b HOF closure-body-aware ownership | not shipped | not needed: M9 acceptance test passes without it |
| 3d `mut ys = xs` upstream-borrow widening | not shipped | conditional, never entered |

Three regressions surfaced during the rollout were diagnosed as design
fault lines and recorded in §4.99:

- DI-3 `peak_bytes` direction bug — fixed.
- DI-4 strict-equality on M8 reuse_hits — relaxed.
- DI-1 type information not reaching IR — partially mitigated; three
  M9 tests remain red and roll into M10.
- DI-2 tail-position drop demoting tail calls — flagged; M10 work item.

**Open M9 acceptance items** (rolled into M10):

1. `perceus_m9_simple_list_drop_increments_free_list_hits`
2. `perceus_m9_build_list_has_chunks_when_dropped`
3. `run_command_debug_alloc_stats_emits_stats_line_for_general_lowered_program`
4. AoC2025 day 4 part 2 (138×138 input) under `--max-memory-mb 256`
5. Step 4 fixture `crates/goby-wasm/tests/fixtures/alloc-baseline/real_world_driver.gb`
6. BUGS.md M9 entry move from "Open" to "Resolved"

The detailed fix prescription is **§4.100 (M10)**.

---

## 4.99 Design Issues Surfaced During M9 (running notes)

Running log of design fault lines exposed during M9 implementation.
Material for revisiting the ideal design before declaring M9 closed.
Each entry follows the *symptom / root cause / stopgap* triplet.

### DI-1. `IrDecl::result_ty` / `IrType` does not propagate into IR (2026-04-30)

- **Symptom:** For a `Let { value: Call }` such as `xs = build_list 5`,
  even though `build_list` returns `List Int`, `Let.ty` is left as
  `IrType::Unknown`. Perceus's `classify_owned_result` cannot mark the
  call result `Owned`, so no `Drop(xs)` is inserted at the end of `main`.
  - Failing tests: `perceus_m9_simple_list_drop_increments_free_list_hits`,
    `perceus_m9_build_list_has_chunks_when_dropped`,
    `run_command_debug_alloc_stats_emits_stats_line_for_general_lowered_program`.
  - All three fail with `free_list_hits=0`.
- **Root cause:** `crates/goby-core/src/ir_lower.rs::lower_resolved_declaration`
  hard-codes `result_ty: IrType::Unknown`. `infer_type_from_expr` only
  classifies scalar literals. `typecheck_annotation.rs` exposes only
  `find_can_keyword_index`; there is no API that turns the annotation
  into a structured type. → At the IR level there is no way to know
  whether a given `Call` returns a heap value.
- **Stopgap in a4c3d7a:** Widened `classify_owned_result` to
  `CompExpr::Call { .. } => Owned` regardless of type. This caused
  scalar-returning calls (e.g. `walk`'s `Int`) to be treated as `Owned`,
  emitting `RefCountDrop` on non-heap locals and breaking loop lowering
  / tail-call detection (graphemes regression / walk → `DeclCall`
  regression / `peak_bytes=0`).
- **2026-04-30 follow-up:** Introduced `type_is_known_heap`, which
  treats `IrType::Unknown` as `false`, rolling back to behaviour
  equivalent to pre-M9 (call results are conservatively Borrowed).
  Walk / graphemes are restored, but Owned classification of
  heap-typed call results is still missing.
- **Design options:**
  - **A.** Lightweight parser for `type_annotation` to populate
    `IrDecl::result_ty`. Hand a callee-name → return-type map to
    Perceus. Medium-sized change.
  - **B.** Walk decl bodies syntactically and infer "heap-returning"
    from the tail expression. Reuse `value_is_fresh_heap`; on mutual
    recursion either fix-point or fall back to `Unknown`.
    Annotation-independent. Medium-sized change.
  - **C.** Add a pass that reflows the typecheck inference results
    into IR. Heaviest, but enables fully precise reuse / drop in the
    future.

### DI-2. Tail-position drop insertion can demote tail-call optimization (2026-04-30)

- **Symptom:** `insert_drop_at_tail` either prepends `Drop(name)` to a
  `Call` in tail position, or — when `name` appears in the call's
  arguments — wraps the call as `let tmp = call in Drop; tmp`,
  **destroying its tail-call status.**
  → A self-recursive call sitting in tail position can be demoted from
  `TailDeclCall` to `DeclCall`.
- **Current mitigation:** `insert_drop_at_tail` only fires for `Owned`
  bindings, so scalar-only functions are unaffected. But "heap-returning
  tail-recursive function with an Owned parameter" still triggers the
  demotion.
- **Design discussion:** "Insert Drop before a tail call" is the
  TRMC-with-Perceus territory in the original paper. Goby lowers tail
  calls to wasm `return_call`, so Drop insertion must stay strictly
  *before* the `TailDeclCall` (i.e. run in the caller frame); wrapping
  the call itself is not an option.

### DI-3. `peak_bytes` update direction bug (2026-04-30, fixed)

- **Symptom:** `reuse_falls_through_when_shared` and friends report
  `peak_bytes=0` permanently.
- **Root cause:** The peak-update tail in `emit.rs` used `i64.gt_u`,
  i.e. "update when peak > live" — the inverse of the intended
  monotone-increasing rule.
- **Fix:** Changed to `i64.lt_u`. Peak is now monotone non-decreasing.

### DI-4. M9 fix overshot the M8 acceptance bound (2026-04-30)

- **Symptom:** `perceus_real_world_driver_borrow_then_update_reuses_and_frees`
  reports `reuse_hits=10000` (test expected `== 200`).
- **Meaning:** M8 only expected per-call `mut ys = xs` seed reuse, but
  M9 (3a + 3c) made per-index `AssignIndex` reuse fire (200 calls × 50
  idx = 10000). This is an improvement.
- **Fix:** Loosened the assertion to `>= 200` and kept it as M8's
  trivial lower bound.

### DI-5. M9 plan vs. implementation diff (2026-04-30)

Mapping between the M9 Steps / fixes defined in §M9 and the
implementation as of a4c3d7a:

| Step / fix | Status | Notes |
|---|---|---|
| Step 0 baseline | done | numbers captured at the time |
| Step 1.1 acceptance test | done | `perceus_real_world_driver_drops_intermediates_and_reuses_per_round` added |
| Step 1.2 IR dump | done | gated on `GOBY_DUMP_PERCEUS_IR` env var |
| 3c TupleProject Owned propagation | done | `classify_return_value` / `classify_consumed_value` |
| 3a Self-tail-call param drop | done | `insert_self_tail_call_param_drops_comp` |
| 3b HOF closure-body-aware ownership | **not implemented** | no `list_hof_closure_signature` / `lambda_consumes_first_param`. M9 acceptance test passes with 3a + 3c alone, so necessity is open |
| 3d `mut ys = xs` upstream-borrow widening | not started (conditional) |
| Step 4 drop insertion audit | partially done | acceptance test passes; fixture (`real_world_driver.gb`) not added |
| Step 5 re-acceptance | **3 fail** | see DI-1: `perceus_m9_simple_list_drop_increments_free_list_hits`, `perceus_m9_build_list_has_chunks_when_dropped`, `run_command_debug_alloc_stats_emits_stats_line_for_general_lowered_program` |
| Step 7 STATE / PLAN / BUGS sync | **not started** |

(Per-step plan / code reviews from a second pair of eyes — whether
human or AI — are part of the surrounding development workflow and
are not tracked as separate plan steps. The original M9 plan listed
them as Step 2 / Step 6; those rows are dropped from this
retrospective.)

**Out-of-plan additions in a4c3d7a:**
- Widening `classify_owned_result` to `CompExpr::Call { .. } => Owned`
  (now gated by `type_is_known_heap` as a DI-1 stopgap). Intended to
  cover cases that 3c's TupleProject extension alone cannot, but ended
  up exposing DI-1 / DI-2.
- New `insert_drop_at_tail` (replacing `append_drop_preserving_result`'s
  simple let-tmp wrap). Goal: ensure the drop also fires through tail
  calls. Carries the DI-2 concern.
- Real `peak_bytes` update logic in `emit.rs` (replacing the
  total → peak placeholder copy). The direction bug was DI-3, now
  fixed.
- Trailing tail Drop in `emit.rs::emit_list_concat_helper`. Initially
  suspected to cause the graphemes regression, but the actual culprit
  was DI-1's `Call=Owned` widening; the tail Drop itself is a sound
  M9 change (concat consumes the tail).

### DI-6. `doc/PLAN.md` and `doc/STATE.md` are stale relative to PLAN_PERCEUS (2026-04-30)

- **Symptom:** `doc/STATE.md` header still reads
  "Perceus M0–M8 complete. §1 goal satisfied." (line 7) and
  `doc/PLAN.md` §4.2 does not mention M9 partial / M10 reopened.
- **Why it matters:** the project guidance in `AGENTS.md` requires
  roadmap and state docs to stay aligned when core terminology
  changes; the mismatch makes restart context misleading for any
  future agent or contributor.
- **Resolution:** rolled into M10 Step 4 (§4.100). One-pass sync
  along with the standard close-time updates. No action required
  in PLAN_PERCEUS proper — this entry exists so the misalignment
  is visible until M10 Step 4 lands.

---

## 4.99.A. Regression / fault-line audit (2026-04-30)

After fixing DI-1 through DI-4, the following angles were checked for
remaining design hazards:

1. **Does borrow inference work without type information?**
   `ownership_classify_module` runs a fix-point that decides Owned /
   Borrowed for each parameter, and `collect_scalar_param_evidence`
   re-marks parameters as Borrowed based on **syntactic scalar usage**
   (BinOp, comparison, etc). → Type-independent; unaffected by DI-1.
2. **Can `drop_insert_params` mistakenly drop scalar parameters?**
   No: it only targets `Owned`. After the DI-1 follow-up, scalars are
   classified Borrowed, so no drop is emitted for scalar params.
3. **Do `reuse_token` / `SizeEnv` propagation make any type
   assumption?** `peek_binding_shape` / `value_alloc_size_or_cell`
   only treat `ListLit` / `TupleLit` / `RecordLit` / `Interp` as reuse
   candidates (Call is `None` from the start). Type-independent.
4. **Are all `alloc_baseline.txt` fixtures still within budget?**
   `cargo test --workspace alloc_baseline` is green. DI-3's switch to
   monotone peak does not affect baselines because they are
   `total_bytes`-based.
5. **Effect handler tests (`reuse_not_across_perform_effect` /
   `reuse_not_across_with_handler`)** — both green. 3a's
   self-tail-call drop only targets `Owned` parameters and is safe
   across effect-induced tail calls.

**2026-04-30 recheck:** the DI-1/DI-2 focused failures are fixed, but the
full acceptance shape is still red. A stdin-based 138x138 driver fails with
`E-MEMORY-EXHAUSTION` under `--max-memory-mb 256` and at the default 1 GiB
ceiling; a reduced `list.map lines graphemes` shape over 138 lines also
fails. Follow-up investigation localized that failure to host-bump allocations
inside `goby:runtime/track-e` imports, not to Perceus IR Drop placement.
M10 now keeps the remaining IR ownership boundary; the runtime allocator fix is
M11 (§4.101).

---

## 4.100. M10 — Let/LetMut-safe return-field ownership

M10 is re-scoped after the 2026-04-30 root-cause localization in
`doc/STATE.md`: the remaining Perceus IR work is **not** the 138x138 memory
exhaustion. That failure is caused by host-bump allocations below the IR layer
and moves to M11 (§4.101).

The active M10 closure path is the smaller, independent ownership fix exposed
by the reverted naive `GlobalRef` promotion. `local.field` is currently lowered
as `ValueExpr::GlobalRef { module: local_name, name: field_name }`, which
conflates a true module-path function reference with a local-shadowed record
field projection. M10 closes when that shape can transfer ownership from an
immutable `Let` parent without transferring from a `LetMut` parent.

### Scope

In scope:

1. Teach `return_ownership_value` and the matching call-result boundary to
   distinguish:
   - immutable `Let` parents, where field-projection ownership transfer is
     sound when the parent is `Owned`;
   - `LetMut` parents, where projection transfer is unsafe because the cell may
     be re-assigned after the read; and
   - true module-path `GlobalRef`s, which remain conservatively `Borrowed`.
2. Enable the ignored forward-looking unit test
   `return_ownership_local_shadowed_global_ref_with_owned_local_is_owned`.
3. Keep the already-green safety guards green:
   `return_ownership_local_shadowed_global_ref_with_borrowed_local_is_borrowed`,
   `return_ownership_global_ref_without_local_shadow_remains_borrowed`, and
   `recursive_multi_part_interpolated_print_after_graphemes_executes`.
4. Preserve the DI-2 tail-call-safe drop fix that has already landed.

Out of scope:

- Host-intrinsic allocator unification and the 138x138 stdin driver memory
  exhaustion. That is M11 (§4.101), because `__goby_drop` cannot free values
  allocated in the host bump arena.
- Treating `list.map lines graphemes` as an M10 closure gate. The hot leak is
  host-bump allocation in `read_lines` / string intrinsics, not only Perceus
  Drop placement.
- Reuse across `perform` / `with_handler` boundaries.
- Reuse on `set xs i v` (M6 Step 5 won't-fix conditions still stand).
- Inter-procedural reuse beyond the §3.7.1 tail-call rule.

### Active design

The fix is binding-kind-aware ownership propagation for `GlobalRef` values that
shadow a local binding.

Implementation direction:

- Extend the return-ownership environment from "name -> `OwnershipClass`" to
  carry the binding kind needed for projection transfer:
  `name -> { ownership: OwnershipClass, projection_parent: ImmutableLet |
  MutableLet | ParamOrUnknown }`.
- When walking `CompExpr::Let`, bind the local as `ImmutableLet`; when walking
  `CompExpr::LetMut`, bind it as `MutableLet`. Parameters and unresolved names
  stay `ParamOrUnknown`.
- In `return_ownership_value`, handle
  `ValueExpr::GlobalRef { module, name: _ }` as:
  - if `module` shadows an `ImmutableLet` local and that local is `Owned`, return
    `Owned`;
  - if `module` shadows a `MutableLet` local, return `Borrowed`;
  - if `module` shadows a `Borrowed` local, return `Borrowed`;
  - otherwise treat it as a true module-path reference and return `Borrowed`.
- Apply the same rule anywhere `classify_call_result_ownership` or its helper
  consumes return-ownership facts for a projected field result.

This is intentionally narrower than the reverted promotion. The reverted rule
"`GlobalRef.module` exists in the local ownership env, so promote from
`env[module]`" was unsound because it promoted `LetMut` parents inside
`graphemes`, freeing per-grapheme strings while the mutable cell was still being
re-assigned.

### Active M10 acceptance

Hard gates:

- [x] `return_ownership_local_shadowed_global_ref_with_owned_local_is_owned`
      is un-ignored and green.
- [x] The two conservative `GlobalRef` tests stay green:
      `return_ownership_local_shadowed_global_ref_with_borrowed_local_is_borrowed`
      and `return_ownership_global_ref_without_local_shadow_remains_borrowed`.
- [x] `recursive_multi_part_interpolated_print_after_graphemes_executes` stays
      green; `debug` must not regress to `debtg`.
- [x] DI-2 tail-position drop demotion remains eliminated.
- [x] `cargo test --release -p goby-core perceus` and the focused
      `goby-wasm` regression set for graphemes / split / walk / rr3 are green.

Historical note: the detailed DI-1 decl-return-ownership draft below was the
pre-root-cause M10 plan. It is retained as implementation context, but it is no
longer the active closure gate for M10 unless a future change explicitly
revalidates it against the M10/M11 split.

### Historical design decision: DI-1 fix

Three options were recorded in §4.99 DI-1. The pre-root-cause M10 draft picked
**Option B** (syntactic decl-body inspection) and rejected A and C for the
following reasons. This is no longer the active M10 closure path after the
M10/M11 split above.

- **Option A** (parse `type_annotation`) is brittle: Goby's type
  annotation grammar allows `(A -> B) -> C`, parametric type
  variables, and `can <Effects>` clauses. A regex-style parser will
  silently miscount `->` arrows in higher-order signatures, producing
  unsound `Owned` classifications. Building a proper parser
  duplicates `typecheck_annotation.rs`.
- **Option C** (reflow typecheck inferences) is the right long-term
  answer but lands too much surface area (typecheck → IR data flow,
  closure capture types, lambda inference) for a milestone whose
  job is closing M9. Tracked as a future plan, not M10.
- **Option B** is bounded: it adds a per-decl **`return_ownership`**
  fact (`Owned` / `Borrowed`), computed by walking the decl body's
  tail position. It reuses `value_is_fresh_heap` for the fresh-heap
  case. Failure mode: any unproven shape stays `Borrowed`, which is
  the pre-M9 behaviour and known to be sound.

The fact is **ownership of the returned reference**, not "shape of the
return value". A function can return a heap pointer that aliases an
input parameter (for example `passthrough xs = xs`). That result is
`Owned` only when the parameter's entry classification is `Owned`;
the call transfers ownership into the callee and the callee transfers
the same reference back to the caller. It is `Borrowed` when the
source binding is `Borrowed`. The earlier draft of this section
called the fact `returns_heap: bool`; that name is rejected because
it invites the exact widening that caused DI-1 (heap-shaped does not
imply owned). The data type is `OwnershipClass` (already defined in
`perceus.rs`); the field name is `return_ownership`.

### Step 0 — Sanity baseline

```sh
cargo test --workspace --release --no-fail-fast 2>&1 | tee tmp/m10_step0.log
```

Expected red set: exactly the three M9 acceptance items listed above.
Anything else is a regression in the pre-M10 fixes; investigate before
proceeding.

### Step 1 — Decl-return ownership inference (DI-1)

File: `crates/goby-core/src/perceus.rs` (or a new sibling
`perceus_returns.rs` if it grows beyond ~80 LOC).

(a) Add a module-level analysis pass:

```rust
/// Per-decl ownership of the returned reference. `Owned` only when
/// every tail-position value in the body is provably a fresh
/// heap allocation (or a call to another `Owned` decl) **and** does
/// not alias an inbound parameter or any other `Borrowed` source.
/// Default and conservative fallback: `Borrowed`.
pub fn classify_decl_return_ownership(
    module: &IrModule,
) -> HashMap<String, OwnershipClass>;
```

The pass walks each decl's body, threading a **local return
environment** `Γ : Var → OwnershipClass`. Initial Γ binds each
parameter to its **already-classified** ownership from
`ownership_classify_module` (the M4.5 fix-point), **not** to
`Borrowed`. This is the rule that lets `build`'s base-case
`acc` propagate to `Owned` when callers pass owned arguments to
`build`.

The walker has one entry point that classifies a tail-position
`CompExpr` and one helper that classifies a `ValueExpr` against
Γ. The transfer rules:

- `CompExpr::Let { name, value, body }`:
    1. Classify `value` against Γ to obtain `c_value`.
       (Use the same conservative rules used at decl level —
       `Call` to an `Owned` decl ⇒ `Owned`; `ListLit` /
       `TupleLit` / `RecordLit` / `Interp` with no spread of a
       Borrowed Var ⇒ `Owned`; everything else ⇒ `Borrowed`.)
    2. Recurse into `body` under `Γ' = Γ[name ↦ c_value]`.
       The result of the recursion is the result for the whole
       `Let`. **Shadowing rule:** if `name` collides with an
       existing binder in Γ, the inner binding wins for the
       lifetime of `body`; restore the outer binding when the
       walker leaves the `Let` node.
- `CompExpr::LetMut { name, value, body }` — same as `Let`. The
  mut-ness only matters for reuse analysis; ownership of the
  *return* is independent.
- `CompExpr::Seq { stmts: _, tail }` → recurse into `tail` under
  unchanged Γ. Statements before the tail cannot affect the
  return's ownership.
- `CompExpr::If { cond: _, then_, else_ }` / `Case { arms }` →
  classify each branch under Γ; result is `Owned` iff **every**
  branch produces `Owned`. Any branch returning `Borrowed`
  collapses the whole expression to `Borrowed`.
- `CompExpr::Value(value)` → classify `value` against Γ.
- `CompExpr::Call { callee, args: _ }` →
    - If `callee == GlobalRef|Var(name)` and `name` resolves to a
      module decl, look up `name` in the in-progress map (see
      fix-point below).
    - Otherwise (closure call, lambda, intrinsic with no recorded
      ownership) ⇒ `Borrowed` conservative.
- All other comp variants (`Drop`, `DropReuse`, `Dup`, `Assign`,
  `AssignIndex`, `AllocReuse`, `PerformEffect`, `Resume`, `Handle`,
  `WithHandler`) → `Borrowed`. They never produce a fresh owned
  return at the IR level.

For a `ValueExpr` against Γ:

- `ValueExpr::Var(name)` → `Γ[name]` if present; else `Borrowed`.
  Parameter ownership in Γ comes from M4.5 (`ownership_classify_module`),
  not from this pass. So `passthrough xs = xs` follows Γ exactly:
  it is `Owned` when M4.5 classifies `xs` as `Owned`, and `Borrowed`
  when Γ marks `xs` as `Borrowed`. `build`'s base case `acc`
  classifies `Owned` because M4.5 marks `acc` as Owned (consumed by
  the recursive call's spread).
- `ValueExpr::ListLit { spread: None, .. }` → `Owned` (fresh).
- `ValueExpr::ListLit { spread: Some(s), .. }` → `Borrowed` if
  `s` resolves (via Γ) to `Borrowed`; otherwise `Owned`. A spread
  of a fresh literal stays fresh; a spread of a parameter does not.
- `ValueExpr::TupleLit(items)` → `Owned` if **at least one** item
  is `Owned`; the projection rule from §M9 3c is what eventually
  exposes the heap part. Otherwise `Borrowed`.
- `ValueExpr::RecordLit { fields, .. }` → same as `TupleLit`.
- `ValueExpr::Interp(_)` → `Owned` (fresh string).
- `ValueExpr::TupleProject { tuple, .. }` → classify `tuple`
  against Γ. (3c already widens projection at the use site;
  this rule lets a decl that returns `t.0` of a fresh tuple
  classify as `Owned`.)
- `ValueExpr::IntLit / BoolLit / StrLit / Unit / GlobalRef` →
  `Borrowed`.
- `ValueExpr::BinOp { .. }` / `ListGet { .. }` → `Borrowed`.
- `ValueExpr::Lambda { .. }` → `Owned` (fresh closure object).

(b) **Fix-point.** Use a greatest-fix-point style iteration for
return ownership:

1. Initialise every decl's `return_ownership` to `Owned`.
2. Iterate using **simultaneous recomputation**: snapshot the previous
   map, classify every decl against the snapshot, install the
   resulting map atomically.
3. Stop when the map stabilises.

This is intentionally different from the earlier LFP draft
(`Borrowed` initial state, `Owned` monotone growth). LFP cannot prove
`Owned` for accumulator-building recursive helpers such as `build`:
the recursive call starts as `Borrowed`, causing the branch join to
collapse to `Borrowed` and pin there. The GFP form treats `Owned` as
the hypothesis and removes it when a tail-position path is
structurally `Borrowed` (`ListGet`, scalar `BinOp`, `Var` whose local
environment says `Borrowed`, or a call to a Borrowed-confirmed
sibling).

**SCCs and recursive cycles.** A cycle of decls that only calls each
other in tail position and has no returning base case may remain
`Owned` under the GFP. This is acceptable for M10 because such a
cycle never returns at runtime, so the classification has no
observable drop-safety effect. Any cycle with a reachable
tail-position `Borrowed` source still collapses to `Borrowed`.

(c) Pass the resulting `HashMap<String, OwnershipClass>` through
`run_perceus_passes` into `classify_owned_result` and `classify_comp`.
Threading is local — mirrors the existing `module_params` parameter.

(d) Replace the current call-result branch in `classify_owned_result`:

```rust
// Before (DI-1 stopgap):
CompExpr::Call { .. } if type_is_known_heap(ty) => OwnershipClass::Owned,

// After (M10):
CompExpr::Call { callee, .. } => match callee_decl_return_class(callee, module_returns) {
    Some(OwnershipClass::Owned) => OwnershipClass::Owned,
    _ => OwnershipClass::Borrowed,
},
```

(e) Drop `type_is_known_heap` and the `_ty: &IrType` parameter from
`classify_owned_result`'s signature once (d) is in place. The
threading was a stopgap; replace it cleanly.

#### Soundness gate

Add unit tests in `crates/goby-core/src/perceus.rs` (the existing
`#[cfg(test)] mod tests`) covering each of the following shapes:

```gb
-- (1) Passthrough of an Owned-classified parameter returns Owned.
-- M4.5 may classify `xs` as Owned at the callee boundary because the
-- caller transfers ownership into the call; returning the same reference
-- transfers that ownership back to the caller.
passthrough : List Int -> List Int
passthrough xs = xs
-- Expected: passthrough → Owned when Γ[xs] = Owned.

-- (2) Branch-mixed projection: Owned branch + Borrowed branch ⇒ Borrowed.
-- The else branch returns a fresh ListLit (Owned); the then branch returns
-- a projected child list from `xss`, which is Borrowed. Branches must agree.
maybe_project : List (List Int) -> Bool -> List Int
maybe_project xss flag =
  if flag
    list.get xss 0
  else
    [0]
-- Expected: maybe_project → Borrowed.

-- (3) Fresh literal ⇒ Owned.
fresh : Unit -> List Int
fresh _ = [1, 2, 3]
-- Expected: fresh → Owned.

-- (4) Tuple of (Owned-payload, scalar) ⇒ Owned (3c projection handles use site).
fresh_tuple : Unit -> (List Int, Int)
fresh_tuple _ = ([1], 0)
-- Expected: fresh_tuple → Owned.

-- (5) Recursive heap-returning function with `acc` consumed at every call.
-- M4.5 classifies `acc` as Owned (it is consumed by the spread `[n, ..acc]`
-- in the recursive call's argument). Body walk: base case returns Var(acc)
-- ⇒ Γ[acc] = Owned ⇒ Owned. Recursive call returns Call to `build` itself,
-- which the fix-point resolves as Owned in the next iteration. Both
-- branches Owned ⇒ Owned.
build : Int -> List Int -> List Int
build n acc =
  if n == 0
    acc
  else
    build (n - 1) [n, ..acc]
-- Expected: build → Owned. (This is the §1.1 goal program's `build`.)

-- (6) Fresh-base recursive function ⇒ Owned.
mk : Int -> List Int
mk n =
  if n == 0
    []
  else
    [n, ..mk (n - 1)]
-- Expected: mk → Owned. Both branches yield ListLit (fresh), even though
-- the spread element is a recursive call.

-- (7) Mutual recursion with no returning base case may remain Owned.
left : List Int -> List Int
left xs = right xs
right : List Int -> List Int
right xs = left xs
-- Expected: left → Owned, right → Owned under the GFP when Γ[xs] = Owned.
-- The cycle has no observable returning path; this is acceptable for M10.
```

**Important dependency.** This pass *consumes* the M4.5 parameter
ownership map produced by `ownership_classify_module`. The M4.5
classifier is the source of truth for whether a parameter binding
counts as `Owned` or `Borrowed` at function entry. The new
return-ownership pass runs **after** M4.5 in the pipeline; do not
re-derive parameter ownership locally.

Each shape gets a focused test that runs `classify_decl_return_ownership`
on the IR and asserts the expected `OwnershipClass`. The test names
should encode the *why* (`return_ownership_passthrough_owned_param_is_owned`,
`return_ownership_branch_with_projected_borrowed_arm_collapses_to_borrowed`,
etc).

#### Verification

```sh
cargo test --release -p goby-core perceus
cargo test --release -p goby-wasm \
  perceus_m9_simple_list_drop_increments_free_list_hits \
  perceus_m9_build_list_has_chunks_when_dropped \
  perceus_real_world_driver_drops_intermediates_and_reuses_per_round \
  refcount_reuse_loop_owned_param_seed_reuses_assign_index \
  perceus_real_world_driver_borrow_then_update_reuses_and_frees \
  compile_module_scan_loop_lowering_eliminates_walk_self_call_in_wasm \
  rr3_non_tail_recursive_scan_repro_survives_tight_stack_limit_after_loop_lowering \
  rr3_callback_assisted_scan_repro_survives_tight_stack_limit_on_same_boundary \
  recursive_multi_part_interpolated_print_after_graphemes_executes \
  wb3_m7_read_split_map_graphemes_get_each_canonical_variant_executes \
  wb3_m7_read_split_map_graphemes_get_each_alias_variant_executes
cargo test --workspace --release alloc_baseline
```

The first three M9 tests must flip green; the remaining tests must
stay green (regression guard for DI-1's previous over-widening).

### Step 1c — Conditional `list.map` ownership and projection-borrow liveness

Step 1a/1b resolves user-decl return ownership but leaves stdlib
wrappers whose bodies end in intrinsics such as `__goby_list_map`
conservative unless the intrinsic return can be proven owned at the
call site. `list.map` is the first required case, and it is
conditional: the outer list is fresh, but dropping it also drops its
elements, so the result is safe to classify as `Owned` only when the
callback returns owned elements.

The rejected simpler rule is "seed `__goby_list_map` as always
Owned". That makes the debug-alloc-stats integration test pass, but
it also breaks callback shapes whose result aliases input-owned data:

```gb
rolls = list.map lines graphemes
row2  = list.get rolls 2
list.each row2 println
```

In that shape, `graphemes` is not currently proven to return owned
elements. Treating the `map` result as unconditionally owned lets the
drop pass free data that the projected child path may still observe.

For callback-owned shapes such as:

```gb
rendered = list.map nums (fn n -> "${n + 1}")
```

the callback body is a fresh string interpolation, so the `map` result
can be classified `Owned` and dropped by the caller.

Once a `map` result is classified `Owned`, `ListGet` still creates the
next ownership boundary. `row2 = list.get rolls 2` creates a projected
reference into the parent. A later `list.each row2 println` still uses
the projected child, so `Drop(rolls)` must not be inserted immediately
after the `ListGet` merely because `rolls` itself has no later direct
textual use.

The historical draft treated Step 1c as part of the DI-1 closure, not as an
optional follow-up. Under the current M10/M11 split, this material is retained
only as context for any future decl-return-ownership work.

Historical design direction:

- Add an explicit, conservative model for **projection borrows**:
  `ListGet(parent, index)` may create a child reference whose lifetime
  depends on `parent`.
- Extend last-use/drop placement so a parent binding remains live while
  any binding derived from it by projection remains live.
- Keep the model conservative. It is acceptable to delay a parent drop
  until after the projected child is last used; it is not acceptable to
  free the parent before that child use.
- Do not seed `__goby_list_map` unconditionally. Instead, classify a
  `list.map` / `__goby_list_map` call result as `Owned` only when the
  visible callback argument is known to return `Owned` values
  (for example, a lambda whose body is a fresh interpolation).
- Keep the intrinsic registry for unconditional heap-returning
  intrinsics only. Add entries one at a time with regression tests.

Alternative design noted but not selected for M10: model `ListGet` as
an ownership transfer with a matching `Dup` on the source. That may
become useful later, but it is more invasive than the projection-borrow
liveness rule and is not required to restore the current acceptance
path.

Required regression test shape:

- A program equivalent to the `rolls` / `row2` / `list.each row2`
  pipeline above must continue to execute.
- A callback-owned `list.map` result must be dropped late enough that a
  `ListGet`-derived child is still valid at its last use.
- `run_command_debug_alloc_stats_emits_stats_line_for_general_lowered_program`
  must pass without weakening its allocation-stat assertion.
- The graphemes / split / walk / rr3 regression set from Step 1 must
  remain green.

### Step 2 — Tail-call-safe drop placement (DI-2)

File: `crates/goby-core/src/perceus.rs::insert_drop_at_tail`.

The current `insert_drop_at_tail` has three relevant cases for a
`Call` in tail position; each must be handled explicitly:

| Case | Where `name` appears | Current behaviour | M10 behaviour |
|---|---|---|---|
| C1 | nowhere | prepend `Drop(name)`; tail-call preserved | **keep** |
| C2 | in `args[i]` | wrap as `let tmp = call in Drop; tmp` (demotes tail call) | **replace** with `Dup(name); Drop(name); TailDeclCall …` |
| C3 | as `callee` (indirect/closure call) | wrap as `let tmp = call in Drop; tmp` (demotes tail call) | **conservative**: keep the wrap; do not attempt to preserve tail-call status |

Rationale for the per-case decisions:

- **C1** is sound today and remains so. No change.
- **C2** is the case that demoted `walk` and the M9 tail-recursive
  helpers (DI-2). The fix:

  ```
  Dup(name)             ; bump rc so args[i] = Var(name) sees rc > 0
  Drop(name)            ; release the local slot's reference
  TailDeclCall …        ; return_call, never returns to caller
  ```

  Because `return_call` does not return to the caller frame, the
  Drop before the call is the only opportunity to free `name` if its
  rc reaches zero — but the `Dup` bumps rc to make sure the arg-side
  reference survives the slot's drop.

  **Equivalence check.** `Dup(name); Drop(name)` is a no-op on rc
  when the slot was the only owner (rc 1 → 2 → 1) and matches the
  original "release the slot, args still own a copy" semantics for
  rc > 1.

- **C3** (`name` is the callee expression itself, i.e. a Var bound
  to a `Closure` or `Func` value) is **deliberately not** preserved
  as a tail call. Reasons:

    1. Indirect tail calls (`return_call_indirect`) are not yet
       wired through `gen_lower::backend_ir::WasmBackendInstr`;
       Goby's `TailDeclCall` only supports direct calls (the
       `decl_name` field).
    2. `Dup(name); Drop(name); IndirectCall` would be sound for rc,
       but the call itself has no tail-call form, so we'd still
       demote.
    3. Closure-typed self-recursion is rare in idiomatic Goby
       programs (the recursive helpers in BUGS.md M9 all use
       module-level decl names as callees).

  Action: keep the existing let-tmp wrap for C3. Add an explicit
  comment in `insert_drop_at_tail` saying "C3 is not preserved
  because indirect tail calls are not implemented; this is intentional
  per PLAN_PERCEUS §4.100 Step 2." If a future workload demands it,
  re-open under a separate milestone with `return_call_indirect` as
  a prerequisite.

(a) Implement the C1 / C2 / C3 split in `insert_drop_at_tail`. The
    callee-vs-arg classification is local to the `Call` arm and only
    needs `value_mentions_name(callee, name)` plus the existing
    `args.iter().any(...)` check.

(b) Add unit tests in `perceus.rs`:
    - C2 regression: a tail-recursive function with an `Owned` param
      mentioned in its self-call args lowers to `TailDeclCall`, not
      `DeclCall`. (Mirrors the `walk` regression that surfaced DI-2.)
    - C3 sanity: a closure-call shape stays in the let-tmp form and
      does **not** lower to `TailDeclCall`. (Documents the deliberate
      conservative behaviour.)

(c) Verification:

```sh
cargo test --release -p goby-wasm \
  compile_module_scan_loop_lowering_eliminates_walk_self_call_in_wasm \
  perceus_real_world_driver_drops_intermediates_and_reuses_per_round \
  refcount_reuse_loop_owned_param_seed_reuses_assign_index
```

Status 2026-04-30: complete. `insert_drop_at_tail` now preserves the C2
direct `Var` callee tail shape with `Dup(name); Drop(name); Call(...)`,
keeps `GlobalRef` calls, runtime intrinsics, and C3 indirect/closure calls in
the temp-wrap form, and has focused unit tests for those cases. The Step 2
verification commands above are green.

All must remain green; the new unit tests must pass.

### Step 3 — Acceptance fixture (and optional AoC evidence)

(a) **Required.** Add
    `crates/goby-wasm/tests/fixtures/alloc-baseline/real_world_driver.gb`
    derived from the BUGS.md M9 repro:

    - Start from the BUGS.md "Open bugs" 2026-04-28 entry's program.
    - Replace `read_lines ()` with a hard-coded six-row 10-character
      grid literal so the alloc-baseline harness can run with empty
      stdin (the harness invokes the test with no piped stdin).
    - Keep the rest of the program byte-for-byte identical to BUGS.md
      so the fixture exercises the same Perceus pathways.
    - Append the post-fix `total_bytes` figure to
      `crates/goby-wasm/tests/alloc_baseline.txt` as the ceiling.

    In the historical draft this fixture was the **reproducible** M10 closure
    gate. Under the current split, the 138x138 stdin-shaped runtime failure is
    an M11 allocator gate.

(b) **Optional, evidence only.** If the contributor has access to the
    AoC2025 day 4 part 2 input locally:

    ```sh
    goby run --max-memory-mb 256 --debug-alloc-stats solve2.gb < input
    ```

    Capture `total_bytes` / `peak_bytes` / `reuse_hits` in
    `tmp/m10_aoc_acceptance.txt` and quote the numbers in the closure
    commit message. The AoC input is not redistributable and not in
    the repo, so this run is supporting evidence rather than a CI
    gate.

(c) Verification:

```sh
cargo test --workspace --release alloc_baseline
```

`refcount_reuse_loop.gb`'s row in `alloc_baseline.txt` must be
unchanged. The new `real_world_driver.gb` row must pass.

Per-step plan / code reviews from a second pair of eyes (human or
AI) are part of the surrounding development workflow and are
intentionally **not** called out as separate Steps here — they are
expected at every step, not specific to M10.

### Step 4 — STATE / PLAN / BUGS sync, commit

- [ ] Move the BUGS.md M9 entry from "Open" to "Resolved on
      YYYY-MM-DD" with a one-paragraph fix summary listing
      DI-1 (Option B), DI-2, and the historical M10 acceptance items.
- [ ] Update `doc/STATE.md` and `doc/PLAN.md` §4.2 to record M10
      outcome and re-close Perceus. **Note:** these two docs are
      already stale relative to PLAN_PERCEUS today (they still say
      "M0–M8 complete", missing M9 partial and M10). This Step 4
      sync should bring them current in one pass — see §4.99 DI-6.
- [ ] Update `memory/project_perceus_status.md` if present.
- [ ] Before the final commit, run the repository quality gate from
      `AGENTS.md` ("Minimal Quality Gate"): `cargo fmt`, `cargo check`,
      and `cargo test --workspace --release`. In addition, hand-check
      the "Always-Global Invariants" listed in `AGENTS.md`: any
      syntax / semantics change must be reflected in
      `doc/LANGUAGE_SPEC.md`, `doc/PLAN.md`, and the relevant
      `examples/*.gb`.
- [ ] Commit each step as its own commit; final commit message
      includes the alloc-baseline numbers from Step 3(c).

### Historical acceptance for the pre-M11 M10 draft

This checklist belonged to the pre-root-cause draft that treated the 138x138
driver as an M10 gate. After the 2026-04-30 localization, these items are
split: the Let/LetMut `GlobalRef` boundary stays in M10, and host-intrinsic
allocator unification moves to M11 (§4.101).

Historical hard gates:

- [ ] All M9 open items in §4 ("Open M9 acceptance items") closed.
- [ ] DI-1 stopgap (`type_is_known_heap`) deleted; replaced by
      decl-return ownership inference.
- [x] DI-2 demotion eliminated; new unit test green.
- [ ] `cargo test --workspace --release` green with no
      `--no-fail-fast` needed.
- [ ] Project invariants check clean (see Step 4).
- [ ] `crates/goby-wasm/tests/fixtures/alloc-baseline/real_world_driver.gb`
      added with a recorded `total_bytes` ceiling in `alloc_baseline.txt`.
      This is the **reproducible** acceptance bench; it exercises the
      same shape as the AoC2025 program.
- [ ] A stdin-based 138×138 real-world driver regression is automated and
      passes under `--max-memory-mb 256`.

Supporting evidence:

- AoC2025 day 4 part 2 (138×138) under `--max-memory-mb 256`. The
  exact AoC input is not in the repository, but the generated/read-lines
  138×138 shape must be CI-checkable before using private input as supporting
  evidence.

### Invariants the fix must preserve

- `LetMut` does not call `bind_alias` (`perceus.rs:306`).
- `lower_assign_index_reuse` only emits `ListSetInPlace` when the
  static uniqueness proof has held since allocation.
- `alloc_baseline.txt` is monotonically non-increasing for every
  pre-existing fixture.
- `reuse_not_across_perform_effect` and
  `reuse_not_across_with_handler` stay green.
- No fix may rely on inter-procedural analysis beyond `IrDecl`-level
  metadata. The decl-body return-shape inference fits this rule
  because it consults only the callee's IR, the same way M5's
  reuse pass already does for `reuse_param`.

### Out of scope for M10

- Closure escape analysis for stack allocation.
- HOF closure-body-aware ownership (the un-shipped M9 3b). Re-open
  if a future workload demands it; the M9 acceptance test passes
  without 3b.
- Type-driven ownership (Option C). Tracked as the long-term
  successor plan to PLAN_PERCEUS.

---

## 4.101. M11 — Host-intrinsic allocator unification

M11 closes the problem localized in `doc/STATE.md`: seven
`goby:runtime/track-e` host imports allocate escaping Goby values in the host
bump arena. That arena is monotonic, is not visible to `__goby_drop`, and is
not counted by `--debug-alloc-stats`. No Perceus IR Drop-placement change can
free those bytes.

### Decision

Use allocator unification: any host import that returns a Goby heap value must
allocate that value in the same refcounted heap layout that Wasm-generated code
uses.

Rejected alternative: per-call-frame host-bump reset. It is unsafe for escaping
values such as `read_lines ()`, `split_lines (read ())`,
`__goby_string_graphemes_list`, and `__goby_string_each_grapheme` resume values.
Those pointers can outlive the import call and be stored in lists, records,
closures, or `mut` cells. Resetting the bump cursor at the call boundary would
turn valid Goby values into dangling pointers; tracking escape precisely would
rebuild ownership on the side. The refcounted heap is already the ownership
model, so host imports should join it.

### Scope

In scope:

1. Replace host-bump allocation for escaping returned values with a host-side
   Perceus allocator helper.
2. Migrate all host imports that currently allocate through
   `alloc_from_host_bump`:
   - `__goby_value_to_string`
   - `__goby_string_each_grapheme_state`
   - `__goby_string_concat`
   - `__goby_list_join_string`
   - `__goby_string_graphemes_list`
   - `__goby_string_split_lines`
3. Keep `__goby_string_each_grapheme_count` allocation-free.
4. Make host-created strings, list headers, and list chunks visible to
   `__goby_drop`, free-list reuse, and alloc stats.

Out of scope:

- A general exact-size allocator for `large` objects. Existing `large`
  abandonment rules from §3.2 still apply.
- A tracing collector or region system.
- Rewriting stdlib `graphemes` away from host intrinsics. The allocator
  boundary should be correct regardless of which intrinsic path is used.

### Implementation plan

#### Step 0 — Measurement guard

Add temporary or test-only instrumentation around `alloc_from_host_bump` to
record cumulative host-bump bytes for:

- one `split_lines_host` call on the 138x138 reduction;
- one `graphemes_list_host` call on representative ASCII and multibyte input;
- the reduced `list.map lines graphemes` driver.

This step is evidence, not the fix. It verifies that the n=50 to n=70 cliff
matches host-bump growth and gives before/after numbers for the M11 commit.

#### Step 1 — Host-side Perceus allocation helper

Add a `wasm_exec.rs` helper that mirrors the runtime contract of
`emit_alloc_from_top`:

- reserve `payload_size + REFCOUNT_WORD_BYTES`, align to 8 bytes, and allocate
  from the Wasm-owned top-down heap, not from `host_bump_cursor`;
- initialize the refcount word to `1` and return the payload pointer;
- update `GLOBAL_HEAP_CURSOR_OFFSET` so the next Wasm allocation cannot
  re-enter the host-created object;
- respect `GLOBAL_HOST_BUMP_CURSOR_OFFSET` as the lower floor while the old bump
  path still exists;
- update `GLOBAL_ALLOC_BYTES_TOTAL_OFFSET` with the aligned allocation size;
- surface memory exhaustion through the same `RUNTIME_ERROR_MEMORY_EXHAUSTION`
  path as the current host bump allocator.

The helper should be small and explicit rather than calling generated Wasm
helpers from inside a host import. Re-entering Wasm allocation from a host
import would complicate borrowing of `Caller` and make failure behavior harder
to audit. Duplicating the few allocation-slot writes in Rust is acceptable if a
focused test locks the slot contract.

#### Step 2 — Migrate host-created strings

Replace `encode_string_in_host_bump` with a refcounted equivalent for escaping
strings. It writes the existing string payload layout unchanged:

```text
[payload + 0] : i32 len
[payload + 4] : utf-8 bytes
```

Only the allocation source changes: the payload pointer now has a refcount word
at `payload - 8`, so `__goby_drop` can release it. Apply this path to:

- `value_to_string_host`
- `string_concat_host`
- `list_join_string_host`
- the fresh-allocation branch of `grapheme_state_host_inner`

The in-place `span.start == 0` / `span.start >= 4` cases in
`grapheme_state_host_inner` can keep their existing no-allocation behavior.

#### Step 3 — Migrate host-created `List String`

Replace `alloc_list_string_host` so it allocates the list header, each chunk,
and each element string via the refcounted host helper. The payload layout stays
identical to the Wasm list layout documented in §3.1 / §3.2:

- list header: `total_len`, `n_chunks`, chunk pointers;
- chunk: `len`, tagged items;
- element string: tagged `String` pointer to `(len, bytes)`.

This migration covers:

- `graphemes_list_host`
- `split_lines_host`

After this step, dropping a `List String` returned by a host intrinsic must walk
the host-created list exactly like a Wasm-created list: drop children, free
chunks and strings into the existing free-list / large-object policy, and update
the existing freed/reuse stats.

#### Step 4 — Keep or narrow the old bump arena

After all escaping host values move to the refcounted heap, audit remaining
uses of `alloc_from_host_bump`:

- if no escaping values remain, keep the bump arena only for non-escaping host
  scratch or remove it if it becomes unused;
- if any remaining use returns a tagged Goby value, it is a bug and must move
  to the refcounted helper before M11 closes.

Do not split the ownership model into "sometimes bump, sometimes Perceus" for
tagged values that can reach Goby code.

### Acceptance for M11 closure

Hard gates:

- [x] `perceus_m11_graphemes_single_call_reports_host_alloc_stats` is active
      and green.
- [x] `perceus_m11_list_map_graphemes_138_lines_runs_under_256mib` is active
      and green.
- [x] The original `doc/BUGS.md` 2026-04-30 138x138 driver is automated or
      otherwise reproducibly exercised under `--max-memory-mb 256`, then moved
      Open -> Resolved.
- [x] `--debug-alloc-stats` observes host-created heap values: the reduced
      string/list drivers report non-zero `total_bytes`, and dropping them
      eventually produces free-list or freed-byte evidence rather than silent
      host-bump growth.
- [x] Existing Perceus regression tests remain green, especially
      `recursive_multi_part_interpolated_print_after_graphemes_executes` and
      the M10 Let/LetMut `GlobalRef` tests.
- [x] Repository quality gate: `cargo fmt`, `cargo check`, and
      `cargo test --workspace --release`.

### M11 invariants

- Tagged Goby values returned from host imports must have exactly the same
  payload layout and refcount prefix contract as values allocated by
  Wasm-generated code.
- Host allocation must update the same global accounting slots as
  `emit_alloc_from_top`; otherwise `--debug-alloc-stats` is not a reliable
  acceptance signal.
- Host allocation must not overwrite either older top-down heap objects or any
  still-live host-bump region while the bump arena exists.
- The M11 fix must not depend on making `return_ownership_value` more
  aggressive. IR ownership precision and host allocator correctness are
  separate boundaries.

---

## 5. Non-Goals

- **Tracing GC.** Not considered. Cycles are disallowed at the language
  level.
- **Generational or moving collection.** Not considered.
- **Linear / uniqueness types surfaced in the type system.** Ownership
  analysis is an internal IR pass.
- **Escape analysis for stack allocation of closures.** Reuse recovers
  most of the short-lived-closure cost; stack allocation is future
  work.
- **Region calculus.** Rejected in favor of Perceus (§2).
- **Spec change to drop shared-cell `mut` capture (Track B in §3.4).**
  Out of scope. If pursued later, it gets its own plan.
- **Inter-procedural reuse beyond the tail-call rule (§3.7.1).**
  Out of scope for M5.

---

## 6. Risks and Mitigations

- **Refcount traffic overhead.** *Mitigation:* M4.5 borrow inference
  with a 10% baseline-regression gate (`tests/alloc_baseline.txt`)
  across every existing example.
- **Reuse missed because block matching is too coarse.**
  *Mitigation:* start with §3.7's basic-block rule in M5. Expand only
  if the goal program or a committed baseline shows a measurable miss.
- **Effect handlers / multi-resume incorrectly freeing captured
  values.** *Mitigation:*
  `multi_resume_captured_list_state_preserved_across_resumes` is an M4
  acceptance test, not a follow-up.
- **Shared mutable cells reducing reuse wins.** *Mitigation:* the goal
  program uses non-captured `mut`; Track B is the separate route to
  lifting the shared-cell restriction.
- **Free-list fragmentation across size classes.** *Mitigation:* size
  classes in §3.2 are statically fixed and coarse. If fragmentation
  grows measurably, coalesce classes; do not add compaction.
- **Interaction with in-flight memory64 migration.** *Mitigation:* the
  M0 gate blocks this plan until `PLAN_LIST_FIX_M4_SITES.md` completes.
