# Goby Memory Management Plan — Perceus Refcount with Reuse

Last updated: 2026-04-18

This document is the roadmap for replacing Goby's current bump-only allocator
with a principled memory management scheme based on **Perceus-style reference
counting with reuse analysis** (Reinking, Xie, de Moura, Leijen; PLDI 2021, as
used in Koka, Lean 4, and Roc).

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
under the current bump allocator.

The success criterion is a self-contained program that lives **in this
repository** so the acceptance shape is reviewable long after this plan
closes. The program is fully specified in §1.1 to keep the target
reproducible.

- Terminate under `goby run --max-memory-mb 16` on the end-state
  implementation.
- Produce the same checksum under `goby check` (interpreter) and `goby run`
  (Wasm backend).
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

Authored in M1 as `examples/refcount_reuse_loop.gb`. The following source is
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

Expected checksum under both backends: recorded by M1 from a `goby check`
run and committed into the integration test as a literal; it becomes
authoritative.

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
| `large`      | overflow bucket                     | exact-size, no free list (bump only) |

Allocations that do not fit any enumerated bucket fall into `large`. `large`
allocations are freed when their refcount reaches zero (returned to a
free-range list keyed by exact size) but are not recycled into any other
bucket.

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
It lives in `goby-core` so IR can carry it and both backends (Wasm and
interpreter) can read it.

**Pass pipeline** (required ordering, asserted at pipeline boot):

```
resolved
  → shared IR lowering (ir::from_resolved)
  → closure_capture::materialize_envs
  → ownership_classify          (M4; perceus_ownership.rs)
  → drop_insert                 (M4; perceus_drop.rs)
  → reuse_pair                  (M5; perceus_reuse.rs)
  → backend_ir::lower  /  interpreter::eval
```

File names use the `perceus_` prefix; the exported pass functions and
types do not carry that prefix (per label hygiene).

### 3.9 Drop specialization

Each heap-typed ADT gets a generated drop helper that decrements refcount
on every owning field. Generation happens during backend lowering
(`crates/goby-wasm/src/gen_lower/`) from the IR-level type information.
For generic polymorphic types (`List a`, `(a, b)`, user-defined `record {
  ... }`) the runtime dispatches on the tagged value representation
(existing tag scheme in `runtime_value.rs`) to decide whether a field is a
heap pointer. No monomorphization.

The generated helpers live in a dedicated Wasm function section
`__goby_drop_*` emitted once per module. `drop(ptr)` on an object of a
given runtime tag dispatches into the matching helper by a small switch.

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

Milestones run top-to-bottom. Each ends with the full test suite green
**and** its acceptance criterion met. The criterion is the checkbox
directly under the milestone's **Acceptance** bullet; other checkboxes
are sub-tasks. Every acceptance criterion is objectively checkable by a
test or a fixture file, not by inspection.

### M0 — Precondition gate

- [x] `doc/PLAN_LIST_FIX_M4_SITES.md` milestones fully complete;
      memory64 is the only supported width (`doc/STATE.md` already
      records M4.6 — reconfirm at this plan's start).
- [x] No outstanding tasks in `doc/BUGS.md` that touch `emit.rs`
      allocation paths or `runtime_value.rs` tag layout.
- [x] **Acceptance:** `doc/STATE.md` records "Perceus plan unblocked";
      the implementation track can proceed to M1.

### M1 — Literal hoisting and goal harness

Hoist constant list/tuple/record literals with no free variables into
module-level static slots. Introduce the in-tree goal program.

- [ ] In `goby-core`, add a "closed-literal" detector over shared IR: a
      `ListLit` / `TupleLit` / `RecordLit` whose transitive
      subexpressions contain no `Var`, no `GlobalRef`, no `Lambda`, and
      no `Call`.
- [ ] Emit detected literals once at module init into a new static arena
      in `emit.rs`; rewrite the original site to a load from the slot.
      Static-arena slots carry the sentinel refcount from §3.1.
- [ ] Add `examples/refcount_reuse_loop.gb` with the normative source
      from §1.1.
- [ ] Add integration test in `crates/goby-wasm/tests/` that:
      (a) runs the example under `goby check` and captures the
      checksum, (b) records it as an `assert_eq!` literal, (c) runs
      under `goby run` with `#[ignore]` (un-ignored in M5). The ignore
      comment cites `doc/PLAN_PERCEUS.md` M5.
- [ ] **Acceptance:** `cargo test -p goby-wasm` green; existing
      `examples/*.gb` outputs byte-identical; the new integration test
      file exists with the recorded checksum literal and the ignored
      run-case.

### M2 — Heap object header with refcount, allocation counter

Add the refcount word, static sentinel, and allocation counter. Refcount
is maintained but not yet consulted for freeing.

- [ ] Adjust layout in `emit.rs` (lines 102–160) so every heap object
      allocation prepends a refcount word. Update
      `chunk_alloc_size_pw`, `header_alloc_size_pw`, `meta_slot_bytes`,
      and tuple/record/closure size helpers accordingly. Load/store
      offsets inside payloads are unchanged.
- [ ] All `alloc` sites initialize refcount to 1 via a new helper
      `emit_init_refcount_one(pw)` in `emit.rs`.
- [ ] Hoisted-literal and string-constant emissions initialize to
      sentinel via `emit_init_refcount_sentinel(pw)`.
- [ ] Add global `__goby_alloc_bytes_total` (i64, reserved linear-
      memory slot adjacent to `host_bump_cursor`). Every `alloc`
      increments it by the allocation size.
- [ ] Add CLI flag `--debug-alloc-stats` to the `run` subcommand. On
      program exit it prints exactly one line to stderr, format frozen:
      `alloc-stats: total_bytes=<N> peak_bytes=<M> free_list_hits=<H>`.
      In M2, `peak_bytes == total_bytes` and `free_list_hits == 0`;
      they are placeholders filled in by later milestones.
- [ ] **Acceptance:** all existing `examples/*.gb` produce
      byte-identical output;
      `goby run --debug-alloc-stats examples/hello.gb` prints the
      frozen line with a positive `total_bytes`. Snapshot test stores
      that hello output.

### M3 — Free lists and `drop` runtime

- [ ] Free-list head pointers declared as a struct at a fixed reserved
      offset in linear memory; layout committed as a comment in
      `emit.rs`. One head per size class from §3.2.
- [ ] `emit_alloc(size_class, pw)` helper: pops from the matching
      free list, falls back to bump. Replaces all current direct
      bump-allocation emissions.
- [ ] `__goby_drop` helper function emitted once per module: refcount
      decrement, on zero dispatches to the per-type child-drop helper
      by runtime tag, then pushes onto the matching free list via
      intrusive first-word next pointer.
- [ ] Per-type child-drop helpers (§3.9) emitted. Generic containers
      (`List`, tuple, record, closure) read tags to decide per-field
      descent.
- [ ] `free_list_hits` counter wired into `alloc`; increments when the
      free-list path is taken. `peak_bytes` wired: max of running
      `(bumped_bytes − freed_bytes)`.
- [ ] **Acceptance:** `cargo test -p goby-wasm` green; a new test
      `drop_frees_unique_list` explicitly calls `__goby_drop` on a
      freshly allocated unique list and observes
      `free_list_hits > 0` on the subsequent allocation.

### M4 — Static drop insertion, effect-aware

The Perceus algorithm proper, with the closure and handler rules from
§3.5 applied from the start, not as a follow-up.

- [ ] `ownership_classify` pass (new file
      `crates/goby-core/src/perceus_ownership.rs`) marks every
      parameter and `let`-binding as owned. Borrowed classification is
      deferred to M4.5. Public API: `classify(decl: &IrDecl) ->
      OwnershipMap`.
- [ ] `drop_insert` pass (new file
      `crates/goby-core/src/perceus_drop.rs`) implements §3.10.
      Inserts `CompExpr::Dup` and `CompExpr::Drop`. Public API:
      `insert_drops(decl: IrDecl, ownership: &OwnershipMap) -> IrDecl`.
- [ ] Pipeline ordering assertion in the compiler driver: panics at
      startup if passes are wired in the wrong order.
- [ ] Handler-boundary rules from §3.5 implemented inside
      `drop_insert`: `dup` live-across bindings at `PerformEffect` /
      `WithHandler`; continuation drop cascades.
- [ ] Correctness tests added in
      `crates/goby-core/tests/perceus_*.rs`:
      `mut_rec_drop`, `partial_application_drop`,
      `closure_captures_heap_values`, `case_heap_bound_pattern`,
      `multi_resume_captured_list_state_preserved_across_resumes`.
- [ ] Residency test `perceus_loop_residency`: a loop that allocates
      and immediately discards a 1024-element list, 1000 times,
      reports `free_list_hits >= 999` and `peak_bytes < 3 *
      list_header_plus_chunk_bytes`.
- [ ] **Acceptance:** residency test passes; all correctness tests
      pass; `cargo test` green.

### M4.5 — Borrow inference for hot parameters

- [ ] Extend `ownership_classify` to mark a parameter `borrowed` iff
      all of the following hold:
      (a) it is not returned,
      (b) it is not stored into any heap allocation's field,
      (c) every use is a read via `Var` that flows into a pure
          operation (`BinOp`, `ListGet`, `TupleProject`,
          `RecordProject`) or a call whose matching parameter is also
          `borrowed`.
      Condition (c) requires a fixpoint; iterate decl-by-decl.
- [ ] `drop_insert` skips `dup`/`drop` for borrowed parameters.
- [ ] Regression gate: new integration test `alloc_baseline` reads
      `tests/alloc_baseline.txt` (one line per example:
      `<example_basename> <max_total_bytes>`). The test runs each
      example with `--debug-alloc-stats` and asserts
      `total_bytes <= max_total_bytes`. The file is committed with the
      M4.5 PR; values are measured at the M4.5 branch + 10% margin.
- [ ] **Acceptance:** `cargo test -p goby-wasm alloc_baseline` passes
      with no number regressing beyond the 10% margin; `fold.gb` and
      `list_case.gb` show at least 20% reduction in `total_bytes` vs
      their M4 values (the M4 values are recorded in the PR
      description).

### M5 — Reuse analysis

- [ ] `reuse_pair` pass (new file
      `crates/goby-core/src/perceus_reuse.rs`) matches `Drop` →
      `alloc` on the same block (§3.7) by identical `SizeClass`. No
      matching across block terminators. Public API:
      `insert_reuse(decl: IrDecl) -> IrDecl`.
- [ ] Tail-position cross-call rule (§3.7.1) implemented: a `Drop` in
      tail position immediately before a tail `Call` to a decl whose
      first allocation matches size class S produces a reuse token
      passed as a hidden trailing argument.
- [ ] Emit `__goby_alloc_reuse` and `__goby_drop_reuse` runtime
      helpers per §3.3. `peak_bytes` accounting stays correct on the
      reuse path.
- [ ] Correctness tests:
      `reuse_fires_on_unique_list_update`,
      `reuse_falls_through_when_shared`,
      `reuse_not_across_perform_effect`,
      `reuse_not_across_with_handler`,
      `tail_call_reuse_passes_token`.
- [ ] Un-ignore the M1 integration test for
      `examples/refcount_reuse_loop.gb` and add an assertion that
      `total_bytes < 200 * 1024`.
- [ ] **Acceptance:** the M1 test passes under `goby run
      --max-memory-mb 16` with `total_bytes < 200 KiB` and the
      recorded checksum.

### M6 — `mut` lowering through reuse primitives (no semantics change)

- [ ] Backend lowering of `CompExpr::AssignIndex` on a `mut xs`
      binding emits the equivalent of a `DropReuse` on `xs` followed
      by an element store and a rebind to the reused token (or the
      new allocation on fall-through). Behavior observably unchanged
      for captured and non-captured `mut` bindings (§3.4).
- [ ] Nested `xs[i][j] := v` lowers to a chain of reuses without
      redundant refcount bumps on the outer list.
- [ ] `stdlib/goby/list.gb`'s `set` body rewritten as the immutable
      update form that benefits from reuse automatically. The
      `__goby_list_set` backend intrinsic may be removed or
      repointed; either is acceptable if example outputs are
      unchanged.
- [ ] `examples/mut_list.gb`, `closure_mut.gb`, `closure_mut_list.gb`,
      `closure_mut_nested_list.gb` produce byte-identical output.
- [ ] **Acceptance:** `cargo test -p goby-wasm` green; the four
      example outputs are unchanged;
      `examples/refcount_reuse_loop.gb` remains within its M5 budget.

### M7 — Remove the bump-only fallback; document

- [ ] `emit.rs` header comment rewritten to describe refcount +
      free-list model.
- [ ] `host_bump_cursor` / `heap_floor` redefined as "free-list miss
      backing" only.
- [ ] `doc/PLAN.md`, `doc/PLAN_IR.md`, `AGENTS.md` updated to reflect
      the new invariant: heap values are refcounted; last-use drop is
      the norm; reuse is the fast path for unique updates.
- [ ] `doc/STATE.md` records §1 goal as satisfied with the measured
      numbers.
- [ ] **Acceptance:** no surviving documentation reference to
      "bump-only allocator, no free"; the goal program runs under its
      budget; `tests/alloc_baseline.txt` re-verified, any deltas
      explained in the commit message.

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
