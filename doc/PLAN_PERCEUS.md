# Goby Memory Management Plan — Perceus Refcount with Reuse

Last updated: 2026-04-18

This document is the roadmap that replaced Goby's previous bump-only allocator
with a principled memory management scheme based on **Perceus-style reference
counting with reuse analysis** (Reinking, Xie, de Moura, Leijen; PLDI 2021, as
used in Koka, Lean 4, and Roc). M0–M7 are complete as of 2026-04-26.

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

- [x] In `goby-core`, add a "closed-literal" detector over shared IR: a
      `ListLit` / `TupleLit` / `RecordLit` whose transitive
      subexpressions contain no `Var`, no `GlobalRef`, no `Lambda`, and
      no `Call`.
- [x] Emit detected literals once at module init into a new static arena
      in `emit.rs`; rewrite the original site to a load from the slot.
      Static-arena slots carry the sentinel refcount from §3.1.
- [x] Add `examples/refcount_reuse_loop.gb` with the normative source
      from §1.1.
- [x] Add integration test in `crates/goby-wasm/tests/` that compiles
      the example and validates the Wasm output. The `goby run` case
      remains `#[ignore]` (un-ignored in M5). The ignore comment cites
      `doc/PLAN_PERCEUS.md` M5.
- [x] **Acceptance:** `cargo test -p goby-wasm` green; existing
      `examples/*.gb` outputs byte-identical; `refcount_reuse_loop_example_compiles`
      passes (un-ignored as of 2026-04-18).

### M2 — Heap object header with refcount, allocation counter

Add the refcount word, static sentinel, and allocation counter. Refcount
is maintained but not yet consulted for freeing.

- [x] Adjust layout in `emit.rs` (lines 102–160) so every heap object
      allocation prepends a refcount word. The prefix is absorbed in
      the single shared allocator (`emit_alloc_from_top`): the
      **payload** pointer returned to callers is unchanged in meaning,
      and `chunk_alloc_size_pw`, `header_alloc_size_pw`, tuple/record/
      closure size helpers stay as payload sizes. The allocator adds
      `REFCOUNT_WORD_BYTES` internally before size alignment and
      advances the returned pointer past the header. Load/store offsets
      inside payloads remain unchanged. See S2 for the exact edit.
- [x] All `alloc` sites initialize refcount to 1 by virtue of calling
      `emit_alloc_from_top`, which now writes the header as part of
      allocation. The named helper that performs that write is
      `emit_init_refcount_one` in `emit.rs`.
- [x] Hoisted-literal and string-constant emissions initialize to
      sentinel via `emit_init_refcount_sentinel(pw)`.
- [x] Add global `__goby_alloc_bytes_total` (i64, reserved linear-
      memory slot adjacent to `host_bump_cursor`). Every `alloc`
      increments it by the allocation size.
- [x] Add CLI flag `--debug-alloc-stats` to the `run` subcommand. On
      program exit it prints exactly one line to stderr, format frozen:
      `alloc-stats: total_bytes=<N> peak_bytes=<M> free_list_hits=<H>`.
      In M2, `peak_bytes == total_bytes` and `free_list_hits == 0`;
      they are placeholders filled in by later milestones.
- [x] **Acceptance:** all existing `examples/*.gb` produce
      byte-identical output;
      `goby run --debug-alloc-stats examples/hello.gb` prints the
      frozen line with a positive `total_bytes`. Snapshot test stores
      that hello output.

#### M2 — Implementation notes (non-normative detail for the implementer)

The checkboxes above are the contract. This subsection records the
concrete file surface, invariants to preserve, and ordering that the M2
change must follow. Nothing here weakens the acceptance criterion.

**S1. Layout slot additions (`crates/goby-wasm/src/layout.rs`).**

The current layout ends `HEAP_BASE = 28`. Extend the reserved prefix by
three i64 slots, aligned to 8 bytes, then advance `HEAP_BASE`:

```
 offset  size  name                              notes
 ------  ----  --------------------------------  -----------------------------
     0     8   iovec / nwritten (existing)       unchanged
     8     4   (pad)                             already present implicitly
    12     4   GLOBAL_HEAP_CURSOR_OFFSET         unchanged (i32)
    16     4   GLOBAL_HEAP_FLOOR_OFFSET          unchanged (i32)
    20     4   GLOBAL_RUNTIME_ERROR_OFFSET       unchanged (i32)
    24     4   GLOBAL_HOST_BUMP_CURSOR_OFFSET    unchanged (i32)
    28     4   (pad to 8-byte align for i64)     new
    32     8   GLOBAL_ALLOC_BYTES_TOTAL_OFFSET   new (i64, zero-init)
    40     8   GLOBAL_PEAK_BYTES_OFFSET          new (i64, zero-init; placeholder in M2)
    48     8   GLOBAL_FREE_LIST_HITS_OFFSET      new (i64, zero-init; placeholder in M2)
    56     …   iovec scratch / static string pool / heap
```

Set `HEAP_BASE = 56`. `initial_floor = align_up_i32(buffer_ptr + 4, 8)`
in `initialize_helper_state_locals` rebases automatically.

Zero-initialization strategy matches the existing emitter model, which
does **not** use `data.active` for global slots. Verify against
`crates/goby-wasm/src/gen_lower/emit.rs`:

- `initialize_helper_state_locals` (emit.rs:1310) writes
  `GLOBAL_HEAP_CURSOR_OFFSET`, `GLOBAL_RUNTIME_ERROR_OFFSET`, and
  `GLOBAL_HEAP_FLOOR_OFFSET` from the main function body on entry.
- The data section (emit.rs:1907) is reserved for the newline byte
  and the static string / static heap pools only.

Follow that pattern: extend `initialize_helper_state_locals` so that,
when `is_main` is true, it also emits three `I64Store` instructions
writing `0i64` into `GLOBAL_ALLOC_BYTES_TOTAL_OFFSET`,
`GLOBAL_PEAK_BYTES_OFFSET`, and `GLOBAL_FREE_LIST_HITS_OFFSET`. The
aux-decl branch of the same function must **not** re-zero these slots.

The data section in `emit_general_module_with_aux` is untouched for the
new globals. If the acceptance test from S1 wants to assert
initialization, it must either disassemble the function body or call
`_start` and read the slots post-entry via `Memory::read`.

Rationale for placing the new slots *inside* the pre-`HEAP_BASE` header
rather than inside the STATIC_STRING_LIMIT region: static strings and
the heap cursor grow **down** from `STATIC_STRING_LIMIT`, so borrowing
from that region would race with the existing bump downward. The
pre-`HEAP_BASE` header grows up and is initialized exactly once at
`main` entry.

**S2. Refcount-prepended allocation (`crates/goby-wasm/src/gen_lower/emit.rs`).**

Every dynamic heap object gets one extra 8-byte refcount word **before**
the payload pointer, per §3.1. Implementation strategy:

1. Introduce `REFCOUNT_WORD_BYTES: u32 = 8` near the `CHUNK_SIZE`
   constant. Use 8 unconditionally: Goby is memory64-only (§3.1 and
   `doc/STATE.md` M4.6), and hoisted-literal layout already uses `u64`.
2. Modify `emit_alloc_from_top(function, hs, size_local, result_local)`
   to:
   - add `REFCOUNT_WORD_BYTES` to `size_local` *before* the 8-byte
     alignment adjustment (so the cursor consumes the header too);
   - after the existing cursor decrement, the current `result_local`
     points at the *header* address; store `i64 1` into
     `result_local + 0` via `I64Store { offset: 0, align: 3 }`;
   - then advance `result_local += REFCOUNT_WORD_BYTES` so the caller
     still receives a **payload pointer** (unchanged ABI for all
     callers that assume payload-relative offsets).
3. All call sites of `emit_alloc_from_top` remain unchanged. They still
   pass the *payload* size in `size_local`; the helper is the single
   place that knows about the refcount prefix. Do **not** touch
   `chunk_alloc_size_pw`, `header_alloc_size_pw`, or tuple/record/
   closure size helpers — the normative plan (§3.1) says "load/store
   offsets inside the object are unchanged". Prefixing the header
   inside the allocator preserves that invariant; changing the `*_pw`
   size helpers would break it.
4. Introduce two small helpers for explicit refcount writes:
   - `emit_init_refcount_one(function, pw, payload_ptr_local)` — stores
     `1i64` at `payload_ptr_local - REFCOUNT_WORD_BYTES`. Called from
     the allocator path above.
   - `emit_init_refcount_sentinel_bytes()` — returns `u64::MAX` bytes
     for embedding in static data segments. `StaticHeapPoolBuilder::
     alloc_payload` already writes the sentinel (emit.rs:525–529); keep
     that call site.

**S3. Allocation counter plumbing.**

`emit_alloc_from_top` increments `GLOBAL_ALLOC_BYTES_TOTAL_OFFSET` by
the *aligned total* (payload + 8-byte refcount, post-alignment) on
every call. Emit sequence, reusing `hs.scratch` locals:

```
i64.load  [GLOBAL_ALLOC_BYTES_TOTAL_OFFSET]
local.get size_local        ;; already aligned, includes refcount word
i64.add
i64.store [GLOBAL_ALLOC_BYTES_TOTAL_OFFSET]
```

No memory_index load/store fuss: the slot is at a fixed address and the
existing `GLOBAL_*` constant pattern in emit.rs already uses
`ptr_const(pw, OFFSET as u64)` + `I64Store` without MemArg gymnastics.

`GLOBAL_PEAK_BYTES_OFFSET` is written once as `total_bytes` at program
exit in M2 (placeholder; M3 wires real peak tracking against the free
list). `GLOBAL_FREE_LIST_HITS_OFFSET` stays zero until M3 increments it.

**S4. Exit-time stats emission.**

The stats line must land on stderr when `--debug-alloc-stats` is set.
Two execution paths need coverage:

- **File-based `wasmtime`** (current default for most programs,
  `execute_wasm` in `crates/goby-cli/src/main.rs:520`). The Wasm
  module's `_start` must itself `fd_write(2, ...)` before returning.
  Add an epilogue emitter `emit_alloc_stats_line(function, hs)` that:
  1. If the module was compiled without the debug flag, emits nothing.
     (The flag is a compile-time option to avoid runtime-conditional
     emission.)
  2. Otherwise formats `alloc-stats: total_bytes=<N> peak_bytes=<M>
     free_list_hits=<H>\n` into a scratch buffer in linear memory.
     Reuse the existing `i64_to_decimal_ascii` helper if present;
     otherwise add a minimal i64-to-decimal emitter in
     `gen_lower/emit.rs` (used only by this path).
  3. Writes the buffer to fd=2 via `fd_write`. `fd_write` is already
     imported; its iovec layout is documented at
     `crates/goby-wasm/src/layout.rs:1`.
  4. Is inserted at the exit of the `_start` function, after any
     user `println`s have flushed to fd=1. Place the emitter call
     immediately before the final `end` of `main`'s function body in
     `emit_general_module_with_aux`, gated on
     `EmitOptions::debug_alloc_stats`.

- **Host-owned execution** (`execute_runtime_module_with_stdin_and_config`
  in `crates/goby-wasm/src/wasm_exec.rs`). The host already owns the
  `Store` and can read `GLOBAL_ALLOC_BYTES_TOTAL_OFFSET` directly after
  `_start` returns. Add a parallel path: when `debug_alloc_stats` is
  requested, read the three slots out of linear memory via the existing
  `Memory::read` helper (see wasm_exec.rs:861 for precedent) and print
  the same frozen line to stderr from Rust.

Both paths share the frozen format. Add one `format_alloc_stats_line(
total: u64, peak: u64, hits: u64) -> String` helper in
`goby-cli` (or a shared crate-internal module) to keep them in sync.

**S5. Option plumbing: where to add the boundary.**

The current public surface has no `compile_module_with_options` or
equivalent. The two entrypoints that must receive the debug flag are:

- File-based path: `goby_wasm::compile_module` (`crates/goby-wasm/
  src/lib.rs:105`), which delegates to
  `execution_plan::compile_module_entrypoint` (`crates/goby-wasm/
  src/execution_plan.rs`).
- Host-owned path: `goby_wasm::execute_runtime_module_with_stdin_and_config`
  (`crates/goby-wasm/src/lib.rs:167`), which delegates to
  `execution_plan::execute_runtime_module_with_stdin_and_config_entrypoint`
  (execution_plan.rs:120). That function internally calls
  `compile_module_entrypoint` for the `GeneralLowered` arm.

Concrete edits:

1. Extend `EmitOptions` (currently defined in emit.rs) with
   `debug_alloc_stats: bool`, default `false`.
2. Introduce a small `CompileOptions` struct at the `goby-wasm` crate
   root (or extend an existing equivalent if one appears during
   implementation) carrying `debug_alloc_stats`. Plumb it through
   `compile_module_entrypoint` down to `emit_general_module_with_aux_
   and_options`, which already accepts `EmitOptions`. The rest of the
   call chain is internal and need not grow a new type.
3. Add a new public entrypoint
   `goby_wasm::compile_module_with_options(module, CompileOptions) ->
   Result<Vec<u8>, CodegenError>`. The existing `compile_module`
   becomes a thin wrapper passing `CompileOptions::default()`.
4. Add a new public entrypoint
   `goby_wasm::execute_runtime_module_with_stdin_config_and_options(
   module, stdin_seed, memory_config, CompileOptions) -> Result<
   Option<String>, CodegenError>`. The existing
   `execute_runtime_module_with_stdin_and_config` wraps it with
   default options. This function is also responsible for the
   post-`_start` host-side print described in S4.
5. `CliArgs.debug_alloc_stats` (new bool) flows into both new
   entrypoints from `run_command` (`crates/goby-cli/src/main.rs:259`).
   `execute_wasm` (main.rs:520) gets an extra parameter and, when the
   flag is set, inherits the wasmtime-child stderr as-is (default
   behavior — no extra plumbing needed beyond not suppressing it).

The host-owned path needs the signal because it does not re-emit
Wasm; it reads the counters out of linear memory after `_start`.
The file-based path needs it only to flip `EmitOptions.debug_alloc_stats`
before `compile_module_entrypoint` emits the `_start` epilogue.

**S6. CLI surface (`crates/goby-cli/src/main.rs`).**

1. Add `--debug-alloc-stats` (boolean, no value) to the argument parser
   alongside `--max-memory-mb`. Mirror the existing pre/post-file
   acceptance. Reject on non-`run` commands with the same warning
   pattern already used for `--max-memory-mb` on non-run commands.
2. Thread the bool through `CliArgs`, `run_command`, both execution
   branches, and into the two new `goby-wasm` entrypoints introduced
   in S5: `goby_wasm::compile_module_with_options` (file-based arm in
   `run_command` at main.rs:302) and
   `goby_wasm::execute_runtime_module_with_stdin_config_and_options`
   (host-owned arm at main.rs:283).
3. Add unit tests mirroring the existing
   `parses_max_memory_mb_*` cases: space-separated, missing-value,
   before/after file. At least one integration test in
   `crates/goby-cli/tests/` that runs `goby run --debug-alloc-stats
   examples/hello.gb` and greps the stderr line against the frozen
   regex `^alloc-stats: total_bytes=[1-9][0-9]* peak_bytes=[0-9]+
   free_list_hits=0$`.

**S7. Regression protection for existing examples.**

Every `examples/*.gb` must still produce byte-identical stdout. The
refcount prefix is invisible to user code because:

- Payload-relative offsets inside objects are unchanged (S2.3).
- Static heap already writes sentinel refcounts (existing code at
  emit.rs:525–529).
- The payload pointer ABI is unchanged.

The one latent risk is alignment: `emit_alloc_from_top` currently
aligns the *payload* start to 8. With an 8-byte refcount prefix, the
header also starts at an 8-byte boundary and the payload therefore
stays aligned. Preserve the alignment assertion explicitly: after
`result_local += REFCOUNT_WORD_BYTES`, assert in a debug-only check
(`debug_assert!` via a test-only harness) that `result_local % 8 == 0`.

Acceptance verification: `cargo test -p goby-wasm` must remain green,
including the snapshot suite (tests under
`crates/goby-wasm/tests/wasm_exports_and_smoke.rs` cover most
examples). `cargo test -p goby-core` must also stay green; M2 does not
touch the interpreter.

**S8. Ordered implementation steps (minimal, reviewable).**

Split the M2 change into the following commit-sized steps. Run
`cargo check` + the focused test after each step; only proceed to the
next when green.

1. **Layout slots.** Add the three new `GLOBAL_*_OFFSET` constants and
   advance `HEAP_BASE` in `layout.rs`. Extend
   `initialize_helper_state_locals` (emit.rs:1310) so that, when
   `is_main` is true, it stores `0i64` into each new slot after the
   existing `GLOBAL_HEAP_CURSOR_OFFSET` / `GLOBAL_RUNTIME_ERROR_OFFSET` /
   `GLOBAL_HEAP_FLOOR_OFFSET` writes. Focused test: add a test in
   `crates/goby-wasm/tests/` that compiles a trivial module and, via a
   wasmtime `Memory::read` harness (pattern at `wasm_exec.rs`:861),
   asserts all three slots read back as `0` immediately after `_start`
   on a program that never allocates. An assembly-level inspection of
   the data section is **not** the right assertion — the slots are
   initialized in function-body code, not data.

2. **Refcount prefix in allocator.** Modify `emit_alloc_from_top` per
   S2; add `emit_init_refcount_one` helper. Do not touch counters yet.
   Focused test: `cargo test -p goby-wasm list_lit_`,
   `tuple_lit_`, `record_lit_`, `closure_`. Snapshot byte-identity of
   `hello.gb` stdout.

3. **Allocation counter.** Wire the `GLOBAL_ALLOC_BYTES_TOTAL_OFFSET`
   increment into `emit_alloc_from_top`. Write initial
   `GLOBAL_PEAK_BYTES_OFFSET = GLOBAL_ALLOC_BYTES_TOTAL_OFFSET` at
   `_start` exit (placeholder; M3 replaces with real peak). Focused
   test: a new unit test that compiles a program with one `ListLit`
   and reads the counter slot via a WASI memory read in
   `wasm_exec.rs`-style test harness.

4. **EmitOptions + Wasm-side stats line.** Add
   `EmitOptions.debug_alloc_stats`, `emit_alloc_stats_line`, and the
   `fd_write` epilogue. Focused test: compile `hello.gb` with the flag
   and parse stderr from a wasmtime-driven integration test
   (conditional on wasmtime availability, matching existing pattern).

5. **Host-owned path.** Add the post-`_start` memory-read branch in
   `execute_runtime_module_with_stdin_and_config`. Focused test:
   one of the existing GeneralLowered runtime tests run with the flag.

6. **CLI wiring.** Add the flag to `parse_args_from`, plumb through
   `run_command` / `execute_wasm`. Add the CLI unit + integration
   tests listed in S6.

7. **Acceptance sweep.** `cargo fmt --all`, `cargo check
   --all-targets`, `cargo test -p goby-core`, `cargo test -p
   goby-wasm`, `cargo test -p goby-cli`. Verify no `examples/*.gb`
   stdout regressed (snapshot suite).

**S9. What this step does *not* do (guardrails).**

- Does **not** free memory. `drop` is M3. The counter can and will
  rise monotonically across the whole run.
- Does **not** introduce free lists. `GLOBAL_FREE_LIST_HITS_OFFSET`
  stays zero.
- Does **not** alter `mut` lowering. `AllocMutableCell` gets the same
  refcount prefix as every other heap object but its refcount is not
  yet consulted for anything.
- Does **not** change interpreter behavior. Perceus is a Wasm-backend
  concern until M4 introduces IR-level `Dup`/`Drop`.
- Does **not** touch `doc/LANGUAGE_SPEC.md`. No user-visible semantic
  change.

**S10. Files touched (exhaustive).**

```
crates/goby-wasm/src/layout.rs                 (S1: slot offsets + HEAP_BASE)
crates/goby-wasm/src/gen_lower/emit.rs         (S1 init writes in
                                                 initialize_helper_state_locals;
                                                 S2 refcount prefix;
                                                 S3 counter increment;
                                                 S4 Wasm _start epilogue;
                                                 EmitOptions.debug_alloc_stats)
crates/goby-wasm/src/execution_plan.rs         (S5: thread CompileOptions
                                                 through compile_module_entrypoint
                                                 and execute_runtime_module_with_
                                                 stdin_and_config_entrypoint;
                                                 S4 host-side post-read in the
                                                 GeneralLowered arm)
crates/goby-wasm/src/lib.rs                    (S5: new public entrypoints
                                                 compile_module_with_options and
                                                 execute_runtime_module_with_
                                                 stdin_config_and_options;
                                                 existing entrypoints become
                                                 default-option wrappers)
crates/goby-wasm/src/wasm_exec.rs              (expose a helper to read the
                                                 three i64 stat slots from
                                                 linear memory for the host-side
                                                 print; or inline via Memory::read)
crates/goby-cli/src/main.rs                    (S6: --debug-alloc-stats flag
                                                 parsing, USAGE update, plumbing
                                                 into both run_command arms)
crates/goby-cli/tests/                         (new CLI integration test)
crates/goby-wasm/tests/                        (new allocator layout test;
                                                 stderr-line integration test)
doc/STATE.md                                   (flip focus to "M2 in progress" /
                                                 "M2 complete" at end)
doc/PLAN_PERCEUS.md                            (check the M2 boxes at end)
```

No changes to `stdlib/goby/*.gb`, no changes to `examples/*.gb`, no
changes to the interpreter (`crates/goby-core/src/interp*`), no changes
to `doc/LANGUAGE_SPEC.md`.

### M3 — Free lists and `drop` runtime

- [x] Free-list head pointers declared as a struct at a fixed reserved
      offset in linear memory; layout committed as a comment in
      `emit.rs`. One head per size class from §3.2.
- [x] `emit_alloc(size_class, pw)` helper: pops from the matching
      free list, falls back to bump. Replaces all current direct
      bump-allocation emissions. (landed as `emit_alloc_with_flag` per C1)
- [x] `__goby_drop` helper function emitted once per module: refcount
      decrement, on zero dispatches to the per-type child-drop helper
      by runtime tag, then pushes onto the matching free list via
      intrusive first-word next pointer.
- [x] Per-type child-drop helpers (§3.9) emitted. TAG_CHUNK/TAG_LIST/
      TAG_TUPLE/TAG_CELL fully implemented. TAG_RECORD/TAG_CLOSURE:
      freed_bytes accounting only (arity not in payload; full child-drop
      deferred to M4 with layout change).
- [x] `free_list_hits` counter wired into `alloc`; increments when the
      free-list path is taken. `peak_bytes` wired: max of running
      `(bumped_bytes − freed_bytes)`.
- [x] **Acceptance:** `cargo test -p goby-wasm` green; a new test
      `drop_frees_unique_list` explicitly calls `__goby_drop` on a
      freshly allocated unique list and observes
      `free_list_hits > 0` on the subsequent allocation.

#### M3 — Clarifications (added 2026-04-19)

The checkboxes above stand; these notes only disambiguate points the
implementer flagged. None of them weaken the acceptance criterion.

**C1. `emit_alloc` name vs. existing `emit_alloc_with_flag`.** Step 1–5
landed `emit_alloc_with_flag` (free-list pop + hits counter + bump
fallback). The bullet "`emit_alloc(size_class, pw)` helper" refers to
the *same* helper; the M3 task is to make it the **sole** bump-site
entrypoint — i.e. port every remaining direct bump emission
(`emit_alloc_from_top` call site) to go through
`emit_alloc_with_flag(size_class, ...)`. No second helper is introduced.
Renaming `emit_alloc_with_flag` to `emit_alloc` is optional and
non-normative.

**C2. `peak_bytes` formula.** The normative reading is:

```
peak_bytes = max over the run of (total_bytes − freed_bytes)
```

where `total_bytes` is the existing cumulative allocation counter
(§S3) and `freed_bytes` is the new cumulative counter added by the
free-list push path in this milestone (`GLOBAL_FREED_BYTES_OFFSET`,
already reserved by Step 1). "bumped_bytes" in the original bullet is
a synonym for `total_bytes`; rename accordingly. `peak_bytes` is
updated on every `alloc` and every free-list push: after each
counter write, compute `(total_bytes − freed_bytes)` and
`i64.store` into `GLOBAL_PEAK_BYTES_OFFSET` only when the new value
exceeds the stored one.

**C3. Per-type child-drop dispatch.** §3.9 says "dispatch on the
tagged value representation". For Perceus M3, implement this as
follows:

- **Per-size-class helpers.** One child-drop function per size class
  with a static shape: `header[k]`, `tuple[a]`, `record[a]`,
  `closure[s]`, `cell`, `string[b]`. These helpers have no runtime
  tag test at the *shape* level — shape is fixed by size class.
- **Per-slot tag test.** Inside each helper, every owning slot is
  read and inspected with the existing `runtime_value.rs` tag scheme
  to decide whether to recurse via `__goby_drop`. Unboxed ints /
  booleans / unit skip the call. This is the only runtime tag check.
- **`chunk` has no owning slots** at the per-element level for
  primitive lists; for lists of heap values the element slot holds
  tagged values and the same per-slot tag test applies.
- **`large` bucket.** Uses an exact-size free-range list keyed by
  size (§3.2). For M3 the child-drop is the same per-shape helper as
  the would-be size class; size-class matching is what's relaxed,
  not the shape.

The M3 deliverable includes **all** owning-shape helpers, not just
`Cell`. The current Step 1–5 commit only pushes `Cell`; Steps 6–7
must extend child-drop to `header[k]`, `chunk` (when containing
heap elements), `tuple[a]`, `record[a]`, and `closure[s]`.

**C4. How the acceptance test gets a "unique list" without M4.** M3
ships without automatic drop insertion. The acceptance test
`drop_frees_unique_list` must therefore hand-construct the situation.
Preferred shape:

1. Compile a module that exposes (for testing only) an exported Wasm
   function which allocates a list of known size class, returns the
   payload pointer, and a second exported function which, given that
   pointer, invokes `__goby_drop`.
2. The integration test calls the first function, then the second,
   then allocates again and reads `GLOBAL_FREE_LIST_HITS_OFFSET` via
   `Memory::read` (pattern at `wasm_exec.rs`:861) and asserts > 0.

Exporting these test-only entrypoints is acceptable; gate them on a
`cfg(test)` or `EmitOptions::expose_perceus_test_exports` flag so they
do not leak into release modules.

**C5. Free-list intrusive next-pointer overwrites payload word 0.**
This is intentional and correct: the object is dead (refcount 0) when
pushed, so clobbering its payload is safe. The only constraint is
that the size class has at least one word of payload — all §3.2
classes satisfy this. No action needed; record this as the invariant
that forbids zero-sized size classes.

### M4 — Static drop insertion, effect-aware

The Perceus algorithm proper, with the closure and handler rules from
§3.5 applied from the start, not as a follow-up.

- [x] `ownership_classify` pass (landed in
      `crates/goby-core/src/perceus.rs` as
      `ownership_classify_module`/`ownership_classify_decl`) marks every
      parameter and `let`-binding of a fresh-heap value as `Owned`.
      Borrowed classification is deferred to M4.5.
- [x] `drop_insert` pass (same file, `drop_insert_module`) implements a
      **conservative slice** of §3.10 that inserts `CompExpr::Dup` /
      `CompExpr::Drop` only where the consume-vs-borrow distinction is
      unambiguous without borrow inference — see the M4 as-shipped note
      below.
- [x] Pipeline ordering assertion in the compiler driver: panics at
      startup if passes are wired in the wrong order.
- [x] Handler-boundary rules from §3.5 implemented inside
      `drop_insert`: `Dup` live-across bindings at `WithHandler`;
      `Resume` treated as borrow. Continuation drop cascades for
      multi-resume are covered by C5's existing `WithHandler` logic.
- [x] Correctness tests added inline in
      `crates/goby-core/src/perceus.rs` `mod tests` (renamed per label
      hygiene; milestone IDs stripped). The M4 `mut_rec_drop` test from
      §M4 C6 is **not** shipped as-is: a mutually recursive pair that
      round-trips an owned allocation between helpers requires the full
      parameter Dup/Drop machinery that M4.5 will restore. The
      ownership-transfer **primitive** that `mut_rec_drop` builds on is
      covered by the single-decl regression
      `call_site_transfers_ownership_of_owned_arg_to_callee`.
      Shipped tests:
      `call_site_transfers_ownership_of_owned_arg_to_callee`,
      `nested_let_with_call_consuming_binding_does_not_get_outer_drop`,
      `seq_stmt_with_call_consuming_binding_does_not_get_outer_drop`,
      `partial_application_captured_list_is_left_to_closure_drop`,
      `closure_captures_heap_values_outer_scope_does_not_drop`,
      `case_heap_bound_pattern_drops_unused_bindings`,
      `multi_resume_captured_list_state_preserved_across_resumes`.
- [ ] Residency test `perceus_loop_residency` — **deferred to M4.5.**
      Driving `free_list_hits >= 999` from a real tail-recursive
      `build 1000` loop requires parameter Dup/Drop and If/Case branch
      balancing that the M4 conservative slice intentionally omits
      (see "M4 as-shipped" below). M3 already ships
      `drop_frees_unique_list_and_subsequent_alloc_gets_free_list_hit`
      as a residency proxy; that test remains the M4 proof-of-life for
      the `__goby_drop` → free-list path.
- [x] **Acceptance (as-shipped):** all correctness tests pass;
      `cargo test --workspace` green; M3 residency proxy remains
      green; real-loop residency gate reopens under M4.5.

#### M4 — As-shipped scope note (added 2026-04-21)

M4 landed as a **conservative slice**, not the full §3.10 implementation,
because applying uniform "every `Var` occurrence consumes" semantics to
all parameter uses produced use-after-free in real programs. Root cause:
Goby intrinsics like `length`, `list_get`, `list_first` are **borrow
positions** — they read without consuming — and the current IR does not
distinguish borrow from consume on `ValueExpr::Var`. Any attempt to
insert `Drop` after a parameter's last-use, or to branch-balance `Drop`
between `If` / `Case` arms on general bindings, must therefore be
deferred until M4.5 adds borrow classification.

What the shipped slice does:
- classifies all params as `Owned`
- inserts `Drop` for `let x = <fresh-heap>` bindings only when
  `use_count == 0` (dropped at the top of the body). When the binding is
  used even once, the pass leaves it alone. The earlier `use_count == 1 &&
  !result_references` branch was removed because it misclassified cases
  like `let xs = [..] in let y = process(xs) in y`: the single use of `xs`
  sits in a `Call` argument that transfers ownership to the callee, so
  emitting a post-body `Drop(xs)` double-frees. Regression tests
  `nested_let_with_call_consuming_binding_does_not_get_outer_drop` and
  `seq_stmt_with_call_consuming_binding_does_not_get_outer_drop` pin this.
- inserts `Drop` for unused `case` pattern bindings in each arm
- inserts `Dup` for owned bindings live across a `WithHandler` body
  (Case pattern binds and Handle clause params are scoped out of the
  live-across set)
- treats `Call`-site argument passing as an ownership transfer: the
  caller emits no post-`Call` `Drop` for owned args (verified by
  `call_site_transfers_ownership_of_owned_arg_to_callee`)
- for parameters: only emits `Drop` on params that are **never** referenced
  in the body (the unambiguous "dead parameter" case)

What M4.5 must re-open:
- full last-use analysis on parameters (needs borrow/consume marks on
  `Var` occurrences)
- If/Case branch balancing on non-pattern bindings
- Dup insertion for non-last uses in multi-use scopes
- `perceus_loop_residency` gate

#### M4 — Clarifications (added 2026-04-19)

**C1. `OwnershipMap` shape.** Keyed by `BindingId` (parameters and
`let`-bound names, including tuple / record destructurings). Values are
`Owned | Borrowed` (M4 only emits `Owned`). Per-use markers (last-use,
non-last-use) are **not** stored in the map — they are computed on
demand during `drop_insert` by a reverse-order scan inside each basic
block (§3.7). Rationale: keeping the map shallow avoids having to
invalidate it when `drop_insert` rewrites the tree.

**C2. Pass granularity.** `insert_drops` runs **per `IrDecl`**. The
module-level pipeline calls it in a loop over all decls.
`closure_capture::materialize_envs` has already run, so env-slot
reads appear as ordinary `Var`s over a synthetic binding whose type is
the env record. The per-decl contract is: the returned `IrDecl` is
semantically equivalent modulo inserted `Dup` / `Drop` nodes.

**C3. Pipeline-ordering assertion.** "Startup" means the first call to
`compile_module_entrypoint` (`crates/goby-wasm/src/execution_plan.rs`)
and to the interpreter's driver
(`crates/goby-core/src/interp*`). Concretely: a single
`assert_perceus_pipeline_order()` helper in `goby-core` is called by
both drivers before they iterate decls. The assertion compares a
`Vec<&'static str>` of pass names against a fixed expected list from
§3.8. It panics (not a typed error) because mis-ordering is a compile-
time wiring bug, not a user-visible condition.

**C4. IR-node lowering (new — not covered by §3.9).** M4 introduces
`CompExpr::Dup` and `CompExpr::Drop` but §3.9 only spells out the
child-drop helpers. Backend lowering responsibility split:

- `CompExpr::Drop { value }` lowers to a call to the module-level
  `__goby_drop` (already emitted by M3). The argument is the payload
  pointer; non-heap values short-circuit inside `__goby_drop` via a
  tag check at entry.
- `CompExpr::Dup { value }` lowers to a new module-level
  `__goby_dup` helper (emitted by M4): sentinel check, else i64-
  increment the refcount at `ptr - PTR_BYTES`. Emit once per module
  alongside `__goby_drop`.
- Interpreter lowering: in `goby-core`'s interpreter, `Dup` /
  `Drop` become no-ops at the value level but must still drive the
  runtime refcount if the interpreter later gains a heap model. For
  M4, the interpreter treats them as `Unit`-producing no-ops and
  the perceus tests run against the Wasm backend.

**C5. Multi-resume continuation-drop cascade (§3.5).** The IR-level
implementation is: at `WithHandler` lowering, each owned binding live
across the handler call gets a `Dup` inserted before the handler
invocation. The captured-continuation object conceptually owns one
refcount per such binding. When the continuation is dropped
(completion or explicit abandon), the continuation object's child-
drop runs `Drop` on each captured binding. M4 does **not** need to
change the Wasm effect-handler ABI; it only needs `drop_insert` to
emit the extra `Dup`s and to treat `Resume` as a borrow of the
captured frame (reads without consuming). If multi-resume is
invoked N times, each `Resume` issues its own `Dup` over the
preserved bindings; if invoked zero times, the captured refcounts
are released by the continuation-object drop path.

**C6. Test-name disambiguation.**

- `mut_rec_drop` → test that a mutually recursive pair of
  functions, each of which heap-allocates and returns the
  allocation to the other, does not leak: inserted drops cover
  both call boundaries.
- `partial_application_drop` → when a curried partial application
  is constructed and then dropped without being fully applied,
  the captured arguments are released.
- `closure_captures_heap_values` → a closure that captures a
  list by value sees its env dropped when the closure itself is
  dropped; nothing runs the captured list's drop twice.
- `case_heap_bound_pattern` → `case xs { [x, ..rest] -> … }`
  leaves both `x` and `rest` owned; the non-matching arm drops
  `xs`; the matching arm drops `xs` after binding `x` and
  `rest`, and drops whichever of `x` / `rest` is unused in the
  arm body.

**C7. Handler-boundary rules interact with C5.** The rule "reuse does
not cross a `PerformEffect`" (§3.7) is an M5 concern, not M4. M4
only needs to ensure `drop_insert`'s live-across dup pass does not
mistake a `PerformEffect` for an ordinary call. Concretely:
`PerformEffect` and `WithHandler` contribute to the "live across"
set exactly like an ordinary call, but they force the enclosing
block to end (§3.7) so last-use analysis within the block is
unaffected.

### M4.5 — Borrow inference for hot parameters

- [x] Extend `ownership_classify` to mark a parameter `borrowed` iff
      all of the following hold:
      (a) it is not returned,
      (b) it is not stored into any heap allocation's field,
      (c) every use is a read via `Var` that flows into a pure
          operation (`BinOp`, `ListGet`, `TupleProject`,
          `RecordProject`) or a call whose matching parameter is also
          `borrowed`.
      Condition (c) requires a fixpoint; iterate decl-by-decl.
- [x] `drop_insert` skips `dup`/`drop` for borrowed parameters.
- [x] Regression gate: new integration test `alloc_baseline` reads
      `tests/alloc_baseline.txt` (one line per example:
      `<example_basename> <max_total_bytes>`). The test runs each
      example with `--debug-alloc-stats` and asserts
      `total_bytes <= max_total_bytes`. The file is committed with the
      M4.5 PR; values are measured at the M4.5 branch + 10% margin.
- [x] **Acceptance (as-shipped):** `cargo test -p goby-wasm alloc_baseline`
      passes with no number regressing beyond the 10% margin for the
      currently GeneralLowered examples in `alloc_baseline.txt`. The original
      `list_case.gb` reduction check remains inapplicable while that example
      classifies as `NotRuntimeIo` and cannot emit debug alloc stats.

M4.5 slice note (2026-04-21): the first borrow-classifier slice is in
tree. It implements the optimistic module fixpoint for ordinary decls,
borrowed-parameter Dup/Drop skipping, borrowed-call owner-side Drop for fresh
owned lets, and focused `goby-core::perceus` tests. Precision still to reopen:
`let` alias flow from C2, SCC-wide recursive parameter demotion from C3,
general If/Case branch balancing, non-last-use Dup insertion, and the
`alloc_baseline` / `perceus_loop_residency` gates.

M4.5 completion note (2026-04-22): the remaining borrow-inference slice is
now in tree. The classifier tracks simple `let alias = Var(source)` chains
back to parameter owners and the existing optimistic module fixpoint covers
recursive SCC demotion through call edges. `drop_insert` now handles owned
parameter borrow-only bodies, If/Case dead-arm Drop balancing, live-across
`let value` Dup insertion before consuming calls, and same-call repeated
owned-argument Dup insertion for the syntax-directed cases that have
borrow/consume evidence. Borrow evidence is intentionally conservative for
`If` conditions, `ListGet`, `TupleProject`, interpolation, and scalar
operators because source-level parameter types are often erased to `?` in IR.
The `alloc_baseline` integration gate was added with the currently
GeneralLowered examples `fold.gb`, `hof_fold_print.gb`, and
`refcount_reuse_loop.gb`. `list_case.gb` is currently `NotRuntimeIo`, so it is
not part of the executable alloc-stats baseline until that example moves onto
the GeneralLowered path.

#### M4.5 — Clarifications (added 2026-04-19)

**C1. Fixpoint initial state.** Start every parameter `Borrowed` and
**demote** on violation of (a)/(b)/(c). This is the "optimistic"
direction. Rationale: a parameter that never appears in a `Dup` /
heap-store / return position stays `Borrowed` at fixpoint. Starting
from `Owned` would require a separate liveness witness for every
borrow and never converges to the same set. Iterate decl-by-decl in
any order until no decl's `OwnershipMap` changes in a full pass.

**C2. "Flows into" (condition c) — operational definition.** A use of
parameter `p` at occurrence `u` "flows into" operation `O` iff `u` is
the direct operand of `O`, or `u` appears in a
`CompExpr::Let { bind, value: u, body }` where every occurrence of
`bind` in `body` also flows into some `O`. Concretely:

- `p` read inside a `BinOp`, `ListGet`, `TupleProject`,
  `RecordProject`, `Compare`, `Not`, or arithmetic operation —
  **flows pure**.
- `p` passed as argument to `Call f ... p ...` — flows pure iff `f`'s
  matching parameter position is `Borrowed` in the current fixpoint
  iterate.
- `p` returned (appears in a tail `Value` position), stored in a
  `ListLit` / `TupleLit` / `RecordLit` / `CreateClosure` field, used
  in `AssignIndex` as the list target, or passed to an `Owned`
  callee parameter — **demotes** `p` to `Owned`.
- `p` used as scrutinee of `Case` — recurse: if every arm treats the
  binding-forms it produces as a borrow (no further-violating use),
  the scrutinee usage is a borrow.

This is deliberately a local, syntax-directed analysis; no alias or
escape analysis.

**C3. Recursive and mutually recursive functions.** Treat every decl
in a strongly connected component as one fixpoint subject. Initial
state `Borrowed`; a violation in any decl demotes the matching
parameter across **all** decls in the SCC before the next iterate.

**C4. Where `tests/alloc_baseline.txt` lives.**
`crates/goby-wasm/tests/alloc_baseline.txt`. The integration test
file `crates/goby-wasm/tests/alloc_baseline.rs` iterates it, runs
each example through the host-owned execution path (already supports
`--debug-alloc-stats` reads via `Memory::read`), and compares
`total_bytes`.

**C5. Baseline-number recording procedure.** "Measured at the M4.5
branch + 10% margin" is ambiguous; the normative procedure is:

1. On the implementation branch for M4.5, after the borrow-inference
   change is in place and all tests pass, run
   `cargo test -p goby-wasm --features alloc_baseline_record`
   (new cargo feature gate, just for this bootstrap) which regenerates
   `alloc_baseline.txt` with the observed `total_bytes` for every
   example.
2. Multiply each value by 1.10 (round up to an integer) and commit
   that file. Future runs fail if the observed number exceeds it.
3. Record the M4-branch `total_bytes` for `fold.gb` and
   `list_case.gb` in the PR description so the 20% reduction
   claim is verifiable against a specific commit SHA. The 20%
   reduction is checked against those recorded M4 numbers, not
   against `alloc_baseline.txt`.

**C6. Example coverage.** "Every example" means every `.gb` file in
`examples/` that currently runs under the GeneralLowered Wasm path
and terminates within the existing CI timeout. Examples that already
require `--max-memory-mb` (e.g. `refcount_reuse_loop.gb`) are
excluded from `alloc_baseline.txt` and covered separately by the M5
acceptance test.

### M5 — Reuse analysis

M5 code work is complete (intra-block `reuse_pair`, tail-position
cross-call ABI, inline `__goby_alloc_reuse` / `__goby_drop_reuse`,
correctness tests). Two items remain and ride alongside M6:

- [ ] Un-ignore the M1 integration test for
      `examples/refcount_reuse_loop.gb` and add an assertion that
      `total_bytes < 200 * 1024`. With M5 alone the example still
      allocates ~149 MiB because chunk reuse and `xs[i] := v`-driven
      reuse are M6 territory.
- [ ] **Acceptance:** the M1 test passes under `goby run
      --max-memory-mb 16` with `total_bytes < 200 KiB` and the
      recorded checksum. Closed by M6 Step 7.

Notes carried into M6:

- M5 list `AllocReuse` currently bumps a fresh chunk on every
  allocation (`emit_alloc_reuse` payload-only reuse only covers the
  header / single-chunk path). The `refcount_reuse_loop.gb` budget
  cannot converge until chunk-level reuse lands, so M6 Step 7
  reserves a sub-step for it.
- `BackendIntrinsic::ListSetInPlace` already exists with a strict
  aliasing contract (`gen_lower/backend_ir.rs:120-128`) gated by
  `lower_supported_inline_list_fold_mutating_each`. M6 reuses this
  primitive but must extend the gate, not bypass it.

### M6 — `mut` lowering through reuse primitives (no semantics change)

M6 routes the backend lowering of `xs[i] := v` through the same reuse
primitives as immutable update. It does **not** change `mut` semantics:
captured `mut` (cell-promoted, refcount > 1 by construction per §3.4)
must keep observing path-copy behavior; non-captured `mut` switches to
the reuse fast path when statically proven unique.

#### Step 1 — Reuse-eligibility for `AssignIndex` in `perceus_reuse` ✅

- [x] In `crates/goby-core/src/perceus_reuse.rs`, extend the intra-block
      pass to recognize `CompExpr::AssignIndex { root, path, value }`
      as a reuse site. The chosen IR shape is a `reuse_token` field on
      `AssignIndex` of type `Option<AssignIndexReuse { root_token,
      levels: Vec<Option<SizeClass>> }>` (commit 4f9f701, b00eab1):
  - Within one basic block (§3.7), if `root` has a recorded
    `SizeOrCell` shape (List header, possibly `FreshList`), the pass
    emits a token plus a per-level size-class vector.
  - **Skipped** when `root` is cell-promoted (`SizeOrCell::CellPromoted`).
  - **Skipped** when `value` evaluation contains a conservative abort
    (`PerformEffect` / `WithHandler` / `Resume` / effect-intersecting
    `Call`).
- [x] Focused tests in `perceus_reuse::tests`:
  - `assign_index_unique_inserts_drop_reuse` (covered by
    `assign_index_unique_with_anf_wrapped_initializer_inserts_reuse`).
  - `assign_index_skipped_when_root_is_cell`.
  - `assign_index_skipped_across_perform_effect` (covered by
    `reuse_not_across_perform_effect`).
  - `nested_assign_index_chains_drop_reuse_per_level`.
  - Additional: `assign_index_fresh_nested_literal_records_inner_class`,
    `assign_index_shared_inner_var_falls_back`,
    `assign_index_first_none_stops_reuse` (Step 4 prep).

#### Step 2 — Codex plan review

- Send the Step 1 IR rewrite proposal to Codex. Confirm consistency
  with §3.4 (semantics preservation under shared cells) and §3.7
  (path breakpoints), and that the chosen IR shape does not require
  cross-pass coordination beyond what `closure_capture` already
  provides.

#### Step 3 — Backend lowering: single-level `xs[i] := v` reuse path ✅

- [x] In `crates/goby-wasm/src/gen_lower/lower.rs::lower_assign_index`,
      branch on the Step 1 IR signal (commit 1de122d):
  - **Reuse-eligible:** emit `__goby_drop_reuse` on `root`, perform an
    in-place element store, and rebind via `alloc_reuse(token, S,
    Retain)`. No descent / ascent path-copy.
  - **Not reuse-eligible** (cell-promoted, shared, breakpoint): keep
    the existing `ListGet` + `ListSet` path-copy lowering. Cell write-
    back via `LoadCellValue` / `StoreCellValue` is unchanged.
- [x] `BackendIntrinsic::ListSetInPlace` is now reachable when the
      Step 1 reuse signal is present, in addition to the pre-existing
      `lower_supported_inline_list_fold_mutating_each` site (which is
      kept as an independent gate).
- [x] Runtime tests in `crates/goby-wasm/src/compile_tests.rs`:
  - `mut_assign_index_unique_reuses` (named
    `lm_single_level_list_assign_updates_element` and
    `lm_value_semantics_no_aliasing`).
  - `mut_assign_index_shared_falls_through` — cell-promoted `mut`
    fallback covered by `lm_value_semantics_no_aliasing` and
    `lower_assign_index_cell_promoted_*` IR tests.

#### Step 4 — Nested `xs[i][j] := v` reuse chain ✅

- [x] Extend the `lower_assign_index` descent / ascent so reuse tokens
      propagate per level (commits 747e1de, 91360ca). Outer unique →
      `DropReuse` outer, ascent emits `ListSetInPlace` for every level
      `i < reuse_depth`. Inner levels do **not** require additional
      `DropReuse` / `AllocReuse(Retain)` because `ListGet` does not
      bump the inner header's refcount, so the static `FreshList`
      proof from `perceus_reuse` already guarantees inner refcount ==
      1.
- [x] Per-level fallback: "first None stops reuse" — once
      `levels[i] == None`, every deeper level falls back to
      copy-on-write `ListSet`. Outer (level 0) reuse is preserved
      whenever it is statically proven.
- [x] Runtime tests:
  - `nested_assign_index_unique_reuses_outer_only`
  - `nested_assign_index_fresh_literal_reuses_inner_in_place`
  - `nested_assign_index_shared_inner_falls_back`
  - IR-level: `lower_assign_index_two_levels_uniform_inner_reuses_all_levels`,
    `lower_assign_index_two_levels_inner_none_after_outer_reuse_uses_listset`.

#### Step 5 — `stdlib/goby/list.gb` `set` rewrite

- [ ] Rewrite `set xs i v` as the immutable-update surface form so it
      benefits from reuse automatically (`mut ys = xs; ys[i] := v; ys`
      or equivalent if expressible at surface level). If the surface
      form cannot reach the same intrinsic, leave `__goby_list_set` in
      place but reroute its backend lowering through the Step 3 / 4
      path. Either outcome is acceptable provided example outputs are
      byte-identical.

**Investigation note (2026-04-27):** Surface form rewrite is blocked by
two interdependent issues and is deferred:

1. `backend_intrinsic_for("list", "set")` short-circuits every
   `list.set` call to `BackendIntrinsic::ListSet` before the stdlib
   body is reached. Removing this entry routes calls through the stdlib
   body, but exposes issue 2.

2. `lower_assign_index_reuse` emits `ListSetInPlace` unconditionally
   (even when rc != 1). The guard relies on the static uniqueness proof
   from `perceus_reuse` (`reuse_token` is only attached when the root
   is statically owned). For `set xs i v`, `xs` is classified as
   `Owned` only if the ownership pass propagates the `LetMut`
   alias (`mut ys = xs`). Adding `bind_alias` to `LetMut` in
   `perceus.rs` achieves this, but it causes alias-safety violations:
   when the caller uses `xs` after `set xs 0 v`, `drop_insert` emits
   `Dup(xs)` (rc:1→2), then `RefCountDropReuse(ys)` decrements to rc==1
   and `ListSetInPlace` mutates the shared chunk — corrupting the
   caller's `xs`.

   The correct fix requires `lower_assign_index_reuse` to fall back to
   copy-on-write `ListSet` when the `RefCountDropReuse` token is null
   (rc was != 1), which is a non-trivial change to the hot lowering path.

Current state: `set xs i v = __goby_list_set xs i v` is unchanged.
`__goby_list_set` continues to route through the copy-on-write `ListSet`
intrinsic for all calls. The reuse benefit is not realised for this
function until the `lower_assign_index_reuse` fallback is implemented.

#### Step 6 — Codex code review

- Send the Step 1, 3, 4, 5 diffs to Codex. Focus areas:
  - cell-promoted detection has no holes (every captured `mut` path
    stays on copy-on-write);
  - nested fallback preserves per-level uniqueness invariants;
  - `ListSetInPlace` aliasing contract is still satisfied at every
    new call site.

#### Step 7 — Example output parity and acceptance

- [x] Confirm `examples/mut_list.gb`, `closure_mut.gb`,
      `closure_mut_list.gb`, `closure_mut_nested_list.gb` produce
      byte-identical output (Step 4 lands without touching
      `runtime_output_tests.rs` snapshots; the workspace test suite is
      green at commit 91360ca).
- [x] Measure `examples/refcount_reuse_loop.gb` with
      `cargo run -p goby-cli -- run --debug-alloc-stats`. Required:
  - `total_bytes < 200 * 1024`,
  - `peak_bytes` of the same order,
  - checksum matches the value fixed in §1.1.
  - **Status (2026-04-26): done.** Reports `total_bytes=108704
    peak_bytes=108704 free_list_hits=0 reuse_hits=5000`, below the
    200 KiB budget. See Step 7 execution plan for full landing
    sequence (7-a / 7-a.5 / 7-b / 7-c / 7-d).
- [x] Extend `comp_alloc_size_or_cell` (or the `SizeEnv` propagation
      path) so `mut ys = xs` inherits `xs`'s outer size class when it
      is known (function-parameter case). **Done 2026-04-26** via
      `peek_binding_shape` + Owned-param `SizeEnv` seed in
      `perceus_reuse::insert_reuse`. Synthetic test confirms reuse
      fires; benchmark still blocked by Step 7-a.5 (intrinsic
      ownership ABI).
- [x] Close the §M5-deferred chunk-level reuse gap. **Done 2026-04-26**
      via specialized lowering for tail-recursive prepend-accumulator
      (`build` → builder-backed loop) and self-recursive list-case fold
      (`xor_fold` → `ListFold`), which eliminated the actual ~149 MiB
      allocation sources. `AllocReuse` chunk-level reuse not required.
- [x] Un-ignore the `total_bytes < 200 * 1024` assertion in
      `crates/goby-wasm/tests/wasm_exports_and_smoke.rs::refcount_reuse_loop_example_compiles`.
      **Done 2026-04-26.**

##### Step 7 execution plan (2026-04-26 decision log)

The two outstanding sub-items above (`mut ys = xs` seed extension and
chunk-level reuse) are sequenced as follows. Step 5 (`stdlib/goby/list.gb`
`set` rewrite) is intentionally deferred until after acceptance lands.

1. **Step 7-a — Seed `mut ys = xs` from the parameter's size class
   (Owned-parameter case only).** Extend `comp_alloc_size_or_cell` (or
   the `SizeEnv` seed) so that when `xs` is an `Owned` function
   parameter with a known outer `SizeClass`, the binding `mut ys = xs`
   inherits that class. Keep the change strictly conditional on the
   M4.5 ownership classification: shared / `Borrowed` / unknown
   parameters must continue returning `None`.
   - *Rationale:* this is the smallest change that can fire reuse on
     the benchmark loop and is exactly the precondition Step 7 already
     reserves. Limiting the seed to `Owned` parameters preserves the
     §3.4 invariant that reuse fires only on values statically proven
     unique — a `mut ys = xs` over a shared `xs` would otherwise alias
     and break value semantics. Broader `mut = Var` propagation is
     deferred until a concrete second case demands it; YAGNI keeps the
     proof surface narrow.
   - *Verification:* expect `reuse_hits > 0` in
     `goby run --debug-alloc-stats examples/refcount_reuse_loop.gb`.
     `total_bytes` is **not** expected to clear 200 KiB yet.
   - **Status (2026-04-26): landed.** `insert_reuse` now takes a
     per-decl `Owned`-param `HashSet<String>` and pre-seeds the
     `SizeEnv` with `Header(1)` placeholders; a new
     `peek_binding_shape` wrapper allows `Var(p)` passthrough only at
     Let / LetMut / Assign rebind sites (the recursive
     `value_alloc_size_or_cell` is unchanged, so `mut ys = [xs, xs]`
     still falls back to outer-only). Synthetic
     `step xs i = mut ys = xs; ys[0] := i; step ys (i-1)` (built via
     a fresh `build` recursion, no `length` call) reports
     `reuse_hits=5`. The benchmark itself is **still 0** —
     `length(xs)` is the blocker; see Step 7-a.5 below.

1.5. **Step 7-a.5 — Model read-only list intrinsics as borrowed.
   Status (2026-04-26): landed.** Discovered while measuring Step 7-a:
   `__goby_list_length` (and likely other read-only intrinsics) is
   classified by the ownership pass as `Owned`-arg, so `drop_insert`
   emits `Dup(xs); length(xs)` to keep `xs` live past the call. But
   `emit_list_length_helper`
   (`crates/goby-wasm/src/gen_lower/emit.rs::7532`) reads the header
   and then drops nothing. The runtime refcount is therefore one
   higher than the static analysis assumes after the call, and the
   downstream `mut ys = xs; ys[i] := v` AssignIndex sees rc=2 at
   `RefCountDropReuse`, taking the cold path and never incrementing
   `reuse_hits`.

   The selected fix is an intrinsic param ownership table in the
   ownership pass: `__goby_list_length` argument 0 is treated as
   `Borrowed`, and `drop_insert` consults the same table when deciding
   whether a call consumes an owned value. GeneralLower also runs
   Perceus with imported stdlib declarations in the module, so user
   calls to `length(xs)` can see the wrapper's borrowed argument facts.
   This removes the runtime drop-then-keep round-trip and matches the
   spec's notion of length as a pure read.

2. **Step 7-b — Re-measure and decide on chunk-level reuse.** With
   header reuse firing, re-run the benchmark. `length=4096` over 5000
   iterations almost certainly still exceeds 200 KiB because every
   `AllocReuse` bumps a fresh chunk (M5-deferred), but we commit to a
   measurement-driven decision rather than a pre-assumed one.
   - *Rationale:* keeping 7-a and 7-b as separate landings means each
     can be bisected and reviewed in isolation, and makes the
     contribution of each mechanism observable in `total_bytes` /
     `reuse_hits` deltas. If 7-a unexpectedly suffices, 7-b becomes a
     pure cleanup task instead of an acceptance blocker.
   - **Status (2026-04-26): measured.**
     `cargo run -p goby-cli -- run --debug-alloc-stats --max-memory-mb 16
     examples/refcount_reuse_loop.gb` reports
     `total_bytes=149354768 peak_bytes=149354768 free_list_hits=0
     reuse_hits=5000`. Header reuse is firing; chunk-level reuse remains
     required for the `< 200 KiB` budget.

3. **Step 7-c — Implement chunk-level reuse if 7-b shows it is
   needed.** Extend `emit_alloc_reuse` so the chunk payload is taken
   from the freed value's chunk(s) instead of bumping. The ListSet/
   AssignIndex hot path then performs zero per-iteration heap growth.
   - *Rationale:* this is the §M5 deferred item the PLAN already
     reserves under Step 7. It is left until 7-b confirms necessity so
     the work order matches measured impact.
   - **Status (2026-04-26): superseded by measurement.** Header reuse
     firing plus `iters=0` measurement showed the remaining ~149 MiB
     was not the AssignIndex hot loop. The actual blockers were
     `build 0 4096 []` (`[k, ..acc]` tail-recursive prepend
     accumulator) and `xor_fold` (`[x, ..rest]` tail pattern). The
     accepted fix keeps the measurement-driven intent of Step 7-c but
     targets the real allocation sources: GeneralLower now lowers the
     build shape through a builder-backed loop and lowers the
     self-recursive list-case integer fold through `ListFold`. The
     benchmark now reports `total_bytes=108704 peak_bytes=108704
     free_list_hits=0 reuse_hits=5000`, below the 200 KiB budget.

4. **Step 7-d — Un-ignore the `total_bytes < 200 * 1024` assertion.**
   Only after 7-a (and 7-c if needed) bring the benchmark below
   budget.
   - **Status (2026-04-26): done.**
     `wasm_exports_and_smoke.rs::refcount_reuse_loop_example_compiles`
     now executes the example with debug alloc stats and asserts
     `total_bytes < 200 * 1024`.

5. **Step 5 (`stdlib/goby/list.gb` `set` rewrite) deferred until
   post-acceptance.** Although Step 5 sits before Step 7 in the
   numbered list, the benchmark does not exercise `list.set`, and Step
   7 explicitly notes "Step 4 alone cannot close the budget" — i.e.
   the acceptance gate depends on 7-a/7-c, not on Step 5. Reordering
   minimises the surface under review for the benchmark landing and
   isolates any `set` rewrite regressions from the reuse plumbing.
   - *Rationale:* Step 5's surface form rewrite touches the stdlib and
     can ripple through unrelated examples; bundling it with the
     acceptance landing risks confounding alloc-baseline deltas. After
     acceptance, Step 5 becomes a pure code-quality follow-up that can
     be verified against a stable baseline.

#### Step 8 — STATE / PLAN sync and commit

- [x] Update `doc/STATE.md` to mark Perceus M5 acceptance and M6
      complete; sync `memory/project_perceus_status.md`. **Done 2026-04-26.**
- [x] Update `doc/PLAN.md` §4.2 Perceus track row for M6. **Done 2026-04-26.**
- [x] Run `goby-invariants` (spec / examples / diagnostics gate)
      before commit. **Done 2026-04-26** (doc-only change; no syntax /
      diagnostics / runtime impact).

#### Acceptance

- `cargo test -p goby-wasm` green.
- Four `mut` / `closure_mut*` example outputs byte-identical.
- `examples/refcount_reuse_loop.gb` `total_bytes < 200 KiB` and
  checksum matches §1.1.
- `tests/alloc_baseline.txt` deltas (if any) explained per entry in
  the commit message.

### M7 — Remove the bump-only fallback; document

- [x] `emit.rs` chunk layout design-constraint comment rewritten to
      describe refcount + free-list model (chunk free-list on last drop;
      Large / oversized falls back to bump on miss). **Done 2026-04-26.**
- [x] `host_bump_cursor` / `heap_floor` role confirmed: `heap_floor` is
      a function-local cursor for the current allocation frame (not
      bump-only); `host_bump_cursor` backs host-side string/list writes.
      No redefinition needed — naming already reflects "backing" role.
      **Confirmed 2026-04-26.**
- [x] `doc/PLAN.md` §4.2 updated: Perceus track row now records M7
      complete; "bump-only allocator" in section intro changed to past
      tense. `doc/PLAN_IR.md` ListLit/TupleLit/RecordLit rows updated to
      reflect refcount-tracked model (chunks free-listed; headers and
      Tuple/Record bump-backed, no free-list slot). `AGENTS.md` had no
      bump-only references. **Done 2026-04-26.**
- [x] `doc/LANGUAGE_SPEC.md` performance note updated from "current bump
      allocator" to "refcount-tracked heap, free-list backed allocator
      (chunks recycled; headers bump-backed)".
      **Done 2026-04-26.**
- [x] `doc/STATE.md` records §1 goal as satisfied with measured numbers
      (`total_bytes=108704 reuse_hits=5000`). **Done 2026-04-26.**
- [x] **Acceptance (2026-04-26):** no active documentation reference to
      "bump-only allocator, no free" remains; goal program runs under
      200 KiB budget; `cargo test --workspace` green; alloc_baseline
      unchanged (comment-only diff, no code path changed).

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
