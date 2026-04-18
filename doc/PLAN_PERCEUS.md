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
