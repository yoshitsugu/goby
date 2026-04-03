# Goby Runtime Memory Plan

Last updated: 2026-04-03

Status: active

This document is the active planning note for Goby's Wasm/runtime memory model.
It is intentionally design-first:

- define the ownership boundaries before changing constants or allocator code,
- prefer shared runtime rules over path-specific memory exceptions,
- treat fixed-size bumps as transitional implementation details, not long-term policy.

Related documents:

- `doc/LANGUAGE_SPEC.md` — current user-visible language semantics
- `doc/PLAN.md` — top-level roadmap and active language direction
- `doc/PLAN_IR.md` — lowering and backend-boundary reference
- `doc/STATE.md` — restart notes and implementation checkpoints

Plan-label hygiene:

- labels in this document are planning-only metadata
- do not copy them into code comments, diagnostics, or user-visible strings
- when implementation notes need context, describe the technical purpose directly

---

## 1. Goal

Give Goby's Wasm execution paths a memory strategy that is:

- predictable,
- resilient to moderate debug output and string-heavy workloads,
- not dependent on repeatedly hand-tuning fixed byte constants,
- compatible with the current bump-allocation runtime,
- open to future garbage collection without forcing GC now,
- proven by representative memory-pressure regressions that execute successfully
  under the intended bounded-growth policy.

The immediate target is not a full managed runtime. The target is a practical
memory model that stops small source edits such as additional `println` or
string interpolation from causing arbitrary out-of-bounds failures.

The end-state for this plan is not only a design document or a refactor. It is
an implementation plus regression coverage showing that memory-heavy but
reasonable workloads run successfully below the configured maximum, and fail
with explicit exhaustion only when that maximum is intentionally exceeded.

---

## 2. Problem Statement

Recent runtime failures exposed two structural issues:

1. The current implementation relies on small fixed memory reservations.
2. Some boundaries still assume a single fixed Wasm memory size rather than a
   growable runtime memory.

Today Goby has multiple memory consumers with different lifetimes:

- Wasm-side persistent heap data allocated by compiled Goby code,
- host-written temporary string payloads for runtime intrinsics,
- runtime I/O scratch regions,
- Wasmtime-managed execution stack,
- static payload regions embedded into the generated module.

The current behavior is fragile because these concerns are only partially
separated. A fixed top-of-memory host bump can work for a prototype, but it
creates recurring failure modes:

- debug printing and interpolation can exhaust temporary space,
- changing one reserved region can silently invalidate another boundary,
- constant-based bounds checks become stale when initial memory layout changes,
- increasing one fixed size often forces increasing several others by hand.

The project needs a more honest rule:

- start from a modest initial memory footprint,
- grow memory when transient or heap demand requires it,
- report explicit exhaustion when growth fails,
- keep GC as a later architectural track rather than an implicit near-term fix.

---

## 3. Design Rules

1. **Runtime memory must be growable.**
   Fixed initial sizes are allowed, but runtime correctness must not depend on
   one exact `WASM_PAGE_BYTES` constant.

2. **Runtime memory growth must also be bounded.**
   Goby must not treat linear memory as unbounded. The memory policy must define
   a realistic maximum page count and fail explicitly when that ceiling is reached.

3. **Ownership boundaries must be explicit.**
   Wasm heap allocation, host temporary allocation, and stack limits must be
   configured separately even if they share one top-level memory policy.

4. **Memory failure must be explicit.**
   If allocation cannot succeed after permitted growth, Goby should return a
   structured runtime error such as memory exhaustion rather than drift into
   pointer corruption or generic out-of-bounds traps.

5. **Do not introduce symbol-specific memory exceptions.**
   `println`, interpolation, grapheme helpers, and future intrinsics must all
   consume the same shared memory policy rather than each reserving ad hoc space.

6. **Prefer growth over premature GC.**
   If a workload is failing only because fixed scratch space is too small, the
   first fix is dynamic growth. GC should be introduced only when retention and
   long-lived allocation pressure become the real bottleneck.

7. **One source of truth for memory configuration.**
   Initial pages, growth limits, temporary-region policy, and stack defaults
   must come from one shared module rather than duplicated backend/runtime constants.

---

## 4. Non-Goals

This plan does not attempt to deliver:

- a moving or compacting garbage collector in the first implementation slice,
- object relocation across existing runtime representations,
- a full generational or tracing runtime,
- per-feature allocator policies for one stdlib helper or one syntax feature,
- a promise that all allocations become reclaimable immediately.

GC remains a valid future direction, but it is not required to solve the
current class of memory-capacity regressions.

---

## 5. Current Model Snapshot

The current Goby runtime effectively uses:

- a Wasm-side upward-growing heap,
- a host-owned temporary bump region near the top of initial linear memory,
- fixed offsets for some runtime I/O and cursor state,
- a separate Wasmtime stack limit configured in the runner.

This model is acceptable as a starting point because:

- it is simple,
- it matches the current compiled representation,
- it avoids introducing tracing or root scanning immediately.

But it needs one architectural correction:

- the layout must become *grow-aware* rather than *fixed-page-aware*.

That means any component that currently asks "is this still within
`WASM_PAGE_BYTES`?" needs to move toward "is this within current linear memory,
and if not, can memory be grown before the write?"

---

## 6. Design Direction

### 6.1 Initial memory stays modest

Goby should keep a moderate initial memory footprint rather than reserving a
large fixed memory size just to avoid implementation work.

Near-term guidance:

- choose an initial linear-memory size in the low-page range that comfortably
  covers ordinary startup, static data, and small programs,
- keep stack limits separate from linear-memory size,
- avoid tying temporary host allocation capacity to the full initial memory size.

The implementation now locks `256 KiB` (`4` Wasm pages) as the default initial
size for this track, but it remains only a starting point, not the maximum
supported workload.

### 6.2 Runtime growth must stop at a realistic maximum

Dynamic growth is a pressure-release mechanism, not permission to consume
unbounded memory.

Near-term default policy:

- choose a modest initial linear-memory size,
- allow automatic growth up to a fixed default maximum,
- make that maximum explicit in one shared runtime configuration,
- return a stable exhaustion error once the maximum is reached.

Locked default for the first implementation slice:

- initial linear memory: `256 KiB` (`4` Wasm pages)
- default maximum linear memory: `64 MiB` (`1024` Wasm pages)

Rationale:

- `64 MiB` is large enough for current CLI-scale Goby programs, debug-heavy
  runs, and transient string workloads without frequent tuning,
- it is still small enough to avoid treating the runtime as effectively
  unbounded,
- it keeps the failure mode understandable when a program genuinely allocates
  too much or enters accidental runaway growth.

This default maximum should remain configurable in implementation, but the
system must always have a bounded ceiling even when the user does not override it.

### 6.3 Runtime growth is the primary pressure-release mechanism

When compiled code or host intrinsics need more linear memory, Goby should:

1. compute the required additional capacity,
2. inspect the current memory size,
3. request `memory.grow` when the current allocation is insufficient,
4. update any local size/cache state after growth,
5. fail with explicit runtime exhaustion if growth is denied.

This applies to:

- host-written temporary string payloads,
- future list/string helper scratch buffers,
- Wasm-side heap allocation once the current heap cursor approaches the end of
  available memory.

Growth requests must respect the configured maximum:

- if the next allocation fits after growth within the maximum, grow and continue,
- if satisfying the request would exceed the maximum, return explicit exhaustion.

### 6.4 Temporary host allocation must stop pretending to be a fixed carve-out

The current host bump region is a useful implementation technique, but it
should become policy-driven rather than hard-coded as "the top N bytes of one
page forever."

Near-term direction:

- keep host temporary allocation as a bump allocator,
- allow that allocator to grow the underlying Wasm memory when needed,
- preserve a simple monotonic allocation model within one `_start` execution,
- reset temporary state per execution rather than attempting immediate reuse.

This keeps the implementation simple while removing the smallest fixed-capacity
failure mode.

### 6.5 One address-space rule must govern host temp and Wasm heap allocation

Dynamic growth is only safe if Goby has one explicit rule for how host temporary
allocation and compiled Wasm heap allocation share linear memory.

The first implementation should lock the following contract:

- both host temporary payloads and Goby heap objects live in the same Wasm
  linear memory,
- Goby heap allocation remains the long-lived upward-growing allocation authority,
- host temporary allocation uses a separate execution-local region above the
  current heap frontier rather than owning an unrelated fixed "top of memory"
  carve-out,
- `memory.grow` only increases total capacity; it does not change the ownership
  rule for which side may allocate where,
- any helper that allocates host temporary memory must consult the shared memory
  layout/configuration layer before reserving bytes.

This means the implementation must not let host helpers and compiled heap code
grow "independently" with separate hidden address-space assumptions. They may
use different allocators, but they must consume one coherent linear-memory
layout contract.

The exact representation can still evolve, but the following are not acceptable:

- one-off reserved gaps that exist only for `println` or one intrinsic,
- a host allocator that assumes "top of current memory" is always free without
  checking the shared heap frontier,
- a Wasm heap allocator that assumes host temporary writes cannot expand into
  its reachable space.

### 6.6 Separate short-lived temporary pressure from long-lived heap pressure

Not all allocation pressure means the same thing:

- temporary host strings created for printing or interpolation are mostly
  execution-local scratch data,
- Goby heap objects created by user code are language-level values that may
  need to survive much longer.

The memory plan should therefore preserve the conceptual distinction even if
both currently live in one Wasm linear memory:

- temporary allocation policy is reset-oriented and growth-friendly,
- heap allocation policy is persistent and future-GC-compatible.

This separation matters because GC may later reclaim heap objects, but it
should not be required to make debug-print scratch space workable today.

### 6.7 GC is a future architectural track, not the first fix

Garbage collection becomes compelling when Goby needs to reclaim long-lived
heap objects during one execution rather than merely survive larger transient
workloads.

A future GC-capable runtime would need to define:

- object layout and metadata,
- root discovery across locals, call frames, closures, and runtime structures,
- pointer-update rules if objects can move,
- interaction with Wasm tables, host imports, and tagged values,
- behavior for host-owned temporary data.

That is substantially larger than the immediate need. This plan therefore locks
the sequence:

- first: dynamic memory growth with explicit exhaustion,
- later: optional non-moving GC or another reclaim strategy for heap values,
- only after that: consider compaction/moving GC if fragmentation and retained
  memory become real measured problems.

---

## 7. Required Runtime Semantics

The memory model is acceptable only when all of the following are true:

1. Small increases in debug printing, string interpolation, or host-backed
   string operations do not require changing compile-time byte constants.
2. Memory writes never intentionally target reserved metadata slots or depend
   on accidental layout overlap.
3. Host-backed runtime helpers either succeed after memory growth or fail with
   explicit exhaustion; they do not return non-pointer fallback values that are
   later misinterpreted as strings or lists.
4. Memory growth always respects a configured maximum and does not silently
   turn Goby execution into an unbounded-memory process.
5. Host temporary allocation and Wasm heap allocation follow one explicit
   shared address-space rule and cannot overlap by convention or accident.
6. Wasm-side heap allocation follows the same rule: grow if allowed, otherwise
   report exhaustion.
7. Backend/runtime code paths that generate or execute modules use the same
   memory configuration source and the same growth assumptions.
8. The chosen design does not block a later non-moving or moving GC design.
9. Representative memory-heavy regression programs execute successfully under
   the default bounded-growth policy without requiring per-test memory tuning.

---

## 8. Architecture Work Items

### 8.1 Shared memory configuration module

Introduce one module that owns:

- initial memory pages,
- default and maximum memory pages,
- temporary-allocation growth policy,
- stack-limit defaults,
- reserved metadata offsets and alignment rules,
- the shared address-space rule between host temporary allocation and Wasm heap allocation.

Done when:

- backend emitters, runtime execution, and host intrinsics no longer duplicate
  page-size, initial-memory, or maximum-memory constants independently,
- the chosen default initial size and default maximum are recorded in one place,
- generated-module emission and runtime execution read from the same memory-configuration source.

### 8.2 Grow-aware linear-memory helpers

Replace fixed-capacity checks with helpers that:

- read current memory size from the active Wasm memory,
- compute required size for upcoming writes,
- perform growth before the write when necessary,
- return structured errors when growth fails.

Done when:

- host runtime code no longer treats one compile-time page-size constant as the
  authoritative runtime capacity,
- at least one regression proves host-backed string work can exceed the initial
  page count and still succeed while staying under the configured maximum.

### 8.3 Host temporary allocator migration

Refactor the current host bump allocator so it:

- allocates within current linear memory bounds,
- requests growth when its next allocation would exceed capacity,
- keeps its monotonic execution-local semantics,
- reports explicit exhaustion on failure.

Done when:

- stringification and concatenation workloads no longer depend on a small fixed
  reserved region to avoid crashes,
- host temporary allocation is defined relative to the shared linear-memory layout
  rather than a hard-coded top-of-page carve-out,
- a focused regression proves host temporary allocation does not overlap the
  compiled heap frontier while growing under load.

### 8.4 Wasm heap-growth boundary

Audit Wasm-side allocation paths and heap-cursor management so they:

- understand current memory capacity,
- can request more pages when the heap reaches the current end,
- preserve existing tagged-value layout and ABI,
- keep ownership of heap policy in one backend/runtime boundary.

Done when:

- compiled Goby heap allocation is no longer hard-capped by the initial module
  page count,
- a focused regression proves heap allocation can exceed the initial page count
  without using host temporary allocation,
- the heap allocator and host temporary allocator both use the documented shared
  address-space rule.

### 8.5 Exhaustion diagnostics

Define one runtime error path for allocation failure that can be reused by:

- host temporary allocation,
- Wasm heap allocation,
- future runtime-managed buffers.

Done when:

- failing to allocate memory produces a stable Goby runtime error instead of
  an opaque Wasm trap or corrupted-pointer follow-on failure,
- at least one regression with an intentionally low configured maximum proves
  the documented exhaustion error appears for host temporary allocation,
- at least one separate regression proves the same for Wasm heap allocation.

### 8.6 Follow-up design note for reclamation

After growth-based stability lands, record a narrower follow-up note covering:

- whether Goby should adopt non-moving mark/sweep first,
- whether temporary host data should remain bump-only,
- whether compaction is justified by measured fragmentation rather than by design preference.

Done when:

- GC discussion is grounded in retained-heap pressure and runtime measurements,
  not as a reflexive response to temporary scratch exhaustion.

---

## 9. Milestones

### 9.1 M0: Boundary lock

- [x] Identify every duplicated memory-capacity constant in backend/runtime code.
- [x] Lock the ownership split between initial memory config, host temporary allocation,
  Wasm heap allocation, and Wasmtime stack configuration.
- [x] Choose one initial-memory default, one default maximum, and one growth policy
  for the first implementation.
- [x] Lock one explicit shared address-space rule for host temporary allocation
  and Wasm heap allocation.

Done when:

- memory behavior is described by one coherent model rather than several local constants,
- the document states one default initial size, one default maximum, and one
  shared linear-memory ownership rule,
- no open implementation-critical question remains about whether host temp and
  heap allocations may claim overlapping space.

### 9.2 M1: Shared configuration and growth helpers

- [x] Add the shared memory-configuration module.
- [x] Add grow-aware helper APIs for host writes.
- [x] Update existing host runtime call sites to use those helpers.

Done when:

- host-backed string work is no longer fixed-page-specific,
- the relevant regressions pass when total memory use exceeds the initial page
  count but stays under the configured maximum.

### 9.3 M2: Heap allocation growth

- [ ] Audit compiled-Wasm heap allocation writes and cursor checks.
- [ ] Introduce growth before heap exhaustion.
- [ ] Add focused regressions for heap growth without host temporary pressure.

Done when:

- ordinary Goby heap allocation is not limited to the initial page count,
- the heap-growth regression passes under the shared address-space rule and
  below the configured maximum.

### 9.4 M3: Explicit exhaustion behavior

- [ ] Add stable runtime errors for failed memory growth.
- [ ] Cover host temporary exhaustion and heap exhaustion separately in tests.
- [ ] Ensure CLI/runtime surfaces render these failures as Goby runtime errors.

Done when:

- memory-capacity failure is diagnosable and deterministic,
- the configured-maximum regressions fail with the documented Goby exhaustion
  error rather than a generic Wasm trap.

### 9.5 M4: Memory-pressure proof set

- [ ] Add at least one representative memory-heavy success regression that exercises
  repeated string/interpolation or print-oriented temporary allocation under the
  default bounded-growth policy.
- [ ] Add at least one representative memory-heavy success regression that exercises
  heap growth under the default bounded-growth policy without relying on host
  temporary allocation as the primary pressure source.
- [ ] Verify these regressions pass without per-test manual memory-constant changes.

Done when:

- the repository contains focused memory-pressure regressions for both temporary
  allocation pressure and heap pressure,
- those regressions pass under the default memory policy,
- the plan is no longer justified only by small synthetic examples or by one-off
  debugging sessions.

### 9.6 M5: Reclamation follow-up decision

- [ ] Measure representative workloads after dynamic growth lands.
- [ ] Decide whether the next pressure point is retained heap growth, fragmentation,
  startup footprint, or none of the above.
- [ ] If needed, open a separate GC/reclamation plan with explicit scope and acceptance criteria.

Done when:

- GC is either deferred consciously or started from measured evidence.

---

## 10. Testing Strategy

Testing should focus on behavior classes, not one fixture program.

Required regression categories:

- repeated `println` and interpolation workloads that previously exhausted temporary space,
- recursive helper plus callback execution where host strings are produced inside nested calls,
- heap-growth scenarios that exceed the initial page count without involving host stringification,
- at least one broader memory-pressure success case large enough to require runtime growth
  under the default policy,
- explicit allocation-failure tests using an intentionally low configured maximum,
- parity checks across the execution paths that share the Goby-owned Wasm runtime boundary.

Anti-patterns:

- tests that encode one AoC solution directly into the repository,
- symbol-specific regressions that only mention one helper when the bug is allocator-wide,
- success criteria defined only as "no trap" without checking the intended output or error class.

---

## 11. Open Questions

1. Should the default maximum remain a single global ceiling, or should Goby
   later separate development/debug defaults from stricter embedded defaults?
2. Should host temporary allocations continue to live in linear memory, or is
   there a better host-side representation that avoids copying for some cases?
3. Does Goby want one shared allocator policy for all host intrinsics, or
   should some future intrinsics allocate directly in Wasm heap space?
4. Is a non-moving collector the preferred first reclamation step if GC becomes
   necessary, because it preserves pointer stability across current tagged values?
5. Which workloads should become the canonical memory-pressure benchmarks after
   the first growth-based slice lands?

Locked default for the first implementation:

- one global default maximum applies unless and until Goby grows an explicit
  profile/configuration surface,
- the first implementation target is `64 MiB` maximum linear memory,
- this value is a runtime policy default, not permission for unbounded growth.

---

## 12. Review Checklist

1. Does the implementation remove fixed-size assumptions at the shared memory boundary
   rather than only increasing constants again?
2. Are host temporary allocation and Wasm heap allocation handled by one coherent policy
   but still kept conceptually separate?
3. If growth fails, does Goby report explicit exhaustion instead of surfacing a generic trap?
4. Does every growth path respect the configured maximum rather than reintroducing
   an effectively unbounded allocator through one helper?
5. Did the change avoid introducing helper-specific or symbol-specific allocator branches?
6. Does the resulting design still leave room for a future GC without forcing GC now?
