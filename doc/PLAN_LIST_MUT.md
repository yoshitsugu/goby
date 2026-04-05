# Goby Mutable List Plan

Last updated: 2026-04-05

This document is the active implementation plan for mutable list updates such as:

```goby
mut a = [1, 2, 3]
a[1] := 10
```

The goal is to extend the existing `mut` / `:=` surface syntax without turning
list mutation into a one-off parser or backend exception.

This document is intentionally architecture-first:

- the language should stay immutable by default,
- `mut` should remain the single opt-in for local mutability,
- list element mutation should reuse the same assignment model as ordinary
  mutable bindings,
- nested list updates should work by the same rule as one-level updates,
- reads from lists should remain value-oriented rather than introducing ambient
  shared mutable aliases,
- the implementation should introduce a reusable deep module boundary rather
  than scattering special cases across parser, typechecker, lowering, and
  runtime code.

Plan-label hygiene:

- labels in this document such as milestone names are planning-only metadata
- do not copy them into code comments, diagnostics, or user-visible strings

---

## 1. Desired Semantics

### 1.1 Surface Language

Locked target surface:

```goby
mut a = [1, 2, 3]
a[1] := 10
a # -> [1, 10, 3]
```

Nested indexing should follow the same syntax:

```goby
mut a = [[1, 2], [3, 4]]
a[1][0] := 99
a # -> [[1, 2], [99, 4]]
```

### 1.2 Mutability Rule

- `List` values bound by ordinary `name = expr` remain immutable.
- Element assignment is valid only when the root storage being updated comes
  from a `mut` binding.
- `a[1] := 10` is therefore legal only if `a` is mutable.
- `b[1] := 10` must be rejected when `b` was introduced with immutable binding
  syntax.

This keeps the language rule simple: mutation is not a property of the `List`
type itself, but of the storage location reachable from a mutable binding.

### 1.3 Read and Alias Semantics

Locked semantic direction:

- list reads are **value reads**, not shared mutable alias creation
- `mut` marks an update-capable root binding, not a globally mutable list object
- reading from a mutable list binding into another binding does not make the new
  binding observe future updates automatically
- storing a read result into a new `mut` binding creates a new mutable root for
  that read value; it does not retroactively share update identity with the
  source root
- this is a general rule for list-producing expressions, not a local-binding
  exception:
  - passing `a[0]` as a function argument reads a value
  - returning `a[0]` from a function returns a value
  - capturing a previously-read list value in a closure captures that value,
    not a shared mutable alias to the original root

Representative examples:

```goby
mut a = [[1, 2, 3], [4, 5, 6]]
b = a[0]
a[0][1] := 10

a[0][1] # -> 10
b[1]    # -> 2
```

```goby
a = [[1, 2, 3], [4, 5, 6]]
mut b = a[0]
b[0] := 10

a[0][0] # -> 1
b[0]    # -> 10
```

The intended user-visible model is therefore:

- mutable list update is a **rooted update** through one `mut` binding,
- nested indexed assignment updates the current value reachable from that root,
- values read from lists behave as ordinary values across bindings, calls,
  returns, and closures; they do not become shared mutable references.

### 1.4 Runtime Meaning

- A mutable binding owns a mutable cell.
- If the current value in that cell is a list, indexed assignment computes an
  updated value for the selected path and stores the resulting value back
  through the same mutable root.
- Nested updates such as `a[1][0] := 99` are interpreted as one assignment to a
  derived mutable place rooted at `a`.
- The observable semantics are value-oriented:
  - readers that previously extracted `a[1]` keep observing the earlier value
  - the implementation may use copying, path-copying, structural sharing, or an
    equivalent internal optimization, as long as the observable behavior matches
    the rooted-update model

This implies that the implementation needs a real notion of a mutable place
path, not only a variable-name assignment primitive.

---

## 2. Architectural Direction

### 2.1 Core Design Rule

Do not implement list mutation as:

- a parser-only special case for `name[index] := expr`,
- a typechecker-only special case that still lowers to variable assignment,
- a backend-only rewrite for one specific AST shape,
- a shallow patch that handles only `a[i]` but not `a[i][j]`.

Instead, introduce one shared abstraction for **assignment targets / mutable
places** and let all relevant phases depend on that abstraction.

### 2.2 Recommended Ownership Split

The compiler should converge on the following conceptual boundary:

- parsing owns recognition of assignable surface forms,
- place analysis in the resolved-form layer owns normalization of assignment
  targets into a structured mutable-place representation,
- typechecking owns legality of mutating that place,
- lowering/runtime own execution of a validated mutable-place write.

The key deep module should be the mutable-place layer. Its public contract
should be small:

- represent the root mutable binding,
- represent zero or more derived projections such as list indexing,
- expose enough information for typechecking and lowering,
- hide surface-shape details so later extensions can reuse it.

Canonical boundary decision:

- parsed AST should stay close to the written source shape so it remains a good
  fit for diagnostics and source-oriented tooling
- the resolved-form layer is the canonical owner of normalized mutable-place
  structure
- lowering and backend layers should consume that normalized resolved-form
  structure rather than re-deriving writable paths from surface syntax

If this boundary is good, future writable projections such as record fields or
other containers can reuse the same model instead of re-opening every phase.

### 2.3 Rooted Mutability

The essential invariant is:

- writability is decided at the root binding,
- projections derive from that root,
- projections do not become writable by themselves.

That avoids unclear semantics such as "mutable list values" floating around
independently from binding ownership.

### 2.4 Observable Contract vs Internal Strategy

The language contract should lock:

- which roots are writable,
- what reads observe before and after rooted updates,
- how nested updates behave from the user's perspective.

The implementation strategy should remain flexible:

- the plan does **not** require one specific runtime technique such as classic
  copy-on-write,
- the implementation may choose path-copying, structural sharing, or another
  representation,
- internal optimization must not change the user-visible value semantics locked
  above.

---

## 3. Scope and Non-Goals

### 3.1 In Scope

- mutable list element assignment through existing `:=` syntax
- nested list mutation through chained indexing
- value-oriented read semantics for mutable and immutable list combinations
- value-oriented read semantics as a general rule for list-producing
  expressions, including bindings, call arguments, returns, and closures
- clear diagnostics for immutable-root or non-list assignment targets
- parser / typechecker / lowering / runtime alignment
- regression tests for direct, nested, and edge-case ownership combinations

### 3.2 Out of Scope for This Plan

- introducing a separate mutable list type
- changing list literal syntax
- making immutable bindings silently copy-on-write
- adding record-field mutation or general reference types in the same slice
- broad runtime container redesign beyond what mutable-place support requires

---

## 4. Milestones

### Milestone LM0: Semantics Lock

- [ ] Define the exact language rule in `doc/LANGUAGE_SPEC.md` for:
  - mutable list element assignment
  - nested indexed assignment
  - immutable-by-default rejection
  - read semantics for values extracted from mutable and immutable list roots
  - creating a new mutable root from a read value
  - value-oriented list reads as a general rule across bindings, call
    arguments, returns, and closures
  - out-of-bounds behavior for indexed assignment
- [ ] Update `doc/PLAN.md` with the roadmap-level decision and scope boundary.
- [ ] Add representative examples covering:
  - mutable one-level list update
  - mutable nested list update
  - invalid assignment through immutable binding
  - immutable read from mutable root staying unchanged after later updates
  - mutable root created from immutable read updating independently
- [ ] Plan a dedicated executable/spec example at `examples/mut_list.gb` that
  records the intended results for the edge cases most likely to confuse users.

Done when:

- there is one stable semantic story that parser, checker, lowering, runtime,
  and examples all target.

### Milestone LM1: Introduce a Shared Mutable-Place Representation

- [ ] Keep parsed AST close to written syntax while extending it only as needed
  to represent assignable surface forms honestly.
- [ ] Replace variable-name-only assignment targets with a structured target
  model in the resolved-form layer as the canonical mutable-place boundary.
- [ ] Support at least:
  - root mutable local binding
  - derived list-index projection
  - repeated projection chaining
- [ ] Keep the representation small and reusable enough to survive future
  writable projections.
- [ ] Refactor existing variable assignment to use that same resolved-form
  representation rather than leaving two unrelated target models.

Done when:

- `x := y` and `a[1] := y` share one canonical assignment-target pipeline rooted
  in resolved form.

### Milestone LM2: Typechecking and Validation

- [ ] Validate that the root of an assignment target is a declared mutable
  binding.
- [ ] Validate every list-index projection:
  - receiver type must be `List a`
  - index expression must be `Int`
  - assigned value must match the projected element type
- [ ] Ensure nested updates are checked recursively rather than by ad hoc shape
  matching.
- [ ] Produce specific diagnostics for:
  - immutable root binding
  - undeclared root binding
  - indexing into non-list values
  - non-`Int` index
  - assigned value type mismatch

Done when:

- writable-place legality is decided completely before lowering/runtime.

### Milestone LM3: Lowering and Runtime Execution

- [ ] Extend resolved-form and shared IR boundaries so writable-place updates
  survive beyond the parser/typechecker honestly.
- [ ] Lower a validated mutable-place write without collapsing nested updates
  back into one-off variable-name logic.
- [ ] Implement runtime/backend support for rooted list updates at arbitrary
  nested depth reachable from a mutable binding while preserving the locked
  value-oriented read semantics.
- [ ] Preserve current immutable-binding behavior and current read-only list
  indexing behavior.

Done when:

- the execution path for `a[1] := v` and `a[1][0] := v` is principled and does
  not depend on frontend-only desugaring tricks.

### Milestone LM4: Regression Lock and Cleanup

- [ ] Add parser tests for valid and invalid writable targets.
- [ ] Add typechecker tests for mutable and immutable root behavior.
- [ ] Add lowering/runtime tests for:
  - one-level list mutation
  - nested list mutation
  - immutable read from mutable root remaining unchanged after later rooted
    update
  - mutable root created from immutable read updating independently from the
    source binding
  - function-argument reads preserving value semantics
  - function-returned reads preserving value semantics
  - closure-captured read values preserving value semantics
  - mutation observed through closures sharing the same `mut` binding, if the
    current closure model allows that path without changing the locked
    value-oriented read semantics
  - invalid nested writable targets:
    - `mut a = [1, 2]; x = a; x[0] := 3` must fail because `x` is immutable
    - `mut a = [[1, 2]]; x = a[0]; x[0] := 3` must fail because extracted `x`
      is immutable
    - `mut a = [1, 2]; a[0][1] := 3` must fail because the intermediate value is
      not a list
- [ ] Add `examples/mut_list.gb` with expected-result comments for:
  - direct mutable update
  - nested mutable update
  - immutable binding rejection cases
  - `mut a` then `b = a[0]` snapshot behavior
  - `a = ...` then `mut b = a[0]` independent-root behavior
  - passing extracted list values through function calls
  - returning extracted list values from helper functions
  - capturing extracted list values in closures
  - invalid nested writable targets:
    - immutable alias of a mutable root cannot be updated
    - extracted immutable nested list cannot be updated
    - nested update through a non-list intermediate must be rejected
  - any additional mixed mutable/immutable list combinations that proved easy
    to misunderstand during implementation
- [ ] Remove temporary compatibility code or duplicate assignment paths left
  behind during migration.
- [ ] Run quality gates:
  - `cargo fmt`
  - `cargo check`
  - `cargo test`

Done when:

- mutable list support is locked by tests and the old variable-only assignment
  path is no longer the hidden real implementation.

---

## 5. Remaining Design Questions

The following questions remain open, but they now sit under a locked observable
semantic contract:

- Which internal representation best preserves the locked rooted-update /
  value-read contract for nested lists with acceptable complexity?
- Does closure capture of `mut` bindings already provide the right sharing model
  once the mutable root contains nested lists, or is additional runtime
  plumbing needed at the cell boundary?
- Where is the honest ownership boundary for out-of-bounds indexed assignment:
  shared frontend rule, shared runtime error, or backend limitation?

These should be settled before implementation starts, but they should no longer
re-open the user-visible alias/read semantics locked above.

---

## 6. Completion Criteria

This plan is complete when all of the following are true:

- `mut` remains the only opt-in surface for local mutability,
- immutable list bindings reject element assignment,
- mutable list bindings support indexed assignment,
- nested list mutation works through the same target model,
- the compiler has one shared assignment-target abstraction instead of separate
  ad hoc paths,
- docs, examples, implementation, and tests all agree on the same semantics.
