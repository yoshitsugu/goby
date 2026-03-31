# Goby Closure Capture Plan

Last updated: 2026-03-31

Status: Planned

Related documents:

- `doc/LANGUAGE_SPEC.md` — current language surface and semantics
- `doc/PLAN.md` — top-level roadmap and syntax direction
- `doc/PLAN_IR.md` — stable IR and Wasm-lowering boundaries
- `doc/STATE.md` — current deferred limitations and restart notes
- `doc/closure-design.md` — existing closure-value design note that must be revised before implementation

---

## 1. Goal

Enable lambda closure capture on the `GeneralLowered` Wasm path with correct lexical semantics.

Specification policy for this track:

- `doc/LANGUAGE_SPEC.md` is the source of truth for the intended user-visible closure semantics.
- During development, `doc/LANGUAGE_SPEC.md` may describe the intended semantics with an explicit implementation-status note.
- `doc/STATE.md` records what currently works and what still does not.
- Interpreter behavior and Wasm behavior must converge to `doc/LANGUAGE_SPEC.md`; neither runtime is allowed to become a competing semantic authority.
- After the feature is complete, the temporary implementation-status note must be removed and the spec text must stand as current behavior.

Scope of this plan:

- read capture of outer `let` bindings
- read capture of outer `mut` bindings
- write capture of outer `mut` bindings
- inline lambda callbacks passed to higher-order functions such as `each`, `map`, and `fold`
- parity between the Wasm path and the interpreter path for captured locals

Out of scope for the first closure-capture implementation:

- garbage collection
- closure serialization or reflection
- cross-module closure ABI
- multi-shot effect-handler continuation capture changes

---

## 2. Required User-Visible Semantics

This plan intentionally changes the mutable-capture direction assumed in `doc/closure-design.md`.

Locked semantic target for this plan:

1. Capturing an immutable binding (`let`) copies its value into the closure environment.
2. Capturing a mutable binding (`mut`) captures a shared mutable cell, not a snapshot value.
3. Reading a captured `mut` inside a lambda observes the latest value in that shared cell.
4. Assigning to a captured `mut` inside a lambda updates the same binding visible outside the lambda.
5. Multiple closures capturing the same `mut` binding observe the same shared state.
6. Non-capturing lambdas keep the current lightweight function-handle path.

These rules are the minimum needed to make closure capture useful for real local state, not only read-only callbacks.

---

## 3. Minimal Acceptance Programs

The implementation is not complete until all programs in this section execute correctly on the Wasm path.

### 3.1 Read-only immutable capture

```goby
make_adder : Int -> (Int -> Int)
make_adder base =
  fn x -> base + x

main : Unit -> Unit can Print, Read
main =
  _ = read()
  add10 = make_adder 10
  println "${add10 5}"
```

Expected output:

```text
15
```

### 3.2 Captured mutable binding can be updated inside a lambda

```goby
import goby/list ( each )

sum : List Int -> Int
sum xs =
  mut total = 0
  each xs (fn x ->
    total := total + x
  )
  total

main : Unit -> Unit can Print, Read
main =
  _ = read()
  println "${sum [1, 2, 3]}"
```

Expected output:

```text
6
```

### 3.3 Outer mutation after closure creation is visible through the closure

```goby
main : Unit -> Unit can Print, Read
main =
  _ = read()
  mut value = 1
  read_value = fn _ -> value
  value := 7
  println "${read_value ()}"
```

Expected output:

```text
7
```

### 3.4 Inline capturing lambda passed to `fold`

```goby
import goby/list ( fold )

sum_with_bias : Int -> List Int -> Int
sum_with_bias bias xs =
  fold xs 0 (fn acc x -> acc + x + bias)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  println "${sum_with_bias 10 [1, 2, 3]}"
```

Expected output:

```text
36
```

### 3.5 Two closures share one captured mutable cell

```goby
pair : Unit -> ((Unit -> Unit), (Unit -> Int))
pair _ =
  mut count = 0
  inc = fn _ ->
    count := count + 1
  get = fn _ -> count
  (inc, get)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  p = pair()
  p.0()
  p.0()
  println "${p.1()}"
```

Expected output:

```text
2
```

---

## 4. Design Direction

### 4.1 Ownership boundary

Closure capture must be solved at the shared IR → backend lowering boundary, not by stdlib-specific rewrites and not by parser-level exceptions.

This means:

- do not special-case `fold`, `each`, or `map` by symbol name to fake closure support
- do not add syntax-specific exceptions for one callback shape
- lower all captured lambdas through one shared closure representation

### 4.2 Closure value representation

Keep the overall direction from `doc/closure-design.md`:

- closure values are distinct runtime values
- closure records live in linear memory
- closure calls use an explicit closure-aware call path

However, mutable capture semantics must change:

- immutable captures may still be stored directly in the closure environment by value
- mutable captures must refer to a shared mutable cell
- the closure wrapper must know, per capture slot, whether it is loading a direct value or dereferencing a mutable cell

### 4.3 Mutable-cell strategy

Preferred implementation direction:

1. Keep ordinary non-escaping `mut` locals in the current local-slot form.
2. Detect `mut` bindings that escape into one or more closures.
3. Promote escaping mutable bindings to heap-backed mutable cells.
4. Rewrite reads and writes of those bindings in both the outer function and the closure body to use the shared cell.

This avoids forcing every mutable local into heap storage while still giving correct shared-state semantics for closure capture.

### 4.4 Closure-environment ownership

To keep the feature maintainable, closure-specific context must have one explicit owner in the implementation.

The implementation should introduce one focused closure-environment abstraction responsible for:

- capture classification result shape
- closure environment layout
- mutable-cell versus direct-value slot kind
- closure wrapper load/store helpers
- lowering-time metadata needed by emit/runtime

The rest of the pipeline should consume this abstraction rather than re-deriving capture facts ad hoc.

This is specifically to prevent closure state from leaking into unrelated modules as scattered booleans, special-case branches, or callback-specific exceptions.

### 4.5 Calls and higher-order functions

Closure support is not complete until all relevant call sites use the same callable dispatch model:

- direct call of a top-level declaration
- indirect call of a non-capturing lambda
- indirect call of a capturing closure
- stdlib-driven indirect calls inside `each`, `map`, and `fold`

The backend must not keep one call path for ordinary lambdas and a separate ad hoc path for capturing callbacks in list combinators.

### 4.6 Diagnostics during rollout

During partial implementation:

- unsupported closure cases must fail with precise backend errors
- wrong-code or silently dropped effects are unacceptable
- mutable write capture must not be temporarily accepted unless the shared-cell semantics are already correct

---

## 5. Milestones

The checklist below is the active progress tracker for closure capture work.

### CC0. Semantics lock and document alignment

- [ ] Add closure-capture semantics to `doc/LANGUAGE_SPEC.md` as the intended behavior, with an explicit implementation-status note while the feature is still incomplete.
- [ ] Confirm the semantic target in this document:
  mutable capture uses shared-cell semantics, not snapshot semantics.
- [ ] Revise `doc/closure-design.md` so it no longer contradicts this plan.
- [ ] Update interpreter-side tests or interpreter behavior as needed so the interpreter is moving toward the spec rather than preserving the old snapshot rule as a hidden authority.
- [ ] Update `doc/STATE.md` to replace the current generic "closure capture unsupported" note with the current slice status once work starts.

Done when:

- `doc/LANGUAGE_SPEC.md` contains the intended closure semantics with a clearly marked implementation-status note
- this plan, `doc/closure-design.md`, and `doc/STATE.md` describe the same closure semantics
- no design note still claims that captured `mut` values are snapshot-only
- it is clear from the docs that `doc/LANGUAGE_SPEC.md`, not current runtime behavior, is the semantic source of truth

### CC1. Analysis and IR ownership

- [ ] Add an analysis that distinguishes:
  - no capture
  - immutable capture
  - mutable read capture
  - mutable write capture
- [ ] Detect escaping mutable bindings at the function/lambda ownership boundary.
- [ ] Define where promoted mutable-cell metadata lives so both outer code and closure wrappers can use it.
- [ ] Introduce one owned closure-environment metadata abstraction rather than duplicating capture-shape logic across lowering and emit.

Done when:

- the compiler can classify every lambda with a stable capture summary
- escaping `mut` bindings are identified before backend-specific instruction emission
- one owned metadata abstraction is the only required source for closure slot kinds and promoted mutable-cell information

### CC2. Runtime representation

- [ ] Finalize the closure-record layout for captured values and mutable-cell references.
- [ ] Add heap representation and helpers for promoted mutable cells.
- [ ] Keep non-capturing lambdas on the existing lightweight funcref path.
- [ ] Define one helper layer for closure-environment load/store logic so wrapper emission does not open-code slot interpretation repeatedly.

Done when:

- runtime values can represent both closures and shared mutable cells without ambiguity
- closure environment loading rules are documented and testable
- closure wrapper code and outer-function promoted-mut code both use the same environment/cell helper model

### CC3. Lowering and call dispatch

- [ ] Lower capturing lambdas into closure records plus wrapper declarations.
- [ ] Rewrite captured mutable reads/writes to shared-cell loads/stores.
- [ ] Add tagged call dispatch that works for both function handles and closure values.
- [ ] Ensure top-level helper calls and nested call sites preserve effect sequencing.
- [ ] Add regression tests for:
  - effectful helper call execution
  - effectful closure body execution
  - direct closure call outside stdlib combinators

Done when:

- a capturing lambda can be created, stored, passed, and called on the Wasm path
- the dedicated regression tests above pass
- no supported closure path silently drops effects or returns `Unit` in place of the expected value

### CC4. Higher-order stdlib parity

- [ ] Make `each` accept capturing closures.
- [ ] Make `map` accept capturing closures.
- [ ] Make `fold` accept capturing closures, including inline callbacks.
- [ ] Remove any remaining backend limitation that exists only because a callback is capturing.

Done when:

- all acceptance programs in Section 3 run through the Wasm path
- list combinators do not need callback-shape exceptions
- there is at least one focused execution test each for capturing `each`, capturing `map`, and inline capturing `fold`

### CC5. Diagnostics and regression safety

- [ ] Keep precise diagnostics for any still-deferred edge cases.
- [ ] Add targeted regression tests beside the ownership modules that implement the behavior.
- [ ] Add end-to-end Wasm execution tests for closure capture parity with the interpreter path.
- [ ] Add a focused spec-conformance test set for the Section 3 acceptance programs.

Done when:

- unsupported cases fail loudly and specifically
- all Section 3 acceptance programs are covered by executable tests
- supported cases have focused tests plus at least one end-to-end parity test
- any remaining unsupported cases are listed explicitly in `doc/STATE.md`

### CC6. Documentation and examples closure

- [ ] Remove the temporary implementation-status note from `doc/LANGUAGE_SPEC.md` once the closure semantics are fully implemented.
- [ ] Update `doc/PLAN.md` and `doc/STATE.md` with the landed status.
- [ ] Add or refresh `examples/` coverage for closure capture and mutable capture.
- [ ] Remove stale comments and notes that describe capture as Wasm-unsupported after the feature lands.

Done when:

- user-facing docs, roadmap docs, and examples all agree on supported closure behavior
- `doc/LANGUAGE_SPEC.md` describes closure capture as current behavior without a temporary in-progress note

---

## 6. Development Rules for This Track

This track should follow the same engineering discipline used elsewhere in Goby's planning documents.

### 6.1 Fix the shared boundary, not one example

- Do not land a narrow patch that only makes one callback shape work.
- Start from the earliest honest boundary:
  capture analysis, mutable-cell promotion, shared IR ownership, backend lowering, then emit/runtime.
- If one attempted fix only helps `fold` or only helps one syntax form, it is probably at the wrong layer.

### 6.2 Preserve clear ownership

- Keep orchestration entrypoints thin.
- Put capture analysis, mutable-cell promotion, closure lowering, and emission logic in focused modules.
- Move subsystem-specific regression tests next to the implementation that owns the behavior.

### 6.3 Prefer precise temporary rejection over wrong execution

- If a slice is incomplete, reject the unsupported case explicitly.
- Never accept code that compiles but drops closure effects, loses assignments, or returns the wrong value silently.

### 6.4 Verify incrementally

- Add the smallest representative failing test first.
- Land one ownership move at a time where practical.
- After each meaningful slice, run focused tests first, then the broader quality gate required by the repo workflow.

### 6.5 Keep docs synchronized

- If implementation changes the locked semantics, update `doc/LANGUAGE_SPEC.md`, `doc/PLAN.md`, and `doc/STATE.md` in the same change.
- If syntax tooling is affected, update syntax definitions in the same change.
- When the feature is partially implemented, `doc/STATE.md` must say exactly which closure cases work and which still fail.
- `doc/LANGUAGE_SPEC.md` may describe intended semantics before implementation is complete, but it must include an explicit implementation-status note until the feature is fully landed.

---

## 7. Recommended Initial Execution Order

1. Add the intended closure semantics to `doc/LANGUAGE_SPEC.md` with an implementation-status note.
2. Update the conflicting closure design note so the document set has one semantic direction.
3. Add failing tests for the acceptance programs in Section 3.
4. Implement capture classification plus escaping-`mut` analysis.
5. Implement shared mutable-cell promotion plus the owned closure-environment abstraction.
6. Implement closure creation and tagged closure calls.
7. Integrate closure-aware callback dispatch into `each`, `map`, and `fold`.
8. Remove the temporary spec status note only after parity is proven and docs/examples are aligned.

This order keeps the work anchored at the real ownership boundaries and reduces the chance of adding callback-specific exceptions that must be removed later.
