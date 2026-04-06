# Plan: Mutable List Runtime Execution Fix

Last updated: 2026-04-06

## Current Status

Staged milestone landed on 2026-04-06:

- rooted mutable-list updates are now classified as a semantic runtime capability and routed to
  the `GeneralLowered` Goby-owned Wasm path, even for `Print`-only modules,
- pure `list.get` reads inside interpolation now lower through shared IR, so exact
  `goby run` reproductions such as `println("${a[0][1]}")` before or after rooted updates
  execute successfully,
- fallback/interpreter execution now centralizes rooted list updates through one path-copy helper
  with parity coverage for single-level updates, nested updates, and read-before-write cases,
- fallback/runtime recursive aggregate convergence still remains open as the follow-up slice in
  MLF-2.

## Goal

Restore `goby run` correctness for mutable list programs without adding special cases for
individual syntax shapes, element types, or single execution paths.

This track exists because mutable list assignment currently spans multiple runtime/execution
layers with inconsistent ownership:

- the frontend accepts and typechecks list-index assignment,
- compiled Wasm lowering implements path-copy list updates,
- but some `goby run` programs are still routed into fallback execution layers that do not
  support list-index assignment as a first-class operation.

The fix must make mutable list behavior a property of the language execution model, not a
property of whichever backend path happened to be selected.

## Problem Statement

Today, a program such as:

```goby
main : Unit -> Unit can Print
main =
  mut a = [[1,2,3], [4,5,6], [7,8,9]]
  println("${a[0][1]}")
  a[1][1] := 30
  println("${a[1][0]},${a[1][1]}")
```

can parse and typecheck, and equivalent behavior is already covered in compiled Wasm tests,
but `goby run` still fails in at least one routing shape.

This is not merely a missing nested-list case. The deeper issue is that the current execution
stack has no stable long-term contract for where list mutation must be supported:

- execution-path selection is based on a narrow notion of "runtime capability",
- fallback/static execution still rejects `AssignTarget::ListIndex`,
- recursive runtime values and rooted mutable updates are not treated as shared semantic
  primitives across all active execution modes.

## Design Principles

1. Mutable list assignment is a language-level semantic, not a backend-specific optimization.
2. Execution-path routing must be derived from semantic capability requirements, not from
   accidental implementation history such as "`Read` needs runtime, pure `Print` does not".
3. Fallback/interpreter execution must either:
   - support a language construct honestly, or
   - reject it at a clearly defined boundary before claiming the program is executable there.
4. Do not add one-off routing exceptions such as "if nested mutable list then use path X".
5. Do not encode combinatorial runtime value variants such as `ListListInt`, `ListBool`, or
   other shape-specific expansions.
6. The same recursive runtime value model should underpin list reads, list updates, equality,
   interpolation, and output formatting where fallback execution claims support.
7. The final architecture must remain compatible with future aggregate growth
   (nested tuples, records containing lists, lists of records, etc.).

## Non-Goals

- Do not patch only the exact reproduction in `~/current/test.gb`.
- Do not fix only interpolation while leaving direct binding/index/update semantics inconsistent.
- Do not rely on inserting fake `Read` effects or other incidental constructs to force a more
  capable runtime path.
- Do not preserve the current path-selection rules if they are semantically wrong.
- Do not declare the track complete merely because one backend passes; `goby run` behavior is the
  contract under repair.

## Target Architecture

### 1. Semantic Capability Model

Execution planning should answer this question:

"What language/runtime capabilities does this module require for correct execution?"

That capability model must include list-root mutation and recursive aggregate handling as explicit
dimensions. General lowering or another authoritative runtime path should be selected because the
program requires those semantics, not because it happens to mention `Read`, handlers, or lambdas.

### 2. Unified Runtime Value Semantics

Any execution mode that evaluates source expressions directly must use one recursive runtime value
domain that can represent:

- scalars,
- tuples,
- records,
- lists of arbitrary runtime values,
- mutable roots that can be updated through indexed paths.

The runtime should not maintain a smaller value universe than the source language for supported
programs.

### 3. Rooted Update Semantics

List-index assignment must be modeled as a rooted update operation:

- evaluate the root binding,
- evaluate each path index,
- rebuild affected list levels structurally,
- write the rebuilt root back into the mutable binding,
- preserve the existing language rule that indexed reads produce values, not shared aliases.

This semantic should be shared across execution modes, whether by common helpers or by a
deliberately aligned abstraction boundary.

### 4. Honest Fallback Contract

After this track, fallback execution must satisfy one of these two states:

- it supports list-index assignment and recursive aggregates correctly, or
- it is no longer selected for programs that require those features.

What is not acceptable is the current middle state where routing claims the program is runnable
but the evaluator silently lacks the operation.

## Recommended Direction

The preferred direction is a two-part repair:

1. Redefine execution-path planning around semantic capability requirements, including mutable
   list path updates.
2. Strengthen the fallback/interpreter runtime so that its supported subset uses one recursive
   runtime value model and one rooted-update semantic.

If engineering tradeoffs require staging, the first shipped milestone may route mutable-list
programs away from underpowered fallback paths, but the overall track is not complete until the
long-term execution contract is coherent and documented.

## Milestones

### MLF-0: Lock the semantic contract

- [x] Write an explicit execution-planning note that defines mutable list assignment and recursive
      aggregate handling as semantic capability requirements, not incidental backend features.
- [x] Define which active execution modes are intended to support rooted list updates.
- [x] Document the fallback contract: supported semantics vs. explicit unsupported boundary.
- [x] Record how this track interacts with existing nested-aggregate parity work in `doc/PLAN.md`
      and `doc/BUGS.md`.

Definition of done:

- A reviewer can read the planning docs and answer, unambiguously, whether a `Print`-only program
  containing `a[i] := v` or `a[i][j] := v` is supposed to run via fallback, general lowering, or
  another declared runtime path.
- No part of the plan relies on "current implementation happens to do X" as the semantic reason.

### MLF-1: Repair execution-path selection

- [x] Introduce an execution capability analysis that classifies mutable list rooted updates and
      recursive aggregate requirements directly.
- [x] Replace or refactor routing gates that currently treat "runtime-needed" as
      `Read`/handler/lambda/tuple-only.
- [x] Ensure `goby run` selects an execution path that can actually execute list-index assignment
      when the source program requires it.
- [x] Add regression coverage for path selection itself, not just final stdout.

Definition of done:

- No well-typed mutable-list program is routed into an execution path that rejects
  `AssignTarget::ListIndex` by construction.
- Routing tests prove the decision is driven by capability requirements rather than by ad hoc
  syntactic exceptions.

### MLF-2: Unify fallback runtime value semantics

- [ ] Remove shape-specific assumptions in fallback/runtime-output evaluation that prevent honest
      recursive aggregate support.
- [x] Ensure recursive runtime values are representable, comparable where supported, and
      stringifiable through one shared formatting path.
- [x] Implement or centralize rooted list-update evaluation for fallback/interpreter execution.
- [x] Validate that mutable bindings containing nested aggregates remain coherent after updates.

Definition of done:

- Fallback/runtime evaluation can carry nested list values and apply rooted updates without
  special-casing list depth or element kind.
- There is no dependency on combinatorial enum growth such as `ListListInt`.
- The implementation point for list updates is shared or clearly centralized rather than
  duplicated piecemeal across unrelated files.

### MLF-3: End-to-end parity and regression closure

- [x] Add `goby run`-equivalent regression tests for:
      - nested list read before interpolation,
      - nested list read inside interpolation,
      - single-level mutable list update,
      - two-level mutable list update,
      - read-after-write through the same mutable root,
      - update followed by interpolation/output.
- [x] Add parity tests that compare fallback/interpreter-visible behavior against the compiled
      Wasm path wherever both are intended to support the same semantics.
- [x] Update `doc/BUGS.md`, `doc/PLAN.md`, and `doc/STATE.md` to reflect the repaired boundary and
      any deliberately remaining unsupported cases.

Definition of done:

- The exact `goby run` reproduction shape from the bug report succeeds.
- A mutable nested-list program like `~/current/test.gb` succeeds under `goby run` with the
  expected output.
- Remaining unsupported behavior, if any, is explicit, documented, and enforced at routing time
  rather than surfacing as a late "fallback runtime output could not be resolved" failure.

## Required Test Matrix

The track is not complete unless the test suite contains end-to-end coverage for all of the
following language behaviors under the intended runtime boundary:

- [x] `mut xs = [1,2,3]; xs[1] := 10; println("${xs[1]}")`
- [x] `mut xs = [[1,2], [3,4]]; xs[0][1] := 99; println("${xs[0][1]}")`
- [x] `mut xs = [[1,2], [3,4]]; inner = xs[0]; println("${inner[1]}")`
- [x] `mut xs = [[1,2], [3,4]]; before = xs[1][1]; xs[1][1] := 30; println("${before}")`
- [x] `mut xs = [[1,2], [3,4]]; xs[1][1] := 30; println("${xs[1][0]},${xs[1][1]}")`
- [x] A non-mutable nested-list read program still succeeds without mutation.
- [x] Routing coverage proves the selected execution path is one that implements the required
      semantics.

## Review Checklist

- [ ] No new routing rule is keyed only to a narrow source spelling such as "nested list literal"
      or "interpolation".
- [ ] No fake capability trigger is introduced merely to force a different backend.
- [ ] The chosen runtime value model scales to lists of tuples, records, and future aggregates.
- [x] Rooted update semantics are defined once and reused, or else multiple implementations are
      intentionally aligned and covered by parity tests.
- [ ] Documentation states both what is supported and where unsupported cases are rejected.
- [ ] The implementation would still make architectural sense if more backends are added later.

## Completion Bar

This track is complete only when all of the following are true:

- [x] `goby run` handles well-typed mutable list programs through an execution path that is
      semantically capable of running them.
- [x] No active runtime path selected for those programs rejects `AssignTarget::ListIndex` as an
      internal unsupported case.
- [ ] Recursive aggregate values used by the supported fallback subset are modeled uniformly rather
      than via ad hoc special cases.
- [x] End-to-end tests cover both reads and writes on nested mutable lists.
- [x] The repaired boundary is documented in `doc/PLAN.md` and reflected in `doc/STATE.md`.
- [x] `doc/BUGS.md` is either cleared for this issue or narrowed to a deliberate, documented
      unsupported remainder.

Failure to meet any one of these items means the track remains open.
