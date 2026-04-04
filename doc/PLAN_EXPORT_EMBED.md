# Goby `@embed` Implicit Export Plan

Last updated: 2026-04-05

This document plans a language/runtime change for stdlib `@embed`.

Current behavior:

- `@embed` is stdlib-only.
- `@embed <EffectName> <HandlerName>` binds an effect declared in the same module
  to a built-in runtime default handler.
- Implicit `Print` / `Read` availability currently depends on
  `goby/prelude` locally declaring those effects and their `@embed` entries.
- `goby/stdio` currently duplicates the `Print` effect declaration that also
  exists in `goby/prelude`.

Planned behavior:

- `@embed` keeps its current runtime-handler meaning.
- `@embed` also implicitly exports the referenced effect into the implicit
  prelude surface.
- As a result, stdlib can centralize effect declarations in modules such as
  `goby/stdio` while preserving user-facing behavior like:

```gb
main : Unit -> Unit can Print
main = print "hello"
```

without requiring an explicit `import goby/stdio`.

## 1. Problem Statement

Current stdlib structure mixes two concerns in `goby/prelude`:

- defining effect identities (`Print`, `Read`)
- declaring which effects are implicitly available to user code

That coupling creates duplication. `Print` currently exists in both
`goby/prelude` and `goby/stdio`, even though the design intent is that stdio-like
effects should have a single canonical stdlib home.

The goal of this plan is to preserve the current user experience while allowing
the canonical effect declaration to live outside `goby/prelude`.

## 1.1 Long-Term Design Constraints

This plan is only acceptable if it improves the long-term ownership model rather
than encoding today's `Print` duplication as a new special case.

The design constraints are:

- one canonical owner module per embedded effect
- one shared compiler model for "embedded effect visibility" used by validation,
  typechecking, and name resolution
- no new hard-coded lists of effect names or operation names outside the stdlib
  metadata path
- implicit availability remains a property of the effective prelude surface,
  not a property of arbitrary stdlib imports in user modules
- runtime-handler binding and implicit availability must remain related but
  distinguishable concepts in the compiler model, even if they currently share
  the `@embed` surface syntax

If an implementation step cannot satisfy these constraints, the step should be
re-scoped before code lands.

## 2. Design Direction

### 2.1 New Meaning Added to `@embed`

`@embed` will mean both:

1. bind this effect to a built-in runtime default handler
2. mark this effect as part of the implicit-prelude-export surface

This is intentionally limited to stdlib modules.

### 2.2 Intended User-Facing Result

If a stdlib module declares:

```gb
effect Print
  print : String -> Unit
  println : String -> Unit

@embed Print __goby_embeded_effect_stdout_handler
```

then user modules should be able to:

- write `can Print` without explicit import
- call bare `print` / `println` without explicit import
- still use qualified `Print.print` / `Print.println` when the owning module is imported

### 2.3 Ownership Model

The canonical effect declaration should live in its domain module:

- `goby/stdio` owns `Print`
- a future input-oriented module may own `Read`, or `Read` may remain where it is
  until a clearer module split exists

`goby/prelude` becomes an implicit-import aggregation surface rather than the
canonical declaration site for every built-in-facing effect.

### 2.4 Locked Architectural Direction

The intended end state is:

- effect ownership lives in domain modules such as `goby/stdio`
- `goby/prelude` remains the only implicit-import entrypoint
- the compiler derives implicit effect names and bare effect-operation names
  from prelude-reachable embedded-effect metadata rather than from ad hoc
  `Print` / `Read` special cases

This means the implementation should not stop at "make `Print` work from
`goby/stdio`". It should land a reusable ownership/resolution path that can also
honestly model `Read` and future stdlib-owned embedded effects if they are later
approved.

## 3. Scope

In scope for this change:

- effect visibility in `can` clauses
- bare effect-operation resolution (`print`, `println`, `read`, etc.)
- implicit-prelude availability derived from stdlib `@embed`
- removal of duplicated effect declarations where the new model makes that possible

Out of scope for this change:

- implicit export of ordinary value declarations not tied to an embedded effect
- implicit export of types unrelated to embedded effects
- user-defined `@embed` outside stdlib
- broader prelude redesign beyond what is required for embedded effects
- arbitrary transitive implicit exports outside the effective prelude surface

## 4. Proposed Semantics

### 4.1 Validation Rule Changes

Current rule:

- `@embed` requires the target effect to be declared in the same module

Planned rule:

- `@embed` requires the target effect to be visible in the module
- visibility may come from:
  - a local effect declaration
  - an imported stdlib effect declaration

This allows `goby/prelude` to import `goby/stdio` and reference `Print` in
`@embed` without locally redeclaring it.

### 4.2 Implicit Prelude Export Collection

The compiler currently adds `goby/prelude` as an implicit import when available.
That stays in place.

What changes is what the implicit prelude surface contains:

- any effect referenced by `@embed` in the effective prelude surface becomes a
  known implicitly available effect name
- the operations belonging to that effect become bare implicitly available names
- embedded default-handler collection should continue to treat the embedded
  effect as active for `main`
- user modules do not gain implicit names merely by importing some other stdlib
  module that itself imports an embedded-effect owner; the implicit path remains
  anchored at `goby/prelude`

### 4.3 Resolution Model

When `goby/prelude` is implicitly imported:

- `can Print` resolves because `Print` is in the embedded-export surface
- `print` resolves to the `Print.print` operation
- `println` resolves to the `Print.println` operation

This keeps current user ergonomics intact while removing the requirement that
`goby/prelude` be the declaration site.

## 5. Compiler Impact

### 5.1 Stdlib Resolution

Current `StdlibResolver` data is too local:

- local declarations become exports
- local effect declarations become visible effect names
- local `@embed` declarations become embedded defaults

The new model needs resolver data that can describe:

- effects visible through imports when those effects are referenced by `@embed`
- the operations attached to such effects
- enough provenance to report conflicts cleanly

Likely change direction:

- introduce a shared resolved metadata shape that can represent:
  - locally declared effects
  - imported visible effects
  - embedded default-handler bindings
  - the subset of visible effects that contribute to the implicit prelude surface
- derive an "embedded implicit export" set from `@embed` targets using that
  shared metadata rather than duplicating collection rules per compiler phase

Anti-goal:

- do not patch `validate_embed_declarations`, `known_effects`, `inject_imported_symbols`,
  and resolved-name built-ins independently with separate local rules for `Print`
  and `Read`

### 5.2 Typechecking

Areas that will need coordinated updates:

- `validate_embed_declarations`
  - stop requiring same-module effect declarations
  - require visible effect declarations instead
- `collect_imported_effect_declarations`
  - ensure imported effect declarations reachable through prelude are available
    to the effect map
- `known_effects`
  - include embedded-exported effects from the implicit prelude surface
- `inject_imported_symbols`
  - add bare op bindings for embedded-exported effects, not only ordinary value exports

### 5.3 Name Resolution

Resolved/builtin name handling currently gives `goby/prelude` special treatment for
bare built-in refs such as `print` and `read`.

That logic should move from a hard-coded built-in list toward:

- deriving implicit bare effect-op names from the prelude's embedded-export surface
- preserving the existing shadowing behavior for local bindings

The end state should reduce special cases tied specifically to the string
`"prelude"` plus a fixed operation-name list.

### 5.5 Recommended Execution Order

To avoid ad hoc implementation drift, code changes should land in this order:

1. define the target semantics in the spec
2. introduce the shared stdlib metadata/resolution model
3. migrate validation/typecheck/name-resolution consumers onto that model
4. only then restructure `prelude` / `stdio`

This ordering matters. Changing stdlib files first would likely force temporary
symbol-specific exceptions and make it harder to verify that the new model is
actually shared.

### 5.4 Runtime / Embedded Defaults

Runtime behavior should remain unchanged:

- embedded default handlers still activate only where currently allowed
- `main` still gets the default embedded effect coverage
- runtime lowering still uses the same handler intrinsic names

The change is primarily about visibility/export plumbing, not a new runtime model.

## 6. Stdlib Restructure Target

Initial target shape:

`stdlib/goby/stdio.gb`

```gb
effect Print
  print : String -> Unit
  println : String -> Unit

@embed Print __goby_embeded_effect_stdout_handler
```

`stdlib/goby/prelude.gb`

```gb
import goby/stdio
...
```

Open design point:

- keep `@embed` at the canonical owning module (`goby/stdio`)
- let the implicit prelude surface collect embedded effects transitively through
  prelude imports

This keeps ownership single-sourced.

## 7. Conflicts and Edge Cases

The implementation must reject:

- two implicitly reachable embedded effects with the same effect name but
  different signatures
- two embedded defaults for the same effect with different handler names
- ambiguous bare op names introduced through multiple embedded effects

The implementation should preserve:

- local binding shadowing over implicit bare names
- explicit imports continuing to work as before
- qualified access (`Print.print`) when the owning module is imported plainly or via alias

## 8. Documentation Changes Required

When implementation starts, update in the same change:

- `doc/LANGUAGE_SPEC.md`
  - `@embed` grammar and semantics
  - prelude / implicit effect visibility rules
- `doc/PLAN.md`
  - top-level roadmap note that `@embed` now also controls implicit prelude export
- `examples/`
  - examples that rely on implicit `Print` / `Read`
  - add at least one example that uses the canonical owning module explicitly
    (`import goby/stdio`)
- `doc/STATE.md`
  - record the locked ownership model and any remaining open questions

## 9. Test Plan

Add or update tests for:

- `can Print` with no explicit import still succeeds
- bare `print` / `println` with no explicit import still succeed
- `goby/prelude` can import an effect-owning stdlib module without redeclaring the effect
- `@embed` targeting an imported effect is accepted under stdlib root
- conflicting embedded-effect visibility across stdlib imports is rejected
- selective/prelude imports continue to resolve effect identities correctly
- qualified `Print.print` still works via explicit `import goby/stdio`
- missing prelude still removes implicit availability as before
- no new hard-coded `Print` / `Read` special-case path remains in resolved-name
  handling beyond the generic implicit-prelude import hook

## 10. Implementation Steps

1. Lock the semantics in `doc/LANGUAGE_SPEC.md`, including the boundary that
   implicit availability is still anchored at `goby/prelude`.
2. Refactor stdlib resolver metadata so embedded effects, visible imported
   effects, and implicit-prelude-export candidates are represented in one shared
   model.
3. Relax `@embed` validation from same-module declaration to visible-effect
   declaration using that shared model.
4. Rework typecheck known-effect collection and implicit symbol injection to use
   embedded-effect-derived metadata instead of local `prelude` declarations.
5. Rework resolved-name built-in handling so bare effect-op availability is
   derived from the prelude embedded-export surface rather than fixed `Print` /
   `Read` name tables.
6. Restructure stdlib modules to remove duplicated effect declarations only
   after the shared compiler path is in place.
7. Update examples, focused regressions, and end-to-end CLI/LSP coverage.
8. Update `doc/STATE.md` with the locked ownership model, residual deferred
   work, and whether `Read` remains in `prelude` or also moved.

## 11. Completion Criteria

This plan is complete only when all of the following are true:

- `goby/stdio` is the single canonical owner of `Print`
- user code can still write `can Print` and bare `print` / `println` without
  explicit imports
- the compiler no longer depends on `goby/prelude` locally redeclaring `Print`
  to provide that behavior
- validation, typechecking, and resolved-name handling all consume one shared
  embedded-effect visibility model rather than separate symbol-specific rules
- CLI and LSP behavior is regression-locked for the preserved user-facing cases
- docs/spec/examples/state are updated consistently
- remaining follow-up work, if any, is explicitly cataloged rather than implied

The plan is not complete if:

- `Print` works only because of a new `if effect == "Print"` style exception
- stdlib files are rearranged but the compiler model remains duplicated
- the ownership model for `Read` is still ambiguous and undocumented at close-out

## 12. Open Questions

- Should embedded-effect implicit export propagate only through `goby/prelude`, or
  through any stdlib import graph?
  - preferred answer: only through the effective implicit prelude surface
- Should all operations of an embedded effect become bare names automatically?
  - preferred answer: yes, to preserve existing `print` / `read` ergonomics
- Should `Read` move out of `goby/prelude` in the same change, or be deferred?
  - preferred answer: implement the mechanism first, then decide whether moving
    `Read` in the same patch improves clarity or adds unnecessary churn
- How much special-casing for prelude remains acceptable in name resolution after
  this change?
  - preferred answer: keep the implicit-import behavior, but reduce hard-coded
    effect/op-name lists
