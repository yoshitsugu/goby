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

- augment resolved stdlib metadata with imported effect declarations selected by
  the module's imports
- derive an "embedded implicit export" set from `@embed` targets

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

- whether `goby/prelude` should still contain `@embed Print ...` itself, or
  whether importing a stdlib module with an embedded effect should be enough to
  propagate that effect into the implicit prelude surface

Preferred direction:

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

## 10. Implementation Steps

1. Lock the semantics in `doc/LANGUAGE_SPEC.md`.
2. Refactor stdlib resolver metadata so embedded effects can be traced through imports.
3. Relax `@embed` validation from same-module declaration to visible-effect declaration.
4. Rework implicit prelude symbol/effect injection to use embedded-effect-derived exports.
5. Restructure stdlib modules to remove duplicated effect declarations where possible.
6. Update examples and regression tests.
7. Update `doc/STATE.md` with the final ownership model and any deferred follow-up.

## 11. Open Questions

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
