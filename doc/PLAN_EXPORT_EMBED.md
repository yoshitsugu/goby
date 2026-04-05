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
- `@embed` becomes a `goby/prelude`-only declaration rather than a general
  stdlib-wide mechanism.
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
- `@embed` itself remains a narrow prelude-surface declaration, not a
  module-local capability that arbitrary stdlib modules can attach everywhere
- runtime-handler binding and implicit availability must remain related but
  distinguishable concepts in the compiler model, even if they currently share
  the `@embed` surface syntax
- implementation choices must optimize for the intended long-term ownership
  model, not for minimizing short-term diff size against the current layout
- do not add transitional compatibility layers that preserve the old split model
  once the new ownership direction is locked

If an implementation step cannot satisfy these constraints, the step should be
re-scoped before code lands.

### 1.2 Execution Policy

This plan should be executed with a design-first bias.

- prefer the clean end-state model over temporary compatibility shims
- do not keep deprecated stdlib `@embed` usage alive "for now" once
  `goby/prelude`-only `@embed` is locked
- do not keep duplicated `Print` / `Read` declarations merely to reduce the size
  of one implementation step
- do not add compatibility logic whose only purpose is to let both the old and
  new ownership models coexist during the final design state
- if an implementation slice appears to require symbol-specific bridging code,
  step back and fix the shared metadata / ownership boundary first

## 2. Design Direction

### 2.1 New Meaning Added to `@embed`

`@embed` will mean both:

1. bind this effect to a built-in runtime default handler
2. mark this effect as part of the implicit-prelude-export surface

This is intentionally limited to stdlib modules.
More specifically, it is limited to `goby/prelude`.

### 2.2 Intended User-Facing Result

If `goby/prelude` imports an effect-owning stdlib module and declares:

```gb
import goby/stdio
@embed Print __goby_embeded_effect_stdout_handler
@embed Read __goby_embeded_effect_stdin_handler
```

then user modules should be able to:

- write `can Print` without explicit import
- write `can Read` without explicit import
- call bare `print` / `println` without explicit import
- call bare `read` / `read_line` / `read_lines` without explicit import
- still use qualified `Print.print` / `Print.println` when the owning module is imported

### 2.3 Ownership Model

The canonical effect declaration should live in its domain module:

- `goby/stdio` owns `Print`
- `goby/stdio` also owns `Read`

`goby/prelude` becomes an implicit-import aggregation surface rather than the
canonical declaration site for every built-in-facing effect.

### 2.4 Locked Architectural Direction

The intended end state is:

- effect ownership lives in domain modules such as `goby/stdio`
- `goby/prelude` remains the only implicit-import entrypoint
- `@embed` remains a `goby/prelude`-only declaration that selects which visible
  stdlib-owned effects participate in the implicit surface and default-handler path
- the compiler derives implicit effect names and bare effect-operation names
  from prelude `@embed` metadata rather than from ad hoc
  `Print` / `Read` special cases

This means the implementation should not stop at "make `Print` work from
`goby/stdio`". It should land a reusable ownership/resolution path that also
moves `Read` into the same ownership model.

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
- stdlib-wide `@embed` usage outside `goby/prelude`
- broader prelude redesign beyond what is required for embedded effects
- arbitrary transitive implicit exports outside the effective prelude surface

## 4. Proposed Semantics

### 4.1 Validation Rule Changes

Current rule:

- `@embed` requires the target effect to be declared in the same module

Planned rule:

- `@embed` requires the target effect to be visible in the module
- `@embed` is only valid in `goby/prelude`
- visibility may come from:
  - a local effect declaration
  - an imported stdlib effect declaration

This allows `goby/prelude` to import `goby/stdio` and reference `Print` / `Read`
in `@embed` without locally redeclaring them, while still preventing `@embed`
from spreading across arbitrary stdlib modules.

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
- `can Read` resolves because `Read` is in the embedded-export surface
- `print` resolves to the `Print.print` operation
- `println` resolves to the `Print.println` operation
- `read`, `read_line`, and `read_lines` resolve to `Read` operations

This keeps current user ergonomics intact while removing the requirement that
`goby/prelude` be the declaration site.

## 5. Compiler Impact

### 5.1 Stdlib Resolution

Current `StdlibResolver` data is too local:

- local declarations become exports
- local effect declarations become visible effect names
- local `@embed` declarations become embedded defaults

The new model needs resolver data that can describe:

- effects visible through `goby/prelude` imports when those effects are referenced by `@embed`
- the operations attached to such effects
- enough provenance to report conflicts cleanly

Likely change direction:

- introduce a shared resolved metadata shape that can represent:
  - locally declared effects
  - imported visible effects
  - prelude `@embed` default-handler bindings
  - the subset of visible effects selected by prelude `@embed` that contribute
    to the implicit prelude surface
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
  - require the source module to be `goby/prelude`
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

effect Read
  read : Unit -> String
  read_line : Unit -> String
  read_lines : Unit -> List String
```

`stdlib/goby/prelude.gb`

```gb
import goby/stdio
@embed Print __goby_embeded_effect_stdout_handler
@embed Read __goby_embeded_effect_stdin_handler
...
```

This keeps effect ownership single-sourced in `goby/stdio` while keeping the
implicit-user-surface rule localized to `goby/prelude`.

## 7. Conflicts and Edge Cases

The implementation must reject:

- two implicitly reachable embedded effects with the same effect name but
  different signatures
- two embedded defaults for the same effect with different handler names
- ambiguous bare op names introduced through multiple embedded effects
- `@embed` declarations outside `goby/prelude`

The implementation should preserve:

- local binding shadowing over implicit bare names
- explicit imports continuing to work as before
- qualified access (`Print.print`, `Read.read`) when the owning module is imported plainly or via alias

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
  - record the locked ownership model for both `Print` and `Read`

## 9. Test Plan

Add or update tests for:

- `can Print` with no explicit import still succeeds
- `can Read` with no explicit import still succeeds
- bare `print` / `println` with no explicit import still succeed
- bare `read` / `read_line` / `read_lines` with no explicit import still succeed
- `goby/prelude` can import an effect-owning stdlib module without redeclaring the effects
- `@embed` targeting imported effects is accepted in `goby/prelude`
- `@embed` outside `goby/prelude` is rejected even under stdlib root
- conflicting embedded-effect visibility across stdlib imports is rejected
- selective/prelude imports continue to resolve effect identities correctly
- qualified `Print.print` and `Read.read` still work via explicit `import goby/stdio`
- missing prelude still removes implicit availability as before
- no new hard-coded `Print` / `Read` special-case path remains in resolved-name
  handling beyond the generic implicit-prelude import hook

## 10. Development Milestones

### M1. Semantics Lock

- [x] Update `doc/LANGUAGE_SPEC.md` so `@embed` is explicitly `goby/prelude`-only.
- [x] Lock that implicit availability remains anchored at `goby/prelude`.
- [x] Lock that `goby/stdio` is the canonical owner of both `Print` and `Read`.
- [x] Remove plan wording that still assumes stdlib-wide `@embed`.

Done when:

- the spec and this plan describe one end-state model with no remaining
  ambiguity about `@embed` placement or `Print` / `Read` ownership.

### M2. Shared Metadata Model

- [x] Refactor stdlib resolver metadata so visible imported effects, prelude
  `@embed` bindings, and implicit-prelude-export candidates are represented in
  one shared model.
- [x] Ensure the new model can represent both `Print` and `Read` without
  symbol-specific branching.
- [x] Remove assumptions that embedded defaults only come from locally declared
  effects.

Done when:

- validation, typecheck, and name-resolution code can all consume the same
  embedded-effect visibility model instead of rebuilding local partial views.

### M3. Validation And Resolution Migration

- [x] Change `@embed` validation to require visible effects rather than
  same-module declarations.
- [x] Restrict accepted `@embed` source modules to `goby/prelude`.
- [x] Rework known-effect collection and implicit symbol injection to use the
  shared metadata model.
- [ ] Rework resolved-name handling so implicit bare effect-op names come from
  prelude `@embed` metadata rather than fixed `Print` / `Read` tables.

Done when:

- the compiler can resolve implicit `Print` / `Read` availability entirely
  through the shared prelude-embed model.

### M4. Stdlib Ownership Restructure

- [ ] Move `Print` and `Read` effect declarations into `stdlib/goby/stdio.gb`.
- [ ] Remove `Print` / `Read` declarations from `stdlib/goby/prelude.gb`.
- [ ] Keep `stdlib/goby/prelude.gb` responsible only for imports plus `@embed`
  declarations for the implicit surface.
- [ ] Avoid transitional coexistence where both old and new ownership layouts
  remain intentionally supported.

Done when:

- `goby/stdio` is the only canonical declaration site for `Print` and `Read`,
  and `goby/prelude` is the only declaration site for `@embed`.

### M5. Regression Lock And Closure

- [ ] Add or update focused tests for implicit `Print` and implicit `Read`.
- [ ] Add regressions proving `@embed` outside `goby/prelude` is rejected.
- [ ] Add CLI and LSP parity coverage for preserved implicit behavior.
- [ ] Update examples and `doc/STATE.md` to record the final ownership model.

Done when:

- preserved user-facing behavior is regression-locked and closure docs no longer
  describe the old split design.

## 11. Completion Criteria

This plan is complete only when all of the following are true:

- `goby/stdio` is the single canonical owner of `Print`
- `goby/stdio` is the single canonical owner of `Read`
- user code can still write `can Print` and bare `print` / `println` without
  explicit imports
- user code can still write `can Read` and bare `read` / `read_line` / `read_lines`
  without explicit imports
- the compiler no longer depends on `goby/prelude` locally redeclaring `Print`
  or `Read` to provide that behavior
- `@embed` is accepted only in `goby/prelude`
- validation, typechecking, and resolved-name handling all consume one shared
  embedded-effect visibility model rather than separate symbol-specific rules
- CLI and LSP behavior is regression-locked for the preserved user-facing cases
- docs/spec/examples/state are updated consistently
- remaining follow-up work, if any, is explicitly cataloged rather than implied

The plan is not complete if:

- `Print` works only because of a new `if effect == "Print"` style exception
- `Read` works only because of a new `if effect == "Read"` style exception
- stdlib files are rearranged but the compiler model remains duplicated
- the ownership model for `Print` / `Read` is still ambiguous and undocumented at close-out

## 12. Locked Decisions

- Embedded-effect implicit export propagates only through the effective implicit
  prelude surface, not through arbitrary stdlib import graphs.
  - Reason:
    - implicit availability is a language-level onboarding rule, not a generic
      property of stdlib imports
    - allowing propagation through arbitrary stdlib graphs would make user-visible
      names depend on incidental module topology rather than the explicit prelude
      boundary
    - keeping the boundary at `goby/prelude` preserves one stable place where
      implicit names enter user code
- All operations of an embedded effect become bare names automatically when that
  effect is present in the implicit prelude surface.
  - Reason:
    - this preserves existing `print` / `println` / `read` ergonomics
    - exporting only the effect name while forcing explicit qualification for its
      operations would be a partial model that breaks the current user experience
    - deriving the bare names from the effect declaration keeps the rule shared
      and avoids per-operation special cases
- `@embed` is a `goby/prelude`-only feature.
  - Reason:
    - the language needs one narrow, stable place where implicit I/O onboarding
      is declared
    - allowing `@embed` in arbitrary stdlib modules would mix effect ownership
      with implicit-surface definition and make prelude behavior depend on module sprawl
    - keeping `@embed` in `goby/prelude` preserves a clear separation:
      domain modules own effects, prelude decides which of those effects are implicit
- `Read` moves to `goby/stdio` in the same change as `Print`.
  - Reason:
    - leaving `Read` behind would preserve the split ownership model this plan
      is explicitly trying to remove
    - `Print` and `Read` are the two current onboarding effects and should share
      one ownership story after this refactor
    - moving both at once makes the end-state simpler and avoids a temporary
      hybrid design becoming sticky
- Prelude-specific handling remains acceptable only at the implicit-import entry
  boundary itself; effect and operation availability must no longer depend on
  hard-coded `Print` / `Read` name tables.
  - Reason:
    - the language already has one intentional special rule: `goby/prelude` is
      implicitly imported when available
    - that does not justify preserving the old split ownership model through
      compatibility code once the new design is locked
    - keeping that boundary is reasonable, but adding further `prelude`-specific
      symbol logic would reintroduce the ad hoc structure this plan is trying to remove
    - after this change, the compiler should ask "what embedded effects does the
      effective prelude surface expose?" rather than "is this `print` or `read`?"
