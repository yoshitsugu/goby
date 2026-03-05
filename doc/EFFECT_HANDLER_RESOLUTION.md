# Effect/Handler Resolution Rules (Draft)

This document defines the intended resolution model for effect operations and
handlers in Goby, including handler matching in `with` scopes and implicit
handling in `main`.

## 1. Goals

- Keep effect operation resolution deterministic and easy to explain.
- Use one consistent resolution pipeline for user handlers and implicit default
  handlers.
- Avoid runtime-only name guessing.
- Keep room for a later `EffectId/OpId` lowering step.

## 2. Top-Level Naming Policy

To reserve compiler-internal symbols safely:

- User top-level declarations must start with an ASCII lowercase letter.
- User top-level declarations cannot start with `_`.
- Names in the reserved internal namespace (`_...`, `__goby_*`) are
  compiler/stdlib-only.

This prevents collisions with compiler-generated helper declarations such as
implicit `main` wrappers.

## 3. Operation Name Resolution (Call Sites)

For a bare operation call (for example `a` or `a x`) inside an expression/body:

1. Collect visible effect operations from:
   - local effect declarations in the current module,
   - imported effects (subject to import mode rules),
   - implicit prelude effects.
2. Find operations whose operation name matches the bare callee name.
3. Resolution outcomes:
   - exactly one match: resolved,
   - zero matches: unresolved operation,
   - multiple matches: ambiguous operation name (error).

Name priority rule:

- Lexical value namespace wins over bare operation names.
- If a local binding/parameter/declaration name matches a visible operation
  name, the lexical name is selected.
- Future tooling may emit a warning for this shadowing pattern.

Qualified operation syntax (`EffectName.op`) can be added to disambiguate in
the future. Until then, duplicate operation names in visible scope are errors.

## 4. `with` Clause Resolution

For each handler clause in `with` inline handler blocks / `handler` values:

- Clause name (left side of `->`) is resolved by operation name using the same
  visibility rules as operation calls.
- Clause must resolve to exactly one operation.
- Duplicate clause names in the same handler block are errors.
- If a clause name is ambiguous across visible effects, it is an error.

Future extension:

- Allow explicitly qualified clause names (for example `EffectA.a -> ...`) to
  support intentional overlap.

### 4.1 Operation-Declared Dependencies (`op ... can Dep`)

Effect member signatures may declare dependency effects:

```goby
effect Trace
  trace : String -> Unit can Print
```

Rules:

- dependency declarations are attached to each operation member (not only effect-wide).
- in a handler clause for that operation, allowed effects are:
  - effects already handled by the enclosing scope,
  - plus effects declared in that member's `can` list.
- if a handler clause uses an effect not in this allowed set, it is an error.

This lets operation implementations stay explicit about which extra effects they
require, while call sites can still expose only the parent effect in their
public `can` signatures.

## 5. Coverage Check Between Calls and Handlers

Inside a `with ... in ...` region:

- Each resolved effect operation call must be covered by an active handler stack
  entry for the same resolved operation.
- If not covered, report unhandled effect operation.

For the example:

```goby
effect EffectA
  a : Unit -> Unit

effect EffectB
  b : Unit -> Unit

main : Unit -> Unit
main =
  with
    a ->
      do_something_1
      do_something_2
      resume ()
    b ->
      do_something_3
      do_something_4
      resume ()
  in
    some_process
    a
    b
```

The resolver confirms:

- `a` call resolves to `EffectA.a` and has a matching `a ->` clause.
- `b` call resolves to `EffectB.b` and has a matching `b ->` clause.

## 6. Implicit Default Handling in `main`

When `main` declares effect requirements (for example `can Print`), default
handlers should be integrated through the same pipeline as user-written
`with`.

Conceptually:

```goby
main : Unit -> Unit can Print
main =
  print "test"
```

is treated like:

```goby
main : Unit -> Unit
main =
  with default_print_handler
  in
    print "test"
```

Important:

- This is a compiler desugaring step.
- The wrapped body uses normal operation/handler resolution rules.
- No runtime special-case should be needed for this path.

### 6.1 Multiple Implicit Handlers (`main can A, B, ...`)

When `main` requires multiple effects with default handlers, wrapper order must
be deterministic because nearest-handler semantics can change behavior.

Concrete order-sensitive example:

```goby
effect Print
  print : String -> Unit

effect Trace
  trace : String -> Unit
```

Assume:

- default `Trace` handler implements `trace msg` by calling `print("[trace] " + msg)`.
- user installs a custom `Print` handler in `main` body.

Then behavior differs by implicit wrapper order:

- if implicit `Print` wrapper is outside implicit `Trace`, `trace` inside
  `main` can be printed by the user custom `Print` handler.
- if implicit `Trace` wrapper is outside implicit `Print`, the same `trace`
  may be captured by the implicit `Print` handler before user custom handling.

Decision for now:

- wrapper order is derived by topological sorting of effect dependencies
  declared on operation members (`op ... can Dep`).
- if two effects are independent (no dependency edge), preserve source `can`
  list order as tie-breaker.
- desugaring then applies nested `with` in that deterministic order.

## 7. Recommended Compiler Pipeline

1. Parse source into AST.
2. Desugar implicit `main` default handlers into explicit handler form.
3. Resolve operation/handler names (string-level names), with lexical value
   names taking priority over bare operation names.
4. Validate ambiguity, duplicates, and handler coverage.
5. Lower resolved operation references into internal stable IDs
   (`EffectId/OpId`) for runtime/codegen.

This keeps user semantics name-based, while runtime dispatch can remain
efficient and deterministic.

## 8. Error Categories (Suggested)

- `UnresolvedEffectOperation`
- `AmbiguousEffectOperation`
- `DuplicateHandlerClause`
- `UnhandledEffectOperation`
- `ReservedTopLevelName` (for `_...` user declarations)

These names are descriptive suggestions; final diagnostics can use project
conventions.
