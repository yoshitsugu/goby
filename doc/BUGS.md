# Known Bugs

## BUG-001: handler `for` clause with unknown effect name gives misleading error

**Status:** Open
**Discovered:** 2026-03-02 (session 28)

### Symptom

```
handler PrintErrorHandler for ErrorEffect   -- ErrorEffect is not declared
  raise e = ...

main =
  using PrintErrorHandler
    raise Error(message: "x")
```

Error reported:

```
typecheck error in main: effect operation `raise` is not handled by any enclosing `using` block
```

### Expected

```
typecheck error: handler `PrintErrorHandler` refers to unknown effect `ErrorEffect`
```

### Root Cause

`EffectMap::covered_ops` looks up `handler_to_effect.get("PrintErrorHandler")` → `"ErrorEffect"`,
then `effect_to_ops.get("ErrorEffect")` → `None` (because `effect ErrorEffect` was never declared).
This silently yields an empty op set, so `raise` appears uncovered.

No validation step checks that a handler's `for` clause names a declared effect.

### Fix

Add a validation pass (e.g. `validate_handler_declarations`) called from `typecheck_module`,
that iterates `module.handler_declarations` and checks each `handler_decl.effect` against the
set of declared effect names. Return a `TypecheckError` on mismatch.

The check should run before `build_effect_map` is used for coverage analysis.

## BUG-002: positional record constructor `Ctor("value")` silently misparses as a function call

**Status:** Fixed (session 28)
**Discovered:** 2026-03-02 (session 28)

### Symptom

```
type Error = Error(message: String)
effect RaiseError
  raise: Error -> Unit
handler PrintErrorHandler for RaiseError
  raise e =
    print e.message

main : Unit -> Unit
main =
  using PrintErrorHandler
    raise Error("AnotherError")   -- no output; handler never fires
```

`cargo run -- run` prints nothing. `cargo run -- check` passes without error.

### Expected

Either:
- `Error("AnotherError")` is accepted as positional constructor sugar and dispatches correctly, or
- A parse/typecheck error is reported explaining that positional constructor syntax is not supported.

### Root Cause

`parse_record_constructor_call` only accepts named-field syntax (`field: value`).
When a field has no `:`, `parse_record_constructor_field` returns `None`, causing
`parse_record_constructor_call` to return `None`.

The expression then falls through to `try_parse_call` (step 11 in `parse_expr`) and is parsed as:

```
Expr::Call { callee: Var("Error"), arg: StringLit("AnotherError") }
```

At runtime `Error` is not a callable function, so handler dispatch receives a wrong value and silently fails.

At typecheck time, `check_expr` returns `Ty::Unknown` for `Expr::Call { callee: Var("Error"), .. }`
(because `Error` has no function type in the environment), so the §4.1.1 arg-type check is skipped
(Unknown → no false positive guard). The mismatch goes undetected.

### Fix Options

1. **Parse-time error**: detect `Ctor(non-field-expr)` in `parse_record_constructor_call` and
   return a `ParseError` ("positional constructor syntax not supported; use `Ctor(field: value)`").
2. **Positional sugar**: if the record type has exactly one field, treat `Ctor(value)` as
   `Ctor(field: value)` automatically.

Option 1 is simpler and safer for MVP. Option 2 is more ergonomic but requires field-count lookup
at parse time.
