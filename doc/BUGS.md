# Known Bugs

## BUG-001: handler `for` clause with unknown effect name gives misleading error

**Status:** Closed (obsolete after effect-renewal removal)
**Discovered:** 2026-03-02 (session 28)

### Symptom

```
handler PrintErrorHandler for ErrorEffect   -- ErrorEffect is not declared
  raise e = ...

main =
  using PrintErrorHandler
    raise Error(message: "x")
```

Error previously reported:

```
typecheck error in main: effect operation `raise` is not handled by any enclosing `using` block
```

### Current behavior (2026-03-04)

```
parse error: legacy top-level `handler ... for ...` is no longer supported; use `handler` expressions with `with`
```

### Root Cause

The bug belonged to legacy top-level handler declarations (`handler ... for ...`), which
were removed in the effect renewal migration.

### Fix

No direct fix is needed in current code because the legacy syntax path no longer exists.
Any reintroduction of top-level handler declarations should include explicit effect-name
validation at declaration time.

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

### Resolution

Implemented as positional single-field constructor sugar:

- `Ctor(value)` is accepted when the constructor has exactly one field.
- Lowered/checked equivalently to `Ctor(field: value)`.
- Multi-field constructors still require named-field syntax.
