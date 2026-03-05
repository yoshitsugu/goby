# Effect Renewal Migration Guide (`using` / `handler ... for ...` -> `handler` + `with`)

Last updated: 2026-03-04

This guide documents how to migrate legacy effect-handler syntax to the canonical
handler-value model.

## 1. Scope

- Legacy syntax (deprecated and rejected by CLI):
  - top-level `handler Name for Effect`
  - `using HandlerA, HandlerB`
- Canonical syntax:
  - handler values via `handler ...`
  - handler application via `with <handler_expr> in ...`
  - inline handler via `with` + indented clauses + `in ...`

## 2. Pattern Mapping

### 2.1 Top-level handler declaration

Before (legacy):
```goby
handler LogHandler for Log
  log msg =
    print msg
```

After (canonical value):
```goby
log_handler = handler
  log msg ->
    print msg
```

### 2.2 Single handler application

Before (legacy):
```goby
using LogHandler
  log "hello"
```

After (canonical):
```goby
with log_handler
in
  log "hello"
```

Or inline:
```goby
with
  log msg ->
    print msg
in
  log "hello"
```

### 2.3 Multiple handlers

Before (legacy):
```goby
using LogHandler, EnvHandler
  show_env_var "GOBY_PATH"
```

After (canonical nested `with`):
```goby
with env_handler
in
  with log_handler
  in
    show_env_var "GOBY_PATH"
```

## 3. Practical Notes

- `with` accepts either:
  - one handler value expression (`with <handler_expr> in ...`), or
  - inline handler clauses (`with` + indented clauses + `in ...`).
- `goby-cli check/run` rejects legacy syntax by default as of 2026-03-04.

## 4. Current Compatibility Window Policy

- Project stage is pre-1.0 and migration is intentionally aggressive.
- Legacy syntax is temporary and already rejected by default in CLI.
- Keep new examples and docs in canonical syntax only.
