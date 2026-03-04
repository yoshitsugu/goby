# Effect Renewal Plan: Handler Value Model (`handler` / `with` / `with_handler`)

Status: Draft  
Owner: Goby core/runtime track  
Last updated: 2026-03-04

## 1. Purpose

This document replaces the previous "inline-clause `with`" migration plan with a
handler-value model aligned with the latest effect renewal notes.

Target direction:

- `handler` becomes an expression/value (can capture lexical scope).
- `with` applies exactly one handler value to a body.
- `with_handler` provides inline handler syntax sugar.
- `Handler(<Effects...>)` is introduced as a first-class type.

## 2. Locked Design (Pre-Implementation)

### 2.1 Effect declaration syntax

```goby
effect Yield
  yield : a -> Unit

effect SayHello
  say_hello : Unit
  say_goodbye : String -> Unit
```

- `effect <CamelCaseName>` starts an effect declaration.
- Members are indented operation signatures.
- Newline-separated and `;`-separated forms are both accepted.

### 2.2 Handler as expression/value

```goby
emit_handler =
  handler
    emit x ->
      f x
      resume True
```

- `handler` is an expression that evaluates to a handler value.
- Handler bodies are lexically scoped and may capture local bindings.
- Closures and local helper functions are visible in handler clause bodies.

### 2.3 Handler type

```goby
a : Handler(Log)
b : Handler(Env, Log)
```

- `Handler(E)` means "handles effect `E`".
- `Handler(E1, E2, ...)` means one handler value can handle multiple effects.
- `Handler(Env, Log)` and `Handler(Log, Env)` are type-equivalent.
- A multi-effect handler must provide all required operations for each listed effect.

### 2.4 `with` and `with_handler`

```goby
with emit_handler
in
  iter xs
```

```goby
with_handler
  emit x ->
    f x
    resume True
in
  iter xs
```

- `with` accepts exactly one handler expression/value.
- `with_handler` is syntax sugar for `with (handler ...)`.
- `in` introduces the handled body block.

### 2.5 Dispatch and conflict policy

- Dynamic dispatch selects the nearest enclosing active handler that defines the op.
- A single handler cannot define duplicate clauses for the same operation.
- If one handler value claims multiple effects that expose the same operation name
  (for example `Log.log` and `Logger.log`), compile-time error for ambiguous op.
- Future extension: explicit qualified clause names (`Effect.op`) may relax this.

### 2.6 Reserved keywords

Lock as reserved keywords for this renewal:

- `with`
- `with_handler`
- `in`
- `handler`
- `effect`

## 3. Locked Decisions (2026-03-04)

1. Resume usage rule:
   - keep current one-shot guarantee,
   - reject double `resume` in the same handler invocation.
2. Handler clause grammar:
   - support only untyped clause headers (`op x -> ...`),
   - type information is sourced from `effect` declarations.
3. `Handler(...)` effect list formatting/tokenization:
   - comma-separated effect list is accepted with flexible whitespace,
   - tokenizer/parser ignores spaces around separators (`Handler(E1,E2)` and
     `Handler(E1, E2)` are equivalent).
4. Ambiguity diagnostic wording:
   - follow existing plain-message style used by typechecker (`... is ambiguous ...`),
   - canonical message:
     `operation '<op>' is ambiguous across effects in Handler(...): <EffectA>, <EffectB>`.
5. Compatibility policy:
   - migration can be aggressive (pre-1.0, no external users),
   - legacy `handler ... for ...` and `using` are supported only in a short bridge phase,
     then removed by default quickly.

## 4. Migration Phases

### P0. Spec Lock in `doc/PLAN.md`

Record the semantics above in `doc/PLAN.md` and link back to this plan.

Completion criteria:

- `doc/PLAN.md` has canonical syntax and static semantics for:
  - `handler` expression,
  - `Handler(...)` type,
  - `with` / `with_handler ... in`.
- Section 3 locked decisions are mirrored in `doc/PLAN.md`.

### P1. Parser + AST

Add syntax and AST nodes for:

- handler expression (`Expr::Handler`-like node),
- `with <expr> in <block>` expression,
- `with_handler ... in ...` sugar (desugared in parser or lowering boundary),
- `Handler(...)` type annotation parsing.

Completion criteria:

- Positive parser tests:
  - local handler value binding,
  - `with` with handler variable,
  - inline `with_handler`.
- Negative parser tests:
  - missing `in`,
  - malformed handler clause,
  - malformed `Handler(...)` type list.

### P1.5 Tooling + Formatter + CLI

- Formatter canonicalizes:
  - `with ... in` layout,
  - `with_handler` clause indentation.
- Formatter may emit either `Handler(E1,E2)` or `Handler(E1, E2)`, but parser must
  accept both forms.
- CLI diagnostics include line/column/snippet quality parity.

Completion criteria:

- Golden formatter tests for new syntax.
- CLI integration tests for parse errors around `with`/`with_handler`.

### P2. Typechecker

Implement core typing/effect rules:

- `handler` expression type inference/checking as `Handler(...)`.
- Clause parameter typing from effect operation signatures.
- `resume` argument type checking against operation return type.
- `with` body effect reduction by handled operations/effects.
- Unhandled effect diagnostics remain consistent with existing `can` policy.
- `Handler(E1, E2)` order-insensitive equivalence.
- Compile-time rejection for operation-name conflicts in a single multi-effect handler.

Completion criteria:

- New tests for:
  - lexical capture in handler expression,
  - `Handler` type annotation acceptance/rejection,
  - conflict rejection for overlapping op names,
  - unresolved effect acceptance/rejection with `can`.

### P3. Runtime

Add runtime support for handler values:

- Runtime representation for handler closures with captured environment.
- Active handler stack for `with`.
- Operation dispatch to nearest active handler.
- Resume continuation bridging parity with existing one-shot model.

Completion criteria:

- Runtime tests:
  - local capture via `handler` value,
  - nearest-handler precedence under nested `with`,
  - abortive path (no `resume`),
  - resume success path.

### P4. Examples + Stdlib Migration

- Migrate effect examples to `handler` value + `with`/`with_handler`.
- Add at least one iterator-style example that captures local callback.
- Remove workarounds caused by top-level-only handler limitations.

Completion criteria:

- `examples/` contains canonical new syntax samples.
- Docs in `doc/PLAN.md` and examples stay consistent.

### P5. Compatibility and Deprecation

- Keep legacy top-level `handler ... for ...` and `using` temporarily.
- Keep bridge phase intentionally short.
- Status update (2026-03-04): compatibility warning window is complete in CLI;
  legacy syntax is now rejected by default by `goby-cli check/run`.

Completion criteria:

- Migration guide old -> new patterns.
- Legacy syntax warning (single release window) references `with`/`with_handler`.
- Legacy syntax is rejected by default immediately after bridge window.

Migration guide reference:
- `doc/EFFECT_RENEWAL_MIGRATION.md`

### P6. Removal

- Remove legacy syntax/parser/runtime paths after migration confidence.
- Status update (2026-03-04): complete.
  - CLI default rejection is landed.
  - P6-R2 core removal landed (`Stmt::Using` active AST removal, runtime/typecheck legacy path removal).
  - P6-R3 schema pruning landed (`Module.handler_declarations` / `HandlerDecl` removal).
  - P6-R4 docs/test naming cleanup and closure validation landed.

Completion criteria:

- No active docs/examples rely on legacy syntax.
- `cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings` pass.

## 5. Governance Rules

- Each phase PR must include:
  - spec delta,
  - tests,
  - implementation changes,
  - `doc/STATE.md` progress update.
- Validation flow is mandatory on every change (no exceptions):
  - `cargo fmt`
  - `cargo clippy -- -D warnings`
  - `cargo test`
  - (optional but recommended) `cargo check`
- No phase is complete without checklist update in this document.

## 6. Immediate Next Step

P6 is closed. Follow-up work should proceed under `doc/PLAN.md` post-MVP tracks
(runtime optimization, stdlib foundation, tooling).
