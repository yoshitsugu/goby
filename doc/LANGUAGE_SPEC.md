# Goby Language Specification (Current)

Status: active
Last updated: 2026-03-06

This file is the current language-spec source of truth for user-visible Goby
syntax/semantics.

`doc/PLAN.md` tracks implementation plans, migrations, and future work.

## 1. Source and Module Basics

- Source extension: `.gb`
- Top-level declarations are indentation-sensitive.
- Statement separators: newline or `;`.
- Comments: `# ...` (line comments only).
- Imports:
  - `import a/b`
  - `import a/b as x`
  - `import a/b ( foo, BarType, BazEffect )`
  - plain/alias import includes module exports for values, types, and effects.
  - selective import may list any mix of value/type/effect names from the module.

## 2. Declarations

- Type annotation: `name : Type`
- Function definition: `name args = expr`
- Naming rules:
  - top-level function/value declarations must start with a lowercase letter (or `_` for intrinsic/internal names).
  - `type` / `effect` names must be `CamelCase`.
  - type constructors in `type` declarations must be `CamelCase`.
- Reserved tokens are not allowed as identifiers (declaration names, parameters,
  local bindings, type/effect member names).
  - Current reserved set:
    - `@embed`, `import`, `type`, `effect`, `handler`, `with`,
      `in`, `resume`, `mut`, `if`, `else`, `case`, `as`, `can`, `using`,
      `True`, `False`
- Type declarations:
  - alias: `type Name = Target`
  - union: `type Name = A | B`
  - record: `type Name = Ctor(field: Type, ...)`
- Effect declaration:
  - `effect EffectName`
  - `effect EffectName a b ...` (optional effect type parameters)
    - effect type parameter names must start with a lowercase letter or `_`
    - duplicate effect type parameter names are rejected
  - indented operation signatures

## 3. Expressions and Statements

- Literals: `Int`, `String`, `Bool` (`True` / `False`), list, tuple.
- List literal spread (expression side):
  - `[a, b, ..xs]` (zero or more prefix elements + one trailing spread segment)
  - spread segment is expression-only syntax and must be trailing
  - `[..xs]`, `[a, ..]`, `[a, ..b, c]` are rejected
  - type rule: prefix elements unify to one element type `a`; spread tail must be `List a`
- Tuple member access uses numeric qualified form: `pair.0`, `pair.1`, ...
- Unit value spelling: `()` (canonical).
  - Legacy expression-form `Unit` is rejected.
- Function call:
  - spaced application: `f x`, `f a b`
  - parenthesized: `f(x)`
  - parenthesized empty-arg form `f()` is accepted and is equivalent to `f ()`
- Anonymous functions:
  - `|x| -> expr`
  - placeholder shorthand: `_ * 10` (current supported subset)
- Operators:
  - precedence: `|>` < `&&` < `==` / `<` / `>` < `+` < `*` < call/application
  - `|>`, `&&`, `==`, `<`, `>`, `+`, `*` are left-associative
  - binary operators require spaces around operators
- Conditionals/case:
  - `if ... else ...` expression form
  - multiline `if` branches may be either a single expression or an indented statement block; block branches return the last expression value
  - `case ...` arms support both:
    - inline body: `pattern -> expr`
    - block body: `pattern ->` then deeper-indented statements (last expression is arm result)
  - list patterns:
    - empty list: `[]`
    - head/tail split: `[head, ..tail]`
      - `head` and `tail` are bindings available in the arm body
      - `tail` is the remaining list value
    - prefix patterns (length `>= item_count`): `[1]`, `[_, _]`, `[a, b, c]`
    - head-checked tail patterns: `[4, ..]`, `["x", ..rest]`
    - tail ignore form: `[..]` is not supported; use at least one head item (e.g. `[x, ..]`)
    - list item literals currently supported in patterns: `Int`, `String` (not `Bool`)
  - `_` is always a wildcard (non-binding), including in list item positions.
- Mutable locals:
  - declaration: `mut x = expr`
  - assignment: `x := expr`

## 4. Type/Entry Rules

- `main` must be `Unit -> Unit` for `run`.
- `check` allows missing `main` annotation, but `run` requires entry constraints.
- Type annotations are optional where inference is sufficient.
- Type-variable identifiers in type annotations are lowercase-start names (or `_`).
- For effect-member generic design, `_` in type position is reserved as an anonymous
  type-hole marker (full inference behavior rollout is tracked in `doc/PLAN.md`).
- `if` / `case` are value expressions and all branches must resolve to one compatible result type.
- `void` spelling is rejected; use `Unit`.

## 5. Effects and Handlers

- Effect annotation: `can EffectA, EffectB`.
- Effect member signatures may also declare dependencies with `can`:
  - example: `trace : String -> Unit can Print`
  - meaning: implementations/handlers of that operation may use `Print`.
  - if omitted, the operation implementation is not allowed to use additional effects.
  - effect dependencies must be acyclic across local `effect` declarations; cycles are a typecheck error.
- Effect-member generic type variables are unified at typecheck time for:
  - effect operation call arguments in handler-covered scopes,
  - `resume` argument validation against the operation return type.
- Handler clause typechecking uses per-clause fresh instantiation for effect-member
  generic signatures (no type-variable leakage across independent calls/clauses).
- Handler value expression:
  - `handler`
  - indented clauses: `op arg -> ...`
- Handler application:
  - inline handler: `with` + indented clauses + `in ...`
  - handler value: `with <handler_expr> in ...`
- Handler clause validation:
  - clause operation name must resolve to exactly one visible effect operation.
  - clause body may use only effects currently handled in scope plus effects declared by
    that operation member's `can` clause.
- `resume`:
  - expression form: `resume <expr>`
  - valid only inside handler operation bodies
  - operation call returns the value passed to `resume`.
  - if a handler clause finishes without any `resume`, evaluation exits the current
    `with ... in ...` scope immediately.
    - the handler clause's final expression becomes the value of the whole
      `with ... in ...` expression.
    - the handler clause's final expression type must unify with the enclosing
      `with ... in ...` expression type.
  - when a handler invocation calls `resume` multiple times, each `resume` restarts the
    continuation from the next resumable point (multi-resume progression contract).
    - once the continuation is fully consumed, further `resume` from the same invocation is a runtime error.
- Legacy syntax removed:
  - top-level `handler ... for ...`
  - `using`

## 6. `@embed` (Stdlib-only)

- Purpose: stdlib default-effect-handler declaration.
- Canonical form:
  - `@embed <EffectName> <HandlerName>`
  - example: `@embed Print __goby_embeded_effect_stdout_handler`
- Rules:
  - allowed only in stdlib sources
  - requires `effect <EffectName>` in same module
  - one embed per effect per module
  - handler name must be in `__goby_embeded_effect_*`
  - handler must be in known intrinsic set
- Legacy form `@embed effect <EffectName>` is rejected.

## 7. Current Runtime/Builtin Notes

- Builtins currently include `print`, `fetch_env_var`, `string.split`, `list.join`.
- List mapping is provided via stdlib `goby/list.map`.
- Iterator runtime intrinsic contract (current):
  - `__goby_string_each_grapheme` expects handler operation
    `yield : String -> state -> (Bool, state)`.
  - 1-arg form (`__goby_string_each_grapheme value`) threads implicit `Unit` state.
  - 2-arg form (`__goby_string_each_grapheme value initial_state`) threads explicit state.
  - `(False, state)` stops iteration early; `(True, state)` continues.
- Stdlib `goby/int` provides `parse : String -> Int can StringParseError`.
  - accepted form: optional leading `-` followed by one or more ASCII digits.
  - invalid input delegates to `StringParseError.invalid_integer : String -> Int`.
- `Print` / `Read` effect resolution is provided via stdlib prelude (`goby/prelude`) embed defaults.
- Prelude `Print` effect exposes:
  - `print : String -> Unit` (no trailing newline)
  - `println : String -> Unit` (ensures trailing `\n`: adds one only when missing)
- Current minimal prelude input contract:
  - `Read.read ()` returns remaining stdin text and then `""` on subsequent reads.
  - `Read.read_line ()` returns one line with one trailing terminator removed
    (`\n`, `\r\n`, `\r`); EOF returns `""`.
  - stdin is decoded as UTF-8 with replacement for invalid sequences.
- `@embed` default handlers are active for `main` effect validation and runtime
  fallback behavior where configured.
