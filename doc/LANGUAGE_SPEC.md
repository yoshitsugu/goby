# Goby Language Specification (Current)

Status: active
Last updated: 2026-03-04

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
    - `@embed`, `import`, `type`, `effect`, `handler`, `with`, `with_handler`,
      `in`, `resume`, `mut`, `if`, `else`, `case`, `as`, `can`, `using`,
      `True`, `False`
- Type declarations:
  - alias: `type Name = Target`
  - union: `type Name = A | B`
  - record: `type Name = Ctor(field: Type, ...)`
- Effect declaration:
  - `effect EffectName`
  - indented operation signatures

## 3. Expressions and Statements

- Literals: `Int`, `String`, `Bool` (`True` / `False`), list, tuple.
- Unit value spelling: `()` (canonical).
  - Legacy expression-form `Unit` is still accepted during migration.
- Function call:
  - spaced application: `f x`, `f a b`
  - parenthesized: `f(x)`
- Anonymous functions:
  - `|x| -> expr`
  - placeholder shorthand: `_ * 10` (current supported subset)
- Operators:
  - precedence: `|>` < `+` < `*` < call/application
  - `|>`, `+`, `*` are left-associative
  - `+` and `*` require spaces around operators
- Conditionals/case:
  - `if ... else ...` expression form
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
- `if` / `case` are value expressions and all branches must resolve to one compatible result type.
- `void` spelling is rejected; use `Unit`.

## 5. Effects and Handlers

- Effect annotation: `can EffectA, EffectB`.
- Handler value expression:
  - `handler`
  - indented clauses: `op arg -> ...`
- Handler application:
  - canonical: `with <handler_expr> in ...`
  - sugar: `with_handler ... in ...`
- `resume`:
  - expression form: `resume <expr>`
  - valid only inside handler operation bodies
  - one-shot continuation semantics
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

- Builtins currently include `print`, `map`, `fetch_env_var`, `string.split`, `list.join`.
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
