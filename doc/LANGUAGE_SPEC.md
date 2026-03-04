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
  - `import a/b ( foo, bar )`

## 2. Declarations

- Type annotation: `name : Type`
- Function definition: `name args = expr`
- Type declarations:
  - alias: `type Name = Target`
  - union: `type Name = A | B`
  - record: `type Name = Ctor(field: Type, ...)`
- Effect declaration:
  - `effect EffectName`
  - indented operation signatures

## 3. Expressions and Statements

- Literals: `Int`, `String`, `Bool` (`True` / `False`), list, tuple.
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
  - `case ...` with `pattern -> expr` arms
- Mutable locals:
  - declaration: `mut x = expr`
  - assignment: `x := expr`

## 4. Type/Entry Rules

- `main` must be `Unit -> Unit` for `run`.
- `check` allows missing `main` annotation, but `run` requires entry constraints.
- Type annotations are optional where inference is sufficient.
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
- `Print` is currently accepted as a built-in effect name.
- `@embed` default handlers are active for `main` effect validation and runtime
  fallback behavior where configured.
