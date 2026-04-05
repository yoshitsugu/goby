# Goby Language Specification (Current)

Status: active
Last updated: 2026-04-06

This file is the current language-spec source of truth for user-visible Goby
syntax/semantics.

`doc/PLAN.md` tracks top-level implementation plans, migrations, and future work.

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
- List index access: `expr[expr]`
  - `xs[i]` evaluates `xs` then `i` and returns the element at zero-based index `i`
  - index must be `Int`; receiver must be `List a`; result type is `a`
  - negative or out-of-bounds index aborts the program (`RuntimeError::Abort`)
  - no negative-index shorthand (e.g. `xs[-1]` aborts rather than wrapping)
  - chaining syntax is supported: `xs[0][1]` indexes into the result of `xs[0]`
    (current runtime limitation: chained indexing only works if intermediate results
    are `Int` or `String` values; `List (List T)` is not yet a representable runtime value)
  - precedence: `expr[expr]` binds tighter than function call application —
    `f xs[0]` parses as `(f xs)[0]`, not `f (xs[0])`; use `f (xs[0])` when indexing before calling
- Tuple member access uses numeric qualified form: `pair.0`, `pair.1`, ...
- Unit value spelling: `()` (canonical).
  - Legacy expression-form `Unit` is rejected.
- Function call:
  - spaced application: `f x`, `f a b`
  - parenthesized: `f(x)`
  - parenthesized empty-arg form `f()` is accepted and is equivalent to `f ()`
- Anonymous functions:
  - single-parameter: `fn x -> expr`
  - multi-parameter: `fn a b -> expr` (desugars to nested single-parameter lambdas)
    - `fn` is a reserved keyword; it cannot be used as a variable name or parameter name
    - the old `|x| -> expr` pipe-lambda syntax has been removed; use `fn x -> expr`
  - placeholder shorthand: `_ * 10` (current supported subset)
  - when a function-typed argument is expected, a named function may be passed directly
    (for example: `map xs add_ten`)
- Operators:
  - precedence: `|>` < `||` < `&&` < `==` / `<` / `>` / `<=` / `>=` < `+` / `-` < `*` / `/` / `%` < unary `!` < call/application
  - `|>`, `||`, `&&`, `==`, `<`, `>`, `<=`, `>=`, `+`, `-`, `*`, `/`, `%` are left-associative
  - unary `!` negates a `Bool`
  - binary operators require spaces around operators
  - current parser still requires parentheses when passing a binary operator expression as the
    argument to a spaced single-argument call:
    - `println (1 + 1)` is supported
    - `println 1 + 1` is not yet supported under the current call/application precedence
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
    - exact-length patterns (no tail): `[1]`, `[_, _]`, `[a, b, c]`
      - these match only when the scrutinee length equals the item count
    - head-checked tail patterns: `[4, ..]`, `["x", ..rest]`
      - these match when the scrutinee length is at least the fixed head-item count
    - tail ignore form: `[..]` is not supported; use at least one head item (e.g. `[x, ..]`)
    - list item literals currently supported in patterns: `Int`, `String` (not `Bool`)
  - `_` is always a wildcard (non-binding), including in list item positions.
- Mutable locals:
  - declaration: `mut x = expr`
  - variable assignment: `x := expr`
  - list element assignment: `x[i] := expr`
    - valid only when `x` is declared with `mut`; assigning through an immutable binding is a type error.
    - `i` must be `Int`; `x` must have type `List a`; `expr` must have type `a`.
    - negative or out-of-bounds index aborts the program (`RuntimeError::Abort`).
    - chained indexed assignment `x[i][j] := expr` updates the element at depth two;
      the same rules apply recursively: the root binding must be `mut`,
      `x[i]` must be `List b`, and `expr` must have type `b`.
    - arbitrary nesting depth is supported by the same rule.
  - mutable list update semantics:
    - mutation is a **rooted update** through the mutable binding.
      After `x[i] := v`, the value stored in `x` changes; any value previously read
      out of `x[i]` is unaffected.
    - reading a list element produces a **value**, not a shared mutable alias.
      ```
      mut a = [[1, 2, 3], [4, 5, 6]]
      b = a[0]          # b is a snapshot value [1, 2, 3]
      a[0][1] := 10
      a[0][1]   # -> 10
      b[1]      # -> 2  (b is unchanged)
      ```
    - a read value stored into a new `mut` binding creates an independent mutable root:
      ```
      a = [[1, 2, 3], [4, 5, 6]]
      mut b = a[0]      # b owns [1, 2, 3] independently
      b[0] := 10
      a[0][0]   # -> 1  (a is unchanged)
      b[0]      # -> 10
      ```
    - this value-oriented read rule applies uniformly across bindings, function call
      arguments, function return values, and closure captures.
- Closure semantics:
  - All lambdas (`fn x -> expr`) are conceptually closures. A non-capturing lambda is the
    zero-capture case of the same model.
  - Capturing an immutable binding copies its value into the closure environment at the
    time the lambda value is created.
  - Capturing a mutable binding (`mut`) captures a **shared mutable cell**, not a snapshot
    of the value at creation time.
    - Reading a captured `mut` inside a lambda observes the latest value in the shared cell.
    - Assigning to a captured `mut` inside a lambda updates the same binding visible outside
      the lambda.
    - Multiple closures capturing the same `mut` binding share the same mutable cell and
      observe each other's writes.

## 4. Type/Entry Rules

- `main` must be `Unit -> Unit` for `run`.
- `check` allows missing `main` annotation, but `run` requires entry constraints.
- Type annotations are optional where inference is sufficient.
- Function type annotations may end with an effect clause: `can EffectA, EffectB`.
  - meaning: evaluating that function body may leave those effects unhandled at the call site.
  - if omitted, the function body must not leave any effect operation unhandled.
  - effects handled internally by `with ... in ...` are not listed in the function's `can` clause.
  - example:
    - `unhandled_effect_function : String -> Unit can Message`
    - if `say_hello s` is wrapped by a matching `with`, the enclosing function type does not use `can Message`.
- Type-variable identifiers in type annotations are lowercase-start names (or `_`).
- For effect-member generic design, `_` in type position is reserved as an anonymous
  type-hole marker (full inference behavior rollout is tracked in `doc/PLAN.md`).
- `if` / `case` are value expressions and all branches must resolve to one compatible result type.
- `void` spelling is rejected; use `Unit`.

## 5. Effects and Handlers

- Function effect annotation: `can EffectA, EffectB`.
- `can` describes the unhandled effects of the annotated function body, not the effects used internally while evaluating it.
- Effect declarations define operations only.
  - current spec does not assign `can` semantics to effect member signatures.
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
  - clause head may use either:
    - unqualified operation name: `yield x -> ...`
    - qualified operation name: `Iterator.yield x -> ...`
  - unqualified clause names are accepted only when they resolve to exactly one visible effect operation.
  - if an unqualified clause name matches multiple visible effects, typechecking fails and the clause must use the qualified `EffectName.operation` form.
  - clause body is checked like an ordinary expression body:
    - effects handled by nested `with ... in ...` do not escape the clause,
    - any effect operation left unhandled by the clause body must be permitted by the surrounding function context.
- Effect checking rule:
  - an effect operation call is valid only if that operation's effect is either:
    - handled by an enclosing `with ... in ...` within the current body, or
    - listed in the surrounding function's `can` clause.
  - otherwise typechecking fails with an unhandled-effect error.
- Call-site rule for effectful functions:
  - calling a function annotated with `can E1, E2` requires the caller context to account for those same effects.
  - a caller may account for them either by:
    - handling them around the call with `with ... in ...`, or
    - itself being inside a function whose `can` clause includes those effects.
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

- Purpose: stdlib default-effect-handler declaration for the minimal built-in
  implicit-prelude `Print` / `Read` onboarding path.
- Canonical form:
  - `@embed <EffectName> <HandlerName>`
  - example: `@embed Print __goby_embeded_effect_stdout_handler`
- Rules:
  - allowed only in `goby/prelude` under stdlib sources
  - the referenced effect must be visible in `goby/prelude`
    - visibility may come from a local declaration or an imported stdlib module
  - one embed per effect per module
  - handler name must be in `__goby_embeded_effect_*`
  - handler must be in known intrinsic set
  - `@embed` both binds the runtime default handler and adds that effect to the
    implicit prelude surface
- Design intent:
  - `@embed` is intentionally narrow.
  - `goby/prelude` remains the only implicit-import entrypoint.
  - `goby/stdio` is the canonical stdlib owner of both `Print` and `Read`;
    `goby/prelude` chooses which visible stdlib-owned effects participate in the
    implicit default surface.
  - current intended use is the prelude-backed `Print` / `Read` defaults so users
    can write simple I/O before learning full effect-handler patterns.
  - the primary user-facing goal is that small scripts can use `Print` / `Read`
    with minimal ceremony, including simple cases that do not require explicit
    imports or user-defined handlers.
  - although `@embed` is stdlib-only, future expansion of its meaning or target
    surface should be treated as a language-design decision, not a convenience-only
    implementation tweak.
  - any future extension should explicitly evaluate its effect on implicit
    availability, module ownership, diagnostics, and runtime semantics before the
    feature is broadened beyond the current `Print` / `Read` onboarding scope.
  - `@embed` is not the general extension mechanism for future host capabilities
    such as file, clock, or network access.
- Legacy form `@embed effect <EffectName>` is rejected.

## 7. Current Runtime/Builtin Notes

- Builtins currently include `print`, `fetch_env_var`, `string.split`, `list.join`.
- Stdlib `goby/list` provides the following higher-order list functions:
  - `each : List a -> (a -> Unit) -> Unit`
    - applies a callback to each element in order; discards results.
  - `map : List a -> (a -> b) -> List b`
    - transforms each element via a callback; returns a new list.
  - `fold : List a -> b -> (b -> a -> b) -> b`
    - left fold: accumulates a result over a list using a binary callback.
    - semantics: `fold [x1, x2, x3] init f == f (f (f init x1) x2) x3`
    - callback argument order: accumulator first, then element (`f acc elem`).
    - `fold [] init f == init` (empty list returns the initial accumulator).
    - **Temporary limitation (2026-03-28):** effectful callbacks follow the same
      caller-`can`-clause model as `each` and `map`; this is not guaranteed to work
      in all execution paths until effect propagation through HOFs is formally designed.
  - `length : List a -> Int`
    - returns the number of elements in the list.
- Stdlib `goby/string` currently provides:
  - `length : String -> Int`
  - `split : String -> String -> List String`
  - `graphemes : String -> List String`
    - returns Unicode Extended Grapheme Cluster segments in source order.
    - `""` returns `[]`.
- Iterator runtime intrinsic contract (current):
  - `__goby_string_each_grapheme` expects handler operation
    `yield : String -> state -> (Bool, state)`.
  - 1-arg form (`__goby_string_each_grapheme value`) threads implicit `Unit` state.
  - 2-arg form (`__goby_string_each_grapheme value initial_state`) threads explicit state.
  - `(False, state)` stops iteration early; `(True, state)` continues.
- Stdlib `goby/int` provides `parse : String -> Int can StringParseError`.
  - accepted form: optional leading `-` followed by one or more ASCII digits.
  - invalid input delegates to `StringParseError.invalid_integer : String -> Int`.
- Stdlib `goby/int` also provides `to_string : Int -> String`.
  - result is canonical base-10 decimal text (`0`, `123`, `-7`).
- `Print` / `Read` implicit availability is anchored at stdlib prelude (`goby/prelude`) `@embed` metadata.
- `goby/stdio` is the canonical owner of the `Print` / `Read` effect declarations and operations.
- These prelude defaults are intentionally a minimal convenience layer, not a
  general host-effect abstraction.
- Prelude `Print` effect exposes:
  - `print : String -> Unit` (no trailing newline)
  - `println : String -> Unit` (ensures trailing `\n`: adds one only when missing)
- Current minimal prelude input contract:
  - `Read.read ()` returns remaining stdin text and then `""` on subsequent reads.
  - `Read.read_line ()` returns one line with one trailing terminator removed
    (`\n`, `\r\n`, `\r`); EOF returns `""`.
  - `Read.read_lines ()` returns remaining stdin split into `List String` by line
    terminators (`\n`, `\r\n`, `\r`), consumes the remaining input, and omits a
    trailing empty item introduced only by the final terminator.
  - stdin is decoded as UTF-8 with replacement for invalid sequences.
- `@embed` default handlers are active for `main` effect validation and runtime
  fallback behavior where configured.
- Execution runtime requirements:
  - `Read` effects require a live WASI Preview 1 host at runtime.
    stdin reads are not available at compile time.
  - The minimum supported runtime is any WASI Preview 1-compliant host (e.g. `wasmtime run`).
  - `Print`-only programs (e.g. `print "hello"`) do not require stdin at runtime;
    the output string is determined during compilation and baked into a minimal Wasm
    module that still executes through the WASI runtime host.
    For programs that use `Read`, a dynamic Wasm module is generated and executed
    against the runtime host's stdin/stdout.
