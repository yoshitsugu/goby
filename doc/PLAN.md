# Goby Language Plan (Draft)

This document tracks:

- what is already visible in the current draft,
- what is fixed for MVP,
- what remains open or deferred.

Notes:

- `doc/LANGUAGE_SPEC.md` is the source of truth for current language
  syntax and semantics.
- `PLAN.md` is the top-level roadmap and execution-planning document.
- `doc/PLAN_IR.md` is the completed detailed roadmap for IR-lowering convergence and
  remains the reference when new lowering-boundary issues appear.
- When language syntax or semantics change, update
  `doc/LANGUAGE_SPEC.md` in the same change.
- When language syntax changes, also verify whether syntax
  highlighting definitions need updates (`tooling/syntax/textmate`,
  `tooling/vscode-goby/syntaxes`, `tooling/emacs`, `tooling/vim`), and update
  them in the same change when needed.

## 1. Confirmed in Current Draft

Based on `examples/*.gb`:

- `.gb` source file extension
- Function definition syntax: `name args = expr`
- Type annotation syntax: `name : A -> B`
- Type inference for omitted annotations
- Statement terminator: newline or `;`
- Indentation-based block expressions
- Function-local scopes and blocks are defined by indentation
  (spaces and tabs are both allowed)
- Last expression is the return value
- Effect annotation syntax: `can Print`
- Anonymous functions: `|x| -> ...` and placeholder shorthand (`_ * 10`)
- Named function references are valid in higher-order call positions
  (for example: `map xs add_ten` when the parameter expects a function value).
- Basic shown types: `Int`, `String`, tuple `(A, B)`, `List T`

## 2. MVP Decisions and Remaining Open Items

### 2.0 Locked for Current MVP

- Development-phase change policy (locked 2026-03-04):
  - This project is still in a personal early-development phase; immediate breaking
    changes are allowed when they improve consistency/simplicity.
  - However, avoid short-sighted shortcuts that are likely to become long-term design debt.
    Even if a change is breaking now, it should still align with plausible future
    language/tooling architecture.
  - Phase 5 execution policy (locked 2026-03-09):
    - optimize for architecture quality, but make progress in small reversible steps.
    - prefer additive `Out`-path entrypoints first; keep legacy fallback until parity is proven.
    - do not refactor `eval_expr_ast` broadly in one step; migrate one call path at a time.
    - after each micro-step, run focused regression tests first, then full quality gate.
    - if a step fails, revert that step and retry with a smaller scope.
  - Intentional-break policy for design-first work (locked 2026-03-09):
    - target is globally rational and healthy architecture over local short-term stability.
    - temporary breakage is allowed when needed for the ideal design change.
    - before changing code, explicitly list expected breakages and affected tests/paths.
    - proceed only while breakages remain understandable and recoverable.
    - operational rollback thresholds:
      - if two consecutive attempts fail to narrow breakage to one subsystem/file group,
        roll back to the last stable commit.
      - if expected breakages cannot be converted into concrete failing tests/paths within
        30 minutes, roll back and retry with a narrower change unit.
    - if breakage becomes hard to reason about or cannot be handled, roll back to the
      last stable point and retry with a narrower change unit.
  - Personal-project architecture policy (locked 2026-03-25):
    - Goby is currently for personal use; temporary instability during development is acceptable.
    - prefer the long-term coherent design from the start, even when it requires a change that
      keeps the tree broken for a while during implementation.
    - do not justify ad hoc architecture by saying the work must be split into very small
      incremental slices.
    - incremental execution is still useful for verification and rollback, but it must serve the
      intended end-state design rather than accumulating temporary local fixes as permanent structure.
  - Plan-label hygiene policy (locked 2026-03-25):
    - plan milestone labels and numbering such as `Track E`, `WB-3`, `E4`, or similar roadmap IDs
      are planning-only metadata.
    - do not leave those labels in code comments, test names, diagnostics, or user-visible strings.
    - when implementation comments need historical context, describe the technical purpose directly
      instead of referencing transient plan numbering.

- Function calls support both `f x` and `f(x)`.
  - spaced application supports multiple args (`f a b c`) and is parsed left-associatively
    (`(((f a) b) c)`).
  - `f(x)` is used when precedence needs to be explicit.
  - `f ()` is parsed as Unit-argument function application.
  - constructor parsing follows naming convention: only `CamelCase(...)` / `CamelCase (...)`
    is treated as constructor form.
- Naming convention lock:
  - top-level declaration names start with lowercase (`_` reserved for intrinsic/internal symbols).
  - `type` / `effect` declarations and constructors are `CamelCase`.
- Block-local binding semantics are fixed for MVP:
  - `name = expr` is a local binding statement only when `name` is an identifier and `=` is assignment (not `==`).
  - bindings are visible to subsequent statements in the same declaration body.
  - re-binding the same name in the same body is allowed; the newer binding shadows the older one from that point onward.
  - bindings are declaration-local and do not escape to other declarations.
  - expression-value policy:
    - local binding forms such as `name = expr` and `mut name = expr` should evaluate to `()`.
    - mutation assignment `name := expr` should also evaluate to `()`.
    - this is the intended direction so these forms can occupy expression positions without requiring an extra trailing `()`.
- Operator precedence/associativity is fixed for MVP:
  - precedence (low -> high): pipeline `|>` < addition `+` < multiplication `*` < call/application (`f x`, `f(x)`, `receiver.method(...)`).
  - `|>`, `+`, `*` are left-associative.
  - for MVP parser compatibility, infix `+` and `*` require spaces on both sides.
  - pipeline callee must be an identifier (`expr |> f`).
- Type annotation style is unified to `name : Type`.
- Statement terminators are newline or `;`.
- Comment syntax is fixed for MVP:
  - single-line comment marker is `#`.
  - line-end comments are allowed (for example: `x = 1 # note`).
  - `#` starts a comment only outside string literals; inside strings it is a normal character.
  - block comment syntax is not included in MVP.
  - `#!` has no special shebang handling in MVP; it is treated as a normal `#` comment.
- Generic type application syntax is fixed to Haskell-style spacing:
  - single-parameter examples: `List Int`.
  - multi-parameter examples: `TypeX a b`.
  - nested applications requiring grouping use parentheses: `TypeX (TypeY a b) c`.
  - diagnostics should render type applications in the same style.
- Indentation-based blocks accept both tabs and spaces.
  - for MVP, any line with at least one leading space or tab is treated as an indented block line.
  - mixing tabs and spaces in the same block is allowed in MVP (no normalization/error rule yet).
- CLI commands:
  - `goby-cli run <file.gb>` uses `main` entrypoint only and executes generated Wasm via external `wasmtime`.
    - if `wasmtime` is not installed, execution is skipped with an informational message.
    - current invocation: `wasmtime run <file.wasm>` with WASI-standard `_start` export.
  - `goby-cli check <file.gb>` performs parse/typecheck without runtime entrypoint.
- `main` type is restricted to `Unit -> Unit` for MVP.
  - `main` type annotation is required for `run`; optional for `check`.
- Legacy `void` type spelling is rejected in type annotations.
- First backend target is Wasm.
- Effects and handlers are available in MVP runtime via `effect`, `handler`, and `with`.
  - handler dispatch follows lexical nearest-handler stack order.
  - `can` clauses are validated (declared effect or built-in effect names).
- Handler application syntax supports:
  - inline handler: `with` + indented clauses + `in <body>`
  - handler value: `with <handler_expr> in <body>`
- Legacy syntax (`handler ... for ...`, `using`) is no longer accepted by the
  language parser as of 2026-03-04.
  - CLI commands (`check` / `run`) therefore reject such sources by default.
- MVP built-ins: `print`, `fetch_env_var`, `string.split`, `list.join`.
  - list mapping should use stdlib `goby/list.map`.
  - `print` execution is resolved by compiler/runtime internals (default stdio print path),
    not by a user-visible stdlib handler definition.
- Stdlib integer parse entrypoint is `int.parse`.
  - contract: parse optional leading `-` + one or more ASCII digits as base-10 `Int`.
  - failure path is effect-based: `StringParseError.invalid_integer : String -> Int`.
- Stdlib integer formatting entrypoint is planned as `int.to_string`.
  - contract: render an `Int` to canonical base-10 decimal `String`.
  - examples:
    - `int.to_string 0 -> "0"`
    - `int.to_string 123 -> "123"`
    - `int.to_string -7 -> "-7"`
  - scope note:
    - this is an explicit conversion helper under `goby/int`; it does not change `Print.println`.
    - users should compose it directly where needed (for example `println (int.to_string n)`).
- `examples/basic_types.gb` is a parse/typecheck target, not a runnable entrypoint target.
  - no `main` addition and no `--entry` option in MVP.
- `examples/function.gb` is a canonical MVP run target and must be preserved as-is.
  - no simplification/downgrade of `examples/function.gb` is allowed.
  - `goby-cli run examples/function.gb` target output is:
    - `90`
    - `[30, 40, 50]`
    - `[60, 70]`
    - `something`
    - `15`
- String escape sequences in string literals: `\n`, `\t`, `\\`, `\"`.
- `case` expression syntax: multi-line lookahead, arm separator ` -> `, wildcard `_`.
  - `CasePattern` supports:
    - `IntLit`, `StringLit`, `BoolLit` (`True`/`False`), `Wildcard`
    - list patterns: `[]`, `[head, ..tail]`, `[x]`, `[x, y]`, `[4, ..]`, `[_, _]`
  - list-pattern typing/matching intent:
    - `[]` matches only empty `List _`.
    - `[p1, p2, ...]` matches list length `>= item_count` (prefix match).
    - in `[head, ..tail]` arm body, `head` is bound to element type `a`, `tail` is bound to `List a`.
    - `_` is wildcard and never creates a binding.
  - parser rejects malformed list patterns (e.g. `[..xs]`, `[x, ..x]`, `[x, ..tail, y]`).
  - `else if` chaining is not supported in MVP.
- `if ... else ...` expression: indentation-based two-branch form.
- `==` equality operator: produces `Bool` at runtime.
- Boolean operators: `&&`, `||`, and unary `!` are implemented.
- Handler dispatch: lexical nearest-handler stack walk (no alphabetical fallback).
- Runtime execution model: prefer native lowering for the supported subset; fallback to
  compile-time interpreter (`resolve_main_runtime_output`) for unsupported forms.

### 2.1 Syntax and Parsing

- **String interpolation `${}`** (implemented).
  - Parser supports `${ expr }` inside string literals and lowers it to `Expr::InterpolatedString`.
  - Typechecker treats interpolated literals as `String`.
  - Runtime/codegen evaluates each segment and stringifies embedded expression values.
  - Near-term direction:
    - keep string interpolation as the default "show a value for debugging/output" path
      instead of introducing a separate `trace` helper for now.
    - when additional runtime value categories need human-readable output, prefer extending
      interpolation stringification coverage consistently before adding a new debug-print surface.
- **List `case` patterns** (implemented).
  - Parser/AST support includes prefix, head-literal, wildcard, and tail-binding forms.
  - List item literals in MVP are `Int` / `String` only (no `Bool` list-pattern items).
  - Parser rejects malformed forms and duplicate binders.
- **`case` arm block body** (implemented, 2026-03-05).
  - Both inline (`pat -> expr`) and block (`pat ->` + indented statements) arm bodies are supported.
  - Block arm type is the type of the last expression; trailing non-expression statement is rejected.
  - `case` is value-returning in all positions; branch type unification enforced for both `if` and `case`.
  - Effectful expressions inside `case` arm blocks work with parity between `check` and `run`.
  - Follow-up: generalize expression-level block parsing beyond `case` arms.
  - Planned semantic relaxation:
    - once binding and assignment forms are expression-valued as `()`, block tails ending in
      `name = expr`, `mut name = expr`, or `name := expr` should be accepted as valid `Unit`-valued endings.
- **Tuple index access `expr.N`** (implemented).
  - `a.0`, `a.1` parse as qualified numeric member access and are typechecked/runtime-evaluated as tuple index access.
  - Numeric member access is valid only for tuple receivers.
  - Non-tuple receivers (for example `Status.0`) are rejected.
- **List spread/concat expression (`[a, b, ..xs]`)** (implemented).
  - Shipped behavior:
    - expression-side list literals support zero or more prefix elements plus one trailing spread segment.
    - examples: `[f(x), ..ys]`, `[a, b, c, ..xs]`.
  - Parsing/AST rules:
    - parser accepts at most one trailing `..expr`.
    - parser rejects malformed forms (`[..xs]`, multiple spread segments, non-trailing spread).
    - `case` list-pattern syntax remains separate from expression syntax.
    - AST stores prefix elements and optional spread tail distinctly.
- **List index precedence realignment (`f xs[0]` / `function list[1][0]`)** (planned).
  - Goal:
    - raise postfix list-index access `expr[expr]` above spaced function application so
      `f xs[0]` parses as `f (xs[0])`.
    - ensure chained forms follow the same rule, so `function list[1][0]` parses as
      `function ((list[1])[0])`, not `(function list[1])[0]`.
  - Current mismatch to resolve:
    - the parser currently runs `parse_list_index_suffix` before `parse_call_expr`,
      which makes `f xs[0]` parse as `(f xs)[0]`.
    - parser regression tests currently lock in that old behavior and must be inverted
      in the implementation slice.
  - Planned implementation steps:
    - refactor expression parsing so postfix forms (`[]`, and any future postfix accessors)
      are parsed from an already-formed atom/call-argument term instead of scanning the whole
      source string before spaced application.
    - keep explicit postfix chaining intact (`xs[0][1]`, `f()[0]`, `[1, 2, 3][0]`) while
      making spaced call arguments parse each argument term before outer application folding.
    - preserve existing operator precedence outside this boundary (`+`, `*`, pipeline, etc.)
      and re-check ambiguous combinations like `f xs[0] + 1` and `print ([1, 2][0])`.
  - Validation/update checklist:
    - update parser tests to assert the new AST shape for `f xs[0]` and add a regression for
      `function list[1][0]`.
    - run parse/typecheck/runtime regression coverage for list indexing, especially nested-list
      cases that currently depend on postfix chaining.
    - update `doc/LANGUAGE_SPEC.md`, examples, and syntax-highlighting rules in the same
      implementation change after the parser behavior is switched.

### 2.2 Types and Checking

- TODO (Deferred): declaration-side generic parameter binders
  (for example, `id : a -> a` with explicit binders).
- Planned numeric expansion: `Float` type backed by Wasm `f64`.
  - intended surface type name is `Float`.
  - initial implementation target is IEEE 754 double precision semantics via Wasm `f64`.
  - MVP scope should stay explicit about unsupported edge cases until diagnostics/runtime text are defined:
    - NaN display/equality wording,
    - infinity parsing/printing policy,
    - mixed `Int`/`Float` operator coercion rules.
- **List `case` pattern typing** (implemented).
  - For `case xs` with list patterns, typecheck verifies scrutinee is `List _` when known.
  - Per-arm local environment extension is implemented:
    - item binders (`[a, b]`, `[a, ..]`) bind each item as `a`.
    - tail binder (`[..tail]` via `[x, ..tail]`) binds `tail : List a`.
  - MVP `Ty::Unknown` tolerance is preserved when information is insufficient.
  - Native Wasm capability checker still treats list patterns as unsupported;
    these cases execute via fallback runtime path.
- **List spread expression typing (`[a, b, ..xs]`)** (implemented).
  - Shipped rules:
    - for `[e1, e2, ..., ..tail]`, all prefix elements must unify to one element type `a`.
    - `tail` must typecheck as `List a`.
    - whole expression type is `List a`.
  - Diagnostics:
    - conflicting prefix element types report expected vs actual element type.
    - non-list or mismatched list tails report the expected `List a` shape explicitly.
- Type annotation placement rules (required vs optional locations).
- Type error diagnostics quality bar is fixed for MVP:
  - diagnostics must be non-empty and human-readable plain text.
  - when a declaration is known, diagnostics must include the declaration name.
  - type mismatch diagnostics must include both expected and actual type names.
  - composite types should be rendered with full shape (for example: `List Int`, `(String, Int)`), not collapsed labels.
  - line/column reporting is not required in MVP.
- **Higher-order function-type checking** (planned, high priority).
  - Goal:
    - higher-order call sites must verify that a function value matches the function type
      required by the callee parameter.
    - the immediate user-facing target is callback position diagnostics such as
      `each xs println`, where the name resolves successfully but its function type does not fit.
    - direct call argument mismatches should continue to be rejected, but the priority gap to close
      now is the unresolved higher-order function-type mismatch.
  - Current known gaps:
    - implicit-prelude `print`/`println` now preserve `String -> Unit`, but higher-order ordinary-call
      checking still allows some callback mismatches to slip through.
    - `ensure_known_call_targets_in_expr` currently answers only "is the callee known?", not
      "does this function-valued argument match the parameter's required function type?".
    - effect operation calls already have argument unification logic in `typecheck_effect_usage.rs`,
      but ordinary declaration/value calls do not yet enforce the same quality bar for function-valued arguments.
  - Design constraints:
    - do not special-case `println` / `print`; fix higher-order function-type checking generically.
    - preserve current named-function higher-order ergonomics (`map xs add_ten`) and generic function instantiation behavior.
    - keep partial-application semantics intact where the parser/AST currently represent curried calls as nested single-arg calls.
    - diagnostics for higher-order mismatches should read as "resolved name, wrong function type",
      not as "unknown function" and not as a direct-call scalar argument mismatch when the actual bug
      is callback incompatibility.
  - Planned implementation slices:
    - `CALL-T1` higher-order signature matcher:
      - extract a reusable helper that compares a function-valued argument type against the
        function type required by a parameter position.
      - the helper should reuse the existing unification machinery (`TypeSubst`) so generic
        callback positions instantiate correctly per call site.
      - the helper must surface "required function type" vs "actual function type" so diagnostics
        can say, for example, that `Int -> Unit` is required but `println` has type `String -> Unit`.
    - `CALL-T2` ordinary-call integration:
      - run the higher-order signature matcher wherever ordinary calls feed a function-valued argument
        into a function-typed parameter.
      - cover direct / qualified / pipeline call shapes as they appear in the AST.
      - keep "unknown function or constructor" diagnostics higher priority when name resolution fails entirely.
    - `CALL-T3` callback regression coverage:
      - lock the representative regressions:
        - `each [1, 2, 3] println` must fail because callback type `String -> Unit` does not satisfy required type `Int -> Unit`
        - the diagnostic should describe a higher-order function-type mismatch, not an unknown-name error
        - `each ["a", "b"] println` must pass
        - `map [1, 2, 3] println` must fail because callback type does not satisfy the required mapper function type
        - named-function callback variants continue to pass when signatures align.
    - `CALL-T4` generic and partial-application parity:
      - verify that generic functions still instantiate independently per call site.
      - verify that partial application does not over-eagerly reject intermediate function values.
      - add regressions for:
        - generic declaration call with concrete argument,
        - generic declaration passed as callback,
        - nested curried calls where only the second application mismatches.
    - `CALL-T5` diagnostics + docs:
      - add wording specific to higher-order mismatches:
        - required callback type
        - actual resolved function type
        - resolved function name when available
      - keep direct-call diagnostics distinct from callback diagnostics.
      - update `doc/LANGUAGE_SPEC.md` only if the documented callable/typecheck rules need clarification;
        no syntax change is intended.
      - add a short example under `examples/` only if an existing sample currently demonstrates the buggy acceptance path.
  - Validation checklist:
    - focused tests for higher-order callback mismatch cases plus adjacent direct/qualified/pipeline cases.
    - `cargo check`
    - `cargo test`
    - verify `import goby/list ( map, each )` plus `each xs println` now fails during `goby-cli check`
      with a higher-order function-type mismatch once `xs : List Int`.

### 2.3 Effect System

- Target semantics lock (2026-03-15):
  - `can` on a function type means "effects that remain unhandled after evaluating this function body".
  - effects discharged by `with ... in ...` inside the function body must not appear in that function's `can` clause.
  - omitting `can` means the function body must not leave any effect unhandled.
  - calling a `can`-annotated function is allowed only when the caller also accounts for those effects
    (either by an enclosing `with` at the call site or by the caller's own `can` clause).
  - inline `with` clause heads default to unqualified operation names (`yield`) when resolution is unique.
  - if multiple visible effects expose the same operation name, the clause must use a qualified name (`Iterator.yield`).
  - example consequence:
    - `stdlib/goby/string.gb` helpers that invoke `yield` only inside a matching `with` should not carry `can Iterator`.
    - a function that directly calls `say_hello` from `effect Message` without handling it must carry `can Message`.
- Current implemented checks:
  - `can` effect names must be declared (or built-in).
  - uncovered effect operation calls are rejected unless covered by enclosing handler scope
    (`with`).
  - calls to `can`-annotated functions require an appropriate enclosing handler scope.
- Current runtime behavior:
  - effect operations dispatch through installed handlers.
- How to represent multiple effects (`can Print + Read` or other syntax) — deferred.
- Effect propagation rules for higher-order functions — deferred.
- Effect diagnostics UX polish (wording/format consistency) — deferred.
- Warning mechanism for lexical shadowing of visible effect operation names
  (for example local `a` shadows operation `a`) — deferred.
  - resolution rule remains: lexical value namespace wins.
  - warning is planned as tooling/diagnostics improvement, not a type error.

#### `can` Semantics Alignment (completed 2026-03-15)

- Old effect-member dependency model removed (dependency cycles, `op_required_effects` bookkeeping).
- Single residual-effect checker: each expression contributes unhandled effects; `with` discharges them.
- `with` clause name resolution: bare names accepted only when unique; qualified `Effect.operation` required when ambiguous.
- Diagnostics updated; `can Iterator` removed from locally-handled stdlib functions.
- `can` is reserved for unhandled function effects only; effect member signatures do not declare dependency effects.


#### Effect Renewal/Resume Status (Summary)

- Renewal syntax model is locked and active (current shipped behavior as of 2026-03-05):
  - `handler` is a value,
  - handler application uses `with` (inline handler or handler value form).
- Legacy forms (`handler ... for ...`, `using`) are fully removed from parser/runtime/typecheck paths.
- `resume` support is active on current semantics:
  - outside-handler `resume` is rejected,
  - resumed-value type mismatch is rejected,
  - conservative syntactic multi-`resume` rejection is removed,
  - multi-resume progression is active (continuation advances to next resumable point),
  - handler clause completion without `resume` exits only the current
    `with ... in ...` scope with clause value.
- Runtime dispatch semantics:
  - nearest lexical handler wins,
  - embedded/default-handler fallback applies only when no explicit handler captures the operation.
- Post-MVP follow-up remains:
  - continue migration from name-based runtime dispatch to compiled operation identity (`EffectId`/`OpId`),
  - evaluate explicit `discontinue` only as a later separate proposal.

#### `with` Syntax Unification (completed 2026-03-05)

- `with` is the only handler-application syntax (inline and handler-value forms).
- Legacy `handler ... for ...` / `using` syntax removed from parser/runtime/typecheck.


### 2.4 Standard Library Surface (MVP)

- Core modules to ship first (`Int`, `String`, `List`, `Env`) — minimal built-ins implemented.
- Naming conventions for stdlib functions — established.
- Minimal collection API for immutable workflows — deferred.
- **`int.to_string` stdlib addition** (planned).
  - Goal:
    - add `to_string : Int -> String` to `stdlib/goby/int.gb` as the explicit integer-formatting helper.
    - support compositions such as `println (int.to_string n)` and `map xs int.to_string`
      without widening the type of `Print.println`.
  - Semantics:
    - output is canonical decimal ASCII with optional leading `-`.
    - no leading `+`, no surrounding whitespace, no separators, no locale-specific formatting.
    - `0` renders as `"0"`.
    - negative values render as `"-"` plus the magnitude digits.
  - Design constraints:
    - keep this as a normal stdlib surface under `goby/int`, not as a `println` convenience path.
    - if runtime/backend support is required, prefer one shared implementation path rather than
      scattering `int.to_string` special-cases across fallback and Wasm code paths.
    - preserve round-trip intent with `int.parse` for decimal strings in range:
      `int.parse (int.to_string n)` should yield `n` absent handler interception.
    - while `print` / `println` remain `String -> Unit`, broad "show" behavior should continue
      to flow through string interpolation (`"${value}"`) rather than a dedicated debug helper.
  - Planned implementation slices:
    - `INTSTR-S1` surface + docs:
      - declare `to_string : Int -> String` in `stdlib/goby/int.gb`.
      - update `doc/LANGUAGE_SPEC.md` stdlib inventory to include `int.to_string`.
      - add or update one focused example/reference entry if useful.
    - `INTSTR-S2` execution path:
      - implement one honest execution path that works across typecheck, runtime fallback,
        and general-lowered Wasm execution.
      - if an intrinsic/helper is introduced, keep it generic enough to serve integer stringification
        broadly rather than only this single surface function.
    - `INTSTR-S3` regression coverage:
      - add focused tests for `0`, positive multi-digit values, negative values, and composition with `println`.
      - add a named-callback regression such as `map [1, 20, -3] int.to_string` once callback type checking is in place.
    - `INTSTR-S4` parity checks:
      - verify the helper is pure and requires no effect annotation.
      - verify behavior matches across runtime fallback and general-lowered execution.
  - Validation checklist:
    - `cargo check`
    - `cargo test`
    - representative execution samples proving:
      - `println (int.to_string 123)` prints `123`
      - `println (int.to_string -7)` prints `-7`
      - `map [1, 20, -3] int.to_string` yields `["1", "20", "-3"]`
- `List.map` migration plan (planned):
  - keep canonical map behavior in `stdlib/goby/list.gb` (`list.map` export path).
  - replace internal/builtin-path map callsites with stdlib module usage where possible.
  - after migration, trim builtin-only `map` special handling so runtime/compiler has a single semantics source.
- `List.fold` addition (planned):
  - Goal:
    - add a left fold as `fold` to `stdlib/goby/list.gb` so users can express accumulator-style traversals
      without relying on mutable closure capture.
    - support representative workflows such as counting, summation, and state-threading over lists.
  - Locked semantic intent for this feature:
    - the public name is `fold`, but the semantics are specifically left-fold semantics.
    - for `fold xs init f`, evaluation order must be left-to-right over the list elements.
    - the callback contract must be accumulator-first and element-second.
    - the observable meaning to preserve is:
      - `fold [] init f == init`
      - `fold [x1, x2, x3] init f == f (f (f init x1) x2) x3`
    - the callback must be invoked exactly once per list element in source order.
  - Motivation:
    - current Goby/Wasm lowering supports non-capturing unary function values well enough for
      `list.each` / `list.map`, but that is not yet sufficient to express a practical fold API end-to-end.
    - closure capture remains a separate future track; `fold` should land first as the idiomatic
      immutable accumulation tool.
  - Current blocker to resolve first:
    - this is not blocked on a new intrinsic.
    - it is blocked on general higher-order call support in the Wasm lowering/runtime path:
      callback invocation currently assumes a narrower function-value calling shape than a useful
      `fold` callback requires.
    - experiments showed that a stdlib-only implementation is not enough today:
      - curried callback shape `b -> a -> b` traps/fails in the current indirect-call path,
      - tuple-packed callback shape `((b, a) -> b)` typechecks but does not yet execute correctly
        through the current higher-order Wasm path.
  - Design constraints:
    - do not add a dedicated `fold` intrinsic unless the generalized higher-order call path proves
      impossible or disproportionately complex.
    - prefer a stdlib-defined `fold` over a compiler-owned special case.
    - keep this work separate from full closure-capture support; immutable fold-style accumulation is the
      immediate user need.
    - do not patch only the `goby/list.fold` call path.
    - any backend/runtime change introduced here must be phrased as a reusable function-value call rule
      that also makes sense for future stdlib higher-order APIs.
    - do not lock in a public callback signature until the callable representation and lowering boundary
      are understood end-to-end; otherwise the surface API risks encoding a temporary implementation quirk.
    - do not choose an API shape that is only justified by one temporary backend limitation if that shape
      would be awkward for future collection APIs or closure-capable code.
    - stdlib surface decisions here must be explainable as part of a coherent future collection/HOF design,
      not merely as "the one shape that happens to work right now".
  - Architectural intent:
    - `fold` is a downstream consumer of a more general capability: "a stdlib higher-order function
      can call a user-provided function value through one principled lowering/runtime path".
    - the design should converge on one stable story across:
      - typechecker callable-shape expectations,
      - resolved/lowered function-value representation,
      - Wasm indirect-call emission,
      - runtime execution parity for stdlib-defined higher-order helpers.
    - avoid an implementation that works only because one phase knows the name `fold`.
    - avoid a plan that requires revisiting the public `fold` API once closure capture or future list APIs land;
      the chosen surface should already be plausible as the long-term standard library shape.
  - Temporary effect-policy decision:
    - higher-order effect propagation remains deferred at the language-design level.
    - for this feature, `fold` should follow the same callback-effect treatment that current stdlib
      higher-order functions use at the time of implementation; it must not invent a `fold`-specific rule.
    - if effectful callbacks are not yet supportable under the shared higher-order model, the first shipped
      version of `fold` may be limited to the currently supportable callback/effect envelope, but that limit
      must be documented explicitly in `doc/PLAN.md` and `doc/LANGUAGE_SPEC.md` rather than hidden in implementation.
  - Detailed milestones:
    - [ ] `FOLD-M1` callback-shape decision record
      - define the candidate public signatures and reject the ones that do not fit the current/future callable model.
      - explicitly compare at least:
        - curried shape `fold : List a -> b -> (b -> a -> b) -> b`
        - tuple-packed shape `fold : List a -> b -> ((b, a) -> b) -> b`
      - record the selection criteria in terms of general architecture:
        - typechecker compatibility,
        - named-function and lambda ergonomics,
        - Wasm lowering simplicity,
        - future closure-capture compatibility,
        - parity with other planned higher-order stdlib APIs.
      - completion criteria:
        - one callback shape is explicitly chosen in `doc/PLAN.md`,
        - rejected alternatives are named with concrete reasons,
        - the left-fold semantics and callback argument order are explicitly locked in writing,
        - the temporary callback/effect policy is explicitly recorded,
        - the chosen API is justified in long-term library-design terms, not only by immediate backend convenience,
        - no implementation begins before this choice is written down.
    - [ ] `FOLD-M2` executable failure baseline
      - add focused tests that capture the current failure mode for the chosen callback shape.
      - keep these tests below the `fold` surface first: prove the underlying function-value call
        shape is the real blocker.
      - include at least:
        - named function callback,
        - inline lambda callback,
        - representative accumulator type such as `Int`,
        - one non-`Int` accumulator case if the chosen shape can expose a second failure mode.
      - completion criteria:
        - the failing path is reproducible in tests,
        - the failure is localized to the shared higher-order execution path rather than the stdlib wrapper,
        - the baseline suite demonstrates why a stdlib-only `fold` addition would still fail today.
    - [ ] `FOLD-M3` shared higher-order call-path extension
      - extend the lowering/runtime boundary so the chosen callback shape executes through one reusable path.
      - keep ownership narrow:
        - callable-shape analysis belongs in the lowering planning layer,
        - argument marshalling belongs in the indirect-call emission/runtime boundary,
        - stdlib `fold` should only consume that capability.
      - explicitly avoid:
        - `fold`-named branches,
        - `goby/list`-specific emit logic,
        - duplicate code paths for named callbacks versus lambdas unless there is a principled representation split.
      - completion criteria:
        - the direct lower-level regression tests from `FOLD-M2` pass,
        - the new path is described as general higher-order support, not as a `fold` exception,
        - no backend/compiler component requires symbol-name knowledge of `fold`,
        - the implementation boundary is documented clearly enough that a future HOF can reuse it without copying logic.
    - [ ] `FOLD-M4` stdlib API addition
      - add `fold` to `stdlib/goby/list.gb` only after `FOLD-M3` is stable.
      - define the implementation in ordinary Goby stdlib code, not as a compiler-owned intrinsic.
      - document the selected public signature and accumulator/callback argument order.
      - update `doc/LANGUAGE_SPEC.md` stdlib inventory and semantics once the API shape is locked.
      - completion criteria:
        - `fold` exists as normal stdlib surface,
        - no compiler/runtime layer needs to know the `fold` symbol name to execute it,
        - the language/spec docs state the left-fold semantics and any temporary callback/effect limitations explicitly.
    - [ ] `FOLD-M5` semantic and regression coverage
      - add typecheck and execution coverage for representative user-facing uses:
        - integer sum,
        - boolean accumulation,
        - state-threading via tuple accumulator,
        - named callback import/use from another declaration.
      - add one example showing how `fold` replaces a mutable accumulator pattern.
      - confirm parity across the available execution paths that can reach stdlib higher-order code.
      - completion criteria:
        - `cargo check` passes,
        - `cargo test` passes,
        - `fold [] init f == init` is covered,
        - left-to-right accumulation order is covered with a non-commutative or otherwise order-sensitive example,
        - callback invocation count/order is covered strongly enough to catch accidental right-fold or duplicated-call behavior,
        - at least one example and one integration-style test prove the final user-facing API.
  - Progress checklist:
    - [ ] left-fold semantics and callback argument order documented
    - [ ] callback shape chosen and documented
    - [ ] temporary callback/effect policy documented
    - [ ] lower-level failing regressions added before backend work
    - [ ] shared higher-order lowering/runtime path generalized
    - [ ] `stdlib/goby/list.gb` exports `fold`
    - [ ] language spec updated after API lock
    - [ ] user-visible semantics and limitations documented
    - [ ] examples and integration coverage added
    - [ ] full validation (`cargo check`, `cargo test`) completed

### 2.5 Runtime / Compiler Scope (MVP)

- Current model: native Wasm code generation for the supported subset, with deterministic
  fallback behavior for unsupported forms.
- Error location strategy (line/column reporting) — deferred.

### 2.6 Tooling Scope (MVP)

- Tooling direction is now explicit:
  - prioritize early developer-environment support (syntax highlight, formatter, linter, LSP),
  - keep diagnostics/messages stable enough to be consumed by editor tooling.

## 3. Later-Phase Decisions

- Module/package ecosystem and remote dependency management.
- Advanced effects (async, state, cancellation, effect-safety diagnostics).
- Potential explicit `discontinue` syntax as a clarity-focused effect-control extension
  (deferred; not part of current `resume` rollout).
- Pattern matching exhaustiveness and advanced ADTs.
- Interoperability/FFI strategy.
- Governance model, RFC process, compatibility policy.
- Real Wasm code generation (replace compile-time interpreter).
- Declaration-side generic parameter binders.
- Handler clause body type-checking improvements (effect-safety and diagnostics polish).
- `else if` chaining in `if` expressions.
- REPL or interactive mode.

## 4. Next Phase Plan

Post-MVP work focuses on reducing fallback-runtime special cases and making
execution paths more predictable.

Priority rule:

- IR-lowering completion and IR-boundary redesign work is tracked as completed in
  `doc/PLAN_IR.md`; use it as the architectural reference for follow-up work.
- WB-3B is explicitly on hold:
  - in-repo preparation work is complete enough for now; see `doc/PLAN_IR.md`.
  - restart only when both of the following are true:
    - WebAssembly stack switching has reached Phase 4 (or equivalent standardization readiness).
    - the local toolchain (`wasm-encoder` plus runtime support such as Wasmtime on required ISAs)
      can emit/validate/run the needed WasmFX instructions honestly.
- active backend/runtime work should prefer unblocking itself by improving shared IR and
  AST-to-IR lowering rather than by adding more source-shape-specific recognizers.
- when there is tension between a local unblock and the long-term IR architecture,
  choose the option that improves or preserves the long-term IR architecture.
- if future work reopens an architectural lowering gap, extend `doc/PLAN_IR.md`
  before adding new boundary-specific workarounds.

### 4.1 Active Track D: Developer Tooling Foundation

Most of Track D is complete. Only still-relevant follow-up items are kept here.

#### Phase D5: `goby lint` — high-signal static checks

Goal: machine-readable linter output for common mistakes not caught by the typechecker.

### 4.2 Track E: Higher-Order Function-Type Checking (complete, 2026-03-27)

Goal: close the remaining typechecker hole where a resolved function name can still be
accepted in a higher-order position even though its function type does not match the
callee's required callback type.

Why this is active now:

- user-visible buggy shape exists today: `println` can still be accepted as a callback
  for `List Int` iteration after explicit import of `each`, even though the required
  callback type is `Int -> Unit` and `println` resolves to `String -> Unit`.
- the runtime assumes `Print.print` / `Print.println` receive `String`, so leaving this
  to runtime causes corrupt execution instead of a normal compile-time error.
- the required architecture is already mostly present: effect-op call checking has the
  unification pieces; higher-order ordinary calls need to reuse the same style of checking.
- follow-up stdlib work such as `list.fold` depends on a principled answer to
  "what does it mean for a function value to satisfy a function-typed parameter?".

Long-term design constraints:

- do not special-case `println`, `print`, `each`, or any other single stdlib symbol.
- do not patch individual call sites with ad hoc callback checks.
- keep one type-driven notion of function-argument compatibility and reuse it everywhere
  ordinary calls consume function-valued arguments.
- preserve current named-function higher-order ergonomics (`map xs add_ten`) and generic
  instantiation behavior.
- keep partial-application semantics intact where the parser/AST currently represent curried
  calls as nested single-argument calls.
- prefer factoring shared matcher logic that can later be reused by effect-op validation,
  rather than duplicating "ordinary-call" and "effect-call" compatibility code forever.
- keep the implementation ownership narrow:
  - call-shape discovery belongs in the ordinary-call validation layer,
  - function-type compatibility belongs in one shared matcher,
  - human-readable diagnostic rendering belongs at the error construction boundary.
- diagnostics must distinguish:
  - unknown callee / unresolved name,
  - direct scalar argument mismatch,
  - resolved callback name with wrong function type.

Out of scope for Track E:

- do not change closure-capture semantics.
- do not expand Wasm lowering or runtime callable representation.
- do not introduce new stdlib intrinsics just to compensate for missing typechecking.
- do not redesign currying or callable syntax.
- do not broaden the language spec unless implementation work proves a genuine semantic gap.

Target architecture:

- one shared higher-order signature matcher owns function-type compatibility checking for
  callback positions in ordinary calls.
- ordinary-call validation is responsible for deciding when a parameter position expects a
  function value and delegating compatibility checking to that matcher.
- the ownership split should stay explicit across current phases:
  - `ensure_known_call_targets_in_expr` remains the prevalidation pass for unresolved-call
    rejection,
  - `check_expr` remains the inference pass that computes result types,
  - Track E hooks callback compatibility into the ordinary call-validation path without
    collapsing those phases together.
- the matcher operates on already-resolved types and uses the same substitution/unification
  model as the rest of the typechecker, so generic callback positions instantiate per call site
  rather than through callback-specific rules.
- diagnostics are built from matcher output (`required type`, `actual type`, optional resolved name)
  instead of from special string checks.
- the matcher API should be explicit enough that both callers and tests can use it without
  reconstructing one particular stdlib example; the expected shape is roughly:
  - `match_function_argument(required: &Ty, actual: &Ty, env: &TypeEnv, next_id: &mut usize)
    -> Result<FunctionArgMatch, FunctionArgMismatch>`
  - exact names may change, but Track E should preserve this boundary:
    matcher owns compatibility and instantiation, caller owns where to invoke it.

Implementation sequencing:

- first lock the compatibility contract and the ownership boundary before touching validation flow.
- then extract matcher logic without changing user-visible diagnostics more than necessary.
- only after the matcher exists should ordinary-call validation start delegating callback positions.
- add parity/regression tests before broad diagnostic polishing so message changes do not hide
  behavioral regressions.
- treat `fold` and other new HOF stdlib work as downstream consumers; Track E should finish
  without needing to land new library APIs.

Milestones:

- [x] **E1: Compatibility Model Lock**
  - define one precise internal contract for callback compatibility in ordinary calls.
  - lock how the matcher should treat:
    - named functions,
    - imported qualified names,
    - generic declarations instantiated per call site,
    - partially applied functions represented as nested single-arg calls.
  - lock the diagnostic boundary:
    - unresolved names remain unresolved-name errors,
    - resolved but incompatible callback values become higher-order function-type mismatches.
  - lock generic instantiation timing:
    - callback declarations with type variables are instantiated with fresh vars at the moment the
      callback argument is matched against the required parameter type,
    - Track E should reuse the existing `instantiate_ty_with_fresh_type_vars(...)` machinery or a
      direct successor, not invent a second generic-instantiation rule for callbacks.
  - deliverable:
    - `doc/PLAN.md` and code comments agree on one matcher ownership boundary before implementation starts,
    - the planned matcher signature and result/mismatch shape are written down clearly enough that
      E2 does not begin with another design round.

- [x] **E2: Shared Matcher Extraction**
  - extract a reusable helper that compares an actual function-valued argument type against the
    required function type for a parameter position.
  - make the helper reuse the existing unification/substitution machinery instead of introducing a
    callback-only compatibility algorithm.
  - ensure the helper returns structured mismatch information suitable for diagnostics rather than
    boolean pass/fail only.
  - non-goals:
    - no stdlib symbol special-casing,
    - no diagnostic string construction inside low-level matching logic.
  - implementation note:
    - `typecheck_unify.rs` is the natural home unless refactoring reveals a better shared
      type-compatibility module; do not hide matcher logic inside one call-site validator.
  - test placement:
    - matcher-level focused tests live beside the matcher implementation,
    - end-to-end ordinary-call regressions stay with the higher-level typecheck tests.
  - exit criteria:
    - the matcher can be unit-tested or otherwise exercised independently of one specific stdlib
      call shape,
    - mismatch data is structured enough that later diagnostic improvements do not require matcher
      redesign.

- [x] **E3: Ordinary-Call Integration**
  - integrate the matcher into ordinary declaration/value call validation wherever a function-typed
    parameter consumes a function-valued argument.
  - confirm actual call-path coverage before wiring:
    - verify whether qualified calls and pipeline forms reach the same ordinary-call validation
      branch today,
    - if they do not, record the real branch ownership explicitly before integrating matcher calls.
  - cover call shapes that reach ordinary validation today:
    - direct calls,
    - qualified calls lowered through the same path,
    - pipeline-adjacent ordinary call forms if they feed function-valued arguments.
  - curried-call note:
    - `each [1, 2] println` is represented as nested unary `Expr::Call`,
    - the integration must reason over the outer call against the function result of the inner call,
    - `effect_op_call_target_and_args()` is a useful structural reference for flattening nested
      call shapes, but Track E should reuse only the shape insight, not effect-specific policy.
  - preserve priority rules:
    - unknown-name diagnostics win when resolution fails,
    - callback mismatch diagnostics appear only after name resolution succeeds.
  - deliverable:
    - the ordinary-call validation layer becomes the single ownership point for higher-order
      callback type checking in non-effect calls.
  - guardrail:
    - if multiple call-validation branches need the same callback check, refactor the dispatch
      shape first instead of cloning the check.

- [x] **E4: Regression and Parity Coverage**
  - add focused regressions for the representative buggy and non-buggy shapes:
    - `each [1, 2] println` must fail,
    - `each [\"a\", \"b\"] println` must pass because `a = String` instantiates cleanly and the
      callback unifies as `String -> Unit`,
    - `map [1, 2] println` must fail,
    - named callback variants continue to pass when signatures align.
  - add generic and partial-application parity coverage:
    - generic declaration call with concrete argument,
    - generic declaration used as callback,
    - nested curried calls where only a later application mismatches.
  - add namespace/import coverage:
    - imported unqualified callback name,
    - qualified callback name from a module import.
  - verify no regression in existing higher-order Wasm/classification tests caused by tighter
    typechecking.
  - exit criteria:
    - the test matrix demonstrates that Track E changed type acceptance, not callable lowering.

- [x] **E5: Diagnostics and Follow-through**
  - render callback mismatch diagnostics in the "resolved name, wrong function type" style.
  - include:
    - required callback type,
    - actual resolved function type,
    - resolved function name when available.
  - lock a provisional regression-friendly wording before broad message churn, for example:
    - `higher-order argument type mismatch: required Int -> Unit, found println : String -> Unit`
  - keep direct-call diagnostics distinct from callback diagnostics.
  - update `doc/LANGUAGE_SPEC.md` only if callable/typecheck rules need clarification; avoid
    wording churn without a real semantics change.
  - after E1-E5 are green, evaluate whether effect-op validation should share more of the same
    matcher internals.
  - exit criteria:
    - callback mismatch messages are stable enough to use as regression expectations,
    - no remaining caller path can accept the known buggy shape and defer failure to runtime.

Progress checklist:

- [x] matcher ownership boundary is documented before implementation branches spread
- [x] representative bug is rejected during `goby-cli check`, not deferred to runtime
- [x] matcher logic is shared and type-driven, not symbol-driven
- [x] generic callback positions still instantiate independently per call site
- [x] partial application behavior remains unchanged for currently supported forms
- [x] diagnostics report higher-order mismatch instead of unknown-name or scalar-argument mismatch
- [x] qualified and unqualified callback names follow the same compatibility rules
- [x] focused tests cover direct / qualified / named / generic / partial-application callback cases

Done criteria:

- `import goby/list ( each )` plus `each [1, 2] println` is rejected.
- `import goby/list ( each )` plus `each [\"a\", \"b\"] println` is accepted.
- the rejection is reported as a higher-order function-type mismatch (`Int -> Unit` required,
  `String -> Unit` found), not as an unknown-name error.
- no regression in existing higher-order named-function callback examples.
- at least one more complex generic higher-order example is covered and behaves correctly:
  - a generic callback path still instantiates per call site and is accepted when the instantiated
    function type matches, for example:
    ```goby
    import goby/list ( map )

    id : a -> a
    id x = x

    main =
      map [1, 2] id
    ```
- at least one nested curried-call example is covered and behaves correctly:
  - a later callback application mismatch is rejected at the higher-order boundary rather than being
    deferred or misreported as a scalar-argument error, for example:
    ```goby
    import goby/list ( each )

    main =
      each [1, 2] println
    ```

### 4.3 Active Track F: Stdlib `int.to_string`

Goal: provide an explicit pure conversion from `Int` to `String` in `goby/int`.

Why this is active:

- once higher-order callback type checking is fixed, users need an explicit conversion helper
  instead of expecting `println` to accept `Int`.
- `int.parse` already exists as the input-side stdlib entrypoint; `int.to_string` is the
  natural output-side counterpart.
- this helper is useful for callback composition, formatted output, and list-to-string workflows.

Execution plan:

1. Add `to_string : Int -> String` to `stdlib/goby/int.gb` and document it in the stdlib surface.
2. Implement one shared execution path that works for runtime fallback and Wasm execution.
3. Add focused regressions for zero, positive, and negative integer rendering.
4. Add one composition regression using `println (int.to_string n)`.
5. After Track E lands, add callback-position coverage such as `map xs int.to_string`.

Done criteria:

- `int.to_string 0` yields `"0"`.
- `int.to_string 123` yields `"123"`.
- `int.to_string -7` yields `"-7"`.
- `println (int.to_string 123)` prints `123`.
- `map [1, 20, -3] int.to_string` yields `["1", "20", "-3"]`.

Lint rules are ordered by ascending analysis cost. The cheapest infrastructure
comes first to unblock the framework early; user-value ranking is noted in
parentheses for prioritization.

1. **Unreachable `case` arm**: wildcard `_` arm followed by more arms (implemented).
   User value: medium — catches subtle logic errors in pattern matching.
2. **Unused local binding**: `x = expr` where `x` is never referenced afterward
   (needs local-use tracking across `Expr`/`Stmt` spans from D1a).
   User value: **high** — most frequently encountered lint in practice; catches typos and dead code.
   Note: despite higher analysis cost than rule 1, consider implementing this early if the
   lint framework from rule 1 is already in place, because it delivers the most user value.
3. **Shadowed effect operation name**: local binding name collides with a visible effect op
   (needs symbol table from D3a, no use-tracking required).
   User value: medium — prevents confusing name collisions specific to Goby's effect system.
4. **Redundant `can` annotation**: effect is fully discharged inside the function body
   (needs effect discharge analysis; implement last to avoid noisy warnings).
   User value: medium — helps keep effect annotations accurate.

Output format:

- Human-readable (default): same caret-snippet format as `goby-cli check`.
- JSON lines (`--json`): `{"severity":"warning","file":"...","line":1,"col":1,"rule":"unreachable-arm","message":"..."}`.

Done when:

- Each rule is implemented and enabled one at a time (separate commits).
- Each rule has at least one positive fixture (warning fired) and one negative fixture (no warning).
- `goby lint examples/function.gb` exits 0 on all existing clean sources.
- JSON output is parseable; schema documented.

#### Phase D6 follow-ups still worth keeping

- **D6c: Shared grammar asset**
  - Extract shared language-definition data for the VS Code grammar and Neovim/Vim syntax files.
  - Keep this as the remaining editor-tooling refactor slice.
- **D6b-ts: Tree-sitter grammar**
  - Defer until after D6c.
  - Keep as optional editor tooling follow-up rather than active architecture work.

### 4.4 Review Follow-ups (Backlog)

The following items were identified in a focused code review and are tracked as
near/mid-term engineering debt after current active tracks.

Priority-ordered follow-ups:

1. Typecheck env cloning strategy:
   - Replace repeated full `TypeEnv` clone on local scope transitions with a
     cheaper scope-chain/persistent-map strategy (`Rc` + parent chain or equivalent).
2. Large-function decomposition for testability/readability:
   - candidate functions include `eval_expr_ast`, `execute_unit_expr_ast`,
     `check_unhandled_effects_in_expr`, `check_resume_in_expr`, `check_body_stmts`,
     and parser/lowering long functions.
3. Hot-path allocation reduction:
   - remove avoidable clones in Wasm runtime dispatch paths (`find_map` + clone patterns,
     local/callable env cloning in frequently executed branches).
4. Planning call-graph closure algorithm:
   - replace naive fixed-point transitive closure with worklist/topological strategy
     to avoid avoidable quadratic behavior on larger declaration graphs.
5. Small algorithmic cleanups:
   - replace `collect + sort + first` selection patterns with `min_by(_key)` where applicable.
6. Shared utility deduplication:
   - unify duplicated `find_can_keyword_index` and resume-detection helpers across modules.
7. Diagnostics metadata completeness:
   - add source span/line metadata to type/effect declaration AST nodes so type errors
     can consistently report precise locations (LSP-friendly diagnostics).

Note:

- Critical correctness items from the same review batch were already fixed:
  parser explicit early-return clarity and planning `u16` overflow fail-fast behavior.

### 4.5 `Float` / Wasm `f64` Support

Goal: add a first-class `Float` type with predictable parser/typechecker/runtime/Wasm behavior.

Why this is a separate workstream:

- `Float` crosses language surface, type rules, runtime behavior, stdlib shape, and Wasm lowering.
- the design needs a few semantics to be locked before implementation to avoid ad-hoc numeric behavior.

Scope to lock before coding:

1. Literal syntax
   - canonical decimal forms for `Float` literals (for example `1.0`, `0.5`, `-3.25`).
   - whether exponent notation (`1e3`, `1.2e-3`) is included in the first slice.
   - parser boundary with tuple/member syntax so `1.0` is never confused with `expr.0`.
2. Type/system rules
   - `Float` is a distinct primitive type, not an alias of `Int`.
   - decide whether mixed arithmetic is rejected initially or whether `Int -> Float` promotion exists for operators/calls.
   - comparison/equality semantics must be explicit, especially around NaN.
3. Runtime/CLI behavior
   - printing/rendering policy for `Float`.
   - parse/runtime behavior for NaN, `inf`, `-inf`, and division edge cases.
4. Stdlib surface
   - introduce a minimal `goby/float` module only if it has a clearly defined first API surface.

Execution phases:

1. Phase E1: Syntax and semantic lock
   - define accepted literal grammar and operator coverage.
   - decide initial coercion policy:
     - preferred conservative default: no implicit `Int`/`Float` mixing in MVP; require exact operand types until explicit conversion APIs exist.
   - update `doc/LANGUAGE_SPEC.md`, examples, and diagnostics wording once semantics are locked.
2. Phase E2: Core representation and parsing
   - add `Float` to AST/type representations.
   - teach the lexer/parser to recognize `Float` literals without regressing tuple-index/member access parsing.
   - add parser regression tests for decimal literals, negatives, malformed literals, and `expr.0` ambiguity cases.
3. Phase E3: Typechecker and operators
   - add `Float` typing rules for literals, annotations, and supported operators.
   - implement diagnostics for invalid mixed arithmetic if coercion stays disabled.
   - ensure branch/type-unification paths report `Int` vs `Float` mismatches clearly.
4. Phase E4: Runtime and Wasm lowering
   - extend interpreter/fallback runtime with `Float` values and operator execution.
   - lower `Float` values/operators to Wasm `f64`.
   - verify parity between fallback execution and native Wasm execution for the supported subset.
5. Phase E5: Stdlib, examples, and tooling follow-through
   - add or update canonical examples using `Float`.
   - extend formatter/linter/LSP assumptions if numeric literal/token handling needs updates.
   - add diagnostics/hover rendering coverage for `Float`.

Acceptance criteria:

- `Float` annotations and literals are accepted by `check` for the supported syntax.
- supported `Float` arithmetic behaves consistently in fallback runtime and Wasm-native execution.
- diagnostics clearly distinguish `Int` from `Float`.
- docs/examples/spec are updated in the same slice that lands behavior.

### 4.6 Parking Lot (Needs Revalidation Before Implementation)

- CLI `build` expansion details (`--target`, `--engine-compat`, verify modes).
- CLI binary naming migration (`goby-cli` -> `goby`) final policy.

These items are intentionally kept as short placeholders until they become active.

## 5. Spec Detail Notes

### Still Open (Post-MVP)

- Effects/handlers: lexical nearest-handler runtime semantics are active; post-MVP work is to move from interpreter/runtime lookup to compiled `EffectId`/`OpId` tables.
- Effects/handlers (`resume` follow-up): tighten multi-shot static analysis beyond
  the current conservative "multiple syntactic `resume`" rejection.
- Effects/handlers (`resume` extension): evaluate explicit `discontinue` syntax
  as a separate language-design proposal.
- Effect namespace rules: qualified vs unqualified calls, unhandled-effect diagnostics format.
- Type annotation placement: where annotations are required vs optional outside current MVP subset.
- Tuple/record roadmap: record update syntax, pattern matching on record fields.
- Import system: filesystem-backed/local package resolution, dependency graph rules.
- Equality/comparison:
  - implemented operator set now includes `||`, `&&`, `!`, `==`, `<`, `>`, `<=`, `>=`, `+`, `-`, `*`, `/`, `%`.
  - still open:
    - exact long-term policy for equality over all language-level value categories,
    - `!=`,
    - whether spaced single-argument calls should accept unparenthesized binary-expression arguments
      such as `println 1 + 1` without changing the broader precedence model.

## 6. Research References (2026-03-01 survey)

- OCaml manual (effect handlers): one-shot continuations are cheaper than multi-shot and are the default operational model.
  - <https://caml.inria.fr/pub/distrib/ocaml-5.0/ocaml-5.0-refman.html#sec281>
- Retrofitting Effect Handlers onto OCaml (Sivaramakrishnan et al., PLDI 2021): runtime design with fibers and one-shot continuations integrated into a production compiler/runtime.
  - <https://arxiv.org/abs/2104.00250>
- Effect Handlers, Evidently (Xie and Leijen): evidence-passing translation strategy for efficient handlers.
  - <https://arxiv.org/abs/2106.00160>
- WasmFX: Typed Continuations and Stack Switching for WebAssembly (Hillerstrom et al., ICFP 2024): practical path for direct-style effect handlers on Wasm backends.
  - <https://arxiv.org/abs/2403.01036>
