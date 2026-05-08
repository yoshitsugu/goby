# Goby Language Plan (Draft)

This document tracks:

- what is already visible in the current draft,
- what is fixed for MVP,
- what remains open or deferred.

Notes:

- `doc/LANGUAGE_SPEC.md` is the source of truth for current language
  syntax and semantics.
- `PLAN.md` is the top-level roadmap and execution-planning document.
- `doc/PLAN_IR.md` is the detailed roadmap for IR-lowering convergence and
  effect-runtime architecture.
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
- Anonymous functions: single-parameter `fn x -> expr`, multi-parameter `fn a b -> expr`,
  and placeholder shorthand (`_ * 10`); `fn` is a reserved keyword
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
  - `goby-cli run <file.gb>` uses `main` entrypoint only.
    - current implementation is hybrid:
      - ordinary file-based Wasm execution writes `<file.wasm>` and launches external `wasmtime`,
      - runtime-stdin / `GeneralLowered` / `InterpreterBridge` cases execute through the
        Goby-owned runtime boundary in `goby-wasm`.
    - if external `wasmtime` is not installed, only the file-based execution path is skipped with
      an informational message.
    - current external invocation: `wasmtime run <file.wasm>` with WASI-standard `_start` export.
    - open design questions:
      - should Goby eventually return to an all-external execution model,
      - or is a Goby-owned host/runtime layer a first-class product boundary,
      - if external-only execution is desired, which current host intrinsics / runtime services
        must be removed, standardized, or moved into a separate launcher.
  - `goby-cli check <file.gb>` performs parse/typecheck without runtime entrypoint.

### 2.1 Syntax and Parsing
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
  - `@embed` direction is locked to `goby/prelude` only.
    - implicit `Print` / `Read` availability remains anchored at `goby/prelude`.
    - `goby/stdio` is the canonical owner of both effects; prelude decides which
      visible stdlib-owned effects participate in the implicit default surface.
- Stdlib integer parse entrypoint is `int.parse`.
  - contract: parse optional leading `-` + one or more ASCII digits as base-10 `Int`.
  - failure path is effect-based: `StringParseError.invalid_integer : String -> Int`.
- Stdlib integer formatting entrypoint is `int.to_string`.
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
    - `[p1, p2, ...]` without a tail matches list length exactly `item_count`.
    - `[p1, p2, ..tail]` matches list length `>= item_count`.
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
  - Current implementation note:
    - this statement is incomplete for runtime-stdin programs.
    - the active runtime boundary also includes a Goby-owned Wasm execution path for
      `GeneralLowered` and `InterpreterBridge` classifications when stdin/host-runtime wiring is
      required.
  - Discussion points to settle later:
    - whether `goby-wasm` should remain responsible for host-runtime-backed execution,
    - whether CLI `run` should always emit a `.wasm` artifact even for Goby-owned execution paths,
    - whether the long-term contract should expose a separate Goby runner instead of overloading
      raw external `wasmtime` execution.
  - Mutable list execution boundary (complete, 2026-04-07):
    - rooted list update (`a[i] := v`, `a[i][j] := v`) is a semantic runtime capability;
      execution planning routes these programs through the `GeneralLowered` Wasm path.
    - `RuntimeLocals` uses a single unified `RuntimeValue`-based store.

- **`fn`-only anonymous functions** (complete, 2026-03-31).
  - `fn x -> expr` is the only anonymous function syntax.
  - Pipe form `|x| -> expr` is removed from the parser; `pipe_lambda_syntax_rejected` test enforces this.
  - Formatter emits `fn n ->` for single-param lambdas.
  - `fn` is reserved keyword; editor syntax definitions highlight it correctly.

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
    - longer-term direction: move the language toward a more expression-oriented model where
      all major constructs, including bindings and assignments, are usable as expressions with
      well-defined result values rather than remaining statement-only forms.
    - once binding and assignment forms are expression-valued as `()`, block tails ending in
      `name = expr`, `mut name = expr`, or `name := expr` should be accepted as valid `Unit`-valued endings.
    - the same relaxation should apply to lambda bodies and other expression positions, so
      callbacks such as `fn i -> b[0] := b[0] + i` do not need a trailing explicit `()`
      when the final assignment is intended to produce `Unit`.
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
- **List index precedence realignment** (complete).
  - `expr[expr]` now binds tighter than whitespace function application:
    `f xs[0]` parses as `f (xs[0])`, `f a b[0]` as `f a (b[0])`.
  - Chaining (`xs[0][1]`, `f()[0]`, `[1,2,3][0]`) is preserved.

### 2.2 Types and Checking

- TODO (Deferred): declaration-side generic parameter binders
  (for example, `id : a -> a` with explicit binders).
- `Float` type backed by Wasm `f64` is shipped. Literal grammar, operator
  coverage, NaN/Inf, and printing rules are spec-locked in
  `doc/LANGUAGE_SPEC.md` §3. Mixed `Int`/`Float` arithmetic is rejected and
  explicit numeric conversion APIs remain deferred.
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
- Higher-order function-type checking is implemented.
  Callback mismatch behavior is now part of the shipped typechecker baseline.

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
- How to represent multiple effects (`can Print + Read` or other syntax) — locked
  (2026-05-07): the `can EffectA, EffectB` form is the closed-row spelling, and
  open rows use `can EffectA, {e}` with a single row variable in braces. See
  `doc/LANGUAGE_SPEC.md` §5 for the full surface lock.
- Effect propagation rules for higher-order functions — landed
  (2026-05-07). Row-polymorphic `can ..., {e}` carries callback effects
  through stdlib HOFs and user-defined HOFs; closed callback rows reject
  effectful lambdas; diagnostic wording distinguishes "missing effect in
  closed row" from "row variable cannot be unified". See
  `doc/LANGUAGE_SPEC.md` §5 for the surface lock and unification rules.
- Effect diagnostics UX polish (wording/format consistency) — landed
  (2026-05-07) alongside the row-polymorphism work.
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
- `int.to_string` is implemented and treated as part of the shipped stdlib baseline.
- `List.map` migration plan (planned):
  - keep canonical map behavior in `stdlib/goby/list.gb` (`list.map` export path).
  - replace internal/builtin-path map callsites with stdlib module usage where possible.
  - after migration, trim builtin-only `map` special handling so runtime/compiler has a single semantics source.
- `List.fold` addition (complete, 2026-03-28):
  - `fold : List a -> b -> (b -> a -> b can {e}) -> b can {e}` — curried
    left-fold, accumulator-first callback; row-polymorphic in the callback's
    effect row (signature retrofitted 2026-05-07).
  - Implemented in `stdlib/goby/list.gb` via ordinary stdlib recursion; no compiler special-casing.
  - Backend: `IndirectCall { arity: 2 }` generalized for 2-arg HOF callbacks.
  - Effect policy: callback effects propagate to the caller's `can` clause via
    row unification (`doc/LANGUAGE_SPEC.md` §5).

### 2.5 Runtime / Compiler Scope (MVP)

- Current model: native Wasm code generation for the supported subset, with deterministic
  fallback behavior for unsupported forms.
- Error location strategy (line/column reporting) — deferred.
- GC / reclamation is not part of the current runtime scope.
  Re-evaluate only if Goby grows long-lived in-process execution, repeated module
  reuse within one runtime, or measured workloads show retained-memory pressure
  or fragmentation rather than bounded-growth pressure.

### 2.6 Tooling Scope (MVP)

- Tooling direction is now explicit:
  - prioritize early developer-environment support (syntax highlight, formatter, linter, LSP),
  - keep diagnostics/messages stable enough to be consumed by editor tooling.

## 3. Later-Phase Decisions

- Module/package ecosystem and remote dependency management.
- Advanced effects (multi-shot continuations, async, state, cancellation,
  effect-safety diagnostics).
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

### 3.1 List update scaling — structural direction (deferred)

Context: the 2026-04-14 `doc/BUGS.md` entry exposed that `root[i][j] := rhs`
inside `each` rebuilds the whole list-of-lists per iteration via
copy-on-write `ListSet`. A targeted lowering fix (tracked separately in
`doc/PLAN_LIST_FIX.md`) closes the bug without language-level changes, but
the structural question of how Goby should represent and update collections
at scale remains open.

Ladder of structural options, recorded so the decision is not lost:

- **L1 — Runtime uniqueness tracking on list values.** Extend the
  tagged-value layout with a "uniquely owned" bit; allocators mark new
  lists unique, reads that could alias clear it, `ListSet` on a unique
  list mutates in place. Catches many alias-free mutations beyond what
  syntactic lowering can. Conflicts somewhat with Goby's "explicit
  boundaries" principle because the guarantee is dynamic. Pursue only
  if post-fix telemetry / user reports show the syntactic lowering
  leaves important shapes uncovered.
- **L2 — Uniqueness / ownership in the type system.** Linear / affine /
  modal modes along the lines of Linear Haskell, Koka, Roc, or OCaml's
  mode system. Structurally the right long-term home for a typed
  functional language with effects: the guarantee is static, shows up
  in signatures, and protects every allocation site uniformly. Large
  language-design commitment; revisit when Goby exits its early-stage
  window.
- **L3 — New list / collection representation.** Persistent RRB-tree,
  rope-like chunked list, or an explicit mutable `Array` type for `mut`
  contexts. Adoptable module-by-module and independent of the type
  system, but without L1/L2 the in-place opportunity is not fully
  exploited.

Deferred decisions:

- Which of L1/L2/L3 belongs first, and whether L1 is pursued at all
  given it may be subsumed by L2.
- Whether a mutable `Array` type should coexist with `List` in the
  surface language, or whether `List` itself should change
  representation.
- Trigger for revisiting: (a) a second `each`/`:=` scaling report that
  the syntactic lowering does not handle, or (b) Goby leaving
  early-stage and committing to a long-term collection story.

### 3.2 Wasm memory ceiling policy — long-term shape

Context: `doc/PLAN_LIST_FIX.md` moves the emitter to memory64 and adds a
`--max-memory-mb` CLI knob with a 1 GiB default. A few policy questions
are parked for later so the initial CLI surface stays minimal.

- **CLI surface shape.** Single `--max-memory-mb` knob vs exposing
  `initial` and `max` pages independently. Current default: single
  knob. Revisit if a concrete workload appears that benefits from a
  non-default initial reservation (batch jobs with known working set).
- **Host-refusal semantics under an unbounded cap.** When the user sets
  `--max-memory-mb=0` ("defer to host"), how `wasmtime` surfaces an
  allocator-side OOM (trap vs `memory.grow` returning `-1` vs host
  process abort) determines whether the emitter can rely on `maximum:
  None` or must always declare a finite ceiling as a safety net.
  **M3.3 decision (2026-04-15):** Validated via existing OOM tests
  (`host_string_concat_reports_runtime_error_when_growth_hits_maximum`)
  that `maximum`-field enforcement surfaces as a `runtime error:
  E-MEMORY-EXHAUSTION` (trap path, not panic or process abort). Adding
  `wasmtime::StoreLimits` as a second ceiling layer requires wrapping
  `WasiP1Ctx` in a limiter-aware store data type — a non-trivial
  refactor deferred to M4. **Conclusion for CLI:** `--max-memory-mb=0`
  MUST continue to translate to a finite `RUNTIME_MEMORY_CONFIG` ceiling
  (not `maximum: None`) until `StoreLimits` is wired. The `None` path
  is unsafe because host-side OOM without a declared `maximum` cannot
  be guaranteed to surface as a trap on all embedders.
- **Embedder portability (wasmer, browser).** Current plan assumes
  `wasmtime` only. If a second embedder is added, the ceiling /
  `StoreLimits` plumbing needs an abstraction; not worth designing
  before a concrete second embedder is in scope.

### 3.3 Effect runtime redesign — lexical capabilities and multi-shot effects

Goby's current compiled effect support is the one-shot tail-resumptive subset
used by the stdlib iterator/string traversal paths. The long-term effect
runtime should be designed around language semantics first, not around a single
backend mechanism.

Target direction:

- effect operation calls resolve to lexical handler capabilities, not dynamic
  handler-name search;
- handler installation creates a stable prompt/capability boundary;
- captured continuations are explicit runtime objects;
- sequential multi-shot resume is a language goal;
- continuation copy uses copy-on-shared-resume, integrated with Perceus
  refcount/dup/drop rules;
- the existing one-shot tail-resumptive path remains a fast path, not the
  definition of effect semantics;
- WasmFX/stack-switching remains a possible future emitter/runtime
  optimization, not the canonical IR model.

Mutation policy:

- ordinary `mut` keeps its current semantics, including shared-cell behavior
  when captured by closures;
- multi-shot handlers must not silently copy or roll back ordinary `mut` state;
- until an explicit branch-local/backtrackable state surface exists, any
  multi-shot continuation that captures ordinary mutable locals should be
  rejected by classification/lowering;
- branch-local mutable state should be introduced later as an explicit
  construct or API, separate from ordinary `mut` and any future explicit
  shared `Cell` / `Ref` surface.
- **Priority note (2026-05-01).** Multi-shot continuations and a branch-local
  state surface are direct prerequisites for Track PC (§4.6 Parser Combinator)
  alternatives/backtracking. Their priority is raised accordingly: WB-4
  classification work continues to land first, but the design lock for the
  branch-local state surface should advance ahead of other §3 deferred items
  once classification is stable.

Implementation should start with classification and rejection behavior before
any Wasm emission work. The first useful slice is a richer handler-use
classifier that distinguishes at least direct one-shot, abortive, delayed
one-shot, multi-shot, reentrant-looking, and multi-shot-with-mutable-capture
shapes.

## 4. Next Phase Plan

Post-MVP work focuses on reducing fallback-runtime special cases and making
execution paths more predictable.

Priority rule:

- IR-lowering completion and effect-runtime redesign work are tracked in
  `doc/PLAN_IR.md`; use it as the architectural reference for follow-up work.
- WB-4 effect-runtime work supersedes the old WB-3B/WasmFX-only plan:
  - start with handler classification, lexical target metadata, and rejection
    of unsupported multi-shot mutable-capture shapes;
  - treat WasmFX as an optional future emitter after the semantics and lower
    control IR are stable;
  - do not make ordinary `mut` semantics depend on whether a continuation is
    physically copied.
- active backend/runtime work should prefer unblocking itself by improving shared IR and
  AST-to-IR lowering rather than by adding more source-shape-specific recognizers.
- when there is tension between a local unblock and the long-term IR architecture,
  choose the option that improves or preserves the long-term IR architecture.
- if future work reopens an architectural lowering gap, extend `doc/PLAN_IR.md`
  before adding new boundary-specific workarounds.

Completed tracks are intentionally summarized in `doc/STATE.md`.
This section keeps only still-active work and deferred-but-live design tracks.

### 4.1 Active Track D: Developer Tooling Foundation

Most of Track D is complete. Only still-relevant follow-up items are kept here.

#### Phase D5: `goby lint` — high-signal static checks

Goal: machine-readable linter output for common mistakes not caught by the typechecker.

Lint rules (ordered by ascending analysis cost):

1. **Unreachable `case` arm**: wildcard `_` arm followed by more arms (implemented).
2. **Unused local binding**: `x = expr` where `x` is never referenced afterward
   (needs local-use tracking across `Expr`/`Stmt` spans from D1a). High user value.
3. **Shadowed effect operation name**: local binding name collides with a visible effect op.
4. **Redundant `can` annotation**: effect is fully discharged inside the function body.

Output format: human-readable (default) and JSON lines (`--json`).

#### Phase D6 follow-ups

- **D6c: Shared grammar asset** — extract shared language-definition data for VS Code grammar
  and Neovim/Vim syntax files.
- **D6b-ts: Tree-sitter grammar** — defer until after D6c.

### 4.3 Review Follow-ups (Backlog)

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
3. Hot-path allocation reduction (partially complete, 2026-03-28):
   - `callables: HashMap<String, IntCallable>` in `Cont::Apply`/`StmtSeq`/`InlineHandlerValue`
     changed to `RcCallables = Rc<HashMap<...>>` — suspend-path clones (~25 sites) are now
     pointer copies. `locals` clone reduction remains open (requires COW strategy).
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

### 4.5 Track OOB: List Index Out-of-Bounds Error Message

Goal: when `xs[i]` uses an index that is negative or at least the list length,
report a human-readable runtime error instead of an internal abort marker or raw
Wasm trap.

Current gap:

- **Interpreter path**: the `Expr::ListIndex` path still reports the internal
  abort marker instead of a user-facing out-of-bounds message.
- **Wasm path**: list-index helpers still trap without storing a dedicated
  runtime error code, so users can see a raw `unreachable`-style engine error.

Target behavior:

```text
runtime error: index out of bounds: index 5, list length 3
```

The long-term goal is to keep the message shape aligned across the interpreter
and Wasm paths. A fixed Wasm message such as `index out of bounds` is an
acceptable first slice if dynamic index/length formatting is not yet available.

Design direction:

- on the interpreter path, replace the abort marker with
  `set_runtime_error_once(...)` so out-of-bounds remains a normal runtime error.
- on the Wasm path, add a dedicated runtime error code and return through the
  existing runtime-error slot instead of trapping with `Unreachable`.
- keep the change at the shared list-index/runtime-error boundary rather than
  introducing path-specific ad hoc formatting.

Milestones:

1. **OOB-1**: interpreter message upgrade.
   - switch `Expr::ListIndex` to a user-facing out-of-bounds error message.
   - add interpreter coverage for both too-large and negative indices.
2. **OOB-2**: Wasm error-code path.
   - add a dedicated out-of-bounds runtime error code.
   - make list-index helpers report that code instead of trapping.
   - map the code to a human-readable CLI/runtime message.
3. **OOB-3**: message parity follow-through.
   - decide whether Wasm should also report index and list length exactly, or
     whether fixed wording is the stable cross-backend contract.
4. **OOB-4**: regression and docs follow-through.
   - run the full test suite.
   - update `doc/BUGS.md` if the current bug entry can be closed or narrowed.

Acceptance criteria:

- both interpreter and Wasm execution paths report a human-readable
  out-of-bounds error.
- ordinary non-OOB runtime abort behavior is unchanged.
- the implementation uses the existing shared runtime-error surface rather than
  backend-specific trap wording.

### 4.5 Track RR: Runtime Resource Failure Diagnostics and Resilience

Goal: when `goby run` fails because user code consumes too much runtime stack,
memory, or other bounded execution resources, report that clearly first, then
incrementally improve the runtime so that straightforward but inefficient Goby
code keeps working on realistic inputs.

Motivation:

- the desired product direction is "prefer code that behaves intuitively even if
  it is not especially efficient".
- for the `each` + nested `AssignIndex` class of failures, the main fix target
  should be Goby's runtime/lowering/runtime-diagnostics layers, not the user's
  program shape.
- before Goby can avoid these failures entirely, it must at least tell the user
  what kind of resource limit they hit.

Ideal end-state for Track RR:

- straightforward Goby code should usually run because the runtime/lowering
  stack is resilient enough, not because users were taught a growing list of
  "safe" rewrites.
- runtime resource behavior should be owned by shared compiler/runtime
  boundaries, not by source-program-specific exceptions.
- diagnostics should expose the remaining limits honestly:
  - classify what Goby knows,
  - say "likely" when Goby is inferring,
  - keep engine detail available as secondary evidence,
  - make it obvious which Goby functions dominated the failure.
- recursion support should converge on principled lowering/runtime rules:
  - tail-recursive shapes should naturally run in constant stack where Goby can
    model that honestly,
  - non-tail recursive scans should either be supported by a shared execution
    strategy or fail with a diagnosis that clearly says why they remain costly.
- list construction should converge on a representation/lowering strategy where
  intuitive persistent-style code such as `[x, ..rest]` is not pathologically
  punished by default for moderate workloads.
- limit tuning should be the final polish layer, not the primary design answer.

Stated differently: the ideal RR outcome is a Goby runtime where "write the
obvious recursive/list-building program first" is a rational default, and where
the remaining failures are principled, attributable, and rare rather than
mysterious or fixture-dependent.

Execution rule for RR work:

- before starting each RR implementation slice, explicitly review the ideal
  end-state above and ask:
  - does this step move a shared boundary toward that end-state,
  - or is it merely a local workaround for one failing program?
- if a proposed step cannot be explained as movement toward the ideal shared
  design, narrow it, redesign it, or reject it before coding.

Current confirmed bug shape:

- a runtime-`Read` / `GeneralLowered` program that recursively builds a list via
  `[x, ..rest]` can fail with Wasm memory exhaustion even when stdin is tiny.
- larger recursive programs can also fail with an unhelpful Wasm backtrace that
  does not explain whether the likely cause is stack growth, memory growth, or
  another runtime trap.

Two-stage plan:

1. **Stage RR-A: diagnose clearly**
   - prioritize user-facing error messages before attempting to eliminate the
     underlying limits.
   - whenever possible, classify runtime failures into coarse buckets such as:
     - stack growth / likely deep recursion,
     - memory exhaustion / large intermediate values,
     - generic runtime trap / unknown resource failure.
   - this classification must be explicitly best-effort. Goby should not claim
     certainty about stack overflow unless the runtime/engine surface provides a
     reliable signal; otherwise the wording should stay at "likely" / "possible".
   - error text may stay in English for now.
   - when classification is confident enough, include actionable wording such as:
     `"runtime error: likely stack overflow from deep recursion; consider rewriting this function in a tail-recursive or iterative style"`
     or
     `"runtime error: memory exhausted while building large intermediate lists; consider reducing recursive list-spread construction"`
   - if exact attribution is not available, prefer an honest partial diagnosis
     over a raw Wasm backtrace.
   - keep the raw engine/backtrace details as an optional secondary detail for
     debugging, not as the primary user-facing error.

2. **Stage RR-B: make straightforward code survive**
   - after diagnostics land, improve the runtime/lowering path so these failures
     become less common for normal Goby programs.
   - prioritize work in this order:
     1. improve tolerance for deep recursion:
        - tail-recursive loops like `count_valid_roll` / `check` should not fail
          early merely because they are written naturally.
        - candidate approaches: explicit tail-recursion lowering to loops, or
          other stack-consumption reductions for general calls where feasible.
     2. allow intuitive list construction more often:
        - patterns like `[(x, y), ..rest]` should not immediately explode on
          moderately large inputs.
        - investigate list representation and list-spread lowering/runtime
          behavior before asking users to rewrite such code.
     3. widen practical runtime resource limits:
        - current Wasm stack/memory bounds may be too conservative for "AoC-size"
          workloads.
        - raising limits is acceptable when it is honest and keeps failure modes
          predictable.
     4. keep refining diagnostics for the remaining hard limits:
        - even after resilience work, residual failures should report likely
          cause rather than a bare trap/backtrace.

Design stance:

- user code rewrites are the last resort, not the default answer.
- Goby should carry the burden of supporting straightforward recursive and list-
  building code as far as the execution environment honestly allows.
- limits may still exist because Wasm is bounded and general recursion is not
  free, but the toolchain should fail transparently and push those limits back
  where practical.

Status summary:

- RR-0 through RR-5 are complete and archived in `doc/STATE.md`.
- generic TCO milestones (M0–M7) are complete (2026-04-10). The published
  guarantee is locked in `doc/LANGUAGE_SPEC.md`: Goby has generic TCO on the
  compiled Wasm path for statically resolvable direct tail calls among known
  top-level declarations.
- any further RR/TCO work is post-publication extension, not contract
  bootstrapping. RR-6 (limit tuning) remains deferred.
- Sequence-backed List milestones (M0–M9) are complete (2026-04-14). The
  published `List` contract is locked in `doc/LANGUAGE_SPEC.md`.

Active milestones:

1. **RR-5: generic tail-call optimization** (complete, 2026-04-10)
   - generic TCO is published for the compiled Wasm path; the full
     implementation history is in `doc/STATE.md`.
   - the language-level contract is locked in `doc/LANGUAGE_SPEC.md`:
     direct tail calls among known top-level declarations execute in constant
     stack on the compiled Wasm path; higher-order and non-tail shapes remain
     outside the guarantee.
   - any future widening (additional backends, call categories) requires
     equally explicit proof and documentation before the wording is broadened.
2. **RR-6: limit tuning and follow-through**
   - revisit stack/memory defaults only after RR-2 through RR-4 give clearer
     ownership and failure modes.
   - only after that, revisit whether docs/examples should recommend more efficient
     user-code patterns for extreme workloads.

Acceptance criteria:

- resource-related runtime failures no longer surface as raw Wasm backtraces in
  the common known cases.
- at least one known failure shape reports a clear English message indicating
  likely stack pressure, memory pressure, or unknown runtime resource failure.
- the `doc/BUGS.md` reproduction for recursive list spread is covered by a
  regression test and is tracked against a concrete resilience improvement step.
- the initial message layer does not over-claim certainty where the engine only
  exposes an opaque trap.

### 4.5a Track GU: Generic Union Types with Constructor Arguments

Priority: **prerequisite for Track PC (§4.6)**.

Goal: extend `type` declarations so that **both unions and records may
carry type parameters**, and union constructors may carry arguments.
Concretely: support `type Maybe a = Just(a) | Nothing`,
`type Either a b = ...`, `type Tree a = Leaf | Node(a, Tree a, Tree a)`,
`type Parser a = Parser(run: Unit -> a can Token, Fail)`, plus the
corresponding `case` constructor patterns and field access on generic
records.

Why this matters:

- `crates/goby-core/src/ast.rs::TypeDeclaration` currently models a
  union as `constructors: Vec<String>` — a flat list of nullary
  constructor names — with no type parameters and no per-variant
  arguments. `CasePattern` has no constructor-pattern variant.
  Together this means `type Maybe a = Just(a) | Nothing` cannot be
  parsed, type-checked, or pattern-matched against.
- Track PC (§4.6) cannot start until GU lands: the very first PC
  milestone (PC-M0) introduces `stdlib/goby/maybe.gb` as exactly that
  declaration. The parser-combinator library further depends on
  `ParseResult a = Ok(a) | Err(ParseError)` and on `optional :
  Parser a -> Parser (Maybe a)`. None of those are expressible today.
- The feature is independently useful for any future user-defined ADT
  (Result, AST nodes, etc.) and is the natural sibling of the existing
  `effect E a b` parameter machinery.

The detailed implementation plan, milestones, and acceptance criteria
live in `doc/PLAN_GU.md`. The high-level shape:

- Design freeze (GU-D0, GU-D1): final AST + spec text locked before
  any code lands.
- Destructive AST swap (GU-S1): old `TypeDeclaration::Union` and
  `CasePattern` shapes removed in one commit; build breaks.
- Layer-by-layer follow-up (GU-S2): walk a closed file list to
  restore `cargo build` green.
- New typecheck semantics (GU-S3): parametric unions and parametric
  records get type-parameter unification, fresh constructor schemes,
  and constructor-pattern type checking.
- New lowering semantics (GU-S4): tagged-record allocation for
  variants with args, tag-dispatch pattern matching.
- Polish (GU-X0..X2): diagnostics, fixtures, spec sync.

Track GU covers **both generic union types** (`type Maybe a = Just(a)
| Nothing`) **and generic record types** (`type Parser a =
Parser(run: ...)`) in the same rewrite, because both share the
type-parameter machinery and PC depends on both.

Blocking dependencies:

- none — work is self-contained in the parser, AST, typecheck, and
  lower layers.

Track PC (§4.6) **hard-depends on GU-S3 (typecheck) + GU-S4
(lowering)**, and operationally gates on GU-X2 (the closed-form
green check). Track GU is independent of Track RP (§4.5b); either
may land first.

### 4.5b Track RP: Relative-Path Imports for Non-Stdlib Modules

Priority: **prerequisite for Track PC (§4.6)**.

Goal: allow a `.gb` source file outside `stdlib/` to import another
`.gb` source file by relative or workspace-rooted path, so that
multi-file libraries can live under `examples/` (or any future
package-mechanism root) without being placed in `stdlib/`.

Why this matters:

- The current resolver `StdlibResolver` (`crates/goby-core/src/stdlib.rs`)
  resolves `import a/b` strictly as `{stdlib_root}/a/b.gb`. The grammar's
  `is_module_path` check (`crates/goby-core/src/parser_util.rs`) rejects
  `./` and `../` prefixes outright. Together this means *any* multi-file
  Goby library outside `stdlib/` is unimportable.
- Track PC (§4.6) places the parser-combinator library under
  `examples/parser/` deliberately — parser combinators are an application
  of the effect system, not a language primitive. Without a way to import
  across files inside `examples/parser/`, the library either has to be
  collapsed into one giant file or wrongly relocated to `stdlib/`. Both
  options compromise the long-term shape (Rubygems / Cargo-style external
  package).
- The same mechanism is what a future package mechanism will be built on.

Scope to lock before coding:

1. Surface form. Likely candidates:
   - `import "./types"` / `import "../parser/core"` (string literal
     path, anchored at the importing file).
   - `import @workspace/parser/core` (sigil-rooted at the project root,
     e.g. the directory containing `Cargo.toml` or an explicit marker).
   - Hybrid: stdlib remains `import goby/string`; non-stdlib uses one of
     the above forms.
2. Resolution rules. Must specify file lookup, ambiguity (does
   `goby/x` ever clash with a relative `./goby/x`?), normalization
   (canonical paths, symlink behaviour), and error wording for
   not-found.
3. Cycle detection. Multi-file libraries inside `examples/` will have
   internal cycles by accident. The existing stdlib resolver already
   has cyclic-import detection; the relative path resolver must match.
4. Sandboxing. Decide whether relative paths can escape the importing
   file's directory tree (e.g. `../../etc/passwd.gb` reading arbitrary
   files). At minimum, document the policy.

Execution phases:

1. **RP-0: Surface lock.** Pick one of the candidate forms, document
   in `doc/LANGUAGE_SPEC.md` §1 imports. No code yet.
2. **RP-1: Parser + grammar.** Extend `is_module_path` (or add a new
   import-path classification) to accept the chosen form. Reject the
   form when it appears in `stdlib/` sources to keep stdlib paths
   canonical.
3. **RP-2: Resolver.** Add a non-stdlib resolver path; thread through
   the import-validation pipeline (`validate_imports` and friends).
4. **RP-3: Acceptance fixtures.** A two-file fixture under
   `examples/_rp_smoke/` where one file imports a type and an effect
   from the other; `goby check` must pass and `goby run` (where
   backend-supported) must produce expected output.
5. **RP-4: Diagnostics.** Not-found / cyclic-import / sandbox-violation
   errors carry the same quality as stdlib import errors.

Acceptance criteria for the track:

- a `.gb` file under `examples/` can import another `.gb` file under
  `examples/` via the chosen surface form, both in `goby check` and (when
  backend-supported) `goby run`.
- the chosen form is documented in `doc/LANGUAGE_SPEC.md` §1.
- `stdlib/` continues to use the canonical `import a/b` form
  unchanged; no stdlib file needs to switch.
- the cycle detection covers cross-file cycles inside the same
  examples-rooted library.

Blocking dependencies:

- none — the work is self-contained in the parser, resolver, and
  validator layers.

Track PC (§4.6) **depends on RP-3**. PC-P0's pre-flight (§PLAN_PC §4.2)
no longer needs to *probe* multi-file import; it consumes the answer
RP-3 has already given.

### 4.5c Track EX: Case Exhaustiveness Checking

Priority: **prerequisite for Track PC (§4.6)**. Locked 2026-05-08.

Goal: reject non-exhaustive `case` expressions at compile time, so
that pattern matches over user-defined unions, lists, and primitive
literal arms are statically guaranteed to cover every possible value.

Why this matters:

- Goby has no exhaustiveness checking today (`crates/goby-core/src/`
  contains zero references to "exhaustive" or equivalent). A `case`
  whose arms do not cover every value reaches the wasm backend with
  no static safety net.
- Track GU (§4.5a) introduces `Maybe`, `ParseResult`, `Tree`, and
  similar user-defined unions whose value space is *finite and
  declaration-known* (one variant per declared constructor). The GU
  window deliberately ships only an interim runtime trap (Wasm
  `unreachable`) for non-exhaustive `case`; lifting that to a
  compile-time error is the natural next step.
- Track PC (§4.6) builds on `Maybe` / `ParseResult` and writes
  `case` expressions in nearly every combinator. Without static
  exhaustiveness, every PC combinator that forgets a variant
  becomes a runtime trap. The user direction (2026-05-08) is that
  `case` non-exhaustiveness must be a compile-time error before PC
  starts.

Scope:

- **In scope**:
  - Constructor patterns over generic / non-generic unions
    (`Just x | Nothing`).
  - List patterns (`[]`, `[x, ..xs]`, fixed-length forms).
  - Bool / Int / String literal patterns where exhaustiveness is
    decidable in conjunction with `_` / wildcard arms.
  - Mixed wildcard handling: a trailing `_ -> ...` arm makes any
    `case` exhaustive by definition.
  - Cross-module union exhaustiveness (a union declared in module
    A and matched in module B).
- **Out of scope** (deferred):
  - Nested constructor patterns (already deferred in Track GU §2).
  - Useless / redundant arm detection (a separate diagnostic
    family; could land as a sibling lint in `goby lint`).
  - Refinement / GADT-style narrowing.

Blocking dependencies:

- **Track GU (§4.5a) must reach GU-X2 first.** EX needs the GU
  AST shape (`CasePattern::Ctor`, `UnionTypeInfo` with per-variant
  metadata) to enumerate the missing variants. EX cannot start
  before GU-X2 closes.
- Track RP (§4.5b) is independent of EX.

Execution phases (high-level — full plan lives in
`doc/PLAN_EX.md` once the track opens):

1. **EX-D0: Design freeze.** Lock the algorithm choice (likely a
   matrix-based usefulness check à la Maranget — *Compiling Pattern
   Matching to Good Decision Trees* — restricted to the single-level
   patterns Track GU surfaces). Lock the diagnostic shape: list
   *which* variants / list shapes are missing, not just "case is
   non-exhaustive".
2. **EX-S1: Typecheck pass.** Add a per-`case` exhaustiveness
   pass in `typecheck_check.rs` (or a new
   `typecheck_exhaustive.rs`). Walks union variant lists,
   list-pattern shapes, and literal arms. Emits a compile-time
   diagnostic naming missing arms.
3. **EX-S2: Lowering simplification.** Remove the GU-S4 interim
   `unreachable` emission for non-exhaustive `case`; the static
   pass now guarantees every reachable runtime value matches some
   arm. Wasm `unreachable` may remain as a defensive backstop for
   genuinely-impossible paths (e.g. arms gated by inferred-but-not-
   declared invariants), but every non-defensive use disappears.
4. **EX-X0: Diagnostic polish + fixtures.** Negative-path fixtures
   for each pattern family (union, list, bool, int / string + `_`)
   covering the missing-arm diagnostic. `examples/` audit: every
   shipping example whose `case` was previously implicitly trusting
   the runtime trap is updated to be statically exhaustive.

Acceptance criteria:

- Non-exhaustive `case` over a user-defined union (e.g.
  `case m  Just x -> ...`) produces a compile-time diagnostic
  naming the missing variant (`Nothing`).
- Non-exhaustive `case` over a list (e.g.
  `case xs  [] -> ...`) produces a diagnostic naming the missing
  shape (the cons case).
- A `case` whose final arm is `_ -> ...` is always exhaustive,
  regardless of preceding arms.
- `cargo test -p goby-core --lib` and `cargo nextest run -p
  goby-wasm` pass.
- All `examples/` files that contain a `case` expression compile
  without exhaustiveness diagnostics; any that previously relied on
  partial coverage are updated.
- `doc/LANGUAGE_SPEC.md` documents `case` exhaustiveness as a
  compile-time guarantee.

Track PC (§4.6) **hard-depends on EX-S1**: the operational start
gate for PC is "GU-X2 closed AND EX-S1 closed". PC's milestone
table (`doc/PLAN_PC.md` §6) is updated to reflect this when EX
opens.

### 4.6 Track PC: Parser Combinator on Algebraic Effects

Priority: **active line (2026-05-07)**. With effect-row polymorphism
landed, PC is the next concrete forcing function for §3.3 (multi-shot /
branch-local state).

Goal: deliver a small, idiomatic parser-combinator surface that **uses Goby's
effect system as the composition mechanism**, not as decoration.

Why this matters:

- a parser-combinator library is the smallest realistic program that
  simultaneously stresses (1) HOF effect propagation, (2) sequential
  multi-shot continuations for alternative/backtracking, and (3) explicit
  branch-local state for input-cursor rollback.
- if Goby cannot host a clean parser combinator, the effect system is not
  yet earning its complexity. Conversely, getting PC right validates the
  language direction laid out in §3.3.
- it produces a self-contained example/stdlib surface that doubles as an
  acceptance fixture for the future branch-local state work.

Scope to lock before coding:

1. Surface shape
   - decide whether `Parser a` is an ordinary function value
     (`fn input -> (a, input)`) lifted by combinators, or a thin wrapper
     around an effect-handler protocol.
   - decide which combinators are first-class (`pure`, `map`, `bind`/`>>=`,
     `alt`, `many`, `optional`, `seq`, `between`, `eof`).
   - decide token granularity (graphemes via `string.graphemes` or byte-ish
     `String` slices); current spec only exposes grapheme-list traversal.
2. Effect protocol
   - decide the operations carried by the parser effect(s). Working draft:
     - `Token` effect with `peek : Unit -> Maybe String` and
       `advance : Unit -> Unit` (cursor as handler-side state),
     - `Fail` effect with `fail : String -> a` (no `resume` → exits the
       enclosing `with`),
     - `Choice` effect with `choose : Unit -> Bool` for alternatives
       (multi-shot resume: `True` for left branch, `False` for right).
   - decide whether `Choice` is a separate effect or fused with `Fail` as
     `try : Parser a -> Parser a -> Parser a`.
3. Backtracking semantics
   - `alt p q` is the canonical multi-shot site. The handler must restore
     the input cursor on the right-branch resume.
   - until §3.3 branch-local state lands, the cursor must be threaded as an
     explicit operation argument (iterator-style), **not** as captured `mut`.
   - document committed-vs-uncommitted alternatives (Parsec-style `try`)
     once the state surface is decided.
4. Error reporting
   - decide the failure value shape (single message, or list of expected
     tokens with position).
   - reuse `goby/iterator` GraphemeState patterns for position tracking.

Blocking dependencies:

- **§4.5a Track GU (generic union types with constructor arguments)
  must land before PC-M0.** PC-M0 introduces
  `type Maybe a = Just(a) | Nothing` plus the corresponding `case`
  pattern. Neither is expressible in the current `TypeDeclaration` /
  `CasePattern` shapes. See `doc/PLAN_GU.md`.
- **§4.5b Track RP (relative-path imports) must land before PC-P0.**
  Without `examples/`-rooted multi-file imports, the
  `examples/parser/` library layout is unreachable, and stdlib
  placement is rejected by lock (`tmp/pc.md`). See `doc/PLAN_GU.md`
  §7.2 for GU↔RP independence and §7.1 for GU↔PC ordering.
- **§3.3 multi-shot classification + branch-local state surface must
  land before PC-2 implementation.** `alt` and `many` need either a
  sanctioned multi-shot path or an explicit branch-local state
  construct to roll the cursor back without violating ordinary-`mut`
  semantics. PC-1 (cursor-threaded MVP) does not depend on §3.3; only
  PC-2 does.

The §3.3 dependency has its priority raised by this track.

Execution phases:

1. **PC-0: Surface and protocol lock**
   - record the chosen `Parser` shape, effect protocol, and backtracking
     story in `doc/LANGUAGE_SPEC.md` (or a dedicated stdlib design note if
     the surface stays in stdlib).
   - add motivating examples (number parser, JSON-like literal, simple
     arithmetic-expression parser) as failing/aspirational fixtures.

2. **PC-1: Cursor-threaded MVP (no multi-shot)**
   - implement the iterator-style variant: cursor is an operation argument,
     `alt` is restricted to non-backtracking / longest-prefix forms, or
     simulated by passing the remaining token list explicitly.
   - lands as `examples/parser/` (multi-file library, depends on Track RP)
     plus top-level `examples/parser_*.gb` demos. **Stdlib placement is
     rejected by lock** (see `tmp/pc.md`, `doc/PLAN_PC.md` §1).
     `stdlib/goby/maybe.gb` is the only stdlib artifact this track adds.
   - acceptance: parses a small grammar end-to-end on `goby run` without
     relying on multi-shot resume or captured `mut`.

3. **PC-2: True backtracking on multi-shot effects**
   - depends on §3.3 branch-local state surface.
   - replace the cursor-threading variant with multi-shot `Choice` /
     `try` combinators that restore branch-local state on right-branch
     resume.
   - acceptance: a left-recursion-free grammar with real backtracking
     (`alt`, `many`, `optional`) parses correctly, and the
     handler-legality classifier accepts the resulting IR shapes.

4. **PC-3: Error messages and diagnostics polish**
   - position-aware failure values, `expected ... got ...` rendering.
   - integrate with `goby/string` grapheme position tracking.

Acceptance criteria for the track:

- `examples/parser_arith.gb` (or equivalent) parses a representative
  expression grammar end-to-end on `goby run`.
- combinators are written as ordinary Goby functions whose effect rows
  propagate via EP, not via per-combinator hand-written `can` clauses.
- multi-shot `alt` does not require capturing ordinary `mut` (uses the
  branch-local state surface or explicit cursor threading).
- the resulting `examples/parser/` library plus top-level
  `examples/parser_*.gb` demos are small enough to read in one sitting
  and serve as the canonical algebraic-effects showcase for the language.

Sketch (PC-1 cursor-threaded variant, illustrative — surface not yet locked):

```goby
import goby/string ( graphemes )

effect Parse
  peek   : List String -> (Maybe String, List String)
  expect : String -> List String -> (Unit, List String)

effect Fail
  fail : String -> a

# Combinator shapes are written as ordinary Goby functions.
# Effect rows now propagate through HOF callbacks via row polymorphism
# (`doc/LANGUAGE_SPEC.md` §5); the explicit `can Parse, Fail` at every
# combinator boundary below is retained only for pre-PC illustration
# (see the post-row-polymorphism note below).

digit : Unit -> String can Parse, Fail
digit () =
  case peek
    Some c ->
      # ... grapheme-class check elided ...
      expect c
      c
    None -> fail "expected digit"

many_digits : List String -> List String can Parse, Fail
many_digits acc =
  case peek
    Some _ ->
      d = digit ()
      many_digits (push acc d)
    None -> acc

# Driver: install Parse + Fail handlers around the top-level parser entry.
parse_int : String -> Maybe Int
parse_int src =
  tokens = graphemes src
  with
    peek input ->
      case input
        []         -> resume (None, input)
        [x, ..xs]  -> resume (Some x, input)
    expect _ input ->
      case input
        [_, ..xs] -> resume ((), xs)
        []        -> fail "unexpected end of input"
  in
    with
      fail _ -> None
    in
      digits = many_digits []
      Some (int.parse (list.join digits ""))
```

Note: the sketch is intentionally written as if effect rows were
monomorphic. With row polymorphism in place (2026-05-07), the explicit
`can Parse, Fail` on every combinator can collapse into an
effect-variable row, and `many_digits` can be expressed via a generic
`many : Parser a -> Parser (List a)` combinator without per-call `can`
rewriting.

References:

- Hutton & Meijer, "Monadic Parser Combinators" (1996).
- Leijen, "Parsec: Direct Style Monadic Parser Combinators" (2001).
- Plotkin & Pretnar, "Handlers of Algebraic Effects" (LICS 2009) — base
  semantics for the multi-shot path used by `alt`.
- Lindley, McBride & McLaughlin, "Do Be Do Be Do" (POPL 2017) — Frank's
  parser-style effect examples.

### 4.7a Track HF: Cond/Scrutinee/Arg Top-Level Scope Fix in Wasm Lowering — COMPLETED 2026-05-09

Resolved. The original BUGS.md 2026-05-08 entry has been moved to the
Resolved section of `doc/BUGS.md` with a full root-cause and fix
description. Summary: four `lower_value(...)` callsites in
`crates/goby-wasm/src/gen_lower/lower.rs` (`If` cond at 2688, `Case`
scrutinee at 3373, three `lower_value_as_arg` fallbacks at 3779 / 3794 /
3796) discarded the surrounding `aliases` / `bindings` / `known_decls`
context, so a `Var(top-level)` in those positions was lowered as
`LoadLocal` and tripped emit-time `unknown local`. A pre-existing
shadowing bug in the same function (`lower_value_as_arg`'s known-decl
arm at 3768) was also fixed by adding the missing
`!bindings.is_bound(name)` guard. Regression coverage: 6 direct IR-level
unit tests in `gen_lower::lower::tests::track_hf_*` (positive cases plus
shadowing-preservation guards for If / Case / arg position) and 2
emit-path integration tests in `gen_lower::tests::track_hf_*` (`if flag`
and `case mode` shapes, both verified red pre-fix and green post-fix).
`cargo nextest run -p goby-wasm` green at 865 tests.

### 4.8 Parking Lot (Needs Revalidation Before Implementation)

- CLI `build` expansion details (`--target`, `--engine-compat`, verify modes).
- CLI binary naming migration (`goby-cli` -> `goby`) final policy.

These items are intentionally kept as short placeholders until they become active.

## 5. Spec Detail Notes

### Still Open (Post-MVP)

- Effects/handlers: lexical nearest-handler runtime semantics are active; post-MVP work is to move from interpreter/runtime lookup to compiled `EffectId`/`OpId` tables.
- Effects/handlers (`resume` follow-up): add WB-4 classification for
  reentrant-looking resumes, delayed one-shot resumes, sequential multi-shot
  resumes, and multi-shot continuations that capture ordinary mutable locals.
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
- WasmFX: Typed Continuations and Stack Switching for WebAssembly (Hillerstrom et al., ICFP 2024): possible future emitter/runtime path for direct-style effect handlers on Wasm backends.
  - <https://arxiv.org/abs/2403.01036>
- Lexical Effect Handlers, Directly (Ma, Ge, Lee, Zhang, OOPSLA 2024): prompt/capability direction for lexical handler lookup.
- Multiple Resumptions and Local Mutable State, Directly (Muhcu, Schuster, Steuwer, Brachthaeuser, ICFP 2025): reference point for multi-shot continuations and mutable-state design tradeoffs.

## 7. Stdlib Design Policy and Normalization Plan

### 7.1 Policy: Stdlib as Ordinary Goby Code

**The guiding principle for the standard library is:**

> stdlib functions are written as ordinary Goby code wherever possible.
> Host-level special treatment is allowed only when the feature genuinely
> cannot be expressed in Goby (e.g. I/O, Unicode algorithms, memory layout).
> Every such exception must be explicitly marked in the `.gb` source.

This policy serves two goals:

1. **Stdlib as a language test.** Because stdlib functions go through the same
   compiler pipeline as user code, they exercise type-checking, IR lowering,
   and code generation. Bugs caught in stdlib are bugs caught before they hit
   users.

2. **No hidden contracts.** A developer who reads only a `.gb` file must be
   able to understand whether adding a new function will work. If a function
   requires a Rust-side hook to be functional, that dependency must be visible
   in the `.gb` source — not silently implied by its name appearing in a Rust
   constant.

### 7.2 How Host-Level Operations Are Expressed in Goby

Goby already has two explicit mechanisms for expressing host-runtime dependencies
in `.gb` source. These are the only acceptable forms of special treatment:

**`__goby_xxx` builtin calls**

Names prefixed with `__goby_` are host builtins resolved by the backend.
They appear directly in `.gb` function bodies:

```goby
# stdlib/goby/string.gb
grapheme_count : String -> Int
grapheme_count value =
  mut n = 0
  with
    yield _ _ -> resume (True, ())
  in
    n := __goby_string_each_grapheme value
  n
```

A function body that contains a `__goby_` call is clearly not pure Goby —
the dependency is visible without reading any Rust code.

**`@embed` annotation**

Used to bind a host-implemented effect handler to an effect type:

```goby
# stdlib/goby/prelude.gb
@embed Print __goby_embeded_effect_stdout_handler
@embed Read  __goby_embeded_effect_stdin_handler
```

**Rule:** if a stdlib function cannot be written in ordinary Goby, it must
use one of these two forms. Any other Rust-side special-casing (e.g. matching
on function names in `lower_comp_inner`) is a violation of this policy.

### 7.3 Current Status (updated 2026-04-14)

Traversal-name special-casing cleanup is complete for `list` stdlib helpers.
Current status:

| Function | Module | Rust special treatment | Policy verdict |
|---|---|---|---|
| `each` | `list` | derived from `__goby_list_fold` in `stdlib/goby/list.gb` | **Compliant** |
| `map` | `list` | `__goby_list_map` intrinsic wrapper in `stdlib/goby/list.gb` | **Compliant** |
| `graphemes` | `string` | `StringGraphemesList` host intrinsic | **Compliant** — body uses `__goby_string_each_grapheme` |
| `split` (empty sep) | `string` | Redirected to `StringGraphemesList` | **Compliant** — body uses `__goby_` builtins |

Publication note (M8, 2026-04-14):
- Explicit-boundary policy is now published as complete for `List` traversal,
  indexing, and update surfaces.

Follow-up closure (M9, 2026-04-14):
- `string.graphemes` / `string.split` no longer rely on temporary stdlib
  name-list wiring.
- `string.graphemes` now resolves through the ordinary
  `backend_intrinsic_for("string", "graphemes")` path.
- `string.split` now keeps empty-delimiter behavior inside the shared
  `StringSplit` boundary instead of rewriting `split text ""` to a separate
  stdlib-name-specific lowering path.

### 7.4 Refactoring Plan

#### Step 1: Remove `each` and `map` special-casing (complete, 2026-04-12)

- Remove `"each"` and `"map"` from `SPECIALLY_LOWERED_STDLIB_NAMES`.
- Remove the `GlobalRef { module="list", name="each"/"map" }` special branches
  in `lower_comp_inner` (`gen_lower/lower.rs`).
- Remove the corresponding `Var` branches for bare-name calls.
- Remove `WasmBackendInstr::ListEach`, `ListEachEffect`, `ListMap` variants
  and all emit/support code that references them.
- Status: completed. `each`/`map` now route through stdlib wrappers and
  `BackendIntrinsic::{ListFold,ListMap}` paths.

#### Step 2: Remove remaining string traversal name-list wiring (complete, 2026-04-14)

- Remove the remaining `graphemes` / `split` exclusions from generic stdlib
  declaration collection in `gen_lower/mod.rs`.
- Route `string.graphemes` through ordinary intrinsic resolution instead of
  dedicated call-site branches.
- Keep empty-delimiter `split` behavior inside `emit_string_split_helper`
  rather than a separate lowering rewrite to `StringGraphemesList`.
- Status: completed. The explicit-boundary story is now aligned for current
  `List` and `string` traversal surfaces.

#### Step 3 (future): Goby-native stdlib test support

Stdlib tests should be written in Goby source wherever possible, not primarily
as Rust strings embedded in compiler/runtime unit tests. This keeps stdlib
behavior close to the language surface it exposes and lets the stdlib double as
a user-facing example of how Goby programs test Goby code.

Reference shape: Gleam's default test flow is source-first. `gleam test` runs a
test module entrypoint, and the standard generated setup calls `gleeunit.main()`;
`gleeunit` then discovers public functions in `test/` whose names end in
`_test` and treats panics/assertion failures as test failures. The runner is a
tooling convention layered on ordinary Gleam modules rather than a separate
Rust-side fixture format.

Goby should follow the same product shape, adapted to current language
capabilities:

- add `goby test`, initially scoped to repository/local package tests.
- discover `.gb` files under `test/` or `tests/`.
- run an ordinary Goby entrypoint per test module, with a conventional stdlib
  runner module such as `goby/test`.
- support a naming convention such as top-level functions ending in `_test`,
  but implement discovery through the Goby test runner contract rather than
  hard-coding stdlib function-specific Rust cases.
- provide a small assertion surface in Goby, for example `assert.equal`,
  `assert.true`, and `assert.fail`, with diagnostics that report the test module
  and test function name.
- allow stdlib regression tests to live beside the language surface, e.g.
  `test/goby/list_test.gb`, importing `goby/list` and checking `append`, `push`,
  `map`, `fold`, `set`, etc. through normal calls.
- keep Rust integration tests for backend/runtime invariants that cannot yet be
  expressed in Goby, but prefer Goby-native tests for public stdlib behavior.
- include stdlib Goby tests in the quality gate once the runner is stable:
  `cargo fmt`, `cargo check`, `cargo test`, and `goby test`.

This makes stdlib regressions immediately visible when the compiler changes
while also exercising the same import, typecheck, lowering, and execution paths
that users rely on.

### 7.5 Risks

- `ListEachEffect` removal drops a print-fusion optimization (direct `fd_write`
  without `call_indirect`). This is a performance trade-off only, not a
  correctness issue. Accept for now; re-add as an explicit backend optimization
  pass later if profiling shows it matters.
- Recursive stdlib `each`/`map` has O(n) stack depth. Accept for MVP;
  tail-call or iterative lowering is a separate optimization track.
