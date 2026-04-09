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
- Higher-order function-type checking is complete; see Track E in §4 for the
  shipped callback mismatch behavior and remaining follow-up boundary.

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
- Effect propagation rules for higher-order functions — deferred; see §4.9 Track EP for the planned effect row polymorphism work.
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
- `int.to_string` is complete; see Track F in §4 for the shipped behavior and
  parity coverage summary.
- `List.map` migration plan (planned):
  - keep canonical map behavior in `stdlib/goby/list.gb` (`list.map` export path).
  - replace internal/builtin-path map callsites with stdlib module usage where possible.
  - after migration, trim builtin-only `map` special handling so runtime/compiler has a single semantics source.
- `List.fold` addition (complete, 2026-03-28):
  - `fold : List a -> b -> (b -> a -> b) -> b` — curried left-fold, accumulator-first callback.
  - Implemented in `stdlib/goby/list.gb` via ordinary stdlib recursion; no compiler special-casing.
  - Backend: `IndirectCall { arity: 2 }` generalized for 2-arg HOF callbacks.
  - Effect policy: follows same callback-effect treatment as `each`/`map` (effectful callbacks
    permitted if caller's `can` clause covers them; cross-path guarantee deferred until HOF
    effect propagation is formally designed).

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

### 4.2 Track E: Higher-Order Function-Type Checking (complete, 2026-03-27)

HOF callback mismatch diagnostic via shared matcher in `typecheck_unify.rs`.

### 4.3 Track F: Stdlib `int.to_string` (complete, 2026-03-25)

`goby/int.to_string : Int -> String` implemented end-to-end.

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

### 4.5 Track ER + TD: Compiler Error Reporting (complete)

ER (name-resolution diagnostics) and TD (typed diagnostic spans, TD0–TD5) complete.

Remaining deferred diagnostic work (not yet planned as a track):

- block-structure validation spans, declaration body vs return-type mismatch spans,
  required-effect coverage failure spans, conflicting effect / `@embed` validation spans,
  type-declaration validation spans, multiline/body-relative expression span ownership.

### 4.6 Track CC: Closure Capture (complete)

CC0–CC6 complete. Immutable capture by value, mutable capture via shared cell.
Semantics in `doc/LANGUAGE_SPEC.md`.

### 4.7 `Float` / Wasm `f64` Support

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

### 4.8 Track LM: Mutable List Element Assignment (complete, 2026-04-06)

`a[i] := v` and `a[i][j] := v` work end-to-end (LM0–LM4). Semantics in `doc/LANGUAGE_SPEC.md` §3.

### 4.8a Track CL: Closure-Captured Mutable List Assignment

Goal: make `AssignIndex` work when the mutable root binding is a closure-captured
shared cell, closing the last open bug in `doc/BUGS.md`.

Reproduction:

```goby
import goby/list

main : Unit -> Unit can Print
main =
  a = [1, 2, 3]
  mut b = [10, 20, 30]
  list.each a (fn i ->
    b[0] := b[0] + i
    ()
  )
  println("${b[0]}")
```

Current behavior: `goby run` fails with `runtime error: gen_lower/emit: unknown local 'b'`.

Root cause:

- Track CC established that mutable bindings captured by a lambda are promoted to
  shared heap cells. Inside the lambda body, the real local is `__cell_<name>` and
  reads/writes go through `LoadCellValue`/`StoreCellValue`.
- `CompExpr::Assign` already handles this: it checks the `aliases` map for
  `CellPromoted` and routes through `StoreCellValue` (lower.rs ~631–658).
- `lower_assign_index` does not perform this check. It unconditionally emits
  `LoadLocal { root }` and `StoreLocal { root }`, which reference a local that
  does not exist inside a lambda body where the root was cell-promoted.

Design principle:

- the lowering layer contract is: any instruction sequence that accesses a named
  binding must resolve the binding's storage mode (plain local, cell-promoted, or
  closure slot) through `aliases`. Direct `LoadLocal`/`StoreLocal` with a raw
  binding name is valid only for locals whose plain-local status is structurally
  guaranteed (e.g. temporaries declared within the same function via `DeclareLocal`).
- for reads, `lower_value_ctx` already centralizes this resolution at the `Var`
  level. For writes, `Assign` handles cell-promotion inline. This fix applies the
  same inline pattern to `AssignIndex`.
- note for future work: if a third write-side node appears (e.g. `AssignField` for
  record updates), consider extracting a shared `store_to_binding` helper to avoid
  repeating the cell-promotion check. This is not in scope for the current fix.

Fix scope (`lower_assign_index` in `gen_lower/lower.rs`):

1. At function entry, determine whether root is cell-promoted via `aliases`.
2. Replace the three sites that reference root as a plain local:
   - descent phase root load (currently `LoadLocal { root }`) →
     `LoadLocal { __cell_<root> }` + `LoadCellValue`.
   - ascent phase root load (same pattern).
   - write-back (currently `StoreLocal { root }`) →
     `StoreCellValue { cell_ptr: LoadLocal { __cell_<root> }, value: ascent result }`.
3. Non-root intermediate locals (`__lset_<root>_d*`, `__lset_<root>_u*`) are
   function-scoped temporaries and are never cell-promoted; they remain unchanged.

Milestones:

1. **CL-1**: Apply the fix to `lower_assign_index`.
2. **CL-2**: Add integration tests:
   - depth-1: the reproduction case above (single-level closure-captured index update).
   - depth-2: nested index update on a closure-captured mut binding
     (e.g. `mut b = [[1,2],[3,4]]; list.each xs (fn _ -> b[0][1] := 99; ())`).
3. **CL-3**: Run full test suite (`cargo test`), update `doc/BUGS.md`.

Acceptance criteria:

- the depth-1 reproduction program prints `16` (= 10 + 1 + 2 + 3).
- the depth-2 test reads back the updated nested value correctly.
- full `cargo test` passes (not just AssignIndex-related tests).
- no new ad-hoc root-name rewriting or source-shape recognizer is introduced.

### 4.8b Track OOB: List Index Out-of-Bounds Error Message

Goal: `xs[i]` で `i` がリストの長さ以上（または負）の場合に、わかりやすいエラーメッセージを出す。

#### 現状

- **インタープリタ**: `runtime_resolver.rs` の `Expr::ListIndex` アームが OOB を検知すると
  `mark_runtime_abort()` を呼ぶ。これは `runtime_error` フィールドに内部マーカー文字列
  `"__goby_runtime_abort__"` をセットするだけで、最終出力は
  `runtime error: __goby_runtime_abort__` という不親切なメッセージになる。
- **Wasm**: `emit_list_get_helper` が OOB を検知すると `emit_abort`（= `Unreachable` 命令）
  でトラップする。エラーコードスロット（`GLOBAL_RUNTIME_ERROR_OFFSET`）には何も書かず、
  wasmtime が `"wasm trap: wasm \`unreachable\` instruction executed"` を返す。
  `RUNTIME_ERROR_MEMORY_EXHAUSTION` のような専用コードは存在しない。

#### 目標動作

```
runtime error: index out of bounds: index 5, list length 3
```

インタープリタ・Wasm 双方で同じ書式のメッセージが出ること。

#### 設計方針

**インタープリタ側** (`runtime_dispatch.rs` / `runtime_resolver.rs`):

- `mark_runtime_abort()` の代わりに `set_runtime_error_once(message)` を呼ぶ。
  `set_runtime_error_once` はすでに存在し、任意のメッセージをセットできる。
- メッセージは `format!("index out of bounds: index {i}, list length {}", items.len())`。
- `mark_runtime_error_is_abort_marker()` の判定ロジックはそのまま保持し、
  OOB のメッセージはマーカーではない普通のエラーとして通過させる。

**Wasm側** (`layout.rs` / `gen_lower/emit.rs` / `wasm_exec.rs`):

- `layout.rs` に `RUNTIME_ERROR_INDEX_OUT_OF_BOUNDS: u32 = 2` を追加。
- `emit_list_get_helper` の OOB ブランチを `emit_abort` から
  `emit_memory_exhaustion_abort` と同パターンの「エラーコードを書いて return」に変更
  （新関数 `emit_index_oob_abort` を追加）。
- `wasm_exec.rs` の `runtime_error_message` に
  `RUNTIME_ERROR_INDEX_OUT_OF_BOUNDS => Some(ERR_INDEX_OUT_OF_BOUNDS)` を追加。
  ただし Wasm では実行時にインデックス値・長さを文字列として出力できないため、
  まず固定メッセージ `"index out of bounds"` で十分とする（OOB-3 でインデックス値付きに拡張可能）。

#### マイルストーン

1. **OOB-1**: インタープリタ側の OOB メッセージ改善。
   - `runtime_resolver.rs` の `Expr::ListIndex` アームで `set_runtime_error_once` を使用。
   - テスト: インタープリタパスで OOB アクセスをするプログラムが
     `"runtime error: index out of bounds: index N, list length M"` を出力する。
2. **OOB-2**: Wasm 側の OOB エラーコード追加。
   - `layout.rs` に `RUNTIME_ERROR_INDEX_OUT_OF_BOUNDS = 2` を追加。
   - `emit_list_get_helper` で OOB 時に新コードをメモリスロットへ書き込んで return。
   - `wasm_exec.rs` の `runtime_error_message` で新コードを `"index out of bounds"` にマップ。
   - テスト: Wasm パスで OOB アクセスが `"index out of bounds"` エラーになる。
3. **OOB-3** (任意): Wasm 側でもインデックス値・長さを含むメッセージを出す。
   - エラーコードスロットの拡張または文字列スロットの追加が必要。
   - 複雑度が高いため MVP では OOB-2 の固定メッセージで許容し、後回し可。
4. **OOB-4**: `cargo test` 全通過確認・`doc/BUGS.md` 更新。

#### 受け入れ基準

- `xs[5]`（`xs` の長さが 3）を実行すると、インタープリタ・Wasm 双方で
  人間が読めるエラーメッセージが出る。
- 既存の OOB 以外のパスで `mark_runtime_abort()` の挙動が変わらないこと。
- `cargo test` 全通過。

### 4.8c Track RR: Runtime Resource Failure Diagnostics and Resilience

Goal: when `goby run` fails because user code consumes too much runtime stack,
memory, or other bounded execution resources, report that clearly first, then
incrementally improve the runtime so that straightforward but inefficient Goby
code keeps working on realistic inputs.

Motivation:

- the desired product direction is "prefer code that behaves intuitively even if
  it is not especially efficient".
- for the `solve2.gb` class of failures, the main fix target should be Goby's
  runtime/lowering/runtime-diagnostics layers, not the user's program shape.
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

Milestones:

1. **RR-0: minimal reproductions and test ownership** (complete, 2026-04-08)
   - `doc/BUGS.md` now records the minimal runtime-`Read` recursive list-spread
     reproduction.
   - `crates/goby-cli/tests/cli_integration.rs` covers the recursive
     list-spread memory-exhaustion shape and asserts that the user-facing error
     stays classified instead of falling back to a raw Wasm backtrace.
2. **RR-1: runtime failure classification and message surface** (complete, 2026-04-08)
   - `goby-wasm` now classifies Wasm execution failures on a best-effort basis as:
     - `memory exhausted [E-MEMORY-EXHAUSTION]`,
     - `likely stack pressure [E-STACK-PRESSURE]`,
     - `unknown runtime trap [E-RUNTIME-TRAP]`.
   - `goby-cli` normalizes the rendered runtime error so classified messages are
     not prefixed twice.
   - the current boundary keeps raw engine detail only as secondary detail for
     unknown traps, and now emits Wasm function names for Goby-generated
     functions so backtraces identify frames such as `goby!check` or
     `goby!update_rolls`.
3. **RR-2: representative failure decomposition** (complete, 2026-04-08)
   - preserved Goby-owned representative repros/tests for the four buckets:
     - self tail recursion:
       `runtime_rr_tests::rr2_self_tail_recursion_repro_surfaces_stack_pressure_under_tight_stack_limit`
     - non-tail recursive scan:
       `runtime_rr_tests::rr2_non_tail_recursive_scan_repro_executes_as_general_lowered`
     - recursive list spread / concat growth:
       `runtime_rr_tests::rr2_recursive_list_spread_repro_reports_memory_exhaustion`
       plus the CLI-level regression
       `run_command_reports_recursive_list_spread_memory_exhaustion_without_raw_backtrace`
     - mixed callback recursion:
       `runtime_rr_tests::rr2_callback_assisted_scan_repro_executes_as_general_lowered`
   - the preserved buckets now imply these ownership boundaries:
     - self tail recursion belongs to shared recursion lowering/runtime call-stack
       behavior, but is not the default next slice because it does not explain the
       hotter `solve2.gb` path by itself.
     - non-tail recursive scans are the primary RR-3 target and belong to a shared
       lowering/runtime boundary that reduces stack growth without symbol-specific
       rewrites.
     - callback-assisted recursion is not split into a separate first fix; current
       evidence keeps it in the same shared RR-3 boundary as non-tail scans because
       the hot path is the scan-with-callback shape rather than callback dispatch
       alone.
     - recursive list spread / concat growth belongs to runtime data representation
       and concat/list-spread ownership, so it stays deferred to RR-4.
   - rejected first-fix boundaries recorded by RR-2:
     - Wasm stack/memory limit tuning alone,
     - symbol-specific rewrites for `count_valid_roll` / `collect_prune_positions`,
     - generic self-tail lowering as the default next slice without a broader user win,
     - treating callback dispatch as an isolated bug before the shared scan boundary.
4. **RR-3: recursion resilience at the right boundary**
   - improve runtime/lowering behavior for recursion depth, but only after RR-2
     confirms which recursion class is worth targeting first.
   - current evidence says self tail recursion alone is not the dominant issue
     for the `solve2.gb` / iterative-grid class; the hotter path is non-tail
     recursive scanning (`collect_prune_positions` / `count_valid_roll`) plus
     callback recursion in `fold`.
   - therefore the first RR-3 candidates should be:
     - a narrow, explicitly modeled lowering for iterative grid/list scans, or
     - another shared boundary that reduces stack growth for non-tail recursive
       scans without changing unrelated call semantics.
   - "iterative grid/list scans" here must be treated as a placeholder symptom,
     not as a source-level special case. Any implementation must be justified as
     a shared rule over a recognizable recursion/control-flow shape, not as a
     symbol-specific or fixture-specific optimization for names such as
     `collect_prune_positions` or `count_valid_roll`.
   - treat "generic self-tail-call lowering" as a useful subproblem, but not as
     the default next slice unless a representative RR-2 repro shows it carries
     a real user-facing win by itself.
   - design guardrails for RR-3:
     - do not add source-symbol special cases,
     - do not key behavior on one AoC fixture's AST spelling,
     - do not accept a local lowering hack unless its applicability can be
       described as a reusable rule over backend-IR or control-flow structure.
   - locked RR-2 handoff into RR-3:
     - start from the shared non-tail scan boundary first,
     - keep callback-assisted recursion in scope only where it exercises that same
       boundary,
     - do not spend the first RR-3 slice on self-tail-only lowering unless a
       representative RR-2 repro demonstrates a broader win than the scan bucket.
   - RR-3 status (2026-04-09):
     - `gen_lower/lower.rs` now recognizes a restricted self-recursive Int scan
       shape and lowers it to backend-IR loop form instead of self `DeclCall`.
     - compile coverage now proves both the lowering decision and the emitted
       Wasm shape for that scan bucket:
       `lowers_supported_self_recursive_int_scan_to_loop` keeps the backend-IR
       proof, and `compile_module_scan_loop_lowering_eliminates_walk_self_call_in_wasm`
       proves the compiled Wasm validates and no longer self-calls in the looped
       helper body.
     - tight-stack runtime coverage now proves the same shared boundary for both
       the primary non-tail scan bucket and the callback-assisted scan bucket:
       `rr3_non_tail_recursive_scan_repro_survives_tight_stack_limit_after_loop_lowering`
       and `rr3_callback_assisted_scan_repro_survives_tight_stack_limit_on_same_boundary`
       both succeed under the RR low-stack configuration.
     - RR-3 is therefore complete for the currently locked representative scan
       buckets; remaining recursion-resilience work now moves to RR-4 list-spread
       ownership.
5. **RR-4: list-spread resilience**
   - improve runtime/lowering behavior for recursive list-spread memory growth.
   - current evidence says contiguous list allocation plus `ListConcat` copying
     still creates avoidable O(n^2)-style growth for shapes like `[x, ..rest]`.
   - focus first on clarifying the real ownership boundary:
     - list representation,
     - spread lowering shape,
     - concat runtime implementation,
     before simply raising limits.
   - selection criteria for the first RR-4 implementation:
     - prefer the boundary that improves the pathological shape without baking in
       a one-off special case for `[x, ..rest]` alone,
     - prefer changes that remain compatible with a future better `List`
       representation instead of hard-coding today's contiguous-copy behavior as
       permanent language semantics,
     - treat pure limit tuning as a fallback after structural options are
       rejected, not as the default fix.
   - RR-4 status (2026-04-09):
     - the first structural slice landed at the list-spread lowering/runtime
       boundary rather than by changing the user-visible `List` representation:
       restricted self-recursive list builders that return `[prefix..., ..rest]`
       now lower to a builder-backed loop instead of recursive `ListConcat`.
     - compile coverage proves the emitted Wasm validates and removes direct
       self-calls from the helper body
       (`compile_module_list_spread_builder_lowering_validates_and_eliminates_build_self_call`).
     - runtime coverage now locks both the historical `doc/BUGS.md` repro and a
       larger builder-shaped variant as successful executions
       (`rr4_recursive_list_spread_repro_executes_after_builder_lowering`,
       `rr4_recursive_list_spread_large_builder_shape_scales_past_bug_repro_size`).
     - the next structural slice widened that same ownership to inline empty-acc
       `fold` callbacks that prepend onto the accumulator:
       `compile_module_inline_fold_prepend_lowering_rewrites_concat_chain_in_main`
       proves main lowering switches from callback-side `ListConcat` to a
       dedicated reverse traversal, and
       `rr4_inline_fold_prepend_builder_executes_after_specialized_lowering`
       locks the runtime success case.
     - one RR-4 bucket still remains open, but it is now narrower:
       named-callback prepend chains still route through ordinary stdlib `fold`
       and preserve the old memory failure shape
       (`rr4_named_callback_list_spread_chain_still_reports_memory_exhaustion`).
     - the next RR-4 decision is therefore narrower:
       determine whether named/local callback prepend builders should reuse this
       same reverse-fold boundary or whether a different shared stdlib-call /
       concat boundary would be more honest.
6. **RR-5: limit tuning and follow-through**
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

### 4.9 Track EP: Effect Row Polymorphism

Goal: add effect-variable quantification so higher-order functions propagate
callee effects through the type system, closing the largest gap between Goby's
effect system and the algebraic-effect theory (Plotkin & Pretnar 2009).

Why this matters:

- without effect polymorphism, `map`, `fold`, `each` and any user-defined HOF
  cannot express "I propagate whatever effects my callback carries" at the type level.
- callers must manually account for callback effects with ad hoc `can` annotations,
  and the typechecker cannot verify completeness.
- this is the single most impactful missing piece for Goby to be a credible
  algebraic-effect language; Koka, Eff, and Frank all have this, and even
  OCaml 5 (which lacks typed effects) motivates its absence as a known limitation.

Scope to lock before coding:

1. Effect-variable syntax
   - decide surface syntax for effect variables in type annotations
     (for example `f : (a -> b can {e}) -> List a -> List b can {e}`
     or Koka-style `f : (a -> e b) -> list<a> -> e list<b>`).
   - decide whether effect variables are implicitly universally quantified
     (like type variables `a`, `b` today) or require explicit binders.
2. Effect-row representation
   - internal representation: ordered set, unordered set, or row-variable model.
   - decide whether effect rows support closed rows (`can Log`) and open rows
     (`can Log, {e}`) or open rows only.
3. Unification / inference rules
   - effect-variable unification strategy in the typechecker.
   - interaction with existing `can` checking and `with`-based discharge.
4. Backward compatibility
   - existing `can X, Y` annotations must continue to work unchanged.
   - monomorphic effect annotations remain valid (they are the zero-variable case).

Execution phases:

1. **EP-0: Semantics lock and spec update**
   - write the effect-variable syntax and inference rules in `doc/LANGUAGE_SPEC.md`.
   - add motivating examples showing current gap and intended behavior.
   - update `doc/PLAN.md` §2.3 deferred items.

2. **EP-1: Internal effect-row representation**
   - extend `Ty` / effect representation with effect variables.
   - implement effect-row unification in `typecheck_unify.rs`.
   - keep runtime behavior unchanged (effects remain name-dispatched).

3. **EP-2: HOF effect propagation**
   - teach the typechecker to infer effect variables for callback parameters.
   - stdlib HOFs (`each`, `map`, `fold`) get polymorphic effect annotations.
   - add regression tests: effectful callback through `map`/`fold` must propagate
     the callback's effects to the caller's `can` clause.

4. **EP-3: Diagnostics and edge cases**
   - error messages for effect-variable mismatch.
   - interaction with `with` discharge (effect variable partially discharged).
   - interaction with generic type parameters (effect + type polymorphism).

Acceptance criteria:

- `map xs (fn x -> Log.log x; x)` in a function without `can Log` is a type error.
- `map xs (fn x -> Log.log x; x)` in a function with `can Log` typechecks.
- the above holds without any special-casing of `map`; the mechanism is general.
- existing programs with monomorphic `can` annotations are unaffected.

References:

- Plotkin & Pretnar, "Handlers of Algebraic Effects" (LICS 2009)
- Leijen, "Type Directed Compilation of Row-Typed Algebraic Effects" (POPL 2017) — Koka's foundation
- Hillerström & Lindley, "Shallow Effect Handlers" (APLAS 2018)
- Bauer & Pretnar, "Programming with Algebraic Effects and Handlers" (JLAMP 2015)

### 4.10 Parking Lot (Needs Revalidation Before Implementation)

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

### 7.3 Current Violations (as of 2026-04-07)

The following stdlib functions are special-cased in Rust
(`SPECIALLY_LOWERED_STDLIB_NAMES` in `gen_lower/mod.rs`,
special branches in `gen_lower/lower.rs`) without any marker in the `.gb` source:

| Function | Module | Rust special treatment | Policy verdict |
|---|---|---|---|
| `each` | `list` | `ListEach`/`ListEachEffect` Wasm instructions | **Violation** — pure Goby recursion works |
| `map` | `list` | `ListMap` Wasm instruction | **Violation** — pure Goby recursion works |
| `graphemes` | `string` | `StringGraphemesList` host intrinsic | **Compliant** — body uses `__goby_string_each_grapheme` |
| `split` (empty sep) | `string` | Redirected to `StringGraphemesList` | **Compliant** — body uses `__goby_` builtins |

### 7.4 Refactoring Plan

#### Step 1: Remove `each` and `map` special-casing

- Remove `"each"` and `"map"` from `SPECIALLY_LOWERED_STDLIB_NAMES`.
- Remove the `GlobalRef { module="list", name="each"/"map" }` special branches
  in `lower_comp_inner` (`gen_lower/lower.rs`).
- Remove the corresponding `Var` branches for bare-name calls.
- Remove `WasmBackendInstr::ListEach`, `ListEachEffect`, `ListMap` variants
  and all emit/support code that references them.
- Done when: all existing tests pass, and `each`/`map` work correctly via
  stdlib `.gb` recursion (both lambda callbacks and named-function references).

#### Step 2: Rename `SPECIALLY_LOWERED_STDLIB_NAMES`

- Rename to `INTRINSIC_STDLIB_NAMES` (or similar) to reflect that it now only
  lists names whose `.gb` bodies contain `__goby_` calls and must not be routed
  through the generic DeclCall path for other reasons.
- Update the doc comment to reference the `__goby_` convention.
- Done when: the Rust constant and the `.gb` sources are in 1-to-1 correspondence
  and the intent is clear without reading both files simultaneously.

#### Step 3 (future): Stdlib as an integration test suite

- Add a test runner that compiles and executes every function in stdlib with
  representative inputs, comparing output against expected values.
- This makes stdlib regressions immediately visible when the compiler changes.

### 7.5 Risks

- `ListEachEffect` removal drops a print-fusion optimization (direct `fd_write`
  without `call_indirect`). This is a performance trade-off only, not a
  correctness issue. Accept for now; re-add as an explicit backend optimization
  pass later if profiling shows it matters.
- Recursive stdlib `each`/`map` has O(n) stack depth. Accept for MVP;
  tail-call or iterative lowering is a separate optimization track.
