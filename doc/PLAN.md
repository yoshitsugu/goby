# Goby Language Plan (Draft)

This document tracks:

- what is already visible from examples,
- what must be fixed for MVP,
- what can be postponed.

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
- Basic shown types: `Int`, `String`, tuple `(A, B)`, `List T`

## 2. MVP Decisions and Remaining Open Items

### 2.0 Locked for Current MVP

- Function calls support both `f x` and `f(x)`.
  - `f(x)` is used when precedence needs to be explicit.
- Block-local binding semantics are fixed for MVP:
  - `name = expr` is a local binding statement only when `name` is an identifier and `=` is assignment (not `==`).
  - bindings are visible to subsequent statements in the same declaration body.
  - re-binding the same name in the same body is allowed; the newer binding shadows the older one from that point onward.
  - bindings are declaration-local and do not escape to other declarations.
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
  - `goby-cli check <file.gb>` performs parse/typecheck without runtime entrypoint.
- `main` type is restricted to `Unit -> Unit` for MVP.
  - `main` type annotation is required for `run`; optional for `check`.
- Legacy `void` type spelling is rejected in type annotations.
- First backend target is Wasm.
- Effects and handlers are available in MVP runtime via `effect`, `handler`, and `using`.
  - handler dispatch is deterministic via `active_handlers: BTreeMap<String, usize>`.
  - `can` clauses are validated (declared effect or built-in effect names).
- `using` handler application syntax: comma-separated handler list (`using HandlerA, HandlerB`).
- MVP built-ins: `print`, `string.concat`, `map`, `fetch_env_var`, `string.split`, `list.join`.
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
  - `CasePattern` supports: `IntLit`, `StringLit`, `BoolLit` (`True`/`False`), `Wildcard`.
  - `else if` chaining is not supported in MVP.
- `if ... else ...` expression: indentation-based two-branch form.
- `==` equality operator: produces `Bool` at runtime.
- Handler dispatch: `active_handlers` uses `BTreeMap` for deterministic (alphabetical) order.
- Runtime execution model: compile-time interpreter — `resolve_main_runtime_output` runs the
  program in Rust at Wasm compile time and embeds output as a static string in the Wasm binary.

### 2.1 Syntax and Parsing

- No additional open syntax/parsing items are tracked for MVP.

### 2.2 Types and Checking

- TODO (Deferred): declaration-side generic parameter binders
  (for example, `id : a -> a` with explicit binders).
- Type annotation placement rules (required vs optional locations).
- Type error diagnostics quality bar is fixed for MVP:
  - diagnostics must be non-empty and human-readable plain text.
  - when a declaration is known, diagnostics must include the declaration name.
  - type mismatch diagnostics must include both expected and actual type names.
  - composite types should be rendered with full shape (for example: `List Int`, `(String, Int)`), not collapsed labels.
  - line/column reporting is not required in MVP.

### 2.3 Effect System

- Current implemented checks:
  - `can` effect names must be declared (or built-in).
  - uncovered effect operation calls are rejected unless covered by enclosing `using`.
  - calls to `can`-annotated functions require an appropriate enclosing `using`.
- Current runtime behavior:
  - effect operations dispatch through installed handlers.
  - bare-name dispatch falls back to deterministic effect-name order (temporary MVP behavior).
- How to represent multiple effects (`can Print + Read` or other syntax) — deferred.
- Effect propagation rules for higher-order functions — deferred.
- Effect diagnostics UX polish (wording/format consistency) — deferred.

#### Post-MVP Implementation Direction (locked 2026-03-01)

- Adopt **deep handlers + one-shot resumptions** as the baseline semantics.
  - Rationale: this matches the efficient path used by OCaml 5 and keeps runtime costs low for the common case.
  - Multi-shot resumptions are deferred; if added later, they must be explicit (clone/copy semantics) and opt-in.
- Replace name-based runtime handler lookup with **compiled IDs**:
  - intern `EffectId` and `OpId` at compile time,
  - compile each handler into a compact operation table indexed by `OpId`,
  - resolve operations by lexical handler stack walk (nearest enclosing handler wins).
- Use **selective CPS + evidence passing** in lowering:
  - keep pure/no-effect functions in direct style,
  - lower only effectful call paths and handler boundaries to continuation/evidence form,
  - pass handler evidence explicitly instead of global maps in hot paths.
- Wasm lowering strategy (phased):
  - Phase A: explicit continuation objects + trampoline/state-machine execution (portable on current Wasm MVP engines).
  - Phase B: optional optimization path on engines with typed continuations/stack-switching support.
- Performance guardrails for implementation:
  - no `HashMap`/`BTreeMap` lookup on hot operation dispatch paths,
  - dispatch target should be `O(handler_depth)` frame walk + `O(1)` op-table index,
  - continuation capture/resume should avoid full-stack copying on one-shot path.

### 2.4 Standard Library Surface (MVP)

- Core modules to ship first (`Int`, `String`, `List`, `Env`) — minimal built-ins implemented.
- Naming conventions for stdlib functions (`string.concat` style consistency) — established.
- Minimal collection API for immutable workflows — deferred.

### 2.5 Runtime / Compiler Scope (MVP)

- Current model: compile-time interpreter (Rust) embeds static output in Wasm binary.
- Real Wasm code generation (actual instruction emission) is a post-MVP target.
- Error location strategy (line/column reporting) — deferred.

### 2.6 Tooling Scope (MVP)

- Minimum CLI commands (`goby-cli run`, `goby-cli check`) — complete.
- Project layout and package metadata format (Cargo workspace with `goby-core`, `goby-cli`, `goby-wasm`) — complete.
- Formatter policy — deferred.

## 3. Later-Phase Decisions

- Module/package ecosystem and remote dependency management.
- Advanced effects (async, state, cancellation, effect-safety diagnostics).
- Pattern matching exhaustiveness and advanced ADTs.
- Interoperability/FFI strategy.
- Governance model, RFC process, compatibility policy.
- Real Wasm code generation (replace compile-time interpreter).
- Declaration-side generic parameter binders.
- `HandlerMethod` type-checking (effect-safety, unhandled-effect diagnostics).
- `else if` chaining in `if` expressions.
- REPL or interactive mode.

## 4. Next Phase Plan

All MVP example targets are complete. The next work is post-MVP:

- Keep regression checks green for the locked MVP subset.
- Treat remaining design items as post-MVP evolution work.
- Track all new syntax requests as explicit change proposals.
- Candidate next focus areas:
  1. Real Wasm code generation (actual instruction emission, remove compile-time interpreter).
  2. Effect runtime redesign (one-shot deep handlers + selective CPS/evidence passing).
  3. Better error diagnostics (line/column, effect-safety errors).
  4. More standard library surface (`Result`, `Option`, etc.).

### 4.1 Short-Term Patch Plan: bare effect call in `main` / top-level statement AST path

Goal: make bare effect operation calls (for example, `catch "NoCoffeeError"`) dispatch
through active `using` handlers when they appear as direct expression statements in `main`
or other Unit-returning function bodies.

Current gap (2026-03-01): in `crates/goby-wasm`, the `eval_ast_side_effect` AST path does
not route bare `Expr::Call { callee: Expr::Var(..) }` through
`find_handler_method_by_name` / `dispatch_handler_method`, while
`execute_unit_ast_stmt` already does. This causes `using ...` + bare effect calls in
top-level AST side-effect execution to be dropped.

Implementation steps:

1. Unify bare-call dispatch logic for AST side-effect execution.
   - Add a shared helper in `RuntimeOutputResolver` for AST bare calls:
     - resolve qualified effect call (`Effect.method arg`) first,
     - resolve bare effect op name (`method arg`) against active handlers,
     - then fall back to unit-function and declaration side-effect paths.
   - Reuse this helper from both:
     - `eval_ast_side_effect` (main/top-level AST path),
     - `execute_unit_ast_stmt` (Unit function / handler body path).
2. Keep deterministic handler resolution unchanged for MVP.
   - Preserve current `active_handlers: BTreeMap<String, usize>` behavior.
   - Preserve current precedence: qualified lookup first, then bare-name scan.
3. Add targeted regression tests in `crates/goby-wasm/src/lib.rs`:
   - `main` with `using Handler` and direct bare op call prints expected output.
   - Nested `using` contexts still restore previous handler map after block exit.
   - Qualified call (`Effect.method`) still dispatches to the effect-specific handler.
4. Add/adjust example coverage:
   - Extend `examples/effect.gb` with one minimal direct bare call inside `using`, or
     add a dedicated tiny fixture under `examples/` if we want to keep existing output
     lock unchanged.
5. Validation gate before merge:
   - `cargo check`
   - `cargo test`
   - `cargo run -p goby-cli -- run examples/effect.gb` (and any new fixture).

Done criteria:

- Direct bare effect call inside `using` is observable in runtime output.
- Existing `effect.gb` behavior remains stable unless explicitly updated.
- No regression in current 176-test baseline (or newer baseline at implementation time).

## 5. Spec Detail Notes

### Still Open (Post-MVP)

- Effects/handlers: current MVP runtime keeps alphabetical fallback (`BTreeMap`) only as temporary behavior; post-MVP semantics will switch to lexical nearest-handler resolution by compiled `EffectId`/`OpId`.
- Effect namespace rules: qualified vs unqualified calls, unhandled-effect diagnostics format.
- Type annotation placement: where annotations are required vs optional outside current MVP subset.
- Tuple/record roadmap: record update syntax, pattern matching on record fields.
- Import system: filesystem-backed/local package resolution, dependency graph rules.
- Equality/comparison: operator set (`!=`, `<`, `>`, etc.) and type constraints.

## 6. Research References (2026-03-01 survey)

- OCaml manual (effect handlers): one-shot continuations are cheaper than multi-shot and are the default operational model.
  - <https://caml.inria.fr/pub/distrib/ocaml-5.0/ocaml-5.0-refman.html#sec281>
- Retrofitting Effect Handlers onto OCaml (Sivaramakrishnan et al., PLDI 2021): runtime design with fibers and one-shot continuations integrated into a production compiler/runtime.
  - <https://arxiv.org/abs/2104.00250>
- Effect Handlers, Evidently (Xie and Leijen): evidence-passing translation strategy for efficient handlers.
  - <https://arxiv.org/abs/2106.00160>
- WasmFX: Typed Continuations and Stack Switching for WebAssembly (Hillerstrom et al., ICFP 2024): practical path for direct-style effect handlers on Wasm backends.
  - <https://arxiv.org/abs/2403.01036>
