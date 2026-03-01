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
- Effects are parse/typecheck metadata in MVP (no effect-safety enforcement).
  - unknown effect names are ignored.
  - `can` clauses must be syntactically valid (non-empty list of identifiers).
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
- Current status (2026-03-01, session 20):
  - **All `examples/*.gb` pass both `check` and `run`.**
  - `function.gb` → `90 / [30, 40, 50] / [60, 70] / something / 15`
  - `type.gb` → `John`
  - `control_flow.gb` → `Five! / 50 / 30`
  - `import.gb` (with `GOBY_PATH=foo,bar`) → `foo / bar`
  - `effect.gb` (with `GOBY_PATH=hello`) → `13 / hello`
  - 147 tests pass; `cargo clippy -- -D warnings` clean.
  - MVP implementation is complete.

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

- Multiple effects in `can` clause are parsed/typechecked as metadata only (no effect-safety enforcement in MVP).
- How to represent multiple effects (`can Print + Read` or other syntax) — deferred.
- Effect propagation rules for higher-order functions — deferred.
- Unhandled-effect diagnostics — deferred.

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

## 5. Example-Driven Feature Checklist

Status (2026-03-01, session 20): **ALL COMPLETE**

- `check` passes: `hello.gb`, `basic_types.gb`, `function.gb`, `generic_types.gb`,
  `control_flow.gb`, `import.gb`, `type.gb`, `effect.gb`.
- `check` fails: none.
- `run` passes (locked output): `function.gb`, `type.gb`, `control_flow.gb`, `import.gb`, `effect.gb`.

### 5.1 Core Syntax/Typing Used by Stable Examples

- [x] Top-level declarations with optional type annotations (`name : Type`, `name = expr`)
- [x] Function types (`A -> B`), including parenthesized function arguments (`(A -> B) -> C`)
- [x] Haskell-style type application in annotations (`List Int`, `TypeX a b`, `TypeX (TypeY a b) c`)
- [x] Integer/string literals, tuple/list literals, local bindings, block-last-value return
- [x] Calls (`f x`, `f(x)`), method call subset (`string.concat(a, b)`), pipeline (`|>`)
- [x] Lambda forms (`|x| -> ...`, `_ * 10`)
- [x] Comments (`#`, line-end comments, shebang-as-comment), mixed tabs/spaces indentation
- [x] MVP diagnostics baseline (non-empty, declaration name when known, expected/actual types)

### 5.2 Runtime/CLI Behaviors Required by Examples

- [x] `check` command (parse + typecheck without runtime entry requirement)
- [x] `run` command with `main` entrypoint only
- [x] `main : Unit -> Unit` enforcement for runnable programs
- [x] Wasm emit + `wasmtime` execution path
- [x] Locked `examples/function.gb` output parity

### 5.3 Features Referenced by Examples

- [x] `import` syntax and minimal module resolution
  (`import goby/x`, alias `as`, selective import `(...)`) for built-in modules
  (`goby/string`, `goby/list`, `goby/env`) used by `examples/import.gb`.
  - collision policy: if colliding names are actually referenced, report an
    unresolved/ambiguous-name error.
- [x] `effect` declarations and effect member signatures
- [x] `handler ... for ...` syntax and handler scope semantics
- [x] `using` handler application syntax (single/multiple handlers, comma-separated)
- [x] `case` expressions with pattern arms and wildcard `_`
  - `CasePattern` supports: `IntLit`, `StringLit`, `BoolLit` (`True`/`False`), `Wildcard`
  - multi-line lookahead parser; arm separator ` -> ` correctly split by `split_case_arm`
- [x] `if ... else ...` expression (indentation-based, two-branch)
- [x] `==` equality operator producing `Bool`
- [x] `type` declarations:
  - alias (`type UserID = String`)
  - union/sum (`type UserStatus = Activated | Deactivated`)
  - record constructor syntax (`type User = User(...)`)
  - qualified constructor/member references (`UserStatus.Activated`, `user.name`)
- [x] Runtime for all examples:
  - `function.gb`: higher-order functions, map, list literals, lambdas
  - `type.gb`: record construction, field access, union constructor reference
  - `control_flow.gb`: case/if/==, Bool runtime value
  - `import.gb`: `fetch_env_var`, `string.split`, `list.join`
  - `effect.gb`: handler dispatch, `Stmt::Using` save/restore, active_handlers BTreeMap
- [ ] Multiple effects in `can` with semantic checking (currently parse/type surface only)
- [ ] Effect-safety / unhandled-effect diagnostics

## 6. Spec Detail Notes

### Resolved in MVP

- Import system: `goby/...` path grammar, built-in module table (`goby/string`, `goby/list`, `goby/env`), alias binding, selective import, collision policy → complete.
- Control flow: multiline `case`/`if` with indentation-based lookahead; `case` uses `split_case_arm` to avoid misparse on lambda bodies; `else if` is unsupported and documented → complete.
- Equality/comparison: `==` produces `Bool`; no other comparison operators in MVP → complete.
- Type declaration system: alias, union, record; constructor/field qualified references → complete.
- Effect/handler: `Stmt::Using` save/install/execute/restore; BTreeMap for deterministic dispatch; handler body cached in `parsed_body` → complete.

### Still Open (Post-MVP)

- Effects/handlers: current MVP runtime keeps alphabetical fallback (`BTreeMap`) only as temporary behavior; post-MVP semantics will switch to lexical nearest-handler resolution by compiled `EffectId`/`OpId`.
- Effect namespace rules: qualified vs unqualified calls, unhandled-effect diagnostics format.
- Type annotation placement: where annotations are required vs optional outside current MVP subset.
- Tuple/record roadmap: record update syntax, pattern matching on record fields.
- Import system: filesystem-backed/local package resolution, dependency graph rules.
- Equality/comparison: operator set (`!=`, `<`, `>`, etc.) and type constraints.

## 7. Completed Slices

### Slice: `import.gb` check (completed 2026-02-28)

- Parser/AST import support (`plain`, `as`, `selective`).
- Minimal built-in resolver for `goby/string`, `goby/list`, `goby/env`.
- Typecheck integration for imported names.
- Import-collision policy (error when ambiguous name is used).

### Slice: `type.gb` check (completed 2026-02-28)

- `type` declaration parsing and AST nodes (alias, union, record).
- Constructor/field typecheck integration.
- Qualified constructor/member references.

### Slice: `effect.gb` check (completed 2026-02-28, commit ddbf19e)

- `effect`/`handler` top-level block parsing and AST nodes.
- `Stmt::Using` with indentation-aware body parsing.
- Effect member registration in type env (qualified and bare keys).
- Relaxed `main` annotation requirement (`check` no longer requires it).
- `parses_effect_gb_declarations` regression test.

### Slice: `type.gb` runtime (completed 2026-02-28, commit 7962891)

- `RuntimeValue::Record { constructor, fields }` variant.
- `Expr::RecordConstruct` and `Expr::Qualified` evaluation.
- `run examples/type.gb` outputs `John`.

### Slice: all remaining runtime (completed 2026-03-01, commit c8e669b)

- `control_flow.gb`: `CasePattern`/`CaseArm`/`Expr::Case`/`Expr::If`, multi-line lookahead parser, `unescape_string`, `RuntimeValue::Bool`, `BinOpKind::Eq` eval.
- `import.gb`: `fetch_env_var`, `string.split` → `ListString`, `.join` → `String`.
- `effect.gb`: `active_handlers: BTreeMap`, `Stmt::Using` save/install/execute/restore, handler dispatch helpers.

### Slice: Codex review fixes (completed 2026-03-01, session 20)

- H1: removed dead `matches!(fn_name.as_str(), _)` guard.
- H2: `ENV_MUTEX` for env-var test serialization; `remove_var` before assert.
- H3: `HandlerMethod.parsed_body` pre-parsed at parse time (no per-dispatch re-parse).
- M1: `split_case_arm` replaces `split_once(" -> ")` (safe for lambda bodies).
- M2: `active_handlers: HashMap` → `BTreeMap`; removed unnecessary `Vec<usize>` collect.
- M3: `arg_val.to_expression_text()` computed only in string-fallback branch.
- L1: `CasePattern::BoolLit(bool)` for `True`/`False` patterns.
- L2: `Stmt::Using` fully handled in `dispatch_handler_method_as_value`.
- Bonus: bare handler value dispatch in `eval_expr_ast` for non-Int/non-List args.
- 4 new regression tests covering review-flagged gaps.

## 8. Research References (2026-03-01 survey)

- OCaml manual (effect handlers): one-shot continuations are cheaper than multi-shot and are the default operational model.
  - <https://caml.inria.fr/pub/distrib/ocaml-5.0/ocaml-5.0-refman.html#sec281>
- Retrofitting Effect Handlers onto OCaml (Sivaramakrishnan et al., PLDI 2021): runtime design with fibers and one-shot continuations integrated into a production compiler/runtime.
  - <https://arxiv.org/abs/2104.00250>
- Effect Handlers, Evidently (Xie and Leijen): evidence-passing translation strategy for efficient handlers.
  - <https://arxiv.org/abs/2106.00160>
- WasmFX: Typed Continuations and Stack Switching for WebAssembly (Hillerstrom et al., ICFP 2024): practical path for direct-style effect handlers on Wasm backends.
  - <https://arxiv.org/abs/2403.01036>
