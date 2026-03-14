# Goby Language Plan (Draft)

This document tracks:

- what is already visible from examples,
- what must be fixed for MVP,
- what can be postponed.

Note:

- `doc/LANGUAGE_SPEC.md` is the source of truth for current language
  syntax/semantics.
- This `PLAN.md` file is for roadmap/migration/execution planning.
- Workflow rule: when language syntax/semantics change, update
  `doc/LANGUAGE_SPEC.md` in the same change.
- Workflow rule: when language syntax changes, also verify whether syntax
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
- Handler dispatch: lexical nearest-handler stack walk (no alphabetical fallback).
- Runtime execution model: prefer native lowering for the supported subset; fallback to
  compile-time interpreter (`resolve_main_runtime_output`) for unsupported forms.

### 2.1 Syntax and Parsing

- **String interpolation `${}`** (implemented).
  - Parser supports `${ expr }` inside string literals and lowers it to `Expr::InterpolatedString`.
  - Typechecker treats interpolated literals as `String`.
  - Runtime/codegen evaluates each segment and stringifies embedded expression values.
- **List `case` patterns** (implemented).
  - Parser/AST support includes prefix, head-literal, wildcard, and tail-binding forms.
  - List item literals in MVP are `Int` / `String` only (no `Bool` list-pattern items).
  - Parser rejects malformed forms and duplicate binders.
- **`case` arm block body** (implemented, 2026-03-05).
  - Goal:
    - support both inline arm bodies and indented block arm bodies.
    - inline (current): `pat -> expr`
    - block (new): `pat ->` then deeper-indented statements, with last expression as arm result.
  - Definition of Done (MVP for this item):
    - `goby-cli check` accepts `case` with mixed inline/block arms.
    - `goby-cli run` executes selected arm block correctly (binding order + last expression value).
    - behavior is consistent between native-lowered and fallback-executed paths.
  - Proposed syntax shape:
    - `case x`
    - `  0 ->`
    - `    y = 1`
    - `    y + 10`
    - `  _ -> 0`
  - Implemented scope:
    - parser accepts both inline and block arm bodies.
    - block arm body is represented by expression-level block AST.
    - typecheck enforces that a block arm ends with an expression.
    - runtime supports evaluating selected arm block and returning tail-expression value.
  - Follow-up scope:
    - generalize expression-level block parsing beyond `case` arms (currently case-arm focused).
  - Known remaining gaps (to be implemented):
    - (done 2026-03-05) `run` now supports effectful expressions inside `case` arm blocks (`print`, effect ops, calls requiring `can ...`) and preserves parity with `check` for covered patterns.
    - in arm-block effect resolution, use the same lexical outer-scope search flow as normal expression evaluation:
      - unresolved effects must be searched in outer scopes.
      - if still unresolved at `main`, keep existing behavior: allow only embedded/builtin runtime-backed effects, and report missing handler error for others.
    - (done 2026-03-05) make `case` consistently value-returning in all positions (including bindings like `x = case ...`), not only direct tail-expression positions.
    - (done 2026-03-05) enforce branch result type unification for both `if` and `case`:
      - all branches must resolve to one compatible result type.
      - type inference + type checking now carry this constraint and report mismatch diagnostics.
  - Phase notes (kept for maintenance):
  - Phase 1 (AST/parser):
    - introduce an explicit expression-level block representation (for example `Expr::Block(Vec<Stmt>)`)
      so arm blocks are first-class and reusable in other expression positions later.
    - scope guard:
      - in this milestone, parser constructs `Expr::Block` only for `case` arm bodies.
      - reuse in other expression positions is handled by follow-up tasks.
    - extend `parse_multiline_expr` (`case` arm path):
      - keep accepting `pattern -> <expr>` inline form.
      - add `pattern ->` form that consumes a deeper-indented statement block.
    - error rules:
      - reject empty arm blocks.
      - reject non-indented lines immediately after `pattern ->`.
      - arm block may include blank/comment-only lines; they do not satisfy non-empty block requirement by themselves.
      - dedent closes current arm block; next sibling arm is parsed at case-arm indent level.
      - preserve existing malformed-pattern diagnostics.
  - Phase 2 (typing/analysis):
    - typecheck block expressions with existing local-binding rules:
      - each statement extends local env in order.
      - block type is the type of the last expression statement.
      - if block tail is not an expression statement (for example trailing binding), report type error.
    - ensure existing case-arm checks still apply:
      - list-pattern binding environment extension.
      - arm result compatibility checks.
      - effect/resume/intrinsic checks traverse block expressions.
  - Phase 3 (runtime/codegen):
    - fallback evaluator: execute arm block statements sequentially and return last expression value.
    - Wasm lowering:
      - either lower block expressions directly, or conservatively route such `case` expressions
        to fallback until native lowering is implemented.
      - keep behavior parity between native and fallback paths.
  - Phase 4 (tests/examples/docs):
    - parser tests:
      - parse mixed inline/block arms in one `case`.
      - reject malformed/empty arm block shapes.
      - accept blank/comment lines inside arm block while still rejecting effectively empty block.
    - typechecker tests:
      - arm-local bindings are visible only inside that arm block.
      - arm block result type participates in arm type checks.
      - trailing non-expression statement in arm block is rejected.
    - runtime tests:
      - selected arm block executes in order and returns last expression.
      - add CLI integration checks for both `check` and `run` paths with arm-block source.
    - examples/spec sync:
      - add one canonical example under `examples/`.
      - update `doc/LANGUAGE_SPEC.md` once implementation is merged.
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

### 2.3 Effect System

- Current implemented checks:
  - `can` effect names must be declared (or built-in).
  - effect member signatures can declare dependency effects (`op : ... can Dep`), and
    handler clause bodies are validated against those declared dependencies.
  - dependency cycles in effect-member `can` declarations are rejected at typecheck time.
  - uncovered effect operation calls are rejected unless covered by enclosing handler scope
    (`with`).
  - calls to `can`-annotated functions require an appropriate enclosing handler scope.
- Current runtime behavior:
  - effect operations dispatch through installed handlers.
  - bare-name dispatch falls back to deterministic effect-name order (temporary MVP behavior).
- How to represent multiple effects (`can Print + Read` or other syntax) — deferred.
- Effect propagation rules for higher-order functions — deferred.
- Effect diagnostics UX polish (wording/format consistency) — deferred.
- Warning mechanism for lexical shadowing of visible effect operation names
  (for example local `a` shadows operation `a`) — deferred.
  - resolution rule remains: lexical value namespace wins.
  - warning is planned as tooling/diagnostics improvement, not a type error.
- Multi-effect implicit `main` wrapper ordering and topological expansion based on
  effect-member dependency declarations (`op ... can Dep`) — in progress.


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

#### Planned Syntax Simplification: `with` Unification

Status: completed (2026-03-05)

Goal: unify handler application syntax on `with` only.

- Implemented summary:
  - canonical syntax is `with` only, for both inline handlers and handler-value form.
  - multiline RHS parsing supports `with` in bindings/assignments.
  - legacy inline-handler syntax is removed and diagnostics now point only to `with`.
  - docs, examples, tests, and quality gates were completed together as one migration slice.

Note: detailed step-by-step renewal history is intentionally omitted here; use
`doc/STATE.md` and git history for chronological implementation records.


### 2.4 Standard Library Surface (MVP)

- Core modules to ship first (`Int`, `String`, `List`, `Env`) — minimal built-ins implemented.
- Naming conventions for stdlib functions — established.
- Minimal collection API for immutable workflows — deferred.
- `List.map` migration plan (planned):
  - keep canonical map behavior in `stdlib/goby/list.gb` (`list.map` export path).
  - replace internal/builtin-path map callsites with stdlib module usage where possible.
  - after migration, trim builtin-only `map` special handling so runtime/compiler has a single semantics source.

### 2.5 Runtime / Compiler Scope (MVP)

- Current model: native Wasm code generation for the supported subset, with deterministic
  fallback behavior for unsupported forms.
- Real Wasm migration plan (Phase 0-8) is completed; implementation record is archived.
- Error location strategy (line/column reporting) — deferred.

### 2.6 Tooling Scope (MVP)

- Minimum CLI commands (`goby-cli run`, `goby-cli check`) — complete.
- Project layout and package metadata format (Cargo workspace with `goby-core`, `goby-cli`, `goby-wasm`) — complete.
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

Post-MVP focus is to reduce fallback-runtime special-cases and make execution paths
predictable.

### 4.1 Completed Work (Summary)

- Effect call dispatch in `main` body (`bare` / `qualified` / `pipeline`) is implemented.
- Mutable local syntax (`mut` / `:=`) is implemented with parser/typecheck/runtime coverage.
- Wasm track refactor split (`call.rs`, `support.rs`) and `function.gb` native HOF subset support are implemented.
- Stdlib foundation milestones already merged:
  - stdlib source loading from `stdlib/goby/*`,
  - stdlib-only `@embed` model,
  - implicit `goby/prelude`,
  - Unit value `()` path,
  - `Print` split (`print` / `println`),
  - `Read` default handler (`read`, `read_line`).
- Editor syntax packs (VSCode/Emacs/Vim/TextMate) are implemented.

Note: detailed execution history for these items is retained in git history and
`doc/STATE.md`; this section keeps only decision-level summaries.

### 4.2 Closed Tracks Archive

Tracks A/B/C are closed. Detailed records were removed from this plan file and
are retained in `doc/STATE.md` and git history.

### 4.3 Active Track D: Developer Tooling Foundation

Goal: establish practical developer tooling aligned with current language behavior.

Near-term scope:

1. `goby fmt` / `goby fmt --check` (deterministic formatting).
2. `goby lint` (high-signal checks + machine-readable output).
3. `goby-lsp` MVP (diagnostics, hover, definition).
4. Cross-editor syntax regression tests for existing highlight packs.

Acceptance criteria:

- Formatter/linter commands and baseline LSP diagnostics are usable on `examples/` and stdlib sources.

#### `goby-lsp` implementation plan

Purpose:

- make compiler diagnostics continuously available from editors without introducing a second analysis stack.
- establish a minimal but extensible protocol boundary for later editor features (rename, references, completion).

Non-goals for MVP:

- project-wide symbol index across packages/workspaces.
- semantic tokens, completion ranking, code actions, rename, references.
- background build orchestration beyond single-file + directly loaded stdlib context.

Architecture direction:

- add a new workspace crate `crates/goby-lsp`.
- reuse parser/typechecker/diagnostic machinery from `goby-core` as the single source of truth.
- keep LSP-specific concerns isolated to:
  - document lifecycle and text synchronization,
  - span/offset conversion,
  - protocol capability wiring,
  - lightweight symbol lookup for hover/definition.
- do not fork diagnostic formatting rules between CLI and LSP; instead introduce a shared structured-diagnostic layer if current CLI output is too string-oriented.

Execution phases:

1. Phase D1: Diagnostic substrate hardening
   - audit current parse/typecheck/runtime-facing errors and normalize them into machine-readable diagnostics with stable severity, message, and source span.
   - close the known metadata gap for type/effect declaration spans so editor diagnostics can point at declaration sites.
   - define UTF-8 byte offset to LSP position conversion rules and cover multi-line / multi-byte text cases with tests.
2. Phase D2: `goby-lsp` crate skeleton
   - add `crates/goby-lsp` to the workspace with a stdio LSP server entrypoint.
   - implement initialize / initialized / shutdown / exit.
   - implement `textDocument/didOpen`, `didChange`, `didClose` with in-memory document storage.
   - on open/change, re-run parse + typecheck for the active document and publish diagnostics.
3. Phase D3: Single-file language intelligence MVP
   - implement `textDocument/hover` for local bindings, top-level declarations, built-ins, and stdlib symbols that resolve through the current compilation context.
   - implement `textDocument/definition` for identifiers resolvable within the current file or loaded stdlib source set.
   - define clear fallback behavior: unresolved symbols return `null`, never best-effort guesses.
4. Phase D4: Editor-facing hardening
   - debounce or coalesce rapid document changes enough to avoid obviously redundant full recompilations.
   - ensure diagnostics are cleared on close and replaced atomically on re-check.
   - add fixture-based regression tests for malformed syntax, type errors, stdlib references, and hover/definition span accuracy.
5. Phase D5: CLI/editor packaging alignment
   - decide whether the long-term entrypoint is a standalone `goby-lsp` binary, a `goby lsp` subcommand, or both.
   - document editor launch examples after protocol stability is proven.

Ownership boundaries:

- `goby-core` owns parsing, typing, source spans, symbol-resolution facts, and diagnostic data structures.
- `goby-lsp` owns protocol transport, document versioning, publish-diagnostics flow, and protocol object translation.
- avoid placing editor-only state or protocol types in `goby-core`.

Suggested internal milestones:

- M1: opening a `.gb` file in an editor yields parse/type diagnostics identical in substance to `goby-cli check`.
- M2: hover returns type/signature information for local/top-level names.
- M3: definition jumps work for same-file declarations and stdlib imports used by examples.
- M4: the server is stable enough to wire into the existing VSCode extension or a minimal editor launch config.

Definition of done for Track D LSP slice:

- `cargo run -p goby-lsp` starts a functional stdio server.
- opening/changing `examples/*.gb` publishes stable diagnostics with correct line/column mapping.
- hover/definition work for the supported single-file + stdlib-backed subset.
- regression tests cover offset conversion, diagnostic publication, and at least one hover/definition case.
- remaining post-MVP gaps are documented explicitly before moving on to richer editor features.

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

### 4.5 Active Track E: `Float` / Wasm `f64` Support

Goal: add a first-class `Float` type with predictable parser/typechecker/runtime/Wasm behavior.

Why this is a separate track:

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

### 4.6 Active Track F: Runtime I/O Correctness and Fallback Containment

Goal: stop compile-time host I/O from leaking into `goby run`, then converge on a
unified runtime-I/O design for effect-boundary programs instead of growing ad-hoc
fallback behavior indefinitely.

Problem statement locked from investigation:

- current `goby run` may execute `main` through `resolve_main_runtime_output` during
  compilation when native lowering cannot handle the program shape.
- if that fallback path succeeds, the compiler collapses the observed output into a
  static print-only Wasm module.
- this is acceptable only for pure/output-only fallback cases.
- it is incorrect for host-input-dependent effects such as `Read`, because the compiler
  process consumes stdin before the generated Wasm runs.

Observed affected surface from pre-plan audit:

- `Print` and `Read` are the only currently registered embedded runtime handler kinds.
- `Print` in fallback is currently a materialization mechanism for static output and is
  not itself evidence of incorrect behavior.
- `Read.read` / `Read.read_line` are unsafe in compile-time fallback because they call
  host stdin directly from the compiler-side runtime environment.
- the broader architectural risk is "any future embedded or host-backed effect that
  depends on runtime environment must not be resolved into static output during compile".

Execution policy for this track:

- prefer a two-stage rollout:
  1. immediate containment of the incorrect compile-time stdin behavior.
  2. real Wasm/WASI runtime support for stdin-backed execution.
- keep fallback evaluation available for pure/static-output programs that do not depend
  on runtime host input.
- make host-dependence an explicit planning/runtime property rather than an accidental
  side effect of whatever the fallback interpreter can execute.
- treat the current shape-based dynamic Wasm support as a temporary coverage bridge, not
  as the final architectural boundary.

Target architecture for the remainder of this track:

- introduce a small internal runtime-I/O planning layer that classifies effect-boundary
  `main` into one of:
  - `StaticOutput`,
  - `DynamicWasiIo`,
  - `InterpreterBridge`,
  - `Unsupported`.
- for this project direction, `InterpreterBridge` is an explicitly temporary migration
  mode, not a desired steady-state execution target:
  - it exists only to keep `goby run` usable while Wasm-only runtime coverage is being
    expanded,
  - new work should prefer shrinking the `InterpreterBridge` surface rather than
    normalizing it as a permanent parallel runtime.
- keep ownership boundaries explicit:
  - planning layer decides which runtime-I/O program shape is present and which execution
    path is allowed,
  - Wasm backend lowers `DynamicWasiIo` plans,
  - CLI/runtime entrypoints only execute the chosen path and do not rediscover policy.
- keep the current exact-shape dynamic Wasm support as a temporary bridge with an exit
  condition:
  - only add new supported forms if they map cleanly into the planning layer,
  - stop extending ad-hoc AST matching once the planning layer can express the same cases
    more directly.

Execution status and remaining work:

Completed:

- [x] Phase F1 containment: classify `Read` as runtime-host-dependent and block
  compile-time fallback from consuming compiler-process stdin.
- [x] Add containment regressions proving that piping input into `goby run` no longer
  causes compile-time stdin capture.
- [x] Phase F2 initial split: separate compile-time static-output synthesis from
  runtime stdin-backed execution by introducing an interpreter-backed runtime bridge
  for `goby run`.
- [x] Audit current embedded handler surface enough to lock that `Print` is
  static-output-safe while `Read.read` / `Read.read_line` are not.
- [x] Phase F3 initial dynamic Wasm slice: support executable WASI modules for
  simple `Read.read ()` / `Read.read_line ()` echo-style shapes.
- [x] Extend the first dynamic stdin/stdout Wasm path to the current structured
  `split(..., "\n")` + `each println` family in a narrow set of local-binding forms.

Remaining:

- [x] Phase F2b: Runtime-I/O planning unification
  - introduce a small internal `RuntimeIoPlan` layer so lowering can
    distinguish:
    - static-print collapsible programs,
    - programs requiring dynamic stdin/stdout Wasm,
    - programs that still require interpreter-backed runtime bridging,
    - unsupported effect/runtime combinations.
  - move host-dependence and execution policy into that planning layer rather than
    rediscovering it in CLI or backend glue.
  - audit existing decision points (`try_emit_native_module_with_handoff`,
    `resolve_main_runtime_output`, embedded handler metadata, CLI `run` fallback
    routing) so the same rule is applied consistently without string-matching on
    diagnostics.
- [x] Phase F2c: Runtime bridge boundary decision
  - decided: `execute_module_with_stdin` stays `pub` (CLI needs it across crate boundary)
    but is doc-commented as "temporary bridge, CLI-only, marked for shrinkage".
  - `classify_runtime_io` now has explicit doc comment documenting the boundary of all 5
    variants (`DynamicWasiIo`, `StaticOutput`, `InterpreterBridge`, `Unsupported`, `NotRuntimeIo`).
  - integration tests added: `execute_module_with_stdin` rejects `StaticOutput` and `NotRuntimeIo` programs.
  - `Unsupported` assignment in `classify_runtime_io` deferred to F3b (interpreter bridge currently
    handles all Read programs; boundary assignment requires F3b expansion work).
- [x] Phase F3a: Temporary coverage bridge on top of the planning layer
  - all runtime-I/O AST-pattern matching was moved to `runtime_io_plan.rs` in prior commits;
    `compile_module` in `lib.rs` contains no direct AST-shape conditionals.
  - stopping rule documented in `classify_runtime_io` doc comment: new shapes must extend
    `RuntimeIoPlan` / `plan_runtime_io`, not add inline conditionals in call sites.
  - exit condition met: `DynamicWasiIo` shapes are routed through `RuntimeIoPlan::emit_wasm`;
    round-trip classification + Wasm tests added covering all four Echo combinations and SplitLinesEach.
- [x] Phase F3b: General lowering expansion
  - extend `RuntimeIoPlan` so it can represent more runtime-I/O programs without
    planner-bypassing matcher growth.
  - use that expanded plan to cover additional `Read.read ()` and `Read.read_line ()`
    programs that preserve current runtime semantics, including nearby structured
    transforms that are currently forced onto the bridge.
  - if this phase reveals that broad support is better served by a more general lowering
    path than by additional pattern coverage, prefer expanding the planning IR/backend
    rather than adding more matcher cases.
  - completed slice summary:
    - echo-path dynamic lowering now covers trailing static literal suffixes and local
      output-function aliases (`print` / `println`, including simple forwarded aliases).
    - `classify_runtime_io` now assigns `Unsupported` to runtime-read programs that match
      neither a `DynamicWasiIo` plan nor the intentionally narrow temporary bridge subset.
    - the temporary interpreter bridge is now explicitly reserved for the remaining
      transformed `read + split(..., "\n") + each ...` callback family while broader
      dynamic lowering remains future work.
- [x] Phase F3c: Backend ownership cleanup
  - reduce duplication in the WASI backend by extracting common stdin/stdout module
    building blocks.
  - separate concerns so:
    - planning decides what runtime-I/O program exists,
    - backend lowers that plan to Wasm,
    - CLI only selects and executes the resulting path.
  - completed slice summary:
    - shared runtime-I/O memory planning and WASI module skeleton setup now live in
      backend-owned helpers instead of being open-coded in each lowering entrypoint.
    - echo/read-line/split-lines lowering paths now reuse shared stdin iovec setup,
      fd_read/fd_write helpers, newline emission, and static suffix emission helpers.
    - ownership boundary is clearer: `runtime_io_plan.rs` selects shapes, `backend.rs`
      owns reusable Wasm building blocks, and callers remain policy-free.
- [ ] Track explicit shrinkage of temporary modes
  - add milestone checks showing that `InterpreterBridge` coverage is shrinking as
    `DynamicWasiIo` support grows.
  - keep at least one test fixture intentionally on the bridge until a matching Wasm
    lowering exists, then move it across rather than leaving both paths in place.
- [ ] Preserve and document the execution contract that effect-boundary programs using
  embedded `Print` / `Read` should prefer executable Wasm modules with runtime imports,
  not `compile_print_module` output, whenever a dynamic Wasm lowering exists.
- [ ] Keep the minimum supported runtime assumption explicit:
  initial target remains `wasmtime run` / WASI Preview 1 unless and until the backend
  runtime contract changes.
- [ ] Phase F4 deterministic tests:
  - capability/planning decisions that reject compile-time fallback for `Read`,
  - stdin buffering and line semantics for runtime `Read.read` / `Read.read_line`,
  - dynamic effect-boundary codegen selection versus static print-module collapse,
  - boundary tests proving which nearby shapes do and do not compile to dynamic Wasm,
  - planning-layer tests proving how a program is classified into `StaticOutput`,
    `DynamicWasiIo`, `InterpreterBridge`, or `Unsupported`.
- [ ] Phase F4 CLI integration tests:
  - `echo "a\nb" | goby run ...` with `Read.read ()`,
  - mixed `read_line` then `read`,
  - empty stdin,
  - repeated reads after exhaustion,
  - programs combining `Read` and `Print`,
  - at least one shape that intentionally remains on the interpreter-backed runtime path.
- [ ] Treat CLI pipe tests as end-to-end coverage only; keep lower-level deterministic
  tests as the primary proof of correctness so Track F does not depend solely on
  `wasmtime` availability.
- [ ] Sweep for other compile-time fallback call sites or future effect hooks that could
  accidentally observe host environment during compilation; encode findings as tests or
  explicit TODOs before closing the track.
- [ ] Phase F5 docs and cleanup:
  - narrow or remove compiler-side stdin-reading helpers once the final runtime-I/O
    ownership boundary is in place,
  - update `doc/LANGUAGE_SPEC.md` once stdin runtime semantics and guarantees are stable,
  - update runnable stdin examples such as `examples/read.gb`,
  - document whether interpreter bridging remains part of the supported internal design
    or only a temporary transition mechanism.

Acceptance criteria:

- `goby run` never consumes stdin during compilation for programs that require runtime input.
- before a matching dynamic Wasm lowering exists, such programs must not silently
  produce a static-output Wasm artifact from compile-time stdin capture.
- `Read.read ()` and `Read.read_line ()` observe the stdin of the executed Wasm program,
  not the stdin of the compiler process.
- effect-boundary programs using embedded `Read` / `Print` are classified explicitly into
  `StaticOutput`, `DynamicWasiIo`, `InterpreterBridge`, or `Unsupported` rather than
  being routed by accidental fallback behavior.
- programs classified as `DynamicWasiIo` execute as dynamic Wasm modules rather than
  being routed through compile-time text collapse.
- `InterpreterBridge` remains a bounded temporary migration mode whose supported surface
  shrinks over time rather than a second permanent runtime path.
- planner-bypassing direct matcher routing is removed once `RuntimeIoPlan` covers the
  currently supported dynamic-Wasm cases.
- existing static-output fallback remains available for pure/output-only programs where it
  is semantically valid.
- the set of host-dependent embedded effects is explicitly audited, with either tests or
  recorded follow-up items preventing this class of bug from reappearing silently.

Non-goals for the first slice of this track:

- general host capability/FFI design beyond current embedded `Print` / `Read` defaults.
- broad replacement of all fallback execution with native Wasm lowering.
- redesign of effect syntax or handler semantics.

### 4.7 Parking Lot (Needs Revalidation Before Implementation)

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
