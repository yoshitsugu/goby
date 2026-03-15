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
  - Both inline (`pat -> expr`) and block (`pat ->` + indented statements) arm bodies are supported.
  - Block arm type is the type of the last expression; trailing non-expression statement is rejected.
  - `case` is value-returning in all positions; branch type unification enforced for both `if` and `case`.
  - Effectful expressions inside `case` arm blocks work with parity between `check` and `run`.
  - Follow-up: generalize expression-level block parsing beyond `case` arms.
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
  - `Read` default handler (`read`, `read_line`),
  - `goby/string.graphemes` iterator-backed Unicode grapheme segmentation helper.
- Editor syntax packs (VSCode/Emacs/Vim/TextMate) are implemented.

Note: detailed execution history for these items is retained in git history and
`doc/STATE.md`; this section keeps only decision-level summaries.

### 4.2 Closed Tracks Archive

Tracks A/B/C are closed. Detailed records were removed from this plan file and
are retained in `doc/STATE.md` and git history.

### 4.3 Active Track D: Developer Tooling Foundation

Goal: establish practical developer tooling — diagnostics in editors, a formatter, and a
linter — built on `goby-core` as the single analysis source.

#### Current infrastructure baseline (2026-03-15 audit)

What exists:

- `ParseError` always carries `line`/`col` (1-indexed, ASCII-only MVP assumption).
- `TypecheckError` has an `Option<Span>` field; ~10 of 30+ error sites populate it.
- `Span` is a `{line, col}` struct — no end position, no byte offset from file start.
- `Declaration` carries a start `line` but no column.
- `goby-cli` renders a caret-style snippet for errors that have line/col.
- `Expr::to_str_repr()` exists as a legacy bridge, not a formatter; `Handler`/`With`/`If`/`Case`/`Block` are not covered.

What is missing:

- AST nodes (`Expr`, `Stmt`) carry no span — once parsed, source location is lost.
- Effect/handler-clause declaration sites have no position metadata.
- No end-of-span (single character point only), so range-based LSP highlights are impossible.
- No unified `Diagnostic` type shared between CLI and a future LSP server.
- No UTF-8 byte-offset ↔ LSP line/character conversion layer.
- No AST pretty-printer (needed for `goby fmt`).
- `typecheck_module` returns a single `TypecheckError`, not a collection.

#### Ownership boundaries

- `goby-core` owns: parsing, typechecking, source spans, symbol-resolution facts, and `Diagnostic` structs.
- `goby-lsp` (new crate) owns: LSP protocol transport, document versioning, and protocol object translation.
- `goby-cli` owns: human-readable rendering of `Diagnostic` for terminal output.
- Do not place LSP-specific types or editor state in `goby-core`.

#### Phase D1: Span infrastructure and unified diagnostics

Goal: make `goby-core` emit structured, span-bearing diagnostics consumable by both CLI and LSP.

Scope:

- Extend `Span` to carry an end position: `{ line: usize, col: usize, end_line: usize, end_col: usize }`.
  - Keep the existing `line`/`col` semantics; add `end_line`/`end_col` defaulting to the same point.
- Add span fields to parser AST nodes where source location is needed for diagnostics:
  - `Declaration`: add `col` and `annotation_span: Option<Span>`.
  - `Expr`/`Stmt`: do not add spans to every node in this phase; instead record spans at
    declaration-body boundaries and key error sites only.
- Introduce a `Diagnostic` struct in `goby-core`:
  ```
  Diagnostic { severity: Severity, span: Option<Span>, declaration: Option<String>, message: String }
  ```
  where `Severity` is `Error | Warning`.
- Migrate `TypecheckError` and `ParseError` to produce `Diagnostic` values (or map to them).
- Change `typecheck_module` to return `Vec<Diagnostic>` instead of a single error, so multiple
  errors in one pass can be reported.
- Add a UTF-8 byte-offset conversion helper: `fn line_col_to_offset(source: &str, line: usize, col: usize) -> usize`.
  - Cover multi-byte characters; add tests for ASCII, 2-byte, and 3-byte codepoint cases.
- Update `goby-cli` to render `Vec<Diagnostic>` (the existing caret-snippet logic is reused).

Done when:

- `cargo test` passes.
- `goby-cli check examples/function.gb` still renders correct line/col diagnostics.
- The `Diagnostic` type is exported from `goby-core` and documented.
- Multi-byte offset conversion has unit tests.

#### Phase D2: `goby-lsp` crate — diagnostics only

Goal: `goby-cli check`-equivalent diagnostics available inside editors via LSP.

Scope:

- Add `crates/goby-lsp` to the Cargo workspace.
- Depend on `goby-core`; use `lsp-types` and `lsp-server` crates for protocol plumbing.
- Implement lifecycle: `initialize` / `initialized` / `shutdown` / `exit`.
- Implement `textDocument/didOpen`, `didChange`, `didClose` with in-memory document store.
- On open/change: run `parse_module` + `typecheck_module`; publish `textDocument/publishDiagnostics`
  using spans from Phase D1.
- LSP position mapping: convert `Span` line/col to LSP 0-indexed line/character using the
  byte-offset helper from D1.
- On `didClose`: clear diagnostics for that URI.

Done when:

- `cargo run -p goby-lsp` starts without error and speaks stdio LSP.
- Opening `examples/function.gb` in an editor (or via `lsp-mode` / `nvim-lspconfig`) shows the
  same errors as `goby-cli check`.
- A fixture-based integration test drives open/change/close and asserts published diagnostics.

#### Phase D3: Hover and go-to-definition (single file)

Goal: hover shows type/signature; definition jumps work within a single file and loaded stdlib.

Scope:

- `textDocument/hover`:
  - for top-level declaration names and their call sites: return `name : TypeAnnotation`.
  - for local bindings: return `name : InferredType` using `check_expr` result.
  - for effect operations: return the effect member signature.
  - unresolved symbols: return `null` (no best-effort guessing).
- `textDocument/definition`:
  - for identifiers resolvable within the current file: return the declaration `Span`.
  - for stdlib imports (`import goby/string`): return the span in the loaded stdlib source.
  - unresolved: return `null`.
- Requires: declaration spans in AST (from D1) and a symbol-lookup helper in `goby-core`.

Done when:

- Hovering a top-level function name shows its type annotation.
- Go-to-definition from a call site jumps to the declaration line.
- Hovering an unknown identifier returns no result (not a crash).
- Tests cover: local binding hover, top-level hover, stdlib symbol hover, unresolved hover.

#### Phase D4: `goby fmt` — AST pretty-printer

Goal: deterministic, idempotent source formatter as a `goby fmt` CLI subcommand.

Scope:

- Implement an AST pretty-printer in `goby-core` covering the full current `Expr`/`Stmt` surface:
  - literals, variables, binary ops, calls, lambdas, records, tuples, lists.
  - `if`/`case` expressions with correct indentation.
  - `with`/`handler` expressions.
  - `Block` expressions.
- Style rules (to be locked before coding):
  - 2-space indentation.
  - one blank line between top-level declarations.
  - trailing newline.
  - no trailing whitespace.
- Add `goby fmt <file>` (in-place) and `goby fmt --check <file>` (exit 1 if not formatted).
- Formatter is idempotent: `fmt(fmt(source)) == fmt(source)`.

Done when:

- `goby fmt examples/function.gb` produces a stable output.
- `goby fmt --check` passes on all files under `examples/` and `stdlib/`.
- Round-trip test: `parse(fmt(source))` produces an AST equal to `parse(source)`.

#### Phase D5: `goby lint` — high-signal static checks

Goal: machine-readable linter output for common mistakes not caught by the typechecker.

Scope (initial lint rules):

- Unused local binding (binding `x = expr` where `x` is never referenced afterward).
- Shadowed effect operation name (local binding name collides with a visible effect op).
- `can` annotation lists an effect that is fully discharged inside the function body
  (redundant `can`).
- Unreachable `case` arm (wildcard `_` arm followed by more arms).

Output format:

- JSON lines: `{"severity":"warning","file":"...","line":1,"col":1,"rule":"unused-binding","message":"..."}`.
- Human-readable mode (default): same caret-snippet format as `goby-cli check`.

Done when:

- `goby lint examples/function.gb` exits 0 with no output on clean files.
- Each lint rule has at least one positive and one negative test fixture.
- JSON output mode is parseable and stable.

#### Milestones summary

- M1 (D1 done): diagnostics carry spans; multi-error reporting works; CLI output unchanged.
- M2 (D2 done): editor shows parse/type errors via LSP on `.gb` files.
- M3 (D3 done): hover and go-to-definition work for single-file + stdlib.
- M4 (D4 done): `goby fmt` is idempotent on all existing sources.
- M5 (D5 done): `goby lint` catches the initial rule set with machine-readable output.

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
