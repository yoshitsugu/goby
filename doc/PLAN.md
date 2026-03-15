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

#### Phase D1a: Source coordinates

Goal: establish a correct, well-specified text-position layer that all later phases depend on.

Split into three sub-phases to keep each step reviewable and independently testable.

##### D1a-i: Span extension and position helpers (completed 2026-03-15)

- `Span` extended with `end_line`/`end_col`; `Copy` derived.
- `Span::point()` and `Span::new()` constructors added.
- All 10 construction sites migrated to `Span::point()`.
- `span.rs` module added with `line_col_to_offset` / `offset_to_line_col` (LF-only, 1-indexed byte-offset cols).
- 21 unit tests (ASCII, multi-byte UTF-8, edge cases, round-trip).

##### D1a-ii: Declaration-level AST node spans

Scope:

- `Declaration`: add `col: usize` and `annotation_span: Option<Span>`.
- `EffectDecl` and `EffectMember`: add `span: Span`.
- `HandlerClause`: add `span: Span`.
- `CaseArm`: add `span: Span`.
- Populate the new span fields in the parser at these sites.
- Add corpus tests asserting span fields are correctly populated for each node type.

Done when:

- `cargo test` passes including new span-population tests for declaration-level nodes.
- `goby-cli check examples/function.gb` output is unchanged.

##### D1a-iii: Stmt/Expr identifier node spans

Scope:

- `Stmt` variants (`Binding`, `MutBinding`, `Assign`, `Expr`): add `span: Option<Span>`.
- `Expr` identifier/call-site forms (`Var`, `Qualified`, `Call`): add `span: Option<Span>`.
- Other `Expr` variants: leave spanless for now.
- Populate the new span fields in the parser at the sites above.
- Add corpus tests for Stmt/Expr span population.

Done when:

- `cargo test` passes including new span-population tests for Stmt/Expr nodes.
- `goby-cli check examples/function.gb` output is unchanged.

#### Phase D1b: Unified diagnostics

Goal: introduce a shared `Diagnostic` type and route both CLI and future LSP through it.
Keep fail-fast behavior; do not attempt multi-error collection yet.

Scope:

- Add `goby-core/src/diagnostic.rs` with:
  ```rust
  pub enum Severity { Error, Warning }
  pub struct Diagnostic {
      pub severity: Severity,
      pub span: Option<Span>,
      pub declaration: Option<String>,
      pub message: String,
  }
  ```
- Add `impl From<TypecheckError> for Diagnostic` and `impl From<ParseError> for Diagnostic`.
- Export `Diagnostic` and `Severity` from `goby-core/src/lib.rs`.
- Update `goby-cli` to render `Diagnostic` through a single formatting path, eliminating the
  current parse-vs-typecheck format divergence (noted as a TODO in `main.rs`).
  - No visual change to output; existing caret-snippet logic is reused.
- Golden tests: assert that `goby-cli check` produces byte-for-byte identical messages for
  parse errors and typecheck errors before and after this change.
- Regression tests for the "unknown column" sentinel (`col = 1`) path to guard existing CLI
  behavior for errors where span is absent.
- **Explicitly deferred**: changing `typecheck_module` return type to `Vec<Diagnostic>` and
  multi-error collection — these require a control-flow refactor and belong in a later step.

Done when:

- `cargo test` passes; no CLI output regression.
- `Diagnostic` is exported and documented.
- Golden tests lock the current CLI error format.

#### Phase D1c: TypecheckError span population

Goal: improve diagnostic source-location coverage by populating `span: Some(...)` at the
remaining ~77 TypecheckError construction sites that currently emit `span: None`.

Scope:

- Audit all `TypecheckError { ... span: None ... }` sites in `typecheck*.rs` modules.
- For each site, determine the nearest AST node with a span (from D1a-ii/iii) and wire it through.
- Priority order: sites reachable from common user errors first (type mismatch, unresolved name,
  effect errors), then rarer internal errors.
- Sites where no span is available (e.g. module-level structural errors) may remain `span: None`
  with a `// no span available: <reason>` comment.
- Add tests verifying that representative error cases now carry non-None spans.

Done when:

- At least 80% of TypecheckError construction sites populate `span: Some(...)`.
- Remaining `span: None` sites are documented with reason comments.
- `cargo test` passes; no CLI output regression.

#### Phase D2a: `goby-lsp` crate — diagnostics only

Goal: `goby-cli check`-equivalent diagnostics available inside editors via LSP.

Scope:

- Add `crates/goby-lsp` to the Cargo workspace.
- Depend on `goby-core`; use `lsp-types` and `lsp-server` crates for protocol plumbing.
- Implement lifecycle: `initialize` / `initialized` / `shutdown` / `exit`.
- Implement `textDocument/didOpen`, `didChange`, `didClose` with in-memory document store.
- On open/change: run `parse_module` + `typecheck_module`; publish `textDocument/publishDiagnostics`
  using spans from D1a and the `Diagnostic` type from D1b.
- On `didSave`: re-run diagnostics (same as didChange path; ensures saved-file state is checked).
- LSP position mapping: convert `Span` line/col (byte offset) to LSP 0-indexed line/character
  using `line_col_to_offset` from D1a; document that LSP `character` is UTF-16 code unit index
  and handle the conversion explicitly.
- On `didClose`: clear diagnostics for that URI.
- Re-analysis debounce: defer re-analysis until the editor has been idle for at least 200ms
  after the last `didChange` to avoid redundant work on rapid keystrokes.
- Stdlib change handling: for MVP, stdlib sources are loaded once at server startup;
  stdlib edits require LSP server restart (document this limitation explicitly).
- Integration test: fixture-based driver sends open/change/close sequences and asserts that
  published diagnostic messages and ranges match `goby-cli check` output for the same source.
  - Golden test: CLI diagnostic text == LSP diagnostic message for the same error.

Done when:

- `cargo run -p goby-lsp` starts a functional stdio LSP server.
- Opening `examples/function.gb` in an editor shows the same errors as `goby-cli check`.
- Fixture integration test passes.

#### Phase D2b: Multi-error collection

Goal: report multiple diagnostics per file instead of stopping at the first error.
This is critical for LSP usability — users expect to see all errors at once.

Scope:

- Change `typecheck_module` return type from `Result<(), TypecheckError>` to
  `Result<(), Vec<TypecheckError>>` (or introduce a parallel `typecheck_module_collect`
  entrypoint to avoid breaking existing callers in a single step).
- Modify the typechecker to continue checking subsequent declarations after an error in one
  declaration, collecting errors into a `Vec`.
  - Intra-declaration recovery is NOT required in this phase; a declaration that fails still
    stops checking within that declaration's body.
- Update `goby-cli` to render all collected diagnostics, not just the first.
- Update `goby-lsp` to publish all collected diagnostics per file.
- Add tests: file with errors in two independent declarations produces two diagnostics.

Done when:

- A file with errors in multiple declarations shows all errors in both CLI and LSP.
- `cargo test` passes; no regression on single-error cases.

#### Phase D3a: Symbol index and top-level hover/definition

Goal: hover and go-to-definition for top-level declarations and effect members.
Defers local bindings and stdlib jumps until expression spans are proven.

Scope:

- Add a `SymbolIndex` type in `goby-core` built from the typechecked module:
  - maps top-level declaration names → `(Span, type_annotation: Option<String>)`.
  - maps effect declaration names and their members → `(Span, signature: String)`.
- Add `fn build_symbol_index(module: &Module) -> SymbolIndex` using spans from D1a.
- Expose `SymbolIndex` from `goby-core`.
- In `goby-lsp`, implement `textDocument/hover` for top-level names:
  - cursor on a top-level function name or its call site → return `name : TypeAnnotation`.
  - cursor on an effect operation name → return the member signature.
  - unresolved or out-of-index → return `null`.
- Implement `textDocument/definition` for top-level names:
  - identifier within the current file → return the declaration `Span`.
  - unresolved → return `null`.
  - stdlib imports and local bindings: explicitly return `null` (documented as deferred).
- Tests: hover top-level function, hover effect member, definition within file,
  hover unknown name returns null.

Done when:

- Hovering a top-level function name shows its type annotation.
- Definition jump lands on the correct declaration line.
- Unresolved hover/definition returns null without a crash.

#### Phase D3b: Local binding hover and stdlib definition

Goal: extend hover/definition to local bindings and imported stdlib symbols.
Depends on `Expr`/`Stmt` spans being populated (D1a) and stdlib source locations being
available from `goby-core`.

Scope:

- `textDocument/hover` for local bindings:
  - cursor on a `let`/`mut` binding name or its use site → return inferred type via `check_expr`.
  - requires `Var` span from D1a to map cursor position to an identifier.
- `textDocument/definition` for stdlib imports:
  - cursor on an imported symbol → return the span in the loaded stdlib source file.
  - requires `goby-core` to expose the loaded stdlib source path and declaration spans.
- Tests: local binding hover, stdlib symbol hover, stdlib definition jump, use-site hover.

Done when:

- Hovering a local variable shows its inferred type.
- Go-to-definition on a stdlib symbol lands on the stdlib declaration.

#### Phase D4: `goby fmt` — AST pretty-printer

Goal: deterministic, idempotent source formatter as a `goby fmt` CLI subcommand.

Dependencies: D4 does not strictly require spans (D1a) for Option A (comment-drop) formatting,
but round-trip testing benefits from span information for error localization. If Option B
(comment preservation) is pursued later, it requires a token/CST layer that depends on
position infrastructure from D1a.

Comment policy decision (must be locked before coding):

- **Option A (drop comments)**: the formatter is comment-unaware; comments are stripped
  during parsing and do not appear in output. Document this explicitly.
- **Option B (preserve comments)**: requires a token/CST layer that retains comment positions.
  This is significantly more work and is not recommended for the first slice.
- **Default**: Option A. If comment preservation is needed, it becomes a follow-up track.

Scope (assuming Option A):

- Implement `fn format_module(module: &Module) -> String` in `goby-core`:
  - covers all `Expr`/`Stmt` variants: literals, variables, binary ops, calls, lambdas,
    records, tuples, lists, `if`, `case`, `with`, `handler`, `Block`.
  - style rules: 2-space indentation, one blank line between top-level declarations,
    trailing newline, no trailing whitespace.
- Add `goby fmt <file>` (in-place rewrite) and `goby fmt --check <file>` (exit 1 if not formatted).
- Formatter is idempotent: `format_module(parse(format_module(parse(src)))) == format_module(parse(src))`.
- Snapshot tests over all `examples/` and `stdlib/` files (locked expected output).
- Round-trip test: `parse(fmt(src))` produces an AST structurally equal to `parse(src)`.
- Comment-drop behavior documented in `goby fmt --help` output.

Done when:

- `goby fmt --check` passes on all files under `examples/` and `stdlib/`.
- Snapshot tests are committed and stable.
- Idempotency property holds for all test inputs.

#### Phase D5: `goby lint` — high-signal static checks

Goal: machine-readable linter output for common mistakes not caught by the typechecker.

Lint rules ordered by ascending analysis cost (cheapest infrastructure first to unblock
the lint framework early; user-value ranking noted in parentheses for future prioritization):

1. **Unreachable `case` arm**: wildcard `_` arm followed by more arms (purely syntactic, cheapest).
   User value: medium — catches subtle logic errors in pattern matching.
2. **Unused local binding**: `x = expr` where `x` is never referenced afterward
   (needs local-use tracking across `Expr`/`Stmt` spans from D1a).
   User value: **high** — most frequently encountered lint in practice; catches typos and dead code.
   Note: despite higher analysis cost than rule 1, consider implementing early if the
   lint framework from rule 1 is already in place, as this delivers the most user value.
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

#### Milestones summary

- M1a-i (D1a-i done): `Span` carries end position; position helpers tested.
- M1a-ii (D1a-ii done): declaration-level AST nodes carry spans.
- M1a-iii (D1a-iii done): Stmt/Expr identifier nodes carry spans.
- M1b (D1b done): unified `Diagnostic` type; CLI format locked by golden tests.
- M1c (D1c done): ≥80% of TypecheckError sites populate span.
- M2a (D2a done): editor shows parse/type diagnostics via LSP identical to `goby-cli check`.
- M2b (D2b done): multiple errors per file reported in both CLI and LSP.
- M3a (D3a done): hover/definition for top-level declarations and effect members.
- M3b (D3b done): hover/definition for local bindings and stdlib imports.
- M4 (D4 done): `goby fmt` is idempotent on all existing sources; comment policy explicit.
- M5 (D5 done): four lint rules shipped; JSON output stable.

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

### 4.6 Active Track F: List Index Access `l[i]`

Goal: add `expr[expr]` syntax for `List` index access.

#### Design decisions (lock before coding)

- Syntax: `list_expr[index_expr]` — postfix bracket form, binds tighter than function application.
- Index type: `Int` only (0-based). Negative indices are a runtime error (not Python-style wrap-around).
- Out-of-bounds: `RuntimeError::Abort` — not `Unsupported` (which silently degrades).
- `list_expr` may be any expression (variable, call result, literal, etc.).
- Chaining: `l[i][j]` is valid (left-associative postfix).
- `l[i]` in assignment position is NOT supported in this track (read-only).
- `case` pattern side: index access is not a pattern; no change to pattern syntax.
- `ListIndex` is NOT added to `needs_parens_as_subexpr` (postfix form never needs wrapping parens).

#### Phase F1a: AST stub — add `Expr::ListIndex` and patch all exhaustive match sites

Scope:

- Add `Expr::ListIndex { list: Box<Expr>, index: Box<Expr> }` to `ast.rs`.
- `to_str_repr`: return `None` (same as other unprinted forms).
- `needs_parens_as_subexpr`: do NOT add `ListIndex` here.
- Add stub arms to every exhaustive `match expr` site (at minimum safe defaults):
  - `goby-core`: `typecheck_check.rs`, `typecheck_resume.rs`, `typecheck_effect_usage.rs`,
    `typecheck_stmt.rs`, `typecheck_ambiguity.rs`, `typecheck_annotation.rs`,
    `typecheck_validate.rs` (if exists), `typecheck_branch.rs`, `typecheck_effect.rs`
  - `goby-wasm`: `lower.rs`, `runtime_expr.rs`, `runtime_replay.rs`, `runtime_io_plan.rs`,
    `planning.rs`, `runtime_resolver.rs`
  - walker/inspector sites (e.g. `inspect_expr` in `planning.rs`) must recurse into
    both `list` and `index` sub-expressions, not just compile with `_ => ()`.

Done when:

- `cargo check` passes with zero warnings on exhaustive-match.
- All new arms are safe stubs (no logic changes).

#### Phase F1b: Parser — simple `identifier[expr]` and call-result `f()[expr]`

Scope:

- In `parse_expr`, before the `starts_with('[') && ends_with(']')` gate, scan from the
  right for a trailing `]`, find the matching `[` via bracket depth, and if the prefix is
  non-empty, parse prefix as list expression and bracketed part as index expression.
- Algorithm: scan right-to-left, track `[`/`]` depth; when depth returns to 0, split there.
  If prefix starts with `[`, that case is deferred to F1c.
- Empty bracket `xs[]` → `None`.
- `xs[0]`, `xs[n]`, `xs[i + 1]`, `f()[0]` must parse correctly.
- `xs[0]` as a call argument (inside `split_top_level_whitespace_terms`) must also work:
  verify that `split_top_level_whitespace_terms` already tracks `[`/`]` depth; if not,
  fix it in this phase.

Done when:

- Parser tests pass for `xs[0]`, `xs[n]`, `xs[i + 1]`, `f()[0]`.
- `print xs[0]` parses as `ListIndex { list: Call(print, xs), index: 0 }` because
  `parse_list_index_suffix` fires before `parse_call_expr` on any string ending with `]`.
  To print the element, write `print (xs[0])` explicitly.
- `xs[]` returns parse error.
- `cargo test -p goby-core` passes; all existing list-literal tests still pass.

#### Phase F1c: Parser — chaining and list-literal receiver `[1,2,3][0]`

Scope:

- Extend F1b to handle prefix starting with `[` (i.e. list literal used as receiver).
  Move the new `ListIndex` detection to run before the `starts_with('[') && ends_with(']')`
  list-literal gate. Adjust parse order so the suffix scan runs first and only falls
  through to `parse_list_expr` when no `]`-terminated index suffix is found.
- Chaining `xs[0][1]`: the suffix-scan loop naturally produces left-associative chaining.
- Test cases: `[1, 2, 3][0]`, `[10, 20][1]`, `xs[0][0]`.

Done when:

- `[1,2,3][0]` parses as `ListIndex { list: ListLit([1,2,3]), index: 0 }`.
- `xs[0][1]` parses as `ListIndex { list: ListIndex { xs, 0 }, index: 1 }`.
- All existing list-literal parse tests still pass.
- `cargo test -p goby-core` passes.

#### Phase F2: Typechecker

Scope:

- In `typecheck_check.rs` (or equivalent `check_expr`), replace the stub arm for
  `Expr::ListIndex` with real inference:
  - Infer `list` type; require `Ty::List(a)` → result is `a`.
  - `Ty::Unknown` receiver → result `Ty::Unknown` (no error).
  - `Ty::List(Ty::Unknown)` receiver → result `Ty::Unknown`.
  - Infer `index` type; require `Ty::Int` → error if mismatch.
  - Non-list receiver → type error "expected List type, got X".
- Add typecheck tests:
  - `xs : List Int`, `xs[0]` → `Int`.
  - `xs : List String`, `xs[1]` → `String`.
  - `xs[True]` → type error (index must be `Int`).
  - `42[0]` → type error (receiver must be `List`).

Done when:

- Type inference and error diagnostics work for all cases above.
- `cargo test -p goby-core` passes.

#### Phase F3: Fallback runtime evaluation

Scope:

- Replace stub arm in `runtime_resolver.rs` / `runtime_expr.rs` for `Expr::ListIndex`:
  - Evaluate `list` → `Value::List(items)`.
  - Evaluate `index` → `Value::Int(i)`.
  - Bounds check: `0 <= i < items.len()` → return `items[i]`.
  - Out-of-bounds or negative → `RuntimeError::Abort { message: "list index out of bounds" }`.
- `planning.rs::inspect_expr` stub is already recursive (from F1a) — verify it.
- Wasm capability check: `ListIndex` is already classified as `UnsupportedValueExpr` via
  the wildcard in `fallback.rs` — no additional work needed; verify with a test.
- Add runtime output tests:
  - `xs : List Int = [10, 20, 30]; println "${xs[1]}"` → `20`.
  - `List String`, `List Bool` variants.
  - out-of-bounds produces `RuntimeError::Abort`.

Done when:

- `goby-cli run` executes list index access correctly for in-bounds cases.
- Out-of-bounds produces a deterministic `Abort` error.
- `cargo test` passes including runtime output tests.

#### Phase F4: Spec and tooling follow-through

Scope:

- Update `doc/LANGUAGE_SPEC.md` section 3 to document `expr[expr]` syntax, precedence,
  out-of-bounds behavior (`Abort`), and negative-index policy.
- Add a canonical example to `examples/` demonstrating list index access.
- Update syntax highlighting if `[` in postfix position needs a new token rule
  (`tooling/syntax/textmate`, `tooling/vscode-goby/syntaxes`, `tooling/emacs`, `tooling/vim`).
- Update formatter (`goby fmt`) to handle `Expr::ListIndex` once D4 is active
  (or add a placeholder rendering `list[index]`).

Done when:

- `doc/LANGUAGE_SPEC.md` is updated.
- Syntax highlight definitions handle the new form.
- Example file parses, typechecks, and runs correctly.

#### Milestones summary

- MF1a (F1a done): AST node exists; all exhaustive match sites patched; `cargo check` clean.
- MF1b (F1b done): parser handles `xs[i]`, `f()[i]` and argument-position uses.
- MF1c (F1c done): parser handles `[1,2,3][i]` and chaining.
- MF2 (F2 done): typechecker infers element type and rejects bad index/receiver types.
- MF3 (F3 done): runtime executes index access; out-of-bounds `Abort`.
- MF4 (F4 done): spec and tooling updated; example committed.

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
