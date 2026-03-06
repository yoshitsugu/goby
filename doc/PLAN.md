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
- Basic shown types: `Int`, `String`, tuple `(A, B)`, `List T`

## 2. MVP Decisions and Remaining Open Items

### 2.0 Locked for Current MVP

- Development-phase change policy (locked 2026-03-04):
  - This project is still in a personal early-development phase; immediate breaking
    changes are allowed when they improve consistency/simplicity.
  - However, avoid short-sighted shortcuts that are likely to become long-term design debt.
    Even if a change is breaking now, it should still align with plausible future
    language/tooling architecture.

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
- **List spread/concat expression (`[a, b, ..xs]`)** (planned).
  - Goal:
    - allow list construction with fixed prefix elements plus tail concatenation in expression position.
    - examples: `[f(x), ..ys]`, `[a, b, c, ..xs]`.
  - Parsing scope:
    - support zero or more normal elements followed by at most one trailing `..expr`.
    - reject malformed forms (`[..xs]` at current MVP scope, multiple spread segments, non-trailing spread).
    - keep `case` list-pattern syntax behavior unchanged; this item is expression syntax only.
  - AST scope:
    - represent list literal elements and optional spread tail distinctly so lowering/typecheck can validate shape without reparsing.

### 2.2 Types and Checking

- TODO (Deferred): declaration-side generic parameter binders
  (for example, `id : a -> a` with explicit binders).
- **List `case` pattern typing** (implemented).
  - For `case xs` with list patterns, typecheck verifies scrutinee is `List _` when known.
  - Per-arm local environment extension is implemented:
    - item binders (`[a, b]`, `[a, ..]`) bind each item as `a`.
    - tail binder (`[..tail]` via `[x, ..tail]`) binds `tail : List a`.
  - MVP `Ty::Unknown` tolerance is preserved when information is insufficient.
  - Native Wasm capability checker still treats list patterns as unsupported;
    these cases execute via fallback runtime path.
- **List spread expression typing (`[a, b, ..xs]`)** (planned).
  - Type rules to implement:
    - for `[e1, e2, ..., ..tail]`, all prefix elements must unify to the same type `a`.
    - `tail` must typecheck as `List a`.
    - whole expression type is `List a`.
  - Diagnostics:
    - when prefix element types conflict, report expected element type vs actual element type.
    - when tail is not a list (or has mismatched element type), report expected `List a` shape explicitly.
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
- `resume` support is active with partial 4.7 alignment:
  - outside-handler `resume` is rejected,
  - type mismatch for resumed value is rejected,
  - no-`resume` handler completion aborts at the handled operation boundary,
  - nested abortive handler propagation is implemented,
  - conservative syntactic multi-`resume` rejection is removed,
  - runtime multi-resume progression is still incomplete.
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

#### Compatibility Cleanup Backlog (Survey: 2026-03-06)

Goal: record remaining implementation-side compatibility bridges so they can be
removed in a deliberate order after active language/runtime work.

- Confirmed already removed from parser/runtime/typecheck acceptance paths:
  - legacy top-level `handler ... for ...`
  - legacy `using`
  - legacy `with_handler`
  - expression-form `Unit` value
  - legacy `@embed effect <EffectName>`
- Remaining compatibility bridges to remove later:
  - [ ] C1. stdlib import builtin fallback
    - current status:
      - `validate_imports` and import-type resolution still fall back to
        `builtin_module_exports(...)` when stdlib files are missing.
      - current fallback-covered modules are `goby/string`, `goby/list`, and `goby/env`.
    - code anchors:
      - `crates/goby-core/src/typecheck.rs`:
        `validate_imports`, `module_exports_for_import_with_resolver`,
        `builtin_module_exports`
    - removal target:
      - make stdlib file resolution the single source of truth for import/export visibility.
      - fail clearly when a stdlib module file is missing instead of silently using builtin exports.
  - [ ] C2. bare builtin `print` availability without import
    - current status:
      - typecheck still treats bare `print` as available without explicit stdlib import.
      - tests currently lock this behavior as intentional compatibility.
    - code anchors:
      - `crates/goby-core/src/typecheck.rs`
        (`baseline_bare_print_builtin_is_available_without_import`)
      - runtime fallback bridge paths in `crates/goby-wasm/src/lib.rs`
    - removal target:
      - decide whether `print` remains permanent prelude sugar or becomes import/prelude-only.
      - if removed, replace compatibility tests with explicit-prelude/import coverage.
  - [ ] C3. builtin fallback tests and migration assumptions
    - current status:
      - tests explicitly assert resolver fallback when stdlib files are absent.
      - planning docs still describe builtin fallback as part of migration strategy.
    - code anchors:
      - `crates/goby-core/src/typecheck.rs`
        (`resolver_falls_back_to_builtin_exports_when_file_missing`)
      - `doc/PLAN_STANDARD_LIBRARY.md`
    - removal target:
      - flip tests from "fallback works" to "missing stdlib fails clearly" once C1 is removed.
      - trim obsolete migration wording from standard-library planning docs.
  - [ ] C4. embedded default handler / runtime bridge revalidation after C1-C3
    - current status:
      - embedded default handlers are current semantics, not legacy syntax.
      - however, runtime behavior still relies on bridge-style stdlib/default-handler plumbing.
    - removal target:
      - re-evaluate after stdlib fallback cleanup whether any remaining bridge code is
        accidental compatibility debt or intentional permanent runtime architecture.
      - keep this item scoped as a review step, not an automatic deletion.
- Recommended removal order:
  - 1. C1 stdlib import builtin fallback
  - 2. C3 fallback-oriented tests/docs
  - 3. C2 bare builtin `print` policy decision and cleanup
  - 4. C4 embedded default handler bridge revalidation
- Out of scope for this backlog:
  - `Unit` as a type name and internal runtime representation (`RuntimeValue::Unit`) is not
    compatibility debt.
  - historical notes in `doc/STATE.md` are execution history, not active compatibility behavior.

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

### 4.5 Active Language Task: List Spread Expressions and `List.map` Consolidation

Goal: enable `stdlib/goby/list.gb` `map` implementation (`[f(x), ..ys]`) and
consolidate map semantics in stdlib.

Implemented summary:

- list spread expressions are implemented in parser, AST, typecheck, and runtime.
- canonical source form is `[a, b, ..xs]` with one trailing spread segment.
- `List.map` semantics were consolidated onto stdlib-backed behavior and redundant builtin-only map handling was trimmed.
- docs/examples/tests were updated and parity was validated across runtime paths.

### 4.6 Parking Lot (Needs Revalidation Before Implementation)

- CLI `build` expansion details (`--target`, `--engine-compat`, verify modes).
- CLI binary naming migration (`goby-cli` -> `goby`) final policy.

These items are intentionally kept as short placeholders until they become active.

### 4.7 Active Language Task: Abortive Handlers and Multi-Resume Progression

Goal: align runtime/typecheck behavior with the current `LANGUAGE_SPEC` contract:

- handler clause without `resume` aborts immediately at the handled operation boundary,
- `resume` returns a value to the operation call site,
- repeated `resume` in one handler invocation progresses continuation to next resumable point,
  and raises runtime error only after continuation is consumed.

Step-by-step checklist:

- Completed summary:
  - [x] Step 1: semantic alignment audit
    - runtime handler dispatch entrypoints were audited across fallback and typed mode.
    - the audit confirmed the pre-fix gap was unit-position no-`resume` continuation.
  - [x] Step 2: runtime abort contract implementation
    - runtime dispatch now carries explicit abort/completion state instead of overloading nested `Option` meanings.
    - no-`resume` handler completion now aborts at the handled operation boundary in both value and unit positions.
    - nested-handler propagation and fallback/typed parity coverage were added.
- [ ] Step 3: multi-resume progression support
  - replace one-shot token consumption model with resumable progression model for one handler invocation.
  - each `resume` continues from the next resumable point; exhausted continuation raises runtime error.
  - keep guardrails for clearly invalid continuation state transitions.
  - implementation status (2026-03-06):
    - this step is not a token-only change; the current runtime must be made continuation-aware at
      the AST evaluator boundary.
    - a direct one-line change from one-shot to multi-shot would be incorrect because `resume`
      currently only returns a value to the handler body and does not reify the caller continuation.
    - groundwork landed in `crates/goby-wasm/src/lib.rs`:
      - introduced `AstEvalOutcome<T>` as the Step 3 runtime-shape carrier.
      - handler-dispatch statement execution now branches on explicit AST outcomes instead of
        relying only on `Option` + token-state probing.
      - `Suspended(...)` is not emitted yet; real checkpoints still need to be implemented.
    - first progression slice landed in `crates/goby-wasm/src/lib.rs`:
      - resume tokens can now carry an AST statement-tail continuation snapshot.
      - top-level `with` bodies and unit-position AST statement sequences now register
        continuation checkpoints for remaining statements.
      - when `resume` consumes such a checkpoint, the remaining unit-position statements execute
        before the handler body continues, enabling handler-local code after the first `resume`.
      - continuation exhaustion after that replay still reports the existing deterministic
        consumed-continuation runtime error.
    - remaining gap after this slice:
      - value-position continuation checkpoints are still not implemented.
      - no explicit `Suspended(...)` result is emitted yet; progression is currently modeled
        through statement-tail replay only.
    - latest groundwork slice after the unit-tail replay:
      - `eval_expr_ast_outcome` now evaluates key composite AST forms recursively instead of
        only wrapping `eval_expr_ast`.
      - `InterpolatedString`, `BinOp`, `ListLit`, `TupleLit`, `Block`, `Case`, and `If`
        can now propagate a future `Suspended(...)` outcome from child expressions without another
        large refactor.
      - this slice intentionally preserves current behavior; it is a preparation step for
        value-position continuation checkpoints, not the checkpoint implementation itself.
  - confirmed investigation findings:
    - current runtime anchor points:
      - `crates/goby-wasm/src/lib.rs`: `dispatch_handler_method_core`
      - `crates/goby-wasm/src/lib.rs`: `resume_through_active_continuation_fallback`
      - `crates/goby-wasm/src/lib.rs`: `resume_through_active_continuation_optimized`
      - `crates/goby-wasm/src/lib.rs`: `eval_expr_ast`
      - `crates/goby-wasm/src/lib.rs`: `execute_unit_expr_ast`
      - `crates/goby-wasm/src/lib.rs`: `execute_unit_ast_stmt`
    - current token model:
      - fallback mode stores `ResumeToken { continuation: Continuation { consumed }, state }`.
      - typed mode stores `OptimizedResumeToken { consumed, state }`.
      - both modes mark the token consumed at the first `resume`.
      - `dispatch_handler_method_core` breaks the handler-body loop as soon as a resumed value is observed.
    - semantic gap:
      - the current bridge can express:
        - single `resume` success,
        - no-`resume` abort,
        - deterministic runtime error on second `resume`.
      - the current bridge cannot express:
        - "resume caller continuation until the next resumable point, then return control to the same handler invocation".
      - this gap affects both value-position and unit-position operation calls.
    - why the gap exists:
      - `Expr::Resume` in `eval_expr_ast` currently calls `resume_through_active_continuation_bridge`
        and receives only a plain `RuntimeValue`.
      - no continuation frame, AST cursor, or statement-sequence checkpoint is stored for the
        operation call site.
      - `dispatch_handler_method_core` therefore has no way to continue from "the next resumable
        point" after the caller resumes and later suspends again.
    - concrete affected examples:
      - state-threaded iterator flows such as `examples/iterator_unified.gb`
        require progression across multiple `yield` operations from one enclosing handler invocation.
      - nested value-position calls such as `resume (op ...)` must distinguish "resumed again later"
        from true continuation exhaustion.
  - required implementation direction (locked for restart):
    - do not try to fake Step 3 by merely changing `consumed: bool` into a counter or queue.
    - first introduce an explicit continuation result at the AST runtime layer so evaluation can
      suspend and later continue.
    - keep fallback and typed-continuation modes on the same semantic contract even if the internal
      storage differs.
  - staged execution plan:
    - [x] Step 3.1: introduce continuation-aware runtime result types
      - replace the current `Option<RuntimeValue>` / `Option<()>`-only bridge at handler-sensitive
        paths with an explicit result that can represent:
        - completed value,
        - completed unit,
        - suspended effect operation carrying the resumed payload / next checkpoint,
        - abortive completion,
        - runtime error.
      - thread the new result through:
        - `eval_expr_ast`
        - `execute_unit_expr_ast`
        - `execute_unit_ast_stmt`
        - `dispatch_handler_method_core`
    - [~] Step 3.2: model resumable caller checkpoints for AST execution
      - capture enough information to continue evaluation after an effect operation:
        - statement index within block/function/handler body,
        - local/callable environment snapshot,
        - pending expression shape where value-position resumption must re-enter.
      - current status:
        - implemented for AST-backed unit-position statement tails only.
        - not yet implemented for value-position resumptions or inner expression checkpoints.
      - start with AST-backed paths only; string-fallback paths are not the target for Step 3.
    - [~] Step 3.3: implement progression in fallback mode first
      - make one handler invocation able to call `resume` repeatedly.
      - each `resume` should drive the captured caller continuation until:
        - another handled operation suspends back to the same handler invocation, or
        - the continuation completes, after which the invocation is exhausted.
      - current status:
        - continuation completion path now works for saved unit-position statement tails.
        - repeated `resume` after that completion is covered by regression tests.
        - progression to intermediate value-position resumable points is still pending.
      - preserve deterministic `continuation_missing` / `continuation_consumed` style runtime errors.
    - [~] Step 3.4: mirror the same contract in typed-continuation mode
      - keep the current mode-parity harness green while reusing the same externally visible
        behavior.
      - current status:
        - typed mode mirrors the new unit-position replay + exhaustion slice.
      - implementation may still use separate token storage, but not separate semantics.
    - [~] Step 3.5: cover the progression matrix with tests
      - fallback success: one handler invocation resumes through multiple operation sites.
      - fallback exhaustion: extra `resume` after continuation completion fails deterministically.
      - nested handlers: inner suspension returns control to the correct enclosing invocation.
      - typed/fallback parity for the same cases.
      - current status:
        - added fallback + typed parity regression for unit-position replay then exhaustion.
        - broader matrix for value-position progression is still open.
  - restart checklist:
    - begin from `crates/goby-wasm/src/lib.rs`; no parser or typecheck blocker remains for Step 3.
    - preserve existing error-kind mapping in `parity_outcome_from_runtime_output`.
    - preserve Step 2 behavior:
      - no-`resume` remains abortive,
      - nested abort propagation remains explicit,
      - nearest lexical handler resolution must not regress.
    - after Step 3 lands, update this section, `doc/LANGUAGE_SPEC.md`, and `doc/STATE.md` together.
  - success criteria:
    - semantic acceptance:
      - one handler invocation can `resume` more than once and each `resume` advances from the
        next resumable point instead of restarting from the beginning.
      - when the resumed continuation finishes, any further `resume` from that same handler
        invocation reports the deterministic consumed-continuation runtime error.
      - no-`resume` handler completion still aborts immediately at the handled boundary.
      - nested handlers still route control to the nearest matching lexical handler.
    - runtime architecture acceptance:
      - Step 3 no longer depends on the current "set resumed value and break" one-shot loop in
        `dispatch_handler_method_core`.
      - AST runtime paths retain enough checkpoint information to resume both:
        - unit-position handled operations,
        - value-position handled operations used in bindings, conditionals, blocks, and call chains.
      - fallback and typed modes share the same externally visible continuation contract.
    - regression acceptance:
      - `examples/iterator_unified.gb` typechecks and its runtime-relevant progression shape is
        covered by tests.
      - dedicated runtime tests exist for:
        - multi-resume progression success,
        - continuation exhaustion error,
        - nested handler progression/dispatch correctness,
        - fallback/typed parity for the same matrix.
    - quality gate acceptance:
      - `cargo fmt`
      - `cargo check`
      - `cargo test`
      - `cargo clippy -- -D warnings`
      all pass after the Step 3 implementation.
- [x] Step 4: typecheck rule update
  - conservative syntactic multi-`resume` rejection was removed.
  - retained checks are `resume` placement, resumed-value type compatibility, and unresolved generic diagnostics.
  - nested handler-clause parsing now exposes valid multi-branch `resume` cases; runtime progression semantics remain tracked in Step 3.
- [ ] Step 5: tests and parity locks
  - add/update fallback runtime tests for:
    - no-`resume` immediate abort in value and unit position,
    - multi-resume progression success path,
    - resume-after-consumption runtime error path.
  - add typed-mode parity tests for the same scenarios.
- [ ] Step 6: docs sync
  - keep `doc/LANGUAGE_SPEC.md` and `doc/PLAN.md` in sync with final behavior wording.
  - add/refresh one effect example showing exception-style abortive handler semantics.
- [ ] Step 7: quality gate
  - run:
    - `cargo fmt`
    - `cargo check`
    - `cargo test`
    - `cargo clippy -- -D warnings`

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
