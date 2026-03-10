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
  - conservative syntactic multi-`resume` rejection is removed,
  - runtime multi-resume progression is still incomplete.
  - no-`resume` runtime behavior is being redesigned away from "program-abort"
    semantics toward "`with ... in ...` scope exit" semantics; the old abortive
    wording below should be treated as historical context unless explicitly
    marked as locked.
  - until 4.7 Phase 1 lands, `doc/LANGUAGE_SPEC.md` remains the source of truth
    for current shipped behavior; the scoped-exit semantics below are target
    behavior, not yet current behavior.
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

### 4.7 Active Language Task: Scoped Handler Exit and Multi-Resume Progression

Goal: redefine no-`resume` handler completion so it exits the enclosing
`with ... in ...` scope instead of aborting the whole program, while preserving
the existing `resume` contract for resumptive handlers.

Status note:

- this section describes the target redesign.
- current shipped/spec behavior is still the abortive contract described in
  `doc/LANGUAGE_SPEC.md` until this track's Phase 1 doc sync lands.

Target semantics:

- `resume v` returns `v` to the operation call site and continues normally.
- if a handler clause finishes without any `resume`, evaluation exits the
  current `with ... in ...` scope immediately.
- the handler clause's final expression becomes the value of the whole
  `with ... in ...` expression.
- the handler clause's final expression type must unify with the enclosing
  `with ... in ...` expression type.
- repeated `resume` in one handler invocation still progresses continuation to
  the next resumable point, and raises runtime error only after continuation is
  consumed.
- nested handlers still route to the nearest matching lexical handler.

Canonical examples for the new semantics:

These examples are target semantics examples for the redesign, not examples of
current shipped behavior.

```goby
effect Fail
  fail : String -> Int

main : Unit -> Unit
main =
  result =
    with
      fail msg ->
        0
    in
      x = fail "bad"
      x + 10
  print result
```

Expected behavior:
- prints `0`
- `x + 10` is not executed
- only the innermost `with ... in ...` is exited; outer program execution continues

```goby
effect Log
  stop : String -> Unit

main : Unit -> Unit
main =
  print "before"
  with
    stop msg ->
      print "handled:${msg}"
      ()
  in
    stop "done"
    print "after"
  print "outside"
```

Expected behavior:
- prints `before`
- prints `handled:done`
- does not print `after`
- prints `outside`

```goby
effect Escape
  next : Int -> Int

main : Unit -> Unit
main =
  result =
    with
      next n ->
        if n == 0
          99
        else
          resume (n + 1)
    in
      a = next 0
      b = next 1
      a + b
  print result
```

Expected behavior:
- first `next 0` exits the `with` scope with value `99`
- `b = next 1` and `a + b` are skipped
- prints `99`

Step-by-step checklist:

- Completed summary:
  - [x] Step 1: semantic alignment audit
    - runtime handler dispatch entrypoints were audited across fallback and typed mode.
    - the audit confirmed the pre-fix gap was unit-position no-`resume` continuation.
  - [x] Step 2: continuation runtime consolidation groundwork
    - runtime dispatch now carries explicit completion/suspension state instead of overloading nested `Option` meanings.
    - the active execution path is centralized on `Cont`, `Out`, `eval_expr`, and `eval_stmts`.
    - this step is treated as groundwork for the new scoped-exit semantics, not as a locked semantic endpoint.
- [ ] Step 3: redesign no-`resume` semantics around scoped `with` exit
  - Goal: rebuild Step 3 around an explicit `with`-scope exit contract rather than preserving
    the previous abortive-handler behavior.
  - Archived Step 3 incremental history (Steps 3.1–3.5) is in
    `doc/old/PLAN_MULTIRESUME_PROGRESSION_SUPPORT.md`.
  - Note:
    - the currently landed `Cont` / `Out` / `eval_stmts` architecture is useful groundwork,
      but this plan intentionally re-evaluates the runtime shape from first principles instead of
      assuming every existing type boundary is final.

  ### Fresh Runtime Shape

  If this were designed from scratch for the new semantics, the runtime should
  model three distinct outcomes instead of collapsing them into "value vs abort":

  1. normal completion with a value,
  2. suspension for `resume`,
  3. non-resumptive escape from the current `with` scope.

  ```rust
  enum Out<T> {
      Done(T),
      Suspend(Cont),
      Escape(Escape),
      Err(RuntimeError),
  }

  enum Escape {
      WithScope {
          with_id:        WithId,
          value:          RuntimeValue,
          handler_stack:  Vec<InlineHandlerValue>,
      },
  }

  enum Cont {
      StmtSeq {
          store:         Option<StoreOp>,
          remaining:     Vec<Stmt>,
          locals:        RuntimeLocals,
          callables:     HashMap<String, IntCallable>,
          depth:         usize,
          handler_stack: Vec<InlineHandlerValue>,
          finish:        FinishKind,
      },
      Apply {
          step:          ApplyStep,
          locals:        RuntimeLocals,
          callables:     HashMap<String, IntCallable>,
          depth:         usize,
          handler_stack: Vec<InlineHandlerValue>,
      },
      Resume,
  }

  enum FinishKind {
      Block,
      WithBody { with_id: WithId },
      HandlerBody { token_idx: usize, produce_value: bool, with_id: WithId },
      Ingest,
  }
  ```

  Key design decisions:
  - "escaping a `with` scope" is neither a normal value return nor a runtime error;
    it needs its own flow variant.
  - `Escape::WithScope` carries the target `with_id`; outer evaluation frames must rethrow the
    escape until that specific `with` body boundary is reached.
  - the handler clause's final expression becomes the escaped value.
  - `FinishKind::WithBody { with_id }` is the only place that consumes a matching
    `Escape::WithScope { with_id, .. }` and turns it into a normal `Done(value)`.
  - `resume` remains explicit and separate from scope escape; these two paths must not share
    one overloaded "abort" representation.
  - `RuntimeError` is reserved for actual runtime errors (`resume` outside handler,
    continuation exhaustion, invalid state transitions), not for structured handler control flow.
  - to keep migration cost bounded, prefer extending the current active `Out<T>`
    with `Escape(Escape)` rather than renaming the runtime result type mid-refactor.

  ### Implementation phases

  - [x] Phase 1 — Lock the new semantics before more cleanup
    - Update `doc/LANGUAGE_SPEC.md` wording from "abortive no-`resume`" to
      "`with`-scope exit on no-`resume`".
    - Add the canonical examples above (or reduced equivalents) to tests/examples/docs.
    - Explicitly define value-position behavior: no-`resume` clause result becomes the value
      of the whole `with ... in ...` expression.
    - checks: doc review + targeted parser/typecheck/runtime tests

  - [x] Phase 2 — Introduce first-class scope-exit flow
    - Add an explicit escape outcome (`Escape::WithScope`) to the active runtime result type.
    - Thread `with_id` / scope identity through `Expr::With`, `eval_stmts`, and handler dispatch.
    - Make `FinishKind::WithBody { with_id }` consume matching escapes and rethrow non-matching ones.
    - Remove the use of runtime "abort" for structured no-`resume` control flow.
    - checks: `cargo build -p goby-wasm`, targeted runtime tests

  - [x] Phase 3 — Rebuild handler dispatch around escape vs resume
    - handler clause without `resume`:
      - evaluate clause body to a value,
      - package it as `Escape::WithScope { with_id, value, ... }`,
      - do not mark the event as runtime error / global abort.
    - handler clause with `resume`:
      - keep the continuation progression semantics already under construction.
    - nearest lexical handler wins remains unchanged.
    - checks: `cargo build -p goby-wasm`, `cargo test -p goby-wasm`

  - [x] Phase 4 — Rewrite tests around scoped exit semantics
    - replace old "no-`resume` aborts program" assertions with:
      - exits only the current `with` body,
      - outer program continues,
      - body remainder is skipped,
      - escaped clause result becomes whole-`with` result.
    - add nested-with tests:
      - inner handler exits inner `with` only,
      - outer `with` can continue unless it is the escaped target.
    - keep existing multi-`resume` progression and exhaustion tests.
    - checks: `cargo test -p goby-wasm`

  - [x] Phase 5 — Cleanup around the new semantics, not the old ones
    - after scoped-exit flow is stable, remove old AST continuation compatibility types/functions.
    - unify resume-token state on the final semantics, not on the old abortive contract.
    - checks: `cargo fmt`, `cargo clippy -p goby-wasm -- -D warnings`, `cargo test --workspace`
    - Final checkpoint (2026-03-10):
      - done:
        - `AstEvalOutcome` / `AstContinuation` compatibility layer removed.
        - `pending_value_continuations` / `AstContinuationFrame` path removed.
        - legacy statement-eval path (`eval_ast_side_effect` / `eval_ast_value`) removed from ingest flow.
        - runtime abort flag state (`runtime_aborted`, `set_runtime_abort_once`,
          `has_abort_without_error`) removed.
        - handler/pipeline/with unit-position evaluation runs on `Out` semantics
          with statement-sequence replay preserved.
      - optional follow-up:
        - `eval_expr_ast` compatibility fallback helper still exists; remove incrementally
          only if future features need further simplification.

  ### Restart checklist

  - Read this section + `doc/STATE.md` latest session note.
  - Begin from `crates/goby-wasm/src/lib.rs`.
  - Do not preserve the old no-`resume` abortive behavior by default; it is now the thing being replaced.
  - Preserve only:
    - nearest lexical handler wins,
    - explicit `resume` progression semantics,
    - runtime errors for invalid `resume` state transitions.
  - After Step 3 lands, update this section, `doc/LANGUAGE_SPEC.md`, and `doc/STATE.md` together.

  ### Success criteria

  - Semantic:
    - one handler invocation can `resume` more than once; each advances from the next resumable point.
    - extra `resume` after exhaustion → deterministic `continuation_consumed` error.
    - no-`resume` completion exits only the current `with ... in ...` scope.
    - the no-`resume` clause result becomes the whole-`with` result.
    - outer program execution continues after the `with` expression finishes.
    - nested handlers route to nearest matching lexical handler.
  - Architecture:
    - structured `with`-scope exit has a first-class runtime representation
      (not encoded as runtime abort/error).
    - `with` scope identity is explicit in the runtime flow.
    - cleanup happens after the new semantics is stable, not before.
  - Quality gate: `cargo fmt`, `cargo check`, `cargo test --workspace`, `cargo clippy -- -D warnings`.
- [x] Step 4: typecheck rule update
  - conservative syntactic multi-`resume` rejection was removed.
  - retained checks are `resume` placement, resumed-value type compatibility, and unresolved generic diagnostics.
  - nested handler-clause parsing now exposes valid multi-branch `resume` cases; runtime progression semantics remain tracked in Step 3.
- [x] Step 5: tests and parity locks
  - add/update fallback runtime tests for:
    - no-`resume` immediate `with`-scope exit in value and unit position,
    - multi-resume progression success path,
    - resume-after-consumption runtime error path.
  - add typed-mode parity tests for the same scenarios.
- [x] Step 6: docs sync
  - keep `doc/LANGUAGE_SPEC.md` and `doc/PLAN.md` in sync with final behavior wording.
  - add/refresh one effect example showing scoped-exit handler semantics.
- [x] Step 7: quality gate
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
