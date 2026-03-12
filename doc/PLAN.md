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
      - bare prelude effect ops now resolve through normal imported-effect visibility rather than a dedicated runtime bridge catalog.
      - embedded default execution now flows through a dedicated `EmbeddedEffectRuntime` layer rather than storing stdin/stdout state directly on `RuntimeOutputResolver`.
      - resolver-side effect dispatch now operates on typed embedded handler kinds instead of branching on raw `__goby_embeded_effect_*` names.
      - stdlib resolver now exposes parsed embedded handler kinds together with the parsed module AST, and wasm runtime consumes that shared typed metadata directly when building its runtime import context.
      - remaining special handling is mainly the runtime-owned `Print` / `Read` intrinsic I/O hook itself, not stdlib metadata collection.
      - project direction is now to keep `@embed` narrow: it exists for the minimal `Print` / `Read` onboarding path, not as a general host-capability extension point.
    - design direction:
      - do not treat `@embed` itself as debt to remove.
      - keep `@embed` scoped to prelude `Print` / `Read`; future host capabilities should use other mechanisms rather than growing the embedded-handler surface.
      - isolate runtime-owned embedded handler execution as a narrow effect-dispatch extension for that convenience path.
      - keep effect dispatch split into:
        1. effect/op visibility resolution,
        2. handler selection (`explicit`, `embedded default`, `unhandled`),
        3. handler execution.
    - implementation plan:
      - introduce a dedicated embedded-effect runtime layer (for example `EmbeddedEffectRuntime`) so stdin/stdout behavior is not implemented directly in `RuntimeOutputResolver`.
      - move current `Print` / `Read` execution details (`stdin` buffer/cursor handling and stdout writes) behind that layer.
      - keep stdlib metadata resolution in `goby-core`; wasm runtime should only assemble its runtime import context from already-resolved typed metadata.
      - replace direct `apply_embedded_default_handler(...)` branching with resolved-handler dispatch so bare/qualified/value/unit call paths share one effect-dispatch route.
      - centralize handler-name-to-runtime-implementation mapping in one catalog/registry so adding a new embedded handler does not require editing multiple call paths.
    - guardrails:
      - only one place in runtime should branch on embedded handler names such as `__goby_embeded_effect_stdout_handler`.
      - `RuntimeOutputResolver` should not directly perform embedded stdin/stdout behavior once the refactor is complete.
      - avoid separate embedded-handler fallback logic for bare calls, qualified calls, and pipeline/value paths.
    - completion criteria:
      - embedded handler execution is routed through the dedicated runtime layer.
      - effect dispatch call paths no longer contain ad hoc embedded-handler special cases.
      - existing `Print` / `Read` end-to-end tests still pass under both fallback and typed-continuation modes.
      - docs clearly state that `@embed` is not intended to grow into the general host-effect mechanism.
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

### 4.5 Active Track F: Maintainability Hardening

Goal: reduce "black box" implementation risk by making parser/typechecker/Wasm
runtime internals easier to understand, modify, and test in isolation.

Why this is active:

- Current behavior is heavily regression-tested, but core logic is still
  concentrated in a few very large files (`parser.rs`, `typecheck.rs`,
  `goby-wasm/src/lib.rs`).
- This creates a maintenance risk where future language/runtime work requires
  understanding too many unrelated branches at once.
- Priority is not cosmetic decomposition; priority is to create explicit module
  boundaries, stable internal contracts, and focused tests per subsystem.

Execution policy:

1. Prefer behavior-preserving refactors with contract tests before broad logic changes.
2. Split by responsibility, not by arbitrary line count.
3. Each extraction must leave a narrower public/internal API than before.
4. If a refactor cannot name its boundary in one sentence, the split is too vague.

Near-term slices:

1. Parser decomposition:
   - extract top-level declaration parsing, statement parsing, expression parsing,
     and parser helpers into separate modules under `goby-core`.
   - keep `parse_module` as a thin orchestration entrypoint.
   - add targeted tests at the extracted-module boundary where behavior is currently
     only covered through end-to-end parser tests.
2. Typechecker phase separation:
   - split declaration validation, import/stdlib resolution, effect validation,
     type-environment construction, and body-expression checking into distinct modules.
   - keep one orchestrator function, but move each major validation phase behind
     a named function group/module with explicit inputs/outputs.
   - document which phases are allowed to perform stdlib I/O or path resolution.
3. Wasm runtime/codegen boundary cleanup:
   - shrink `goby-wasm/src/lib.rs` so that public compile entrypoints do not also
     own most fallback runtime execution details.
   - extract fallback runtime evaluator, embedded runtime bridge, and compile-time
     output resolvers into dedicated modules with narrow interfaces.
   - keep lowering-plan selection and runtime fallback diagnostics observable via
     dedicated structs instead of ad hoc cross-file knowledge.
4. Shared contract cleanup:
   - remove duplicated parsing/runtime helper logic where one canonical utility
     can serve multiple modules.
   - define stable internal data contracts for effect-operation lookup, runtime
     imports, and diagnostic span propagation.
5. Test-shape hardening:
   - add subsystem-focused tests for extracted modules, not only large-file
     regression tests.
   - preserve end-to-end parity tests so refactors can be proven behavior-preserving.

Priority order:

1. `crates/goby-wasm/src/lib.rs` boundary reduction.
2. `crates/goby-core/src/typecheck.rs` phase separation.
3. `crates/goby-core/src/parser.rs` decomposition.

Detailed implementation plan:

1. [x] Milestone F0: establish refactor harness before moving code.
   - Goal:
     - lock in behavior-preserving checks so later file moves are judged by contracts, not intuition.
   - Planned work:
     - identify and keep a stable smoke subset for each subsystem:
       - Wasm: `cargo test -p goby-wasm`
       - core parser/typecheck: `cargo test -p goby-core`
       - workspace regression: `cargo test --workspace`
       - compile sanity: `cargo check`
     - add or preserve focused tests around extracted seams before moving logic when current coverage is only end-to-end.
     - record intended module boundaries in `doc/STATE.md` before the first code-moving patch.
   - Acceptance criteria:
     - every later milestone can point to a focused test command plus one broader regression command.

2. [x] Milestone F1: reduce `crates/goby-wasm/src/lib.rs` boundary width first.
   - Goal:
     - keep `compile_module` as the orchestration entrypoint while moving fallback-runtime mechanics behind explicit internal modules.
   - Planned extraction order:
     - [x] Step F1.1: extract runtime value/state types and simple helpers.
       - likely targets: `RuntimeValue`, `RuntimeLocals`, equality/format helpers, local-binding utilities.
     - [x] Step F1.2: extract embedded runtime and runtime import loading.
       - likely targets: `EmbeddedEffectRuntime`, `RuntimeImportContext`, stdlib import-loading helpers.
     - [x] Step F1.3: extract fallback evaluator/runtime executor.
       - likely targets: `RuntimeOutputResolver`, continuation state, handler dispatch, `Out` / `Escape` / `Cont` family.
     - [x] Step F1.4: extract compile-time output resolvers that are logically separate from public codegen entrypoints.
       - likely targets: `IntEvaluator`, `ListIntEvaluator`, static-output helpers used by `resolve_main_runtime_output*`.
     - Progress note:
       - completed extractions: `runtime_value`, `runtime_env`, `runtime_flow`, `runtime_eval`, `runtime_resolver`, `runtime_dispatch`, `runtime_decl`, `runtime_exec`, `runtime_replay`.
       - `goby-wasm/src/lib.rs` is now reduced to the codegen entrypoint, orchestration glue, and runtime helpers not yet targeted by later milestones.
   - Constraints:
     - do not change lowering-plan selection or public `compile_module` behavior in the same patch as a large extraction.
     - keep module dependencies one-directional:
       - public entrypoint -> lowering/fallback/runtime helper modules
       - evaluator/runtime helpers must not call back into `compile_module`.
   - Acceptance criteria:
     - `goby-wasm/src/lib.rs` becomes a thin orchestration layer plus public API.
     - extracted modules own their tests where practical, while integration parity tests remain in place.

3. [x] Milestone F2: separate `goby-core` typecheck phases.
   - Goal:
     - replace one giant mixed-responsibility file with explicit checking phases and shared internal data contracts.
   - Planned extraction order:
     - [x] Step F2.1: move shared type environment and internal type representations into a dedicated internal module.
     - [x] Step F2.2: move import/stdlib/intrinsic policy validation into a dedicated validation module.
     - [x] Step F2.3: move effect declaration/effect dependency checks into a dedicated effect-validation module.
     - [x] Step F2.4: move expression/statement checking into dedicated inference/checking modules.
     - [x] Step F2.5: keep `typecheck_module_with_context` as a top-level orchestrator that sequences the extracted phases.
     - Progress note:
       - completed extractions: `crates/goby-core/src/typecheck_env.rs`, `crates/goby-core/src/typecheck_validate.rs`, `crates/goby-core/src/typecheck_effect.rs`, `crates/goby-core/src/typecheck_check.rs`.
       - `typecheck_validate.rs` now owns import resolution, stdlib-root policy, embedded default handling, intrinsic namespace validation, and related import helpers.
       - `typecheck_effect.rs` now owns effect declaration checks, effect dependency checks, effect-map construction, and `can`-clause parsing helpers.
       - `typecheck_check.rs` now owns expression inference, statement checking, resume validation, branch consistency checks, and type rendering helpers.
       - `typecheck.rs` now reads as a phase orchestrator: validate module inputs, prepare checking state, then check declaration bodies.
   - Constraints:
     - avoid creating a new hidden god-module under a different file name.
     - path/stdlib resolution code must remain clearly isolated from pure expression checking.
   - Acceptance criteria:
     - the typechecker has named phase modules with explicit inputs/outputs.
     - tests can target validation/inference phases without traversing the full file.

4. [x] Milestone F3: decompose parser responsibilities.
   - Goal:
     - keep `parse_module` as the entrypoint while splitting top-level parsing, statements, expressions, and helper utilities.
   - Planned extraction order:
     - [x] Step F3.1: isolate shared lexical/splitting helpers.
     - [x] Step F3.2: isolate top-level declaration parsing (`import`, `type`, `effect`, top-level definitions).
    - [x] Step F3.3: isolate statement parsing and multiline block handling.
    - [x] Step F3.4: isolate expression parsing and pattern parsing.
     - Progress note:
       - completed extractions: `crates/goby-core/src/parser_util.rs`, `crates/goby-core/src/parser_top.rs`, `crates/goby-core/src/parser_stmt.rs`, `crates/goby-core/src/parser_expr.rs`, `crates/goby-core/src/parser_pattern.rs`.
       - `parser_util.rs` now owns shared identifier/keyword predicates, comment stripping, indentation helpers, and top-level split helpers used across parser phases.
       - `parser_top.rs` now owns top-level import/embed/type/effect/declaration header parsing, while `parse_module` remains the public entrypoint/orchestrator.
       - `parser_stmt.rs` now owns declaration-body statement parsing, multiline `with` / `case` / `if` block handling, handler-body parsing, and statement-oriented binding/assignment splitting.
       - `parser_expr.rs` now owns expression parsing, interpolation parsing, application/method parsing, and top-level expression splitting helpers.
       - `parser_pattern.rs` now owns case/list pattern parsing shared by multiline statement parsing.
       - `parser.rs` is now reduced to module orchestration plus public parser entrypoints that delegate into focused parser modules.
   - Constraints:
     - preserve current parse error wording unless a separate diagnostics task intentionally changes it.
     - preserve current parser test corpus during moves; add narrower tests only where it reduces ambiguity.
   - Acceptance criteria:
     - parser modules correspond to syntax responsibilities rather than historical file growth.
     - expression and statement parsing can be read independently of top-level declaration handling.

5. [x] Milestone F4: cross-cutting cleanup after the first extractions land.
   - Goal:
     - remove duplication exposed by the refactor rather than during speculative up-front cleanup.
   - Planned work:
     - unify duplicated helper logic only after canonical ownership is clear.
     - tighten span/diagnostic propagation where extracted interfaces currently drop source context.
     - prune transitional re-exports or compatibility shims created during file moves.
   - Acceptance criteria:
     - no transitional module layout remains as permanent architecture by accident.
   - Completed:
     - parser submodule tests now live beside owned parser modules (`parser_expr`, `parser_pattern`, `parser_stmt`, `parser_top`).
     - shared parser example fixtures are centralized in `parser_test_support.rs`.
     - `parser.rs` is now limited to public entrypoints plus parse-module integration/error-span coverage.

6. [x] Milestone F5: shrink `crates/goby-wasm/src/lib.rs` below the resolver-god-module threshold.
   - Goal:
     - make `goby-wasm/src/lib.rs` a public codegen/orchestration layer, not the long-term home of fallback runtime mechanics.
   - Why this milestone exists:
     - after the first extraction wave, `goby-wasm/src/lib.rs` is still 7k+ lines and still owns the main `RuntimeOutputResolver` impl plus several helper clusters.
     - this is the highest remaining black-box risk in the codebase.
   - Planned extraction order:
    - [x] Step F5.1: extract direct-call/pipeline/string helper cluster.
       - likely targets: `flatten_direct_call`, `module_has_selective_import_symbol`, `parse_pipeline`, `eval_string_expr`.
       - preferred destinations: `runtime_call_shape.rs`, `runtime_string.rs`, or similarly narrow helper modules.
    - [x] Step F5.2: extract print-only codegen helper cluster.
       - likely targets: `compile_print_module` and any print-module-only layout/encoding helpers.
       - preferred destination: `print_codegen.rs` or `compile_print.rs`.
    - [x] Step F5.3: split `RuntimeOutputResolver` by behavior phase instead of leaving one giant `impl`.
       - likely seams:
         - expression/value evaluation glue
         - imported declaration resolution
         - handler/effect dispatch orchestration
         - string/static-output-specific fallback helpers
       - acceptable structure:
         - separate extension impls in dedicated modules
         - or a smaller coordinator plus helper structs/modules
     - [x] Step F5.4: leave `compile_module` and `resolve_main_runtime_output*` as orchestration entrypoints only.
   - Constraints:
     - do not change fallback/native parity behavior in the same patch as a structural move unless tests force a behavior fix.
     - keep runtime-phase ownership explicit; avoid merely moving one giant impl into a differently named giant file.
   - Acceptance criteria:
     - `goby-wasm/src/lib.rs` is primarily entrypoints/constants/thin wiring.
     - resolver/runtime helper clusters have names that describe their role without reading all call sites.
   - Progress note:
     - `runtime_support.rs` now owns direct-call flattening, selective-import symbol lookup, simple pipeline parsing, and string-expression helpers.
     - `print_codegen.rs` now owns the print-only Wasm emission helper used by fallback static-output compilation.
     - `runtime_apply.rs` now owns declaration/value-call/binop helper methods that previously lived in the tail of the `RuntimeOutputResolver` impl.
     - `runtime_expr.rs` now owns expression/value evaluation, imported declaration value resolution, and continuation-to-value completion helpers from `RuntimeOutputResolver`.
     - `runtime_unit.rs` now owns unit-position expression execution and side-effect dispatch from `RuntimeOutputResolver`.
     - `runtime_entry.rs` now owns `resolve_main_runtime_output*` helper entrypoints and runtime evaluator construction.
     - `goby-wasm/src/lib.rs` now keeps `compile_module`, shared runtime constants/state, and test coverage; the runtime mechanics live in named modules.

7. [ ] Milestone F6: split residual phase-building responsibilities out of `crates/goby-core/src/typecheck.rs`.
   - Goal:
     - keep `typecheck_module_with_context` as the orchestrator while moving environment construction, annotation validation, and type conversion out of the remaining mixed file.
   - Why this milestone exists:
     - `typecheck.rs` is now phase-oriented, but still mixes orchestration with type-env building, type declaration validation, and annotation/type parsing helpers.
   - Planned extraction order:
     - [x] Step F6.1: extract type-environment construction and global-symbol injection.
       - likely targets: `build_type_env`, `ensure_no_ambiguous_globals`, `inject_effect_symbols`, `inject_type_constructors`.
       - preferred destination: `typecheck_build.rs`.
     - [x] Step F6.2: extract annotation/effect-clause validation helpers.
       - likely targets: `validate_type_annotation`, `validate_handler_type_expr`, `validate_effect_clause`, `strip_effect_clause`, `find_can_keyword_index`.
       - preferred destination: `typecheck_annotation.rs`.
    - [x] Step F6.3: extract type-expression to internal-type conversion helpers.
       - likely targets: `ty_from_annotation`, `ty_from_type_expr`, `ty_from_type_expr_with_holes`, `ty_from_name`, `is_type_variable_name`.
       - preferred destination: `typecheck_types.rs` or `typecheck_convert.rs`.
       - landed in `typecheck_types.rs`; downstream validation/build/effect phases now depend on that shared conversion module.
    - [x] Step F6.4: reduce `typecheck.rs` to orchestration + minimal phase contract definitions.
      - landed by moving phase-state structs and validation/checking sequencing helpers into `typecheck_phase.rs`.
   - Constraints:
     - keep stdlib/path validation isolated from pure type conversion.
     - avoid circular dependencies between `typecheck.rs`, `typecheck_build.rs`, and `typecheck_annotation.rs`.
   - Acceptance criteria:
     - `typecheck.rs` reads as orchestration plus phase wiring.
     - environment building and annotation parsing can be reviewed independently.
   - Progress note:
     - `typecheck_build.rs` now owns `build_type_env`, global-symbol insertion, import-backed symbol staging hookup, and constructor/effect symbol injection.
     - `typecheck.rs` now calls the build-phase module instead of carrying environment construction inline.
     - `typecheck_annotation.rs` now owns declaration/main annotation validation, `can`-clause parsing helpers, and declaration annotation shape helpers.
     - `typecheck_phase.rs` now owns validation/checking phase state and orchestration helpers; `typecheck.rs` is narrowed to public API, error type, and shared identifier predicate.

8. [ ] Milestone F7: decompose `crates/goby-core/src/typecheck_check.rs` by checking concern.
   - Goal:
     - stop `typecheck_check.rs` from becoming the new single-file black box for every semantic rule after phase extraction.
   - Why this milestone exists:
     - `typecheck_check.rs` now concentrates expression inference, resume validation, effect-usage checking, ambiguous-ref checks, branch consistency, substitution/unification, and type rendering.
   - Planned extraction order:
     - [x] Step F7.1: extract resume-context validation.
       - likely targets: `check_resume_in_stmts`, `check_resume_in_expr`, `infer_binding_ty_with_resume_context`, related local-env helpers.
       - preferred destination: `typecheck_resume.rs`.
       - landed in `typecheck_resume.rs`; resume validation and generic resume-substitution helpers now live outside `typecheck_check.rs`.
     - [x] Step F7.2: extract effect-usage and handler-coverage checking.
       - likely targets: `check_unhandled_effects_in_expr`, callee-required-effect checks, handler coverage helpers.
       - preferred destination: `typecheck_effect_usage.rs`.
       - landed in `typecheck_effect_usage.rs`; effect dispatch coverage and required-effect checks now sit outside `typecheck_check.rs`.
     - [x] Step F7.3: extract ambiguity and branch-consistency checking.
       - likely targets: `ensure_no_ambiguous_refs_in_expr`, `ensure_no_ambiguous_refs_in_stmts`, `check_branch_type_consistency_in_expr`, `check_branch_type_consistency_in_stmts`.
       - preferred destination: `typecheck_ambiguity.rs` and/or `typecheck_branch.rs`.
       - landed in `typecheck_ambiguity.rs` and `typecheck_branch.rs`; `typecheck_check.rs` now delegates these rule families instead of hosting them inline.
     - [x] Step F7.4: evaluate whether type substitution/unification deserves its own internal module.
       - likely targets: `TypeSubst` application/binding/unification/instantiation helpers.
       - preferred destination if worthwhile: `typecheck_unify.rs`.
       - landed in `typecheck_unify.rs`; resume/effect-usage phases now share explicit unification helpers instead of reusing `typecheck_resume.rs` as an accidental common layer.
     - [x] Step F7.5: keep `check_expr` / `check_body_stmts` as explicit top-level checking entrypoints over smaller helpers.
       - landed by reducing both entrypoints to orchestration over internal helper layers (`infer_expr_ty`, statement-sequence checking, return-type validation).
   - Constraints:
     - do not create opaque “semantic helpers” modules; each new file must have a specific rule family.
     - preserve diagnostic wording and current effect semantics unless a separate task intentionally changes them.
   - Acceptance criteria:
     - no single checking file becomes the default dump site for unrelated semantic rules.
     - rule families can be tested and reviewed independently.

9. [ ] Milestone F8: close the second maintainability pass with boundary hardening.
   - Goal:
     - ensure the second extraction wave leaves stable internal contracts instead of another temporary arrangement.
   - Planned work:
     - [x] Step F8.1: move statement/body checking and type rendering out of `typecheck_check.rs`.
       - landed in `typecheck_stmt.rs` and `typecheck_render.rs`.
       - `typecheck_check.rs` is now narrowed back toward expression inference and expression-adjacent helpers.
       - focused type-rendering regression tests now live beside `typecheck_render.rs`.
     - [x] Step F8.2: move `typecheck_check`-owned regression tests beside the owned module.
       - expression inference, tuple-member access, and list-spread regression tests now live in `typecheck_check.rs`.
       - `typecheck.rs` no longer hosts those module-specific checks as spillover tests.
     - [x] Step F8.3: move `goby-wasm` compile/native-lowering regression tests out of `lib.rs`.
       - landed in `crates/goby-wasm/src/compile_tests.rs`.
       - `lib.rs` now keeps runtime-parity/runtime-execution coverage instead of also acting as the codegen smoke-test host.
     - [x] Step F8.4: move runtime parity/perf helpers out of `goby-wasm/src/lib.rs`.
       - landed in `crates/goby-wasm/src/runtime_parity.rs`.
       - `lib.rs` runtime tests now depend on a dedicated parity helper layer instead of hosting mode-comparison infrastructure inline.
     - re-review file sizes and dependency directions after F5-F7.
     - move any new subsystem-specific tests beside owned modules.
     - remove temporary helpers introduced only to make moves compile.
     - update `doc/STATE.md` with the boundary decisions future refactors should preserve.
   - Acceptance criteria:
     - `goby-wasm/src/lib.rs`, `typecheck.rs`, and `typecheck_check.rs` are no longer the obvious default place for unrelated new logic.
     - Track F can be considered complete again without known “next god module” hotspots.

Change-unit policy:

1. One extraction step per commit where practical.
2. If a move also changes behavior, split it into:
   - contract-test patch,
   - move/refactor patch,
   - behavior-change patch.
3. If a file move causes broad test breakage across unrelated subsystems, stop and narrow the seam before continuing.

Quality gates for each milestone:

1. `cargo fmt`
2. `cargo check`
3. Focused tests for the touched subsystem:
   - `cargo test -p goby-wasm` for F1
   - `cargo test -p goby-core` for F2/F3
4. Broad regression before closing a milestone:
   - `cargo test --workspace`

Plan update rule during execution:

1. If an extraction reveals a tighter seam than planned, update this Track F section before continuing.
2. If a proposed module split introduces bidirectional dependencies, revise the boundary and note the reason in `doc/STATE.md`.
3. If a milestone stalls after two narrowing attempts, pause implementation and rewrite the milestone into smaller steps here.

Definition of done for this track:

- No single core implementation file should continue growing as the default place
  for unrelated new features.
- `compile_module`, `parse_module`, and `typecheck_module_with_context` remain as
  orchestration entrypoints, but most policy/mechanics live behind smaller modules.
- Newly extracted subsystems have direct unit tests that do not require traversing
  the full compiler/runtime stack.
- `doc/STATE.md` captures any internal boundary decisions that future refactors
  should preserve.
- Remaining high-risk concentration points identified by design review
  (`goby-wasm/src/lib.rs`, `typecheck.rs`, `typecheck_check.rs`) have been either
  split or intentionally justified as stable boundaries.

Non-goals:

- large semantic rewrites under the banner of cleanup,
- replacing working regression suites with only smaller unit tests,
- splitting files without reducing conceptual coupling.

Plan review:

- Review result: proceed, with sequencing constraints.
- Findings:
  - starting with `goby-wasm/src/lib.rs` is the right first move because it has the widest current boundary and can be reduced without changing language semantics.
  - typechecker extraction must keep stdlib/path resolution separate from pure checking; otherwise the refactor only renames the same coupling.
  - parser extraction should come after typechecker, not before, because parser tests are numerous and currently provide a stronger safety net than the typechecker's internal module boundaries.
  - duplication cleanup should stay late; doing it early would mix architectural movement with semantic edits.
- Risks to watch:
  - cyclic dependencies between newly extracted internal modules,
  - test relocation that accidentally drops regression coverage,
  - "utility" modules becoming new dumping grounds.
- Review verdict:
  - plan is acceptable if milestones F1-F4 are executed incrementally with the listed quality gates and the change-unit policy above.

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
