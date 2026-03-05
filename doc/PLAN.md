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
- Effects and handlers are available in MVP runtime via `effect`, `handler`, `with`, and `with_handler`.
  - handler dispatch follows lexical nearest-handler stack order.
  - `can` clauses are validated (declared effect or built-in effect names).
- Canonical handler application syntax is `with <handler_expr> in <body>` and
  `with_handler ... in ...` (inline sugar).
- Legacy syntax (`handler ... for ...`, `using`) is no longer accepted by the
  language parser as of 2026-03-04.
  - CLI commands (`check` / `run`) therefore reject such sources by default.
- MVP built-ins: `print`, `map`, `fetch_env_var`, `string.split`, `list.join`.
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
    (`with` / `with_handler`).
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
  - canonical application is `with <handler> in ...`,
  - `with_handler ... in ...` is inline sugar.
- Legacy forms (`handler ... for ...`, `using`) are fully removed from parser/runtime/typecheck paths.
- `resume` support is active with one-shot continuation guardrails:
  - outside-handler `resume` is rejected,
  - type mismatch for resumed value is rejected,
  - obvious multi-resume misuse is rejected conservatively.
- Runtime dispatch semantics:
  - nearest lexical handler wins,
  - embedded/default-handler fallback applies only when no explicit handler captures the operation.
- Post-MVP follow-up remains:
  - improve precision of multi-resume analysis,
  - continue migration from name-based runtime dispatch to compiled operation identity (`EffectId`/`OpId`),
  - evaluate explicit `discontinue` only as a later separate proposal.

#### Planned Syntax Simplification: `with` Unification

Status: planned (not yet implemented)

Goal: remove `with_handler` and use only `with`.

- Target syntax:
  - inline handler form:
    - `with`
    - indented handler clauses
    - `in`
    - body block
  - handler value form: `with <handler_expr> in <body>`
- Scope and placement rules (target):
  - `with` statement form remains supported.
  - multiline RHS `with` is supported for bindings/assignments to preserve existing
    iterator-style code patterns currently written with multiline `with_handler`.
  - parser entry points for multiline RHS handling must treat `with` the same way as
    current multiline `case` / `if` handling.
- Parser disambiguation rule:
  - if the statement line is exactly `with`, parse inline handler clauses from the next indented block.
  - if the statement line starts with `with `, parse the remainder as `<handler_expr>`.
  - do not rely on fixed token-count lookahead before `->` (handler clauses allow variable arity).
- Compatibility/migration policy:
  - remove `with_handler` parser support directly (no warning mode).
  - parse error should point users to `with` inline form.
  - remove `with_handler` from reserved keyword/docs/typecheck diagnostics wording in the
    same change set to avoid mixed guidance.
- Planned update sequence:
  - 1) parser:
    - add `with` exact-line inline-handler branch,
    - extend multiline RHS parser path to support `with` (in addition to current `case`/`if`),
    - remove `with_handler` parse branch.
  - 2) typecheck/diagnostics:
    - replace user-facing `with`/`with_handler` guidance with `with`-only guidance.
  - 3) language/docs cleanup:
    - `doc/LANGUAGE_SPEC.md`: remove `with_handler` from reserved tokens and handler syntax section.
    - `doc/PLAN.md`/`doc/STATE.md`: mark completion and remove transitional wording.
  - 4) examples/tests migration:
    - migrate all `examples/*.gb` from `with_handler` to `with`.
    - update parser/typecheck/CLI tests to assert `with_handler` rejection.
  - 5) quality gate:
    - run `cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings`.
- Migration scale note:
  - repository currently contains many `with_handler` references (including tests/docs/examples),
    so migration should be landed as one coherent change to avoid partial-state breakage.

Note: detailed step-by-step renewal history is intentionally omitted here; use
`doc/STATE.md` and git history for chronological implementation records.


### 2.4 Standard Library Surface (MVP)

- Core modules to ship first (`Int`, `String`, `List`, `Env`) — minimal built-ins implemented.
- Naming conventions for stdlib functions — established.
- Minimal collection API for immutable workflows — deferred.

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

### 4.5 Parking Lot (Needs Revalidation Before Implementation)

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
