# Goby Language Plan (Draft)

This document tracks:

- what is already visible in the current draft,
- what is fixed for MVP,
- what remains open or deferred.

Notes:

- `doc/LANGUAGE_SPEC.md` is the source of truth for current language
  syntax and semantics.
- `PLAN.md` is the top-level roadmap and execution-planning document.
- `doc/PLAN_IR.md` is the active detailed plan for IR-lowering completion and is
  the current highest-priority execution track.
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

Post-MVP work focuses on reducing fallback-runtime special cases and making
execution paths more predictable.

Priority rule:

- top priority is now IR-lowering completion and IR-boundary redesign work tracked in
  `doc/PLAN_IR.md`.
- active backend/runtime work should prefer unblocking itself by improving shared IR and
  AST-to-IR lowering rather than by adding more source-shape-specific recognizers.
- when there is tension between a local unblock and the long-term IR architecture,
  choose the option that improves or preserves the long-term IR architecture.
- progress for this top-priority track is tracked by the checkbox milestones in
  `doc/PLAN_IR.md`; when a milestone is reached, update its checkbox in the same change.

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
specialized active plans; this section keeps only decision-level summaries.

### 4.2 Closed Tracks Archive

Tracks A/B/C are closed. Detailed records were removed from this plan file and
are retained in git history.

### 4.3 Active Track D: Developer Tooling Foundation

Most of Track D is complete. Only still-relevant follow-up items are kept here.

#### Phase D5: `goby lint` — high-signal static checks

Goal: machine-readable linter output for common mistakes not caught by the typechecker.

Lint rules are ordered by ascending analysis cost. The cheapest infrastructure
comes first to unblock the framework early; user-value ranking is noted in
parentheses for prioritization.

1. **Unreachable `case` arm**: wildcard `_` arm followed by more arms (implemented).
   User value: medium — catches subtle logic errors in pattern matching.
2. **Unused local binding**: `x = expr` where `x` is never referenced afterward
   (needs local-use tracking across `Expr`/`Stmt` spans from D1a).
   User value: **high** — most frequently encountered lint in practice; catches typos and dead code.
   Note: despite higher analysis cost than rule 1, consider implementing this early if the
   lint framework from rule 1 is already in place, because it delivers the most user value.
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

#### Phase D6 follow-ups still worth keeping

- **D6c: Shared grammar asset**
  - Extract shared language-definition data for the VS Code grammar and Neovim/Vim syntax files.
  - Keep this as the remaining editor-tooling refactor slice.
- **D6b-ts: Tree-sitter grammar**
  - Defer until after D6c.
  - Keep as optional editor tooling follow-up rather than active architecture work.

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

### 4.6 Active Track F: General Wasm Lowering for Runtime `Read` Programs

Goal: replace runtime-I/O shape matching (`Echo`, `SplitLinesEach`, etc.) with a
general lowering path that can compile effectful `main` bodies from IR/owned runtime
representation to Wasm without enumerating per-program patterns.

Dependency / priority note:

- Track F now depends on the IR-lowering plan in `doc/PLAN_IR.md` and must follow it as
  the higher-priority architecture source for front-end/shared-IR boundary decisions.
- if Track F work discovers that a language construct is blocked primarily by AST-to-IR
  lowering, the default action is to fix the IR boundary rather than add another Wasm-side
  recognizer or fallback classification.

Why this is a separate track:

- current `goby run` support for `Read` depends on handwritten `DynamicWasiIo` plan shapes.
- IR classification has improved program understanding, but it does not yet imply a general
  Wasm execution engine for effectful programs.
- continuing to add one-off runtime-I/O patterns would create architecture debt in exactly the
  area where Goby most needs a coherent long-term execution model.
- this project is currently optimized for long-term design quality over short-term compatibility;
  temporary breakage is acceptable if it buys a cleaner execution architecture.

Locked direction for this track:

- prefer one general runtime-lowering architecture over further growth of ad-hoc
  shape-specific Wasm emitters.
- do not treat `InterpreterBridge` revival as the target architecture; if used at all,
  it is only a temporary development aid.
- new runtime `Read` support should, by default, advance the general lowering pipeline rather
  than add new special cases to `runtime_io_plan.rs`.
- keep `runtime_io_plan.rs` as a temporary compatibility/planning layer and plan to shrink or
  remove it once the general lowering path is viable.

Concrete user-visible problem this track is meant to solve:

- programs such as `text = read(); lines = split text "\n"; println(lines[1])`
  should eventually run because the backend can execute the program semantics generally,
  not because a dedicated recognizer was added for that exact AST shape.

Milestones:

- [x] F1. Runtime-lowering architecture lock
  - choose the single source IR/owned runtime representation that general Wasm lowering will consume.
  - choose the lowering pipeline layering explicitly.
    - preferred default: `Goby IR -> backend IR -> Wasm emitter`.
    - do not collapse directly from high-level Goby IR into raw `wasm_encoder` calls unless the
      architecture note explains why a backend IR would be a net loss.
  - define the minimum executable surface for phase 1 of general lowering:
    - locals/bindings,
    - strings/ints/unit,
    - lists needed by stdlib `split`,
    - direct calls,
    - effect ops for `Print` and `Read`,
    - indexing / simple list access if supported by the chosen IR.
  - write down the boundary between:
    - native direct-style lowering for the "fully compiled" subset, and
    - general runtime-lowered Wasm execution for effectful/dynamic programs.
  - explicitly decide whether the backend targets:
    - a low-level VM-like instruction stream hosted in Wasm memory, or
    - direct lowering from Goby IR to Wasm control/data flow.
  - produce a durable architecture note (initially landed as `doc/wasm_runtime_architecture.md`;
    after Track F completion the maintained reference may move into code-adjacent module docs) that fixes:
    - pipeline layers and the responsibility of each layer,
    - lowering input format,
    - backend IR shape, if one is used,
    - runtime helper responsibility boundaries,
    - value representation boundaries,
    - call/effect lowering boundaries,
    - module ownership boundaries,
    - test strategy by layer,
    - what remains in handwritten fast paths vs what must move into the general path.
  - define module ownership before implementation spreads:
    - lowering/planning,
    - backend IR definitions,
    - value/layout representation,
    - helper ABI,
    - Wasm emission,
    - parity/integration tests.
  - lock the invariants that all later implementation must preserve:
    - panic-free lowering for user-controlled programs in normal compiler flows,
    - explicit error paths for unsupported backend states,
    - stable ownership/lifetime rules for runtime values,
    - one semantic source of truth for runtime behavior even if optimized fast paths remain.
  - done when:
    - one documented architecture is chosen and competing approaches are rejected with reasons.
    - the architecture document is detailed enough that F2/F3 work does not need to invent new backend boundaries.
    - module ownership and invariants are documented well enough that new code does not default into `lib.rs`
      or another catch-all file.
  - required validation artifacts:
    - architecture decision note committed in the repo.
    - a short rejected-alternatives section explaining why the non-chosen design is worse for Goby.
    - a module ownership table for the new/general backend path.
    - an invariant checklist that later PRs/slices can validate against.
  - representative programs that must be explained by the chosen architecture:
    - `print (read())`
    - `text = read(); lines = split text "\n"; each lines println`
    - `text = read(); lines = split text "\n"; println(lines[1])`

- [x] F2. Runtime value and memory model
  - define Wasm-side runtime representations with extensibility in mind rather than only for one workflow.
  - define representations for:
    - `Unit`,
    - `Int`,
    - `String`,
    - `List String` at minimum.
  - make the representation strategy general enough to extend to future dynamic values
    without redesigning the whole backend surface.
  - define ownership/lifetime rules for strings and derived values from `Read.read` and `string.split`.
  - lock the ABI between lowered code and runtime helper functions, if helpers are used.
  - document the minimum dynamic runtime data model needed by the first execution slice:
    - owned string buffers,
    - `List String`,
    - list length and element access,
    - error/trap strategy for invalid indexing.
  - decide whether runtime values are represented as:
    - typed handles into heap-managed storage,
    - tagged immediate/boxed unions,
    - or another explicit scheme.
  - describe how the chosen representation keeps future additions (`List Int`, tuples, records,
    effect payloads) from forcing an ABI reset.
  - done when:
    - memory layout is documented well enough that backend work does not need to invent formats ad hoc.
    - the indexing and allocation rules for `List String` are explicit.
    - the representation story is credible beyond the initial `split` use case.
  - required validation artifacts:
    - memory/value model section landed in the architecture doc or a linked design doc.
    - at least one backend-facing test/spec fixture describing expected runtime representation behavior.
    - explicit encode/decode or layout invariant tests for the chosen representation.

- [x] F3. General runtime execution core in Wasm
  - implement general lowering/execution for:
    - local bindings,
    - variable lookup,
    - `Read.read`,
    - `Print.print` / `Print.println`.
  - ensure stdin/stdout behavior is runtime-host driven, not compile-time collapsed, for `Read` programs.
  - keep this milestone scoped to dynamic scalar execution without requiring list-producing stdlib calls.
  - done when:
    - simple dynamic programs no longer need `DynamicWasiIo(Echo)` special handling to run.
    - at least the following representative programs execute through the general path:
      - `print (read())`
      - `text = read(); println(text)`
  - required validation artifacts:
    - regression tests proving the above programs do not route through shape-specific `RuntimeIoPlan` lowering.
    - unit tests for backend IR lowering or execution-step invariants, if backend IR is used.
    - tests covering explicit error returns instead of panics for unsupported lowering states.

- [x] F4. Dynamic runtime values and helper-call boundary
  - add general runtime execution support for direct calls to supported stdlib/runtime helpers.
  - first required target is the helper boundary needed by `goby/string.split`.
  - ensure the chosen helper-call model matches the F1 architecture decision rather than introducing a parallel execution path.
  - done when:
    - list-producing helper calls can return dynamic runtime values consumed by later general-lowered code.
    - the representative program `text = read(); lines = split text "\n"; each lines println`
      runs through the general lowering path.
  - required validation artifacts:
    - regression tests proving helper-returned runtime values survive local binding and subsequent use.
    - helper ABI contract tests that fail clearly when call signatures/layout assumptions drift.

- [x] F5. Dynamic collections and indexing
  - add Wasm execution support for:
    - `goby/string.split` producing a runtime list value,
    - list indexing,
    - the error/diagnostic policy for out-of-range access.
  - keep collection behavior aligned with the runtime/interpreter semantics.
  - done when:
    - representative programs like `println(lines[1])` after `split` run through the general lowering path.
  - required validation artifacts:
    - positive regression:
      - `text = read(); lines = split text "\n"; println(lines[1])`
    - negative regression:
      - out-of-range index behavior is tested and matches the chosen policy.
    - unit tests for collection/indexing layout invariants where possible, not only end-to-end tests.

- [ ] F6. Convergence and deletion of special cases
  - route currently special-cased runtime-I/O programs through the new general lowering path.
  - keep specialized fast paths only if they remain a strict optimization layer on top of the same semantics.
  - remove or sharply reduce `RuntimeIoPlan`-specific handwritten emitters once parity is proven.
  - done when:
    - runtime I/O support is explained primarily in terms of the general lowering architecture,
      not a catalog of recognized shapes.
    - any remaining handwritten fast path is documented explicitly as an optimization, not as the semantic source of truth.
    - plain `Echo` / `SplitLinesEach`-style runtime-I/O programs no longer require `RuntimeIoPlan`
      classification in the normal `compile_module` path.
  - required validation artifacts:
    - parity tests for old shape-specific programs now passing through the general path.
    - deletion or deprecation plan for the remaining `RuntimeIoPlan` machinery is committed.
    - if specialized fast paths remain, a documented proof obligation that they share the same
      semantic source of truth and ABI as the general path.
    - basic size/perf guardrails for representative programs so helper explosion or accidental
      Wasm bloat is noticed during follow-up work.

Tracking rule:

- when a milestone is reached, update this checklist in `doc/PLAN.md` by changing its checkbox from
  `[ ]` to `[x]` in the same change that lands the milestone outcome.
- do not check a milestone based only on design discussion or partial code; the required validation
  artifacts and representative programs for that milestone must land in the same slice.
- if a milestone is too large to complete cleanly, split it into child milestones in `doc/PLAN.md`
  before continuing implementation.
- prefer a test pyramid over end-to-end-only validation:
  - unit tests for invariants and helper/layout contracts,
  - focused backend/lowering tests,
  - parity tests against the runtime/interpreter,
  - end-to-end CLI/Wasm execution tests.

Design constraints:

- semantics must stay aligned between:
  - typechecked source meaning,
  - fallback/interpreter behavior used during development,
  - generated Wasm behavior.
- avoid embedding more stdlib semantics into recognizers when those semantics should live in
  ordinary lowered execution.
- optimize for an execution model that can naturally extend to later features
  (effects/handlers, richer collections, additional stdlib functions), even if the first
  implementation slice is narrow.
- prefer deleting temporary machinery over institutionalizing it.

Near-term implementation note:

- if a short-term unblock is unavoidable, the least-bad stopgap is a clearly temporary bridge
  that executes general runtime-read programs without pretending they are fully lowered.
- however, do not let that bridge become the reason to defer Phase F1-F4 architecture work.

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
