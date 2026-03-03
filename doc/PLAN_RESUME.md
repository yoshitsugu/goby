# Goby Resume Implementation Plan

Status: Draft (research-backed execution plan)
Owner: Goby core/runtime track
Last updated: 2026-03-02

## 1. Purpose

This document defines a concrete implementation plan for adding `resume` to
Goby's algebraic effects.

Primary goal:

- Enable effect handlers to capture and resume the delimited continuation,
  so features like iterator-style user-space abstractions can be built without
  adding many new intrinsics.

Secondary goal:

- Keep alignment with already-locked direction in `doc/PLAN.md`:
  deep handlers, one-shot resumptions, selective CPS/evidence passing, and
  eventual compiled `EffectId`/`OpId` dispatch.

## 2. Current Baseline (as of 2026-03-02)

Current implementation characteristics:

- Parser/AST/typechecker support `effect`, `handler`, and `using`.
- Runtime dispatch in `goby-wasm` uses handler-name/effect-name lookup and
  executes handler method bodies directly.
- There is no continuation capture object and no `resume` syntax/semantics.
- Bare-name dispatch currently depends on deterministic `BTreeMap` ordering
  (explicitly temporary in `doc/PLAN.md`).

Implication:

- Current handlers are effectively "interceptors" but not full algebraic
  handlers with resumable continuations.

## 3. Research Survey and Design Implications

### 3.1 OCaml 5 effect handlers (production runtime)

Reference:

- OCaml 5 reference manual, section 12.24 (`Effect.Deep`)
  <https://caml.inria.fr/pub/distrib/ocaml-5.0/ocaml-5.0-refman.html#sec281>

Key points:

- Continuations are one-shot by default (`continue` on an already-resumed
  continuation raises).
- Discontinuation (`discontinue`) is a first-class control path.
- Operational model is practical in a production compiler/runtime.

Implication for Goby:

- Keep one-shot as baseline semantics.
- Enforce linear-use of captured continuation at runtime (hard error on second
  resume).

### 3.2 Koka (language + implementation strategy)

References:

- Koka language docs (`with` handlers and `resume`)
  <https://koka-lang.github.io/koka/doc/index.html>
- "Generalized Evidence Passing for Effect Handlers"
  <https://www.microsoft.com/en-us/research/publication/generalized-evidence-passing-for-effect-handlers-or-efficient-compilation-of-effect-handlers-to-c/>

Key points:

- `resume` is central to handler operation clauses.
- Evidence-passing translation is a practical compilation strategy that avoids
  full CPS everywhere.

Implication for Goby:

- Add `resume` in source semantics first.
- Stage compilation toward selective CPS/evidence passing instead of global
  CPS conversion.

### 3.3 Wasm backend direction (typed continuations)

References:

- WasmFX explainer repository
  <https://github.com/wasmfx/oopsla23-artifx>
- "Continuing WebAssembly with Effect Handlers" (ICFP 2024)
  <https://sigplan.org/OpenTOC/icfp24.html#continuing-webassembly-with-effect-handlers>

Key points:

- Typed continuations and stack switching provide a direct-style fast path.
- Current portable strategy still needs fallback/trampoline style for engines
  without continuation features.

Implication for Goby:

- Phase the runtime: portable continuation objects first, optional WasmFX-based
  optimization later.

## 4. Semantics to Lock for Goby `resume`

1. Handler style: deep handlers.
2. Continuation policy: one-shot only (multi-shot deferred).
3. Scope: `resume` is valid only inside handler method bodies.
4. Control behavior:
   - If handler calls `resume v`, execution continues from the effect site with
     operation result `v`.
   - If handler does not call `resume`, captured continuation is discarded
     (abortive handling path).
5. Selection rule: nearest enclosing handler for the effect wins
   (lexical stack discipline, not alphabetical map order).

## 5. Surface Syntax Plan

Phase-1 syntax (minimal, explicit keyword):

```goby
effect Iter
  yield : String -> Unit

handler Collect for Iter
  yield item =
    # user logic
    resume Unit
```

Parser/AST changes:

- Add `Expr::Resume { value: Box<Expr> }`.
- Parse `resume <expr>` as a dedicated expression form.
- Reserve `resume` keyword (disallow top-level declaration name `resume` and
  handler parameter named `resume`).

Rationale:

- Keeps user-facing semantics direct.
- Avoids encoding continuation as an implicit extra parameter in source syntax.

## 6. Type System Plan

MVP typing rule for `resume`:

- For an operation signature `op : A -> B`, inside its handler method body:
  - handler parameter has type `A`.
  - `resume` consumes type `B`.
  - `resume x` expression has the enclosing computation result type `R`.

Implementation strategy (incremental):

1. Add handler-method type environment that knows the currently handled op and
   its `(A, B)` signature.
2. Typecheck `resume arg`:
   - reject outside handler body,
   - require `arg : B`,
   - return `Ty::Unknown` in first pass if `R` inference is not ready.
3. Add dedicated diagnostics:
   - `resume_outside_handler`,
   - `resume_arg_type_mismatch`,
   - `resume_in_unknown_operation_context`.

One-shot static checks (initial conservative rule):

- First implementation: runtime linearity guard only.
- Later tightening: add a syntactic check for obvious multi-resume in the same
  handler method body.

## 7. Runtime Architecture Plan

### 7.1 Runtime data structures

Introduce continuation/runtime frame structures in `goby-wasm` evaluator path:

- `HandlerFrame { effect_id, handler_decl_idx, parent }`
- `Continuation { frames, handler_stack_snapshot, consumed: bool }`
- `ResumeToken` bound in handler execution context.

### 7.2 Dispatch flow

On effect operation call:

1. Resolve operation to `(EffectId, OpId)`.
2. Walk active handler stack to nearest matching handler frame.
3. Capture delimited continuation from call-site to handler boundary.
4. Execute handler method with access to current `ResumeToken`.
5. On `resume v`:
   - fail if `consumed == true`,
   - mark consumed,
   - reinstate captured continuation and feed `v` as operation result.

### 7.3 Compatibility constraints

- Preserve existing non-effect expression behavior.
- Keep current recursion-depth guard (`MAX_EVAL_DEPTH`) until trampoline is
  introduced.
- During migration, keep old dispatch path behind a temporary feature gate to
  ease regression isolation.

## 8. Step-by-Step Execution Plan

Step 0: Spec lock and parser contract — DONE (2026-03-02)

- Syntax/keyword decision locked in this document and `doc/PLAN.md`.
- Parser contract tests added:
  - reject top-level declaration name `resume`,
  - reject handler parameter name `resume`,
  - accept handler-body `resume Unit` shape (currently parsed as call form before Step 1 AST change).

Step 1: AST + parser implementation — DONE (2026-03-02)

- Added `Expr::Resume { value }` and parser support for `resume <expr>`.
- Added parse diagnostics for malformed `resume` expressions (`resume` without argument).

Step 2: Typecheck integration — DONE (2026-03-02)

- Track handler method operation context.
- Added `resume` typing diagnostics:
  - `resume_outside_handler`,
  - `resume_arg_type_mismatch`,
  - `resume_in_unknown_operation_context`.
- Added regression tests for success and failure cases.

Step 3: Runtime continuation object (interpreter path) — DONE (2026-03-02)

- Added continuation carrier structures (`HandlerFrame`, `Continuation`, `ResumeToken`) in
  `goby-wasm` interpreter path.
- Implemented one-shot consume guard for `resume`.
- Routed value-position bare effect calls through handler dispatch before Int/List fast paths,
  enabling `resume` return values to flow back to call sites.
- Added runtime error surfacing for illegal resume usage:
  - `resume used without an active continuation`,
  - `resume continuation already consumed`,
  - internal token-stack mismatch guard.
- Runtime now reinstalls captured handler snapshot from continuation frames on `resume`.

Step 4: Nearest-handler stack semantics — DONE (2026-03-02)

- Replaced alphabetical fallback semantics in runtime dispatch path.
- Runtime handler lookup now walks active handlers in lexical stack order (nearest first).
- Ensured lexical nesting controls operation capture for both bare and qualified calls.

Step 5: Validation and regression stabilization — DONE (2026-03-02)

- Add focused tests:
  - single resume success,
  - no-resume abortive behavior,
  - double resume runtime error,
  - nested `using` nearest-handler selection.
- Added focused runtime tests in `goby-wasm` for all items above.
- Quality gates completed:
  - `cargo check`
  - `cargo test`
  - `cargo clippy -- -D warnings`

Step 6: Stdlib enablement tasks (consumer track) — DONE (2026-03-02)

- Iteration API direction (resume-based) documented:
  - keep iterator-style producer API on effects/handlers (`yield` + `resume`) as
    the first-class pattern,
  - maintain `examples/iterator.gb` as the lock sample for this shape.
- Coverage updates:
  - added runtime lock test for `examples/iterator.gb` output,
  - included `examples/iterator.gb` in `goby-core` examples typecheck suite.
- `__goby_*` intrinsic re-evaluation result (current codebase):
  - no `__goby_*` intrinsic names are currently implemented or consumed by
    compiler/runtime code paths,
  - keep intrinsic naming as a deferred standard-library bridge convention
    (tracked in `doc/PLAN_STANDARD_LIBRARY.md`, ExtraStep B),
  - immediate runtime bridge priorities remain:
    - env access boundary (currently served by builtin/runtime path),
    - string length parity gap in stdlib placeholder implementation.

Step 7: Lowering optimization track (post-correctness)

- Objective:
  - introduce selective CPS/evidence passing only on effectful boundaries,
  - keep pure/no-effect paths in direct style.

Step 7.1: Introduce execution-style planning metadata — DONE (2026-03-02)

- Add a lowering-planning pass that classifies declarations into:
  - `DirectStyle` (pure path candidates),
  - `EffectBoundary` (requires handler evidence / continuation plumbing).
- Initial boundary signals:
  - explicit `can <Effect>` requirements in type annotation,
  - `using` statements in declaration body,
  - `resume` usage in declaration/handler context.
- Propagate classification transitively over declaration call graph:
  - if `f` calls `g` and `g` is `EffectBoundary`, mark `f` as `EffectBoundary`.
- Implemented in `goby-wasm` as planning metadata (`LoweringPlan`) with tests for:
  - pure declaration classification,
  - `can` / `using` / `resume` boundary signals,
  - transitive caller propagation,
  - handler-resume presence marker.

Step 7.2: Define evidence payload shape (internal IR-level contract) — DONE (2026-03-02)

- Introduce internal representation for handler evidence:
  - active handler stack snapshot/evidence pointer,
  - operation dispatch tokens (`EffectId`, `OpId` placeholders until full ID intern pass).
- Keep this internal-only (no source syntax change in Step 7).
- Implemented in `goby-wasm` planning metadata:
  - `EffectId`, `OpId`, `EffectOperationRef`,
  - per-declaration `DeclarationEvidenceRequirement`,
  - module-level `EvidencePayloadShape` (operation table + declaration requirements).
- Evidence requirements currently include:
  - required effects from `can` clauses (mapped to `EffectId` where declared),
  - referenced effect operations (qualified + bare-name candidates) mapped to
    `(EffectId, OpId)` entries.

Step 7.3: Direct-style lowerer path stabilization — DONE (2026-03-02)

- Keep existing native lowerer behavior for `DirectStyle` declarations.
- Ensure unsupported effectful constructs do not regress pure-path codegen:
  - direct-style subset remains bytecode-compatible with current tests.
- Implemented direct-style gating inside native evaluator/lowerer call paths:
  - declaration callables are only materialized when the target declaration is `DirectStyle`,
  - named function evaluation bails out for `EffectBoundary` declarations.
- Added lowerer-focused regression tests:
  - call-graph with `can` declaration causes native lowerer to return `None`,
  - pure direct-style declaration call still lowers natively.

Step 7.4: Effect-boundary lowering skeleton (no full feature parity yet) — DONE (2026-03-02)

- Add explicit handoff points where `EffectBoundary` lowering will inject:
  - continuation capture/re-entry hooks,
  - evidence passing through calls crossing effect boundaries.
- In this phase, handoff points may still route to fallback runtime for execution,
  but boundaries must be explicit and testable in lowering decisions.
- Implemented explicit lower-path result contract in `goby-wasm`:
  - `NativeLoweringResult::{Emitted, EffectBoundaryHandoff, NotLowered}`,
  - `EffectBoundaryHandoff` includes boundary metadata
    (`main_style`, handler-resume marker, evidence shape summary fields).
- `compile_module` now evaluates this result and uses an explicit handoff branch
  for `EffectBoundaryHandoff` before fallback runtime execution.
- Added regression coverage to assert effect-boundary modules return
  `EffectBoundaryHandoff` via lowerer entry API.

Step 7.5: Regression and observability hooks — DONE (2026-03-02)

- Add focused tests for planning correctness:
  - pure declaration remains `DirectStyle`,
  - declaration with `can`/`using`/`resume` becomes `EffectBoundary`,
  - transitive propagation marks callers correctly.
- Add diagnostics/dev hooks (internal) to inspect selected lowering mode per declaration.
- Added planning regression tests for:
  - multi-hop transitive boundary propagation (`main -> mid -> fx(can ...)`),
  - deterministic declaration-mode snapshot exposure.
- Added internal observability hook:
  - `LoweringPlan::declaration_lowering_modes() -> Vec<DeclarationLoweringMode>`
    (name-sorted snapshot).
- Extended Step 7.4 handoff payload with declaration-mode snapshot so callers can
  inspect lowering mode selection per declaration at runtime handoff points.

Step 7.6: Step-7 completion criteria

- Planning metadata and boundary classification are implemented and covered by tests.
- Pure-path native lowering remains green.
- Effect-boundary handoff points exist in lowering pipeline (even if final CPS runtime wiring is Step 8+).
- Quality gates pass:
  - `cargo check`
  - `cargo test`
  - `cargo clippy -- -D warnings`

Step 8: Wasm advanced path (optional)

- Investigate typed continuation optimization path (WasmFX-capable engines).
- Keep portable fallback as baseline.

## 9. Test Matrix (minimum)

Parser tests:

- `resume` inside handler method parses.
- `resume` at top-level/user function fails.

Typechecker tests:

- operation return type and `resume` argument mismatch is rejected.
- `resume` outside operation context is rejected.

Runtime tests:

- one-shot resumption works exactly once.
- second resume on same continuation fails deterministically.
- nested handlers dispatch to nearest handler.

End-to-end tests:

- iterator-like sample built with effect+resume executes as expected.

## 10. Risks and Mitigations

Risk: continuation capture complexity causes evaluator instability.

- Mitigation: land parser/typecheck first; runtime behind feature gate;
  keep regression suite green at each step.

Risk: type inference around `resume` result type `R` becomes complex early.

- Mitigation: staged typing (`Ty::Unknown` bridge), then tighten once runtime
  behavior is stable.

Risk: mismatch between interpreter-path semantics and future native Wasm.

- Mitigation: define runtime-invariant tests that must pass in both paths.

## 11. Open Questions

1. Do we expose explicit `discontinue` in syntax, or keep abortive behavior as
   "not calling `resume`" in the first phase?
2. Should `resume` return type participate in full local inference immediately,
   or after runtime stabilization?
3. How aggressively should we reject potentially multi-shot patterns statically
   in phase 1?
