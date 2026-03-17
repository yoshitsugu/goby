# Goby IR Architecture Plan

Status: proposed  
Last updated: 2026-03-18

## 1. Purpose

This document defines the long-horizon plan for introducing a real compiler
intermediate representation (IR) between the parsed/typechecked Goby AST and
backend-specific lowering.

The immediate trigger is the current `runtime_io_plan.rs` design in
`crates/goby-wasm`, which classifies supported `Read` / `Print` programs by
matching source-level AST shapes. That approach was useful as a short-term
bootstrap mechanism, but it does not scale:

- semantically equivalent programs are treated differently because they are
  written with different AST shapes,
- backend policy leaks into source-shape detection,
- effectful execution paths are split between ad-hoc pattern matching and the
  fallback interpreter,
- every new supported form risks becoming another one-off classifier branch.

The goal of this plan is to replace that architecture with a backend-independent
IR pipeline.

## 2. Design Intent

### 2.1 Primary Goal

Introduce a typed effect-aware IR that becomes the canonical handoff boundary:

- parser builds AST,
- typechecker validates AST and enriches typed information,
- AST lowers into IR,
- backends consume IR rather than raw AST shape,
- unsupported cases fail at the IR-lowering or IR-backend boundary, not through
  source-shape-specific special cases.

### 2.2 Non-Goals

This plan does not require:

- immediate optimization passes,
- SSA form in the first version,
- a fully generic multi-backend compiler in the first implementation slice,
- preserving the current `runtime_io_plan` architecture for compatibility.

Breaking internal architecture is acceptable if the replacement boundary is
cleaner and more future-proof.

## 3. Problem Statement

Today, Goby has at least three execution-oriented representations:

1. parsed AST in `goby-core`,
2. ad-hoc Wasm-side planning structures such as `RuntimeIoPlan`,
3. interpreter/runtime-specific execution logic in `goby-wasm`.

This causes structural duplication:

- some decisions are made from syntax shape,
- some decisions are made from runtime behavior,
- some decisions are made from type/effect evidence,
- and these layers do not share one canonical program model.

The result is especially visible in runtime I/O:

- `print(read())` can be lowered,
- `text = read(); print text` can be lowered,
- `text = read(); decorated = "${text}!"; print decorated` is rejected,
- even though these programs differ only by a pure intermediate transformation.

This is a sign that the compiler is recognizing syntax patterns rather than
program meaning.

## 4. Target Architecture

### 4.1 Pipeline

The intended pipeline is:

1. Parse source into AST.
2. Typecheck and effect-check AST.
3. Lower typed AST into a canonical IR.
4. Run a small number of mandatory normalization passes on IR.
5. Lower normalized IR into a backend-specific plan or directly into backend code.

The key architectural rule:

- backend selection and lowering must depend on IR semantics, not on source AST
  surface shape.

### 4.2 IR Layers

The recommended long-term split is:

1. `Typed AST`
   - Current Goby AST plus type/effect knowledge known after checking.
   - Closest to source.
   - This is a conceptual layer, not necessarily a separately persisted data
     structure that must survive long-term as a public compiler boundary.
2. `Core IR`
   - Canonical effect-aware expression/statement representation.
   - Source sugar removed.
   - Pure enough to reason about control flow and effects explicitly.
3. `Backend IR` or backend plan
   - Optional.
   - Used only where a backend needs a more operational representation
     (for example Wasm-specific control or memory planning).

The first implementation step may merge (2) and (3) for practicality, but the
ownership boundary should still be designed as if `Core IR` is backend-neutral.

### 4.3 Architectural Constraint

The shared IR must become the semantic handoff not only for native Wasm
lowering, but eventually for the portable fallback path as well.

Short-term duplication during migration is acceptable, but the end state should
not be:

- AST interpreted directly for fallback execution,
- while IR is used only for native lowering.

That would simply recreate the current split under a new name. The intended end
state is:

- one shared IR owns the execution semantics boundary,
- multiple execution strategies consume that IR.

## 5. Core IR Requirements

The first real IR should satisfy these requirements.

### 5.1 Semantic Requirements

- represent pure values and pure expressions explicitly,
- represent sequential evaluation explicitly,
- represent local bindings explicitly,
- represent branch control flow explicitly,
- represent direct function calls explicitly,
- represent effect operations explicitly,
- represent handler installation and resume points explicitly enough to support
  future effect lowering work,
- preserve declaration boundaries and entrypoint structure.

### 5.2 Compiler Engineering Requirements

- typed: every IR node must carry enough type information for backend lowering
  and diagnostics,
- effect-aware: effectful operations are explicit nodes, not rediscovered later,
- normalizable: semantically equivalent source programs should lower to the same
  or nearly the same IR after normalization,
- debuggable: IR should be printable for tests and diagnostics,
- backend-neutral: `goby-wasm` should not own the source-to-IR semantics.

### 5.3 Initial Simplifications

The first IR version may intentionally exclude:

- cross-function optimization,
- closure conversion beyond what is needed for correctness,
- polymorphic IR specialization,
- advanced CFG optimizations,
- aggressive dead-code elimination.

The priority is correctness and architectural clarity, not optimization.

## 6. Proposed Core IR Shape

The exact Rust enums can change, but the conceptual model should be close to the
following.

### 6.1 Module / Declaration Level

- `IrModule`
  - declarations
  - effect declarations needed by lowering
  - imports or resolved symbol references as required

- `IrDecl`
  - name
  - parameters
  - result type
  - residual effects
  - body

### 6.2 Computation / Expression Split

Use a deliberate distinction between pure values and effectful computations.

Example conceptual split:

- `ValueExpr`
  - literals
  - variables
  - tuples/lists/records from value operands
  - pure primitive ops
  - pure string interpolation from value operands

- `CompExpr`
  - `let`
  - `seq`
  - `if`
  - `case`
  - function call
  - effect operation
  - `with` / handler installation
  - `resume`
  - block / tail result
  - explicit `return`-style terminal if needed by the lowering pipeline

This split is important because it prevents the compiler from repeatedly asking
"does this AST subtree contain a runtime read?" and instead lets the lowering
phase classify operations structurally.

### 6.3 Effects

Effects should be explicit IR nodes rather than inferred later from call names.

Conceptually:

- `PerformEffect { effect, op, args, result_ty }`
- `Handle { handler, body }`
- `Resume { value }`

If direct representation of handlers is too large for the first execution slice,
the minimal acceptable alternative is:

- lower handlers into a structured effect-boundary IR with explicit handler/op
  metadata,
- but still keep that representation in a shared IR module rather than embedding
  it only inside Wasm lowering code.

Important constraint:

- even if handler/resume lowering is incomplete at first, the IR surface should
  reserve explicit ownership for effect boundaries early, so the migration does
  not create a second ad-hoc intermediate representation for handlers inside
  `goby-wasm`.

### 6.4 Calls and Names

The IR should distinguish:

- local variable reference,
- top-level declaration reference,
- builtin/intrinsic reference,
- effect operation reference.

Do not keep these as ambiguous strings once lowering begins.

Symbol resolution should happen before or during AST-to-IR lowering so the
backend does not need to repeat name interpretation logic.

## 7. Mandatory Normalization

The first IR pipeline should include a small normalization phase before backend
lowering.

Minimum normalization goals:

1. Flatten trivial alias chains.
2. Make sequencing explicit.
3. Normalize pure intermediate bindings so equivalent programs converge.
4. Lower source sugar to one canonical form:
   - block expressions,
   - interpolation,
   - operator sugar,
   - import-qualified effect operation references.
5. Separate pure value construction from effectful computation where possible.

This is the step that should make:

- `print(read())`
- `text = read(); print text`
- `text = read(); decorated = "${text}!"; print decorated`

look like small variations of one IR flow rather than unrelated source shapes.

## 8. Runtime I/O Under the New Design

### 8.1 Current Issue

`runtime_io_plan.rs` currently owns both:

- detection of runtime-read programs,
- and policy for which source shapes are lowerable to Wasm today.

That is the wrong long-term boundary.

### 8.2 Planned Replacement

Under the IR design:

- AST-to-IR lowering expresses `Read.read`, `Read.read_line`, and `Print` calls
  as explicit computation nodes,
- normalization rewrites simple pure transformations into canonical form,
- a dedicated backend pass analyzes IR dataflow and decides whether a program is:
  - lowerable to direct WASI I/O,
  - lowerable to a general effect runtime path,
  - or unsupported by the current backend.

This means the decision becomes:

- "can this IR program be lowered by backend X?"

instead of:

- "does this source program match one of N AST patterns?"

### 8.3 Expected Outcome

`runtime_io_plan.rs` should eventually disappear, or become a backend pass over
IR rather than over parsed AST statements.

If a small planning struct remains for Wasm, that is acceptable, but it must be:

- IR-derived,
- backend-local,
- and free of source-syntax matching logic.

## 9. Effect System Integration

The IR plan must align with Goby's effect direction rather than treating runtime
I/O as a special subsystem.

Long-term expectation:

- `Read` / `Print` are not special in the core IR,
- effectful calls use the same structural representation as user-defined effects,
- handlers and `resume` have explicit lowering representation,
- the current split between "native subset", "runtime I/O planner", and
  interpreter fallback can be replaced by a clearer backend capability decision
  over the same IR.

This is important because otherwise Goby will repeat the current problem for
every new effectful feature.

The existing typed-continuation work in `crates/goby-wasm/src/lower.rs` should
therefore be treated as transitional backend-local evidence, not as the final
shared IR boundary. As Track G progresses, that information should either:

- migrate into shared IR definitions owned outside `goby-wasm`, or
- become a backend analysis product computed from shared IR.

## 10. Ownership Boundaries

Recommended ownership:

- `goby-core`
  - AST
  - type/effect checking
  - IR definitions
  - AST-to-IR lowering
  - IR pretty-printing / debug rendering
  - IR validation passes

- `goby-wasm`
  - backend capability analysis over IR
  - IR-to-Wasm lowering
  - backend-specific runtime/import planning

The key constraint:

- `goby-wasm` should not be the owner of source-language semantic
  canonicalization.

## 11. Migration Strategy

This plan prefers an ideal architecture, but implementation still needs a
deliberate order.

### Phase IR1: Lock the IR boundary

- define the IR scope and invariants,
- choose module ownership and file layout,
- document what must no longer be decided from raw AST inside backends.

Acceptance:

- this document is stable enough to guide implementation,
- follow-up code work can reject changes that add new AST-shape special cases in
  backend code.

### Phase IR2: Introduce IR data structures and printer

- add `goby-core` IR module(s),
- define core enums/structs,
- reserve explicit nodes or boundary forms for effect operations and handler
  boundaries, even if some of them are not fully lowered yet,
- implement debug rendering suitable for snapshot tests,
- add validation helpers for well-formed IR.

Acceptance:

- a parsed/typechecked declaration can be represented in IR,
- IR snapshots are readable and stable enough for tests.

### Phase IR3: Lower a pure subset to IR

- literals,
- vars,
- bindings,
- arithmetic,
- conditionals,
- blocks,
- direct function calls.

Acceptance:

- pure programs can compile through IR without using AST-driven backend logic.

### Phase IR4: Lower `Print` / `Read` / interpolation into IR

- represent runtime I/O through explicit effect/intrinsic nodes,
- normalize pure intermediate transformations,
- add IR-level tests for equivalent source programs.

Acceptance:

- the previously divergent `read` + transform examples lower to related IR,
- backend support checks operate on IR rather than raw AST statements.

### Phase IR5: Route Wasm compilation through IR

- make Wasm compilation consume IR,
- remove or isolate direct AST-shape checks from `goby-wasm`,
- replace current runtime-I/O planning entrypoints with IR-based planning.

Acceptance:

- `compile_module` no longer needs source-shape runtime I/O classification from
  parsed statement lists.
- the backend-local typed-continuation/effect-boundary metadata is either
  derived from shared IR or clearly marked as a temporary backend pass over IR.

### Phase IR6: Effect handlers and continuation boundary

- complete lowering of `with`, handler clauses, and `resume` into explicit IR,
- retire remaining backend-local effect-boundary-only representations that are
  still sourced directly from AST-era structures,
- make effect-boundary handoff an IR concern rather than a mixed AST/backend one.

Acceptance:

- effect lowering strategy is expressed in one canonical shared representation.

## 12. Test Strategy

The IR work should add new test layers instead of relying only on end-to-end
CLI or Wasm behavior.

Required test layers:

1. AST-to-IR snapshot tests
   - canonical source examples
   - semantically equivalent surface forms
2. IR validation tests
   - malformed construction rejected
3. IR-to-backend tests
   - Wasm-lowerable subset
   - explicit unsupported-feature diagnostics
4. End-to-end parity tests
   - interpreter / fallback vs Wasm behavior for supported programs

The most important new regression class:

- semantically equivalent source programs should lower to the same canonical IR
  shape, or to a deliberately equivalent normalized variant.

Additional long-term expectation:

- once migration is complete, fallback/native parity should be testable from the
  same IR input, not only from duplicated AST-driven pipelines.

## 13. Progress Milestones

Use this section as the step-by-step implementation tracker for Track G. Check a
box only when the milestone is complete enough that follow-up work can rely on
it as the new baseline.

- [x] G1. IR boundary lock
  - `doc/PLAN_IR.md` is accepted as the active architecture plan.
  - Module ownership is fixed: shared IR in `goby-core`, backend analysis in
    `goby-wasm`.
  - New backend work stops adding source-shape-specific runtime-I/O branches
    except for emergency unblock cases.

- [x] G2. Shared IR module skeleton exists
  - `goby-core` contains initial IR definitions.
  - The IR has a readable debug/text rendering format.
  - IR validation helpers exist for basic well-formedness checks.

- [x] G3. Pure AST-to-IR lowering works
  - Pure declarations can lower from typed AST into IR.
  - Covered forms include literals, vars, local bindings, arithmetic, blocks,
    conditionals, and direct calls.
  - Snapshot tests lock representative pure IR output.

- [ ] G4. Effect boundary ownership is reserved in shared IR
  - Shared IR contains explicit representation for effect operations and handler
    boundaries, even if some lowering paths remain incomplete.
  - `goby-wasm` no longer needs to invent new backend-only intermediate forms to
    describe effect boundaries.

- [ ] G5. Runtime I/O lowers through IR
  - `Read` / `Print` paths lower into explicit IR nodes.
  - Normalization covers trivial alias chains and pure intermediate transforms.
  - Equivalent source programs such as `print(read())` and `text = read(); print text`
    lower to the same or intentionally equivalent normalized IR.

- [ ] G6. Wasm runtime-I/O planning is IR-based
  - Backend runtime-I/O capability checks consume IR rather than parsed AST
    statement lists.
  - `runtime_io_plan.rs` is either removed or reduced to a backend-local pass
    derived from IR.
  - Unsupported cases are reported as IR/backend capability limits rather than
    source-pattern mismatches.

- [ ] G7. Native Wasm compilation routes through shared IR
  - `compile_module` no longer depends on raw AST shape for native-vs-fallback
    decisions in the runtime-I/O path.
  - The remaining typed-continuation/effect-boundary metadata is derived from
    shared IR or clearly isolated as a temporary backend pass over IR.

- [ ] G8. Handler and resume lowering is shared-IR based
  - `with`, handler clauses, and `resume` lower into explicit shared IR forms.
  - Backend-local effect-boundary representations sourced directly from AST-era
    structures are retired.
  - The effect lowering strategy is expressed in one canonical shared
    intermediate representation.

- [ ] G9. Portable fallback aligns to the IR boundary
  - Portable fallback execution runs from shared IR directly, or from a thin
    adapter over the same IR-owned semantic boundary.
  - Fallback/native parity can be tested from the same IR input for supported
    programs.

- [ ] G10. Track G architecture is complete
  - Shared typed IR is the canonical semantic handoff.
  - Backend compilation decisions are made from IR rather than source AST
    spelling.
  - Runtime I/O and other effectful execution paths no longer depend on ad-hoc
    source-shape classifiers.

## 14. Expected Breakages During Migration

These are acceptable during the migration if they are understood and short-lived:

- direct `goby-wasm` code paths that currently inspect AST may stop compiling,
- runtime-I/O tests may need temporary rewrites around the new IR boundary,
- fallback/native selection logic may move or be temporarily duplicated while the
  boundary is being cut over,
- some effect-lowering code may need to pause feature work until ownership is
  clarified.

The migration should still avoid one class of breakage:

- do not add fresh AST-shape special cases as a shortcut while introducing IR.

## 15. Acceptance Criteria for the Architecture

This plan should be considered achieved when all of the following are true:

- Goby has a shared typed IR owned outside backend code,
- backend compilation decisions are made from IR, not raw AST shape,
- portable fallback execution is either driven from the same IR or from a thin
  adapter over that IR-owned semantic boundary,
- runtime I/O support no longer depends on enumerating source spellings,
- effectful execution paths use one canonical intermediate representation,
- unsupported backend cases are reported as IR/backend limitations rather than
  syntax-pattern mismatches,
- new features can extend the compiler by adding IR lowering and backend support
  instead of patching ad-hoc source classifiers.

## 16. Immediate Next Implementation Notes

The next active development after this planning document should be:

1. introduce `goby-core` IR module skeletons and a text printer,
2. choose the first minimal typed subset for AST-to-IR lowering,
3. freeze a rule that no new runtime-I/O AST pattern branches are added to
   `crates/goby-wasm/src/runtime_io_plan.rs`,
4. begin moving runtime-I/O support decisions behind IR-based analysis.

`doc/PLAN.md` should track this as the next architecture track, with this file
as the detailed reference.
