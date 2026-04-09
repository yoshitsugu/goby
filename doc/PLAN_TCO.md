# Generic Tail-Call Optimization Plan

This document defines the long-term plan for adding tail-call optimization that
is strong enough to say, in ordinary language, that "Goby has generic TCO" on
its supported execution path.

The goal of this plan is not to extend the current ad hoc recursion rewrites.
The goal is to establish a coherent language/runtime/compiler contract for tail
calls, then implement that contract in a way that remains maintainable as Goby
grows.

For this plan, **generic TCO** means:

- tail-call optimization is available as a general property of the documented
  compiled execution path for statically resolvable direct calls;
- the guarantee is not limited to one function, one stdlib helper, or one
  source pattern;
- the guarantee includes both self recursion and direct tail calls across
  multiple top-level functions, including mutually recursive groups, once the
  plan is complete.

This plan does **not** use "generic TCO" to mean "every possible higher-order
or backend-dependent tail call is optimized".

## 1. Product Goal

Goby should support tail-call optimization as a general property of the main
compiled execution path, not only as a collection of special-case loop rewrites.

In practical terms, a user should be able to write the obvious tail-recursive
function and reasonably expect it to run in constant stack on the supported
compiled path, without depending on function names, stdlib internals, or one
particular AST spelling.

## 2. Completion Goal

This plan is complete only when all of the following are true:

- Goby has a documented, user-facing statement of what tail calls are
  guaranteed to run in constant stack on the main compiled backend.
- The guarantee is expressed in language/compiler terms, not in terms of
  individual examples or hand-picked helper functions.
- Tail-call optimization is implemented through a shared compiler/runtime
  boundary, not through symbol-specific or fixture-specific rewrites.
- The supported subset includes at least:
  - direct self tail recursion,
  - direct tail calls to other top-level functions,
  - direct mutually recursive tail-call groups,
  - tail-position control-flow joins (`if`, `case`, local `let`/block shaping),
  - tail calls preserved across ordinary local aliases to known functions.
- The project has locked regression coverage proving constant-stack behavior for
  representative programs under an intentionally tight stack budget.
- Diagnostics remain honest for unsupported cases. If a tail-looking call falls
  outside the optimized subset, Goby must not silently over-claim TCO support.

Until these are true, Goby may have tail-call-related optimizations, but should
not claim "generic TCO" as a finished language/runtime property.

## 3. Non-Goals

The following are explicitly out of scope for the first version of generic TCO:

- claiming universal TCO across every backend, interpreter mode, and future
  runtime path;
- optimizing arbitrary higher-order indirect calls before the direct-call model
  is coherent;
- solving all recursion-related memory problems under the TCO umbrella;
- hiding unsupported recursion shapes behind misleading diagnostics;
- baking current Wasm-only implementation details into permanent language
  semantics.

These may become later extensions, but they must not distort the first design.

## 4. Design Principles

### 4.1 Tail Calls Are a Semantic Compiler Boundary

Tail-call optimization should be modeled as a compiler contract over
tail-position control flow, not as a peephole over emitted Wasm.

That means Goby should decide tail-position eligibility in an explicit
intermediate representation or analysis pass where:

- tail position is structurally visible,
- local bindings and control-flow joins are already normalized enough to reason
  about,
- the implementation is independent of source-symbol names,
- the backend can consume a dedicated "tail call" or equivalent looping form.

### 4.2 Direct-Call TCO Comes First

The first generic TCO target should be direct calls to known top-level
functions. This is the minimum scope that is broad enough to be meaningful and
still narrow enough to keep the semantics precise.

The first completed TCO slice should therefore cover:

- self recursion,
- sibling/top-level function tail calls,
- tail-position branches that end in one of those calls.

It should not depend on whether the call appears inside a stdlib helper or a
user helper. The rule should be structural.

Mutually recursive direct-call groups do not need to land in the first
implementation slice, but they do need to be part of the completed plan. If
they are omitted permanently, Goby should not claim generic TCO in the stronger
ordinary-language sense targeted by this document.

### 4.3 Tail-Position Normalization Must Be Shared

If TCO requires normalizing block structure, `if`, `case`, or let-chains, that
normalization should be introduced as a shared compiler phase or ownership
boundary. It should not be duplicated across unrelated lowering paths.

The implementation should reject these anti-patterns:

- "optimize this one function shape";
- "optimize only the stdlib spellings we already have";
- "add another one-off rewrite whenever a new failing example appears".

### 4.4 The Runtime Contract Must Stay Honest

If Goby claims TCO for a subset, stack diagnostics and runtime behavior must
reflect that subset accurately.

That implies:

- supported tail calls should be covered by tight-stack execution tests;
- unsupported tail-looking calls should remain diagnosable as ordinary deep
  recursion when they still consume stack;
- the docs must name the guarantee boundary clearly.

### 4.5 The Long-Term Design Must Not Depend on Wasm Tail-Call Availability

The first implementation may lower tail calls to explicit loops or equivalent
backend-owned control flow. It must not require Wasm tail-call instructions in
order for Goby to have a coherent TCO story.

If a future backend can take advantage of native tail-call support, that should
be an implementation refinement, not the definition of the feature.

## 5. Target Architecture

The preferred end-state architecture is:

1. A tail-position analysis pass marks which subexpressions are in tail
   position within a declaration.
2. A call-graph-aware phase identifies which direct tail calls are statically
   resolvable and which strongly connected groups must share one constant-stack
   execution model.
3. A normalization phase rewrites eligible direct tail calls into an explicit
   backend-neutral form.
4. The backend lowers that form into constant-stack execution, such as loops,
   jumps, trampoline-style dispatch, or another structured representation that
   works for both self and non-self direct tail calls.
5. Diagnostics and tests are defined against this shared form, not against one
   backend's instruction choices.

In other words, the project should move toward:

- source/IR analysis decides *what is a tail call*,
- shared call-shape normalization decides *which direct-call groups are covered*,
- backend lowering decides *how to execute them without stack growth*.

This split is the main defense against ad hoc implementation.

## 6. Acceptance Standard For Saying "Goby Has Generic TCO"

The project may say "Goby has generic TCO" only when all of the following are
locked:

- documented guarantee for the compiled Wasm path;
- direct self tail recursion works in constant stack;
- direct non-self tail calls work in constant stack;
- direct mutually recursive tail-call groups work in constant stack;
- tail calls in `if` and `case` branches preserve the guarantee;
- local aliasing of known top-level functions does not accidentally break the
  guarantee when the target is still statically resolvable;
- representative user-written examples succeed under a deliberately tight stack;
- unsupported cases are explicitly documented.

If any of these remain missing, the wording should stay narrower, such as
"Goby optimizes some tail-recursive shapes".

## 7. Milestones

- [ ] **M0: Lock the feature contract**
  - Define the precise first-version TCO guarantee in `doc/LANGUAGE_SPEC.md`
    and align `doc/PLAN.md`/`doc/STATE.md`.
  - Decide exactly which execution path may claim generic TCO first.
  - Define the unsupported cases that must remain explicit.

- [ ] **M1: Introduce a tail-position model**
  - Add a compiler-owned representation or analysis that can answer whether a
    computation is in tail position without relying on symbol names.
  - Cover `if`, `case`, block tails, and let-tail structure.
  - Reject source-shape-specific hacks at this stage.
  - 2026-04-09 initial slice:
    - shared `goby_core::tail_analysis` now models tail-position propagation
      over IR control flow and exposes nested-body policy explicitly;
    - `effect_handler_legality` consumes that shared analysis so the boundary is
      exercised by existing compiler behavior before TCO normalization begins.

- [ ] **M2: Normalize direct tail calls into a shared IR boundary**
  - Introduce a backend-neutral form for eligible direct tail calls.
  - Ensure self and non-self direct tail calls use the same conceptual model.
  - Preserve correct behavior for argument evaluation order and locals.
  - 2026-04-09 initial slice:
    - direct top-level declaration calls in tail position now lower to
      `WasmBackendInstr::TailDeclCall` instead of the ordinary `DeclCall`;
    - `if` / `case` tail joins propagate that normalization, while non-tail
      statement positions keep the ordinary call form;
    - the emitter still executes `TailDeclCall` like `DeclCall`, so this slice
      locks the shared normalization boundary without yet claiming constant-stack
      execution.

- [ ] **M3: Add a direct-call group execution model**
  - Define how non-self and mutually recursive direct tail calls execute in
    constant stack.
  - Choose the long-term model explicitly: grouped loops, trampoline-style
    dispatch, backend jumps, or another shared mechanism.
  - Prove that the model is compatible with Goby's evaluation order and local
    variable semantics.

- [ ] **M4: Lower the shared tail-call form to constant-stack Wasm execution**
  - Implement backend lowering for the shared tail-call form and direct-call
    group model.
  - Keep the implementation reusable across direct self, non-self, and mutual
    direct-call cases.
  - Do not couple the feature definition to Wasm tail-call instructions.

- [ ] **M5: Prove control-flow completeness**
  - Add regression coverage for tail calls through:
    - `if` joins,
    - `case` joins,
    - let-bound tail expressions,
    - local aliases to known functions.
  - Ensure these remain constant-stack under tight-stack execution.

- [ ] **M6: Define and test the failure boundary**
  - Lock diagnostics and tests for unsupported cases such as:
    - indirect/higher-order calls that are not yet guaranteed,
    - unresolved local function values,
    - recursion shapes that are not in tail position.
  - For each unsupported case, define the contract explicitly:
    - compile-time rejection, or
    - ordinary call execution with possible stack growth, or
    - documented backend-dependent behavior.
  - Make sure Goby does not over-claim support.

- [ ] **M7: Publish the user-facing TCO guarantee**
  - Update docs/examples so users can understand what is guaranteed and what is
    not.
  - Add representative examples that demonstrate the supported generic TCO
    subset.
  - After this milestone, it should be reasonable to say that Goby has generic
    TCO on the documented compiled path.

## 8. Implementation Guardrails

Every implementation slice under this plan must satisfy these rules:

- no symbol-specific rewrites;
- no stdlib-only fast path unless the same rule applies equally to user code;
- no test that proves only one named function works;
- no completion claim based solely on compile-time IR shape without tight-stack
  runtime proof;
- no documentation wording that implies more than the tested guarantee;
- no permanent architecture decision justified only by current Wasm backend
  convenience.
- no self-recursion-only design presented as if it already solved generic
  direct-call TCO.

## 9. Open Strategic Questions

These questions must be resolved during the plan, not bypassed:

- What is the best compiler layer for tail-position analysis in Goby's long-term
  architecture?
- Should the first shared tail-call form live in current IR, in a new lowered
  IR, or in backend IR?
- How far should "statically resolvable local alias" support go in the first
  guarantee?
- What is the best execution model for non-self and mutually recursive direct
  tail-call groups?
- Which unsupported tail-call shapes should be compile-time rejected versus
  allowed as ordinary stack-consuming calls?
- What exact wording in the language/runtime docs is honest once M7 is done?

## 10. Current Status

Current status before this plan starts:

- Goby already has limited recursion-related rewrites for specific representative
  RR buckets.
- Goby does **not** yet have a completed generic TCO feature.
- This document defines the work required to move from isolated rewrites to a
  general, defensible tail-call optimization story.
