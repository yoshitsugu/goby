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

- [x] **M0: Lock the feature contract**
  - Define the precise first-version TCO guarantee in `doc/LANGUAGE_SPEC.md`
    and align `doc/PLAN.md`/`doc/STATE.md`.
  - Decide exactly which execution path may claim generic TCO first.
  - Define the unsupported cases that must remain explicit.
  - 2026-04-10 contract lock:
    - `doc/LANGUAGE_SPEC.md` now defines the current TCO guarantee as a
      compiled-Wasm-path property of statically resolvable direct tail calls
      among known top-level declarations;
    - the currently guaranteed subset is explicit: direct self tail recursion,
      sibling direct tail calls, and mutually recursive top-level direct-call
      groups on that path, including the supported tail `if` / `case` joins;
    - unsupported cases are also explicit: indirect/higher-order calls,
      local function values that are not statically resolved as direct
      top-level calls, non-tail recursion, and direct-call shapes outside the
      currently documented compiled-Wasm subset;
    - the contract also locks the honesty rule: unsupported shapes may still be
      accepted and run as ordinary calls, but Goby does not promise
      constant-stack execution for them and must not describe them as covered
      generic TCO.

- [x] **M1: Introduce a tail-position model**
  - Add a compiler-owned representation or analysis that can answer whether a
    computation is in tail position without relying on symbol names.
  - Cover `if`, `case`, block tails, and let-tail structure.
  - Reject source-shape-specific hacks at this stage.
  - 2026-04-09 initial slice:
    - shared `goby_core::tail_analysis` now models tail-position propagation
      over IR control flow and exposes nested-body policy explicitly;
    - `effect_handler_legality` consumes that shared analysis so the boundary is
      exercised by existing compiler behavior before TCO normalization begins.

- [x] **M2: Normalize direct tail calls into a shared IR boundary**
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

- [x] **M3: Add a direct-call group execution model**
  - Define how non-self and mutually recursive direct tail calls execute in
    constant stack.
  - Choose the long-term model explicitly: grouped loops, trampoline-style
    dispatch, backend jumps, or another shared mechanism.
  - Prove that the model is compatible with Goby's evaluation order and local
    variable semantics.
  - 2026-04-10 initial slice:
    - aux declarations that contain self `TailDeclCall` now emit a looped Wasm
      helper body and rewrite the recursive tail edge to parameter rewrites plus
      a branch back to the loop head;
    - this makes generic direct self-tail recursion run in constant stack on
      the compiled Wasm path without reusing the old RR-2 symbol-specific
      strategy;
    - non-self and mutually recursive `TailDeclCall` still execute like ordinary
      direct calls until a wider direct-call group model lands.
  - 2026-04-10 follow-up slice:
    - strongly connected aux-decl groups linked by `TailDeclCall` now emit one
      shared dispatcher helper with a looped constant-stack execution path for
      the whole group;
    - each public aux decl remains a thin wrapper so existing direct-call and
      funcref entrypoints stay stable while grouped tail recursion moves onto
      the shared dispatcher boundary;
    - representative compile/runtime coverage now proves sibling/mutual direct
      tail recursion survives a deliberately tight stack limit on the compiled
      Wasm path.

- [x] **M4: Lower the shared tail-call form to constant-stack Wasm execution**
  - Implement backend lowering for the shared tail-call form and direct-call
    group model.
  - Keep the implementation reusable across direct self, non-self, and mutual
    direct-call cases.
  - Do not couple the feature definition to Wasm tail-call instructions.
  - 2026-04-10 completion slice:
    - `goby-wasm` now routes every aux declaration that participates in the
      covered direct `TailDeclCall` graph through one shared dispatcher-based
      constant-stack engine rather than splitting self recursion into a
      separate loop path and leaving other covered members on ordinary calls;
    - the grouped execution boundary now covers single-member self-tail
      declarations, sibling/mutual groups, and acyclic direct-tail chains into
      covered recursive members on the compiled Wasm path;
    - public aux declarations remain thin wrappers that seed the dispatcher
      tag/arguments, preserving direct-call and funcref-visible entry behavior
      while keeping constant-stack execution ownership in one backend-owned
      mechanism;
    - compile coverage now proves both self-tail and acyclic covered members
      lower through the shared dispatcher without recursing back through public
      wrappers;
    - tight-stack runtime coverage now proves the widened covered set executes
      successfully, and a dedicated function-value regression locks wrapper /
      funcref behavior after the routing change;
    - the implementation still uses explicit Wasm loops/dispatcher control
      flow only; it does not depend on Wasm native tail-call instructions.
  - Checkpoints:
    - [x] identify the remaining covered direct-call shapes that still leave the
      shared form but do not yet enter the grouped constant-stack engine;
    - [x] make the grouped execution mechanism own those shapes without adding a
      second independent TCO execution path;
    - [x] prove argument evaluation order is preserved when entering the shared
      constant-stack engine;
    - [x] prove parameter/local update semantics are preserved across repeated
      loop/dispatcher iterations;
    - [x] prove observable direct-call semantics and function-value behavior
      are preserved after routing covered calls through the shared engine,
      without requiring the current wrapper structure to remain the long-term
      implementation shape;
    - [x] add compile tests showing covered shapes no longer fall back to
      ordinary direct `DeclCall` emission on the constant-stack path;
    - [x] add tight-stack runtime tests showing the widened covered set now
      survives without stack-growth failures;
    - [x] confirm the implementation still does not depend on Wasm native
      tail-call instructions.

- [x] **M5: Prove control-flow completeness**
  - Add regression coverage for tail calls through:
    - `if` joins,
    - `case` joins,
    - let-bound tail expressions,
    - local aliases to known functions.
  - Ensure these remain constant-stack under tight-stack execution.
  - Completion criteria:
    - each covered shape must have both compile-path proof and tight-stack
      runtime proof;
    - compile-path proof must show the call stays on the shared tail-call form
      rather than silently falling back to ordinary direct calls;
    - runtime proof must use representative user-written programs rather than
      synthetic single-function micro-cases only.
  - Clarifications for this milestone:
    - `if` / `case` completion means the tail-call guarantee survives control-
      flow joins where all outgoing tail branches target the covered direct-call
      subset;
    - let/block completion means tail-position administrative structure does not
      accidentally break the same shared lowering boundary;
    - local-alias completion means ordinary local alias chains to known top-
      level declarations preserve the same guarantee when the callee remains
      statically resolvable, not merely when the source uses the declaration
      name directly.
  - 2026-04-10 completion slice:
    - compile/runtime proof now locks tail `if` joins, tail `case` joins,
      let/block-shaped tail expressions, and local alias chains to known top-
      level declarations on the shared dispatcher boundary;
    - local alias calls in tail position now resolve through the same
      direct-declaration normalization path instead of falling through to an
      indirect call when the alias is still statically resolvable;
    - the language spec now explicitly includes let/block tails and statically
      resolvable local alias chains in the current compiled-Wasm direct-call
      constant-stack guarantee;
    - this milestone closes proof of the currently intended covered source
      shapes and leaves M6 to define the explicit unsupported-bucket contract.
  - Checkpoints:
    - [x] add a compile-path regression for tail `if` joins targeting the
      covered direct-call subset;
    - [x] add a tight-stack runtime regression for the same tail `if` join
      shape;
    - [x] add a compile-path regression for tail `case` joins targeting the
      covered direct-call subset;
    - [x] add a tight-stack runtime regression for the same tail `case` join
      shape;
    - [x] add a compile-path regression for let/block-shaped tail expressions
      that should preserve the covered direct-call guarantee;
    - [x] add a tight-stack runtime regression for the same let/block-shaped
      representative;
    - [x] add a compile-path regression for local alias chains to known top-
      level declarations in tail position;
    - [x] add a tight-stack runtime regression for the same local-alias
      representative;
    - [x] confirm all newly covered control-flow shapes stay on the shared
      tail-call form rather than a shape-specific fast path;
    - [x] update the milestone notes once the representative proof set is
      complete.

- [x] **M6: Define and test the failure boundary**
  - Lock diagnostics and tests for unsupported cases such as:
    - indirect/higher-order calls that are not yet guaranteed,
    - unresolved local function values,
    - recursion shapes that are not in tail position.
  - For each unsupported case, define the contract explicitly:
    - compile-time rejection, or
    - ordinary call execution with possible stack growth, or
    - documented backend-dependent behavior.
  - Make sure Goby does not over-claim support.
  - Failure-boundary matrix that this milestone must close:
    - covered but not yet optimized due to a compiler bug:
      this is a regression and must be fixed, not documented as an allowed
      fallback;
    - intentionally unsupported tail-looking direct-call shape:
      execution may remain an ordinary call, but the docs/tests must classify it
      as outside the guarantee;
    - indirect/higher-order tail-looking call:
      allowed to execute ordinarily unless a stronger rejection rule is chosen,
      but must not inherit the direct-call TCO claim;
    - non-tail recursion:
      explicitly outside the TCO guarantee and expected to retain ordinary
      stack-consuming behavior unless another optimization track covers it;
    - backend/path mismatch:
      any claim stronger than the compiled Wasm path must be documented as
      backend-dependent until separately proven.
  - Diagnostic/coverage requirements:
    - at least one locked test per unsupported bucket proving the chosen
      contract, not just a prose note;
    - docs and diagnostics must distinguish "outside the guarantee" from
      "compiler bug/regression" and from "compile-time unsupported";
    - the user-facing wording must stay consistent with `doc/LANGUAGE_SPEC.md`
      and must not imply that ordinary successful execution means the shape is
      covered by constant-stack TCO.
  - Checkpoints:
    - [x] enumerate the unsupported tail-looking buckets that remain after M5,
      with one owner decision per bucket;
    - [x] decide which buckets are compile-time rejected and which are allowed
      to execute as ordinary stack-consuming calls;
    - [x] add at least one locked test for an indirect/higher-order tail-looking
      call proving the chosen contract;
    - [x] add at least one locked test for an unresolved local function-value
      tail-looking call proving the chosen contract;
    - [x] add at least one locked test for non-tail recursion proving it
      remains outside the TCO guarantee;
    - [x] either add at least one locked test for a backend/path mismatch case
      or explicitly remove that bucket from the supported/unsupported matrix
      with matching doc wording;
    - [x] audit diagnostics/docs so "outside the guarantee" wording is distinct
      from "unsupported" and from "compiler regression";
    - [x] update `doc/LANGUAGE_SPEC.md` / `doc/PLAN_TCO.md` if any unsupported
      bucket contract changes while closing this milestone;
    - [x] record the final unsupported-shape matrix in the plan/state docs once
      the boundary is stable.
  - 2026-04-10 completion slice:
    - locked compile-path regressions now prove the chosen outside-the-
      guarantee contract for:
      - indirect/higher-order tail-looking calls, which remain on
        `IndirectCall`;
      - unresolved local function-value tail-looking calls, which also remain
        on `IndirectCall`;
      - non-tail recursion, which remains on ordinary `DeclCall` rather than
        the shared tail-call form;
    - for these buckets, the chosen M6 contract is “accepted execution may
      remain ordinary and stack-consuming”; Goby does not promise constant-
      stack behavior for them;
    - backend/path mismatch is no longer tracked as a separate unsupported
      source-shape bucket in the matrix:
      the TCO guarantee is already scoped only to the compiled Wasm path, so
      other paths are documented as scope differences instead;
    - docs now explicitly distinguish “outside the guarantee” from a compiler
      regression that misses a covered shape.

- [ ] **M7: Publish the user-facing TCO guarantee**
  - Update docs/examples so users can understand what is guaranteed and what is
    not.
  - Add representative examples that demonstrate the supported generic TCO
    subset.
  - After this milestone, it should be reasonable to say that Goby has generic
    TCO on the documented compiled path.
  - Completion criteria:
    - the user-facing docs name the supported direct-call TCO subset in
      language terms rather than implementation terms;
    - the docs also name the main unsupported buckets and state that they are
      outside the guarantee;
    - the examples used to illustrate the guarantee correspond to locked
      compile/runtime coverage already present in the test suite;
    - the final wording is strong enough to be useful but narrow enough to stay
      true across the documented compiled path.
  - Clarifications for this milestone:
    - this milestone is not just a wording edit; it is the point where Goby
      decides whether the stronger phrase "Goby has generic TCO" is now honest;
    - if the covered subset is still too narrow when M7 is reached, the docs
      should publish the narrower guarantee instead of over-claiming completion;
    - user-facing docs should avoid internal mechanism names unless they are
      necessary to explain a limitation.
  - Checkpoints:
    - [ ] choose the final public wording for the TCO guarantee on the
      documented compiled path;
    - [ ] update `doc/LANGUAGE_SPEC.md` so the published contract matches the
      completed M4-M6 reality;
    - [ ] update `README.md` with a short high-level statement of the TCO
      guarantee and its current boundary;
    - [ ] add or update examples that demonstrate:
      - [ ] direct self tail recursion in the published subset;
      - [ ] direct sibling/non-self tail calls in the published subset;
      - [ ] direct mutually recursive tail-call groups in the published subset;
      - [ ] at least one supported control-flow-preserving tail-call example
        (`if`, `case`, let/block, or alias) from the completed M5 set;
    - [ ] state explicitly in the docs/examples whether one example is allowed
      to satisfy multiple published guarantee buckets;
    - [ ] add or update documentation that explicitly names the main unsupported
      buckets from M6;
    - [ ] verify the published examples correspond to locked compile/runtime
      proof and are not merely aspirational;
    - [ ] review `doc/PLAN.md`, `doc/PLAN_TCO.md`, and `doc/STATE.md` so the
      public wording and internal plan wording do not contradict each other;
    - [ ] make the final call on whether it is now honest to say "Goby has
      generic TCO" or whether the published wording must remain narrower.

## 8. Internal Implementation Target

The user-facing contract in `doc/LANGUAGE_SPEC.md` intentionally avoids fixing
backend internals. For M4 and later work, the project also locks the following
implementation target so the remaining slices converge on one coherent
architecture instead of a growing collection of compatible-looking rewrites.

Internal target:

- tail position is decided before Wasm emission at a shared compiler boundary,
  not rediscovered by backend-specific peepholes;
- eligible direct tail calls are represented in one shared form across self,
  sibling, and mutual recursion rather than split into unrelated special cases;
- execution ownership for constant-stack direct calls lives in one backend-owned
  grouped-call mechanism that can describe both single-function and multi-
  function strongly connected components;
- public call entrypoints may remain thin wrappers, but the constant-stack
  engine itself should be shared for all covered direct-call members in the
  group, while leaving internal entry structure free to change as long as
  observable direct-call semantics and function-value behavior stay correct;
- control-flow preservation (`if`, `case`, let/block tail structure, and later
  local-alias support) should widen the same shared form, not introduce new
  per-shape execution paths;
- unsupported tail-looking shapes should fail or fall back at an explicit phase
  boundary that can be documented, tested, and reasoned about independently of
  Wasm instruction choices.

Practical implication for M4+:

- prefer extending the existing shared tail-call boundary and grouped dispatcher
  model over adding a second constant-stack mechanism for another source shape;
- if a new covered case cannot be expressed in the current shared boundary,
  first fix the boundary definition, then extend lowering/emission;
- treat backend-native tail-call instructions, if adopted later, as an
  implementation refinement of this shared model, not as a replacement product
  definition.
- when deciding whether a remaining task belongs to M4, M5, or M6:
  - M4 owns execution machinery changes;
  - M5 owns proof that already-covered source/control-flow structure stays on
    that machinery;
  - M6 owns the explicit contract for shapes that still do not.

## 9. Implementation Guardrails

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

## 10. Open Strategic Questions

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

## 11. Current Status

Current status before this plan starts:

- Goby already has limited recursion-related rewrites for specific representative
  RR buckets.
- Goby does **not** yet have a completed generic TCO feature.
- This document defines the work required to move from isolated rewrites to a
  general, defensible tail-call optimization story.
