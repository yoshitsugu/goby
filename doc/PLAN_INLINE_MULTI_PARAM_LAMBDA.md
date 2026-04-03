# Goby Inline Multi-Parameter Lambda Plan

Last updated: 2026-04-03

Status: acceptance coverage and core execution parity landed; remaining work is cleanup and follow-up proofs

Related documents:

- `doc/LANGUAGE_SPEC.md` — current user-visible syntax and semantics
- `doc/PLAN.md` — top-level roadmap and active language direction
- `doc/PLAN_IR.md` — stable IR and backend-lowering boundaries
- `doc/STATE.md` — closure-capture implementation history and current restart notes

Plan-label hygiene:

- labels in this document such as milestone IDs are planning-only metadata
- do not copy those labels into code comments, test names, diagnostics, or user-visible strings
- when implementation notes need context, describe the technical purpose directly

---

## 1. Goal

Make inline multi-parameter lambdas work as ordinary first-class function values across Goby's
supported execution paths, not only in one stdlib function or one backend shortcut.

The implementation must support:

- inline multi-parameter lambdas passed to existing higher-order functions such as `fold`
- the same inline lambda shape passed to future higher-order functions without backend-specific rewrites
- direct calls, let-bound calls, and higher-order callback calls through one shared callable model
- parity between compiled Wasm execution and the runtime-stdin / fallback execution paths

This plan is explicitly not a `fold`-only plan. The purpose is to remove a structural limitation
at the callable/lowering boundary so future APIs such as `filter`, `flat_map`, `reduce`, or
user-defined higher-order functions inherit the same capability automatically.

---

## 2. Problem Statement

Today, Goby already has partial ingredients for this feature:

- multi-parameter lambdas parse and typecheck
- the Wasm backend has indirect-call support for callback arity 2
- zero-capture and capturing lambdas already have a callable representation on some paths

The original gaps for this track were:

- inline multi-parameter lambdas failed or degraded on some runtime paths
- some success cases depended on named helper declarations rather than the inline lambda itself
- the shape invited ad hoc fixes in individual stdlib combinators

The core issue is not syntax and not `fold` specifically. The real issue is that Goby does not yet
have one fully shared lowering/runtime story for:

1. constructing a function value from an inline lambda,
2. preserving its arity and environment,
3. passing it through locals and higher-order calls, and
4. invoking it from any execution path.

---

## 3. Required User-Visible Semantics

The feature is complete only when the following statements are true:

1. `fn a b -> expr` is a normal function value, not a privileged `fold` callback shape.
2. Inline multi-parameter lambdas behave the same as equivalent named top-level functions,
   except for closure capture of surrounding locals.
3. A higher-order function that accepts a callback of type `(a -> b)`, `(a -> b -> c)`, or
   any future supported function type should not need backend-specific feature work to accept
   an inline lambda of that shape.
4. Lambda invocation semantics are determined by the shared callable model, not by the name of
   the stdlib helper that happens to call the function.
5. If the language accepts a callback shape statically, runtime execution must either support it
   through the shared path or reject it with one backend limitation at the true ownership boundary.

During implementation:

- `doc/LANGUAGE_SPEC.md` may document the intended semantics with an explicit status note
- `doc/STATE.md` must say exactly which execution paths are complete and which still fail

---

## 4. Minimal Acceptance Programs

The implementation is not complete until all programs in this section execute correctly on the
supported runtime paths.

### 4.1 Inline multi-parameter lambda passed to `fold`

```goby
import goby/list ( fold )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  total = fold [1, 2, 3] 0 (fn acc x -> acc + x)
  println "${total}"
```

Expected output:

```text
6
```

### 4.2 Effectful inline multi-parameter lambda passed to `fold`

```goby
import goby/list ( fold )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  total =
    fold [1, 2, 3] 0 (fn acc x ->
      println "acc=${acc} x=${x}"
      acc + x
    )
  println "total=${total}"
```

Expected output:

```text
acc=0 x=1
acc=1 x=2
acc=3 x=3
total=6
```

### 4.3 Let-bound inline lambda reused through a user-defined higher-order function

```goby
apply_twice : (Int -> Int -> Int) -> Int -> Int
apply_twice f x =
  f x x

main : Unit -> Unit can Print, Read
main =
  _ = read()
  add = fn a b -> a + b
  result = apply_twice add 5
  println "${result}"
```

Expected output:

```text
10
```

### 4.4 Capturing inline multi-parameter lambda passed through a higher-order function

```goby
import goby/list ( fold )

sum_with_bias : Int -> List Int -> Int
sum_with_bias bias xs =
  fold xs 0 (fn acc x -> acc + x + bias)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = sum_with_bias 10 [1, 2, 3]
  println "${result}"
```

Expected output:

```text
36
```

### 4.5 Future-facing acceptance gate: a new higher-order helper needs no lambda-specific backend work

```goby
pairwise_apply : List Int -> (Int -> Int -> Int) -> List Int
pairwise_apply xs f =
  case xs
    [] -> []
    [x] -> [x]
    [x, y, ..rest] -> [f x y, ..pairwise_apply rest f]

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = pairwise_apply [1, 2, 3, 4] (fn a b -> a + b)
  println "${result}"
```

Expected output:

```text
[3, 7]
```

This last program is important because it proves the feature is not coupled to stdlib symbol names.

---

## 5. Design Direction

### 5.1 Solve this at the callable boundary, not in individual APIs

Inline multi-parameter lambda support must be implemented where Goby represents callable values
and callable invocation.

This means:

- do not special-case `fold`
- do not special-case `map`, `each`, or future stdlib helpers by symbol name
- do not add parser exceptions or backend-only recognizers for one callback shape
- do not introduce one execution strategy for named callbacks and another for inline callbacks

If a new higher-order function is added later, it should work automatically once it is expressed
in terms of the shared callable model.

### 5.2 One callable model, multiple optimizations

The long-term design target is:

- all lambdas are callable values with code, arity, and environment
- zero-capture lambdas are the empty-environment case
- named function references are compatible with the same invocation model
- direct `DeclCall` fast paths remain allowed as an optimization, not as a competing semantic path

This keeps the semantics simple:

- the typechecker owns function shape
- the callable layer owns invocation shape
- the backend may optimize representation, but not redefine the model

### 5.3 Arity is shared callable metadata

The current two-argument support should be treated as an early instance of a general rule.

The shared callable boundary must therefore preserve:

- callable arity
- callable environment layout
- whether the callable is invoked directly or indirectly
- the calling convention needed by the backend/runtime

The implementation should not treat arity-2 as a permanent special case. The design should make
arity-N extension a local evolution of one deep module, not a repeated cross-cutting refactor.

### 5.4 Deep-module requirement

This track should follow the spirit of *A Philosophy of Software Design*: prefer a small public
surface with concentrated internal power over many shallow helper layers.

Concretely:

- create one focused shared callable-lowering module or subsystem if the current code lacks one
- let that module own lambda lifting, callable metadata, arity-aware invocation shape, and the
  closure/non-closure call split
- keep most backend and runtime code as consumers of that module's output rather than re-deriving
  callable facts

Avoid the shallow alternative:

- one helper for `fold`
- another helper for `map`
- another helper for fallback
- another helper for closure lambdas
- another helper for arity-2 only

That would spread the same concept across many files without creating a real abstraction boundary.

### 5.5 Modular code organization

Modularity here means cohesive ownership, not file-count maximization.

Preferred ownership split:

- parsing stays responsible only for surface syntax
- typechecking stays responsible only for function types and callback compatibility
- shared callable lowering owns lambda normalization, callable metadata, and invocation planning
- backend emission owns concrete machine/Wasm encoding of the chosen callable operations
- runtime/fallback execution owns execution of the shared callable representation, not re-parsing
  special syntax shapes

When extracting modules, prefer modules that hide difficult policy and expose simple inputs/outputs.
Do not split one cohesive callable subsystem into many thin files just to reduce line count.

---

## 6. Non-Goals and Anti-Ad-Hoc Rules

The following are explicitly out of scope or forbidden for this track:

- adding a `fold`-only workaround
- adding a `runtime_io_plan` branch specifically for inline multi-parameter lambdas
- supporting the feature only on one runtime path while leaving the other as a silent divergence
- adding backend recognizers for specific stdlib names in order to fake callback execution
- duplicating callable-shape logic in Wasm lowering and fallback execution independently
- landing support only for arity 2 if the design makes arity 3+ require another architectural rewrite

Temporary limitations are acceptable during development, but they must be recorded honestly in
`doc/STATE.md` and must converge toward the shared callable boundary rather than away from it.

---

## 7. Architecture Work Items

### 7.1 Shared callable normalization

Introduce or strengthen one compiler-owned representation for callable values that is rich enough to
cover:

- top-level function references
- zero-capture lambdas
- capturing lambdas
- multi-parameter lambdas
- local variables holding callable values

This representation should be established before backend-specific emission decisions.

### 7.2 Shared invocation planning

Add one shared lowering layer that decides how a callable is invoked:

- direct decl call when legal and profitable
- indirect non-closure call
- indirect closure call

That decision should depend on callable metadata, not on the caller being `fold`, `map`, `each`,
or any other particular function.

### 7.3 Arity-aware backend/runtime support

The backend/runtime boundary must support arity-aware indirect calls through one scalable scheme.

Expected direction:

- represent arity in callable invocation instructions
- let the emitter manage Wasm function-type allocation/reuse by arity
- let fallback/runtime execution consume the same invocation plan rather than inventing a separate
  callback protocol

### 7.4 Closure-capture integration

This plan builds on the closure-capture work rather than replacing it.

Required integration points:

- multi-parameter lambdas must use the same closure-environment ownership model as single-parameter lambdas
- closure and non-closure call sites should differ only where the runtime representation actually differs
- callable invocation planning should not need to know the source syntax that produced the callable

### 7.5 Diagnostics boundary

If a callable shape is still unsupported on some backend path during implementation, the rejection
must come from the shared callable/invocation boundary with one explicit backend limitation.

Do not let support failures leak out as:

- parser errors
- misleading type mismatches
- stdlib-specific crashes
- silently missing output

---

## 8. Milestones

- [x] IMP0: Lock semantics and acceptance coverage
  - Scope:
    - add the acceptance programs in §4 as failing or ignored tests at the right ownership boundaries
    - update `doc/LANGUAGE_SPEC.md` and `doc/STATE.md` if the current docs overstate support
    - document the exact current limitation in user-visible terms
  - Done when:
    - the repo has stable regression coverage for the current failure shapes
    - docs say clearly that inline multi-parameter lambdas are intended semantics but not yet complete

- [x] IMP1: Shared callable shape is explicit before backend emission
  - Scope:
    - audit the current lambda-lifting and callable metadata path
    - remove any remaining places where multi-parameter inline lambdas survive too long as syntax-shaped special cases
    - make arity and environment ownership explicit in the shared callable representation
  - Done when:
    - one compiler-owned callable representation covers the relevant lambda/function-value cases
    - backend consumers no longer need to rediscover multi-parameter lambda structure from surface-shaped nodes

- [x] IMP2: Invocation planning becomes shared and generic
  - Scope:
    - introduce one invocation planner for direct, indirect, and closure calls
    - route stdlib higher-order execution through that planner rather than bespoke callback decisions
    - keep fast direct-call paths only as optimization choices inside the shared plan
  - Done when:
    - `fold`, `map`, `each`, and user-defined higher-order functions rely on one invocation story
    - no new API-specific callback dispatch code is needed to add another higher-order helper

- [x] IMP3: General-lowering and runtime-stdin paths execute inline multi-parameter lambdas
  - Scope:
    - extend the runtime/fallback execution path to consume the same callable representation
    - eliminate the current gap where named helper callbacks work but equivalent inline lambdas do not
    - verify parity for pure and effectful inline multi-parameter callbacks
    - add compiled-Wasm execution coverage for the same representative callback shapes where that path is expected to support them
  - Done when:
    - the acceptance programs in §4 execute through the runtime-stdin / general-lowering path
    - the representative pure/effectful/capturing cases also execute through the compiled-Wasm path, or are rejected there with the same explicit backend-limitation boundary when support is intentionally still pending
    - failures, if any remain, are explicit backend limitations at one shared boundary

- [x] IMP4: Closure parity for inline multi-parameter lambdas
  - Scope:
    - ensure capturing inline multi-parameter lambdas use the same closure environment model as the rest of the closure-capture implementation
    - add regression tests for by-value capture and captured mutable-cell reads/writes where relevant
  - Done when:
    - inline multi-parameter lambdas with captures behave like equivalent named or single-parameter closure cases
    - no callback-shape exception remains in higher-order helpers

- [x] IMP5: Future-helper proof and cleanup
  - Status note:
    - complete for the current acceptance proof set; further cleanup follow-ups may remain
  - Scope:
    - add at least one representative higher-order helper beyond the current stdlib acceptance set
    - remove transitional ad hoc branches introduced during development
    - update docs to describe the feature as implemented behavior rather than intended-only behavior
  - Proof rule:
    - the representative helper should be introduced as ordinary language/stdlib code first
    - after that helper exists, its inline multi-parameter lambda acceptance case should pass without any helper-specific parser, typechecker, backend, or runtime changes
    - if extra compiler/runtime work is still needed after introducing the helper, that is evidence that the shared callable boundary is still incomplete and IMP5 is not done
  - Done when:
    - a new higher-order helper can accept inline multi-parameter lambdas after being added without any helper-specific compiler/runtime changes
    - the same proof case is covered on every execution path that this track claims to support
    - the temporary implementation-status note can be removed from the spec/docs

---

## 9. Suggested Execution Order

1. Add failing coverage and honest doc status notes.
2. Strengthen the shared callable representation and arity metadata.
3. Introduce shared invocation planning.
4. Wire backend emission and fallback/runtime execution to that shared plan.
5. Land closure parity for capturing inline multi-parameter lambdas.
6. Prove the design by adding one new higher-order helper that needs no lambda-specific backend work.
7. Remove temporary status notes only after parity and cleanup are complete.

This order keeps the work anchored at the real ownership boundary and reduces the chance of
shipping one more special case that later has to be removed.

---

## 10. Review Checklist

Every implementation slice for this plan should be reviewed against the following questions:

1. Does this change improve the shared callable boundary, or only make one fixture pass?
2. Would a future higher-order helper inherit this capability automatically?
3. Did we move policy into one deep module, or scatter it across shallow helpers?
4. Are the compiled-Wasm and fallback/runtime paths converging on one model?
5. If a limitation remains, is it reported at the true ownership boundary with an explicit message?
6. Did we avoid introducing stdlib-name checks, syntax-shape checks, or fixture-specific exceptions?

If the answer to any of these questions is no, the slice should be reworked before the track is
considered complete.
