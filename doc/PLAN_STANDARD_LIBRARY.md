# Goby Standard Library Remaining Work Plan

Status: complete closure record
Owner: Goby core/runtime track
Last updated: 2026-03-25 (S3 closure)

## 1. Scope

This document records the now-complete stdlib split-retirement track that
remained after C4 completion.

Completed stdlib retirement track:

- retire the last direct runtime builtin branch for `goby/string.split`,
- make the ownership boundary explicit between:
  - stdlib declaration execution,
  - backend intrinsics used by Wasm lowering,
  - compile-time/runtime-output fallback evaluation,
- lock regression coverage and docs around that final boundary.

This plan is intentionally narrower than `doc/PLAN_IR.md`:

- `PLAN_IR` owns Wasm lowering, backend intrinsics, and `GeneralLowered` runtime
  execution.
- this document owns only the final ownership cleanup for stdlib `split`.
- backend `StringSplit` / `SplitEachPrint` / `SplitGetPrint` instructions are
  not deletion targets here unless their ownership changes explicitly in a later
  plan.

Everything from the earlier stdlib bootstrap and C4 implementation work should
be treated as completed historical work and recovered from git history if
needed.

## 2. Current State

Already true today (updated 2026-03-25):

- `stdlib/goby/string.gb` fully owns source-level `split` semantics for all
  delimiter cases.
- the stdlib `split` definition no longer calls runtime `string.split(...)` for
  any delimiter case.
- empty-delimiter, single-delimiter, multi-grapheme-delimiter, `split "" ""`,
  and import/compile-path coverage are locked by tests.
- `StringGraphemesList` handles the empty-delimiter Wasm path.
- backend `StringSplit` and fused split-print helpers still exist for Wasm
  lowering and are currently independent of stdlib declaration ownership.

Still not finished:

- compile-time/runtime-output fallback evaluation still contains a legacy direct
  runtime handling path for `string.split`,
- that legacy path currently masks broader evaluator ownership gaps,
- docs still describe the branch as pending deletion rather than fully retired.

## 3. Locked Behavior

These semantics remain locked while finishing the work:

- preserve empty segments,
- preserve leading and trailing empty segments,
- empty delimiter means grapheme-wise split,
- grapheme definition is Unicode Extended Grapheme Cluster,
- empty input returns `[]` for current compatibility.

## 4. Architecture Goal

The target architecture after this remaining split-retirement track is:

- source-level `goby/string.split` behavior is owned by stdlib declarations,
- backend intrinsics remain an execution-detail boundary for Wasm lowering only,
  not a semantic fallback for stdlib source resolution,
- compile-time/runtime-output fallback evaluation executes imported/local stdlib
  declarations through one coherent declaration-evaluation path rather than
  through ad-hoc per-function shortcuts,
- deleting one stdlib builtin branch should not require new
  function-specific special cases elsewhere.

This means the remaining goal is not merely "delete a few `split` if-statements".
The real goal is to remove them **after** the generic evaluator path is strong
enough that they are unnecessary.

## 5. Remaining Milestones

- [x] S1. Evaluator ownership convergence
  - close the generic fallback-evaluator gaps that are currently hidden by the
    legacy `string.split` runtime branch,
  - make imported/local stdlib declaration execution coherent enough that
    `split` no longer needs function-specific rescue logic.

- [x] S2. Legacy split branch retirement
  - remove the direct runtime handling path for source-level `string.split`,
  - preserve the existing backend intrinsic boundary for Wasm lowering.

- [x] S3. Boundary lock and closure
  - lock regressions around the final ownership boundary,
  - sync docs/state,
  - run final quality gates.

The previous `C6` / `C7` / `C8` breakdown has been intentionally collapsed into
`S3`. Those items are still required, but they are completion conditions for the
retirement work, not independent roadmap themes.

## 6. Detailed Execution Plan

### 6.1 Non-goals

Do not do the following in this track:

- do not rewrite `stdlib/goby/string.gb` again unless a generic evaluator gap
  proves that the stdlib surface itself is inconsistent,
- do not add a replacement `split`-specific branch in another runtime file,
- do not delete backend `StringSplit` merely because source-level stdlib
  ownership is complete,
- do not paper over generic evaluator limitations with another targeted
  `goby/string` special case.

### 6.2 Design Principle

If removal exposes a failure, fix the **generic ownership layer** that should have
been responsible all along.

Preferred ownership order:

1. local/imported declaration value execution,
2. statement/block local-state propagation in the fallback evaluator,
3. method/bare-call dispatch normalization,
4. only then, deletion of the legacy `split` fast-path.

This ordering is important because deleting the branch first without the generic
path ready leads directly to ad-hoc compensating hacks.

### 6.3 Known Risk Surface

Based on current code structure, the risky areas are:

- `crates/goby-wasm/src/runtime_decl.rs`
  - imported declaration execution and imported-call shortcut logic,
- `crates/goby-wasm/src/runtime_apply.rs`
  - local declaration value execution for helper chains used by stdlib `split`,
- `crates/goby-wasm/src/runtime_expr.rs`
  - block/value evaluation and statement-versus-expression handling,
- `crates/goby-wasm/src/runtime_replay.rs`
  - propagation of updated locals through `with`, nested block, and resumptive
    statement flows,
- `crates/goby-wasm/src/runtime_resolver.rs`
  - AST/option-based fallback path parity with the `Out`-based evaluator.

These should be treated as one cohesive evaluator ownership surface, not as
isolated opportunities for more split-specific special casing.

### 6.4 Step-by-step Plan

- [x] S1-1. Lock the evaluator ownership contract with targeted regressions
  - add focused tests for:
    - selective-import `split(...)` through runtime-output fallback,
    - canonical qualified `string.split(...)` through runtime-output fallback,
    - empty-delimiter runtime-output behavior,
    - Unicode multi-grapheme delimiter runtime-output behavior.
  - keep these tests beside the existing `runtime_output_tests` split coverage.
  - done-when:
    - the tests fail for understandable reasons when the legacy branch is
      removed,
    - the failures identify generic evaluator gaps rather than ambiguous
      end-to-end breakage.

- [x] S1-2. Unify declaration value execution before deleting the branch
  - make imported/local declaration value execution use one coherent
    responsibility model for:
    - argument binding,
    - declaration-body result extraction,
    - call-shape normalization between bare/qualified/imported declaration calls.
  - the bar is not "make split pass"; the bar is "imported/local decl execution
    is no longer split-specific".
  - done-when:
    - helper chains inside `stdlib/goby/string.gb` evaluate through the generic
      declaration path without relying on `split` shortcuts,
    - no new `split`-specific branches are introduced.

- [x] S1-3. Normalize fallback evaluator state propagation where `split` depends on it
  - close generic gaps in:
    - statement/block result extraction,
    - `with ... in` local update propagation,
    - statement-versus-expression parity,
    - AST/option path versus `Out` path behavior.
  - done-when:
    - the regressions from `S1-1` pass through the generic evaluator path with
      no dedicated `split` handling,
    - the remaining failures, if any, are about branch deletion itself rather
      than evaluator incompleteness.

- [x] S2-1. Delete the legacy `string.split` runtime branch
  - remove the direct runtime handling in the fallback/runtime-output path once
    `S1` is green.
  - deletion targets include:
    - bare imported `split` special cases,
    - qualified `string.split` special cases,
    - any now-dead helper predicates that exist only for those branches.
  - expected review surface after deletion:
    - `crates/goby-wasm/src/runtime_decl.rs`,
    - `crates/goby-wasm/src/runtime_expr.rs`,
    - `crates/goby-wasm/src/runtime_resolver.rs`.
  - done-when:
    - a search for the retired branch shape no longer finds runtime-only
      `string.split` handling in the fallback evaluator path,
    - split behavior remains correct through the generic path.

- [x] S2-2. Re-run ownership regressions across affected execution boundaries
  - prove imported/qualified `split` still works after branch deletion,
  - prove empty-delimiter and Unicode multi-grapheme behavior still hold across
    fallback and Wasm-owned execution boundaries.
  - explicitly review the `S2-1` target files to confirm no source-level
    `string.split` semantic escape hatch remains in the fallback evaluator.
  - done-when:
    - the legacy branch is gone and the regression set still passes without
      reintroducing a new special case,
    - the remaining uses of `StringSplit` are clearly Wasm-lowering details
      rather than source-level fallback semantics.

- [x] S3-1. Ownership cleanup and documentation sync
  - update `doc/STATE.md` and this document to say:
    - source-level `split` ownership is fully stdlib-driven,
    - backend `StringSplit` remains only a Wasm execution detail,
    - any remaining split-specific code is optimization or lowering support,
      not semantic fallback.
  - done-when:
    - a future reader can tell exactly why `StringSplit` still exists without
      confusing it with the retired runtime builtin branch,
    - the docs match the reviewed post-deletion file boundary from `S2-1`.

- [x] S3-2. Final quality gates
  - run:
    - `cargo fmt`
    - `cargo check`
    - `cargo test`
    - `cargo clippy -- -D warnings`
  - done-when:
    - the retirement slice closes with no pending breakage and no unverified
      boundary assumptions.

## 7. Regression Strategy

Minimum regression set for closing this track:

- compile/runtime-output fallback:
  - selective-import `split`,
  - canonical qualified `string.split`,
  - empty delimiter,
  - Unicode multi-grapheme delimiter,
  - representative helper-chain shape (`split -> list.get` or `split -> each`)
    where relevant to the fallback evaluator.
- Wasm/general-lowered path:
  - preserve the existing C4 regressions,
  - prove no accidental regression in `GeneralLowered` split execution.
- documentation/state:
  - docs must stop implying C4 is still an active milestone in this document,
  - docs must distinguish semantic ownership from backend intrinsic existence.

## 8. Definition Of Done

This stdlib retirement track is complete when all of the following are true:

- no direct runtime builtin branch remains for source-level `string.split`,
- stdlib declaration execution owns source-level split semantics across the
  remaining fallback paths,
- backend `StringSplit` exists only as an explicitly documented Wasm execution
  detail,
- regression tests cover the locked split semantics and the ownership boundary,
- active planning/state docs reflect the final boundary,
- quality gates pass.
