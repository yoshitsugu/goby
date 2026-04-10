# Goby Project State Snapshot

Last updated: 2026-04-10

## Current Focus

Next slice: **Track RR, RR-5** (`PLAN.md §4.5`) - M7 is complete; any next TCO
work is post-publication extension work rather than contract bootstrapping.

Locked TCO contract reminder:

- the current language-level TCO guarantee is now locked in
  `doc/LANGUAGE_SPEC.md`;
- it applies to the compiled Wasm path for the currently covered statically
  resolvable direct tail-call subset among known top-level declarations;
- unsupported or uncovered shapes may still execute as ordinary calls, but they
  are not part of the constant-stack guarantee and must not be described as
  covered generic TCO.

Locked ideal goal for RR:

- make "write the obvious recursive/list-building program first" a rational
  default for Goby users;
- push resilience into shared lowering/runtime boundaries instead of accumulating
  source-shape-specific exceptions;
- keep the remaining limits explicit, attributable, and honestly diagnosed.

RR execution reminder:

- at the start of each RR implementation step, review the locked ideal goal
  above and reject any step that cannot be justified as progress toward that
  shared design.

Immediate next steps:

- **RR-5**: continue the planned generic tail-call optimization track now that
  the shared tail-position analysis, direct tail-call normalization, and
  unified dispatcher-based constant-stack execution model are in place.
  - keep the RR-3/RR-4 shared-boundary discipline: prefer reusable control-flow
    rules over symbol-specific recursion rewrites.
  - the public wording decision is now locked:
    Goby has generic TCO on the documented compiled Wasm path for statically
    resolvable direct tail calls among known top-level declarations.
  - any next RR-5/TCO task is therefore an extension question:
    whether to widen backend scope, call categories, or diagnostics beyond the
    published direct-call compiled-Wasm contract.
  - keep the documented supported/unsupported split stable:
    let/block tails and statically resolvable local alias chains are covered;
    indirect/higher-order tails, unresolved local funcrefs, and non-tail
    recursion remain outside the guarantee.
  - keep current diagnostics and docs aligned with the locked M0 contract for
    shapes that still fall outside the optimized subset or still execute as
    ordinary calls.
- keep RR-1 diagnostics best-effort and explicit about uncertainty
  (`likely stack pressure`, `memory exhaustion`, `unknown runtime trap`) as
  later resilience work lands.

## Recently Completed

- **Track RR, RR-5 published TCO guarantee** (complete for M7, 2026-04-10).
  - the final public wording decision is now locked:
    Goby has generic TCO on the documented compiled Wasm path for statically
    resolvable direct tail calls among known top-level declarations.
  - `README.md` now carries a short high-level version of that statement, while
    `doc/LANGUAGE_SPEC.md` keeps the detailed supported/unsupported boundary.
  - `examples/tco.gb` and `examples/README.md` now publish one representative
    example that is explicitly allowed to satisfy multiple guarantee buckets:
    self recursion, sibling/non-self calls, mutual recursion, control-flow
    preservation, and local alias preservation.
  - the unsupported buckets from M6 remain explicit in the published wording,
    so the “generic TCO” statement stays bounded by the documented direct-call
    compiled-Wasm contract rather than implying universal higher-order or
    backend-agnostic TCO.

- **Track RR, RR-5 failure-boundary contract** (complete for M6, 2026-04-10).
  - locked compile-path regressions now prove these buckets stay outside the
    shared direct-call TCO guarantee:
    - indirect/higher-order tail-looking calls via
      `compile_module_indirect_tail_call_stays_outside_direct_tco_guarantee`;
    - unresolved local function-value tail-looking calls via
      `compile_module_unresolved_local_funcref_tail_call_stays_outside_direct_tco_guarantee`;
    - non-tail recursion via
      `compile_module_non_tail_recursion_stays_outside_tco_guarantee`.
  - the chosen M6 contract for those buckets is explicit:
    accepted execution may remain on ordinary `IndirectCall` / `DeclCall`
    paths, may still consume stack, and must be documented as outside the
    guarantee rather than as covered TCO.
  - backend/path mismatch is no longer tracked as a separate unsupported-shape
    matrix row; the TCO contract is simply scoped to the compiled Wasm path.
  - docs now distinguish “outside the guarantee” from “compiler regression” so
    a missed covered shape remains a bug to fix, not an allowed fallback.

- **Track RR, RR-5 control-flow completeness proof** (complete for M5,
  2026-04-10).
  - compile/runtime proof now locks the current covered shared-tail-call
    boundary through:
    - tail `if` joins via
      `compile_module_tail_if_join_uses_shared_dispatcher_boundary` and
      `rr5_tail_if_join_repro_survives_tight_stack_limit`;
    - tail `case` joins via
      `compile_module_tail_case_join_uses_shared_dispatcher_boundary` and
      `rr5_tail_case_join_repro_survives_tight_stack_limit`;
    - let/block tail structure via
      `compile_module_let_tail_decl_call_uses_shared_dispatcher_boundary` and
      `rr5_let_tail_repro_survives_tight_stack_limit`;
    - local alias chains to known top-level declarations via
      `compile_module_local_alias_tail_decl_call_uses_shared_dispatcher_boundary`
      and
      `rr5_local_alias_tail_repro_survives_tight_stack_limit`.
  - `goby-wasm` tail-call lowering now resolves statically known local alias
    chains through the same direct-declaration boundary, so covered alias tails
    no longer fall back to indirect calls.
  - `doc/LANGUAGE_SPEC.md` now explicitly includes let/block tails and
    statically resolvable local alias chains in the current compiled-Wasm
    constant-stack direct-call guarantee.

- **Track RR, RR-5 shared dispatcher execution model** (complete for M4,
  2026-04-10).
  - `goby-wasm` now routes every covered aux declaration that participates in
    the direct `TailDeclCall` graph through one shared dispatcher-based
    constant-stack engine on the compiled Wasm path.
  - this replaces the split self-tail loop vs SCC-dispatch design with one
    backend-owned execution boundary that covers single-member self-tail
    declarations, sibling/mutual groups, and acyclic direct-tail chains into
    covered recursive members.
  - public aux declaration wrappers remain as stable direct-call/funcref
    entrypoints, so grouped declarations still behave the same when referenced
    as function values.
  - compile coverage now locks:
    - shared-dispatch self-tail execution without wrapper recursion via
      `compile_module_self_tail_decl_member_uses_shared_dispatcher_without_wrapper_recursion`;
    - shared-dispatch acyclic covered-tail execution without direct-call
      fallback via
      `compile_module_acyclic_tail_chain_member_uses_shared_dispatcher_without_direct_call_fallback`;
    - shared-dispatch mutual recursion without wrapper recursion via
      `compile_module_mutual_tail_decl_group_emits_dispatch_loop_without_wrapper_recursion`.
  - runtime coverage now locks:
    - tight-stack shared-dispatch self tail via
      `rr5_self_tail_recursion_repro_survives_tight_stack_limit_after_tail_decl_loop`;
    - tight-stack shared-dispatch mutual recursion via
      `rr5_mutual_tail_recursion_repro_survives_tight_stack_limit_after_group_dispatch`;
    - tight-stack shared-dispatch acyclic covered tail via
      `rr5_acyclic_tail_chain_repro_survives_tight_stack_limit_on_shared_dispatcher`;
    - preserved grouped function-value entry behavior via
      `grouped_tail_decl_member_still_works_through_function_value_entrypoint`.

- **Track RR, RR-5 shared tail-position boundary** (partial, 2026-04-09).
  - added `goby_core::tail_analysis` as a compiler-owned IR analysis for tail
    position propagation through `let`, `seq`, `if`, and `case`.
  - made nested computation boundaries explicit via configurable lambda /
    handler-clause policies so future TCO work can state exactly which bodies it
    analyzes.
  - locked focused tests for direct tail-call collection in declaration mode and
    non-tail treatment of nested lambda bodies for resumptive-handler mode.
  - rewired `goby-wasm` handler legality to use the shared analysis, proving
    the new RR-5 boundary in existing behavior without widening the accepted
    resumptive subset.
  - this slice establishes RR-5 milestone M1 ownership but does not yet
    normalize or execute generic tail calls in constant stack.

- **Track RR, RR-5 direct tail-call normalization** (partial, 2026-04-09).
  - normalized eligible direct top-level declaration calls in tail position to
    `WasmBackendInstr::TailDeclCall` rather than leaving them indistinguishable
    from ordinary `DeclCall`.
  - propagated that normalization through tail `if` / `case` joins while
    keeping non-tail statement positions on the ordinary direct-call form.
  - kept emitter behavior unchanged for now, so `TailDeclCall` still executes
    like a normal direct Wasm call until the direct-call group execution model
    lands.
  - locked focused lowering tests for:
    - root tail-position direct calls,
    - non-tail `seq` statements staying ordinary,
    - `case` arm tails normalizing to the tail-call marker.

- **Track RR, RR-5 self-tail loop execution** (partial, 2026-04-10).
  - `goby-wasm` now gives aux declarations with self `TailDeclCall` a looped
    Wasm helper body and rewrites the recursive tail edge to parameter updates
    plus a branch back to the loop head.
  - this moves the historical self-tail recursion bucket off raw stack growth
    through the shared `TailDeclCall` boundary instead of another declaration-
    specific lowering rule.
  - compile coverage now proves the emitted helper body validates and contains a
    loop without directly calling itself via
    `compile_module_self_tail_tail_decl_call_emits_looped_helper_without_recursive_call`.
  - runtime coverage now proves the tight-stack self-tail representative
    succeeds via
    `rr5_self_tail_recursion_repro_survives_tight_stack_limit_after_tail_decl_loop`.
  - non-self and mutually recursive direct tail-call groups remain the next
    RR-5 execution slice.

- **Track RR, RR-5 sibling/mutual direct-call group execution** (partial, 2026-04-10).
  - `goby-wasm` now detects strongly connected aux-decl groups linked by
    `TailDeclCall` and emits a shared dispatcher helper with one looped
    constant-stack execution path for the whole group.
  - each public aux decl in such a group now becomes a thin wrapper that seeds
    the dispatcher tag/arguments, preserving existing direct-call and funcref
    entrypoints while moving sibling/mutual recursion onto the shared
    backend-owned boundary.
  - tail-position intra-group edges now rewrite to dispatcher-local tag/arg
    updates plus a branch back to the shared loop head, extending the RR-5
    constant-stack story beyond self recursion without adding symbol-specific
    rewrites.
  - compile coverage now proves a representative mutual tail-recursive pair
    lowers to a looped dispatcher without recursing through the public wrappers
    via
    `compile_module_mutual_tail_decl_group_emits_dispatch_loop_without_wrapper_recursion`.
  - runtime coverage now proves the tight-stack mutual representative succeeds
    via
    `rr5_mutual_tail_recursion_repro_survives_tight_stack_limit_after_group_dispatch`.
  - this historical slice established the initial grouped dispatcher boundary;
    the later M4 completion slice widened it beyond SCC-local membership.

- **Track RR, RR-4 builder-backed list-spread lowering** (complete, 2026-04-09).
  - fixed the historical recursive `[x, ..rest]` memory-exhaustion bug without
    changing the external `List` representation.
  - restricted self-recursive list builders now lower to a loop plus internal
    growable builder ops instead of recursively materializing singleton prefix
    lists and concatenating the full tail at every step.
  - compile coverage now proves the emitted Wasm validates and no longer
    directly self-calls in the specialized helper body.
  - runtime coverage now locks both the original `doc/BUGS.md` repro and a
    larger builder-shaped variant as successful executions.
  - a follow-up slice widened the same RR-4 ownership to inline empty-acc
    `fold` callbacks that prepend with `[x, ..acc]`, replacing callback-side
    concat chains with a reverse traversal over the source list.
  - compile coverage now locks that main-level rewrite via
    `compile_module_inline_fold_prepend_lowering_rewrites_concat_chain_in_main`.
  - runtime coverage now locks the inline callback success case via
    `rr4_inline_fold_prepend_builder_executes_after_specialized_lowering`.
  - the final RR-4 slice rewrote supported named/local callback prepend shapes
    onto the same reverse-fold boundary, closing the remaining `fold seed []
    prepend` style bucket without changing user-visible `List` semantics.
  - compile coverage now locks the named/local callback rewrite via
    `compile_module_named_fold_prepend_lowering_rewrites_decl_callback_chain_in_main`
    and
    `compile_module_local_alias_fold_prepend_lowering_rewrites_decl_callback_chain_in_main`.
  - runtime coverage now locks both named/local callback executions via
    `rr4_named_callback_list_spread_chain_executes_after_callback_rewrite` and
    `rr4_local_named_callback_list_spread_chain_executes_after_callback_rewrite`.
  - RR-4 is complete for the currently locked representative list-spread /
    prepend-builder buckets.

- **Track RR, RR-3 tight-stack proof** (complete, 2026-04-09).
  - kept the RR-3 fix at the shared scan boundary: restricted self-recursive
    Int scans now lower to loop-form backend IR instead of self `DeclCall`.
  - proved the compile path end-to-end: emitted Wasm validates and the looped
    helper body no longer directly calls itself.
  - proved the execution boundary under the RR low-stack configuration for both
    the primary non-tail scan bucket and the callback-assisted scan bucket.
  - RR-3 is now closed for the currently locked representative scan buckets;
    remaining resilience work moves to RR-4 list-spread ownership.

- **Track RR, RR-3 emitted-Wasm proof** (partial, 2026-04-09).
  - added backend-IR loop support plus a lowering rewrite for a restricted
    self-recursive Int scan shape in `gen_lower/lower.rs`.
  - switched loop emission to `loop (result i64)` so the specialized scan shape
    produces valid Wasm without relying on an outer result block.
  - verified both that supported scan shapes lower to `Loop` and that the
    compiled Wasm helper body validates and eliminates direct self `call`.
- **Track RR, RR-2 representative decomposition** (complete, 2026-04-08).
  - added focused Goby-owned representative repro tests in
    `crates/goby-wasm/src/runtime_rr_tests.rs` for:
    - self tail recursion under a tight stack limit,
    - non-tail recursive scan,
    - recursive list spread / concat growth,
    - callback-assisted recursion in the scan hot path.
  - locked the RR-2 ownership decision:
    - self tail recursion belongs to shared recursion lowering/runtime stack
      behavior, but is not the default next slice by itself;
    - non-tail recursive scans are the primary RR-3 target;
    - callback-assisted recursion currently stays bundled with that same RR-3
      scan boundary rather than a separate callback-only fix;
    - recursive list spread remains RR-4 work owned by list representation /
      concat behavior, not by recursion lowering.
  - explicitly rejected these first-fix boundaries:
    - Wasm limit tuning alone,
    - symbol-specific rewrites for `count_valid_roll` /
      `collect_prune_positions`,
    - self-tail-only lowering as the default next slice.
- **Track RR, RR-2 exploration note** (recorded, 2026-04-08; reverted, no code change kept).
  - tried a narrow lowering/emission experiment for aux-decl self tail recursion
  only.
  - the experiment was reverted because it broke existing Wasm validation / runtime
    behavior for recursive list-pruning programs such as
    `iterative_grid_pruning_after_render_executes_without_heap_cursor_corruption`
    and `aoc2025/04/solve2.gb`.
  - the failed slice still established two useful facts:
    - self tail recursion alone is not the dominant issue for the `solve2.gb`
      class of failures;
    - with named Goby frames now present in Wasm backtraces, the hot failing path
      is visible as `fold -> should_prune_cell/check_around_rolls ->
      collect_prune_positions/count_valid_roll`, which points more strongly to
      non-tail recursive scanning and list-building than to `check` alone.
- **Track RR, RR-0 / RR-1** (complete, 2026-04-08).
  - added a CLI regression for the minimal recursive list-spread memory bug.
  - classified runtime traps in `goby-wasm` with user-facing English messages
    and normalized CLI rendering.
  - emitted Wasm function names for Goby-generated functions so unknown-trap
    secondary detail now identifies frames like `goby!check` instead of only
    raw function indices.
  - fixed direct-call heap-cursor synchronization so:
    - caller-owned live heap values are flushed before every direct call,
    - heap-returning callees still refresh the caller cursor after the call.
  - verified this against:
    - the new recursive list-spread regression,
    - `cc4_mutable_write_capture_via_each_executes_correctly`,
    - `iterative_grid_pruning_after_render_executes_without_heap_cursor_corruption`,
    - `cargo clippy -- -D warnings`,
    - `cargo test --workspace`.
- **Track RR planning** (recorded, 2026-04-08). Added `PLAN.md §4.5` to prioritize
  user-facing diagnostics first, then recursion/list-spread/runtime-limit resilience work.
- **Bug capture** (recorded, 2026-04-08). Added a minimal non-AoC reproduction to `doc/BUGS.md`
  for runtime-`Read` recursive list-spread memory exhaustion.
- **Track CL**: Closure-captured mutable list fix (complete, 2026-04-07). `lower_value_ctx` for effect/call args; cell-promoted `AssignIndex` write-back.
- **Track MLF**: Mutable list runtime execution fix (complete, 2026-04-07). Unified `RuntimeLocals` store.
- **Track LM**: Mutable list element assignment (complete, 2026-04-06). `a[i] := v`, `a[i][j] := v`.
- **Track CC**: Closure capture (complete, 2026-04-02). CC0–CC6, shared-cell model.

## Architecture State

| Layer | Status |
|---|---|
| Parser (`parser_stmt.rs`) | Stable. Includes list-index assignment (`AssignTarget`) |
| Resolver (`resolved.rs`) | Stable. `ResolvedTarget` for assignment targets |
| Typechecker | Stable. Validates assignment targets, effects, generics |
| IR (`ir.rs`) | Stable. Includes `CompExpr::AssignIndex` |
| IR lowering (`ir_lower.rs`) | Stable. `doc/PLAN_IR.md` is the architectural reference |
| Wasm backend | Stable. Closures, effects, list mutation implemented |
| Effect handlers | Non-tail / multi-resume produces `BackendLimitation` |
| GC / reclamation | Out of scope. Bump allocator only |

## Known Deferred Items

- Multiline/body-relative span ownership for diagnostics (`doc/PLAN.md`, review follow-ups and spec-detail notes)
- `Float` type (`PLAN.md §4.3`)
- Migrate effect runtime dispatch to compiled `EffectId`/`OpId` tables (`PLAN.md §5`)

## Open Questions

- Which direct-call forms should the next normalization slice accept once
  `tail_analysis` has marked tail position: only direct declaration names, or
  also resolvable local aliases to those names?
- How far should the new SCC-local dispatcher boundary extend before Goby can
  honestly broaden the current M0 contract toward the stronger generic-TCO
  statement targeted by `doc/PLAN_TCO.md`?

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
