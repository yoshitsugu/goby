# Goby Project State Snapshot

Last updated: 2026-04-13

## Current Focus

Next slice: **Sequence-backed List M6 follow-up** (`doc/PLAN_SEQUENCE.md`) —
continue shared-boundary optimization while closing the remaining trap root
cause on point/nested immutable update workloads.

M6 context:
- Sequence-backed List M0–M5 are complete as of 2026-04-12.
- Candidate B (Chunked Sequence) remains the locked direction in
  `doc/PLAN_SEQUENCE.md §8`.
- Traversal boundaries are now explicit and M5 is fully closed:
  - `length` / `fold` / `map` use explicit `__goby_*` boundaries;
  - `each` is Goby code derived from `__goby_list_fold`;
  - M5-8 runtime regressions now lock fold/each/map execution plus
    `ListReverseFoldPrepend` coexistence for both public `fold` and direct
    `__goby_list_fold` entrypoints;
  - `cargo test -p goby-wasm` was green at the latest recorded M5 snapshot
    (`641 passed; 0 failed; 4 ignored`).

TCO contract reminder (stable, no action needed):
- generic TCO is published and locked in `doc/LANGUAGE_SPEC.md`.
- any next TCO work is post-publication extension (widening backends or call
  categories), not contract bootstrapping.
- the supported/unsupported split (direct tail calls among known top-level
  declarations vs. higher-order/non-tail) is stable and must not be silently
  widened.

Immediate next steps:

- **Sequence M6-2**: close the indexed-read performance gate.
  - keep `xs[i]` on the shared boundary (`BackendIntrinsic::ListGet`) with no
    syntax-specific exceptions;
  - retain new large indexed-read regressions for both `xs[i]` and
    `goby/list.get`;
  - decide whether to further optimize the shared boundary or revise the locked
    M6-2 performance target, since current indexed-read baseline remains near
    M6-0.

- **Sequence M6-3**: close trap root cause after chunk-local `ListSet` rewrite.
  - `ListSet` now copies header + touched chunk only (no full-list copy), but
    locked 4k point-update / 64x64 nested-update workloads still trap.
  - determine whether remaining trap source is update lowering shape, tail-call
    path, or another runtime limit outside the `ListSet` allocation profile.

Checkpoint update (2026-04-10, later slice):
- `goby-wasm` Candidate B migration advanced substantially in
  `gen_lower/emit.rs`, `gen_lower/lower.rs`, and `wasm_exec.rs`.
- Newly fixed in this slice:
  - host-side List formatting/allocation paths now understand chunked List
    headers/chunks (instead of flat `[len][items...]` assumptions);
  - `string.split` helper now emits chunked List values and passes
    `e5_split_list_get_print_executes_via_goby_owned_wasm_runtime`;
  - list-pattern (`[]`, `[h, ..t]`) Wasm validation/runtime regressions caused by
    missing i64 scratch accounting were fixed;
  - nested heap-value list literals (e.g. `List (Tuple ...)`) no longer corrupt
    element placement due to chunk-pointer scratch clobber during element
    emission;
  - mutable nested-list parity and tuple-in-list fold regressions are now green.
- Current blocker set after this checkpoint:
  - RR-4 large-shape runtime regressions remain:
    - `runtime_rr_tests::rr4_recursive_list_spread_large_builder_shape_scales_past_bug_repro_size`
    - `runtime_rr_tests::rr4_named_callback_list_spread_chain_executes_after_callback_rewrite`
    - `runtime_rr_tests::rr4_local_named_callback_list_spread_chain_executes_after_callback_rewrite`
  - all other `cargo test -p goby-wasm list` cases are passing at this checkpoint.

Additional checkpoint (2026-04-10, continued):
- fixed allocator-boundary inconsistency in `emit_alloc_from_top`:
  allocation size is now rounded to 8-byte alignment before both capacity
  checks and cursor movement, removing the previous mismatch where checks used
  unaligned `size` but cursor movement consumed aligned bytes.
- despite that fix, three RR-4 large-shape regressions remain red:
  - `runtime_rr_tests::rr4_recursive_list_spread_large_builder_shape_scales_past_bug_repro_size`
  - `runtime_rr_tests::rr4_named_callback_list_spread_chain_executes_after_callback_rewrite`
  - `runtime_rr_tests::rr4_local_named_callback_list_spread_chain_executes_after_callback_rewrite`
- current observed shape:
  - small/medium recursive list-spread builder samples are green;
  - large `build N` (around N≈40k+) can trap when consuming the resulting list
    via index/pattern paths (`xs[0]`, `[h, ..t]`), while a build-only path can
    still complete.
  - this suggests remaining corruption/contract mismatch around large-shape
    builder result consumption rather than parser/typecheck or small-shape
    lowering.

Latest checkpoint (2026-04-10, RR-4 large-shape follow-up):
- RR-4 large-shape regressions are now resolved.
  - `runtime_rr_tests::rr4_recursive_list_spread_large_builder_shape_scales_past_bug_repro_size`
  - `runtime_rr_tests::rr4_named_callback_list_spread_chain_executes_after_callback_rewrite`
  - `runtime_rr_tests::rr4_local_named_callback_list_spread_chain_executes_after_callback_rewrite`
- Root cause (Candidate B builder path): top-down heap growth reused already
  allocated segments after `memory.grow`, which corrupted earlier list chunks
  in large builders.
- Fixes landed:
  - `emit_alloc_from_top` now treats each post-grow heap span as a disjoint
    segment by moving the heap floor to the previous top and rebasing cursor to
    the current dynamic top (`memory.size * page - reserved`);
  - `list.each` fused print-effect path no longer aliases `item_iter` and
    string-pointer scratch locals; this restored full-element iteration for
    direct callbacks like `each xs println`.
- Verification checkpoint:
  - `cargo test -p goby-wasm` is green.

M4 kickoff checkpoint (2026-04-10, pattern-boundary refactor slice):
- `emit_case_match` list-pattern extraction now reuses shared chunked-sequence
  load helpers (`emit_chunked_load_const` / `emit_chunked_load`) instead of
  inlined shape-specific pointer math.
- Added regression coverage for chunk-boundary pattern extraction:
  - `runtime_rr_tests::rr4_repeated_head_tail_decomposition_crosses_chunk_boundaries`
    (33-item prefix + tail binding over a chunked list).
- Verification checkpoint:
  - `cargo test -p goby-wasm` is green (`623 passed, 0 failed`).

M4 follow-up checkpoint (2026-04-11, shared list-view guards):
- `emit_case_match` now shares list-scrutinee boundary helpers between `[]`
  and non-empty list-pattern arms:
  - `emit_case_list_tag_check`
  - `emit_decode_list_header_ptr`
  - `emit_load_list_total_len`
- Added exact-length list-pattern regression across the first chunk boundary:
  - `runtime_rr_tests::rr4_exact_length_pattern_crosses_chunk_boundary_and_matches`
- Verification checkpoint:
  - `cargo test -p goby-wasm` is green (`624 passed, 0 failed`).

M4 follow-up checkpoint (2026-04-11, tail-binding ownership split):
- Extracted `ListPattern` tail-construction mechanics out of `emit_case_match`
  into `emit_case_bind_tail_list`, keeping list-view arm orchestration thin and
  pushing chunked tail allocation/copy/finalization into a dedicated helper.
- Added regression coverage for empty-tail binding on `[h, ..t]`:
  - `runtime_rr_tests::rr4_head_tail_pattern_binds_empty_tail_for_single_item_list`
- Verification checkpoint:
  - `cargo test -p goby-wasm` is green (`625 passed, 0 failed`).

M4 completion checkpoint (2026-04-11):
- List-pattern forms (`[]`, `[x, ..rest]`, exact-length, prefix/tail variants)
  now run on shared sequence-view boundaries in emitter ownership.
- Honest performance language for repeated list-pattern extraction was added to
  `doc/LANGUAGE_SPEC.md` (amortized O(1) with chunk-boundary
  O(n/CHUNK_SIZE) header-copy cost under bump allocation).
- `doc/PLAN_SEQUENCE.md` now marks M4 complete; next focus is M5.

M5 traversal-boundary checkpoint (2026-04-12):
- M5-6 complete: `stdlib/goby/list.gb` now defines
  `each xs f = __goby_list_fold xs () (fn _ x -> f x)`.
- M5-7 complete: removed legacy dedicated traversal instructions
  (`ListEach`, `ListEachEffect`, `ListMap`) from general-lowering backend IR
  and emitter dispatch. `SPECIALLY_LOWERED_STDLIB_NAMES` now excludes `each`/`map`
  (remaining: `graphemes`, `split`).
- M5-9 complete: `backend_ir.rs` intrinsic docs now explicitly capture
  `ListLength`/`ListFold`/`ListMap` ownership and stdlib wrapper boundaries.
- Root-cause follow-up complete:
  - direct-call codegen now reloads shared heap state after heap-using callees,
    even when their return value is immediate
  - `list.join` now uses `__goby_list_join_string`, avoiding repeated temporary
    Wasm-string allocation during render-heavy loops
- Verification snapshot:
  - `cargo test -p goby-wasm`: `641 passed; 0 failed; 4 ignored`
  - `cargo check`: green at repo root

M5 completion checkpoint (2026-04-12, runtime regression lock):
- M5-8 complete: `crates/goby-wasm/src/runtime_rr_tests.rs` now locks:
  - empty/single-chunk/multi-chunk fold execution;
  - string fold accumulation followed by `println`;
  - `each` general callback and effect callback execution;
  - multi-chunk `map` execution;
  - `ListReverseFoldPrepend` coexistence on both public `fold` and direct
    `__goby_list_fold` lowering/exec paths.
- Result: all M5 sub-steps in `doc/PLAN_SEQUENCE.md` are now complete; next
  planned work is M6 baseline capture and boundary definition.

M6-0 baseline checkpoint (2026-04-12):
- added ignored baseline harness:
  - `runtime_rr_tests::m6_0_baseline_index_update_workloads`
  - command: `cargo test -p goby-wasm m6_0_baseline_index_update_workloads -- --ignored --nocapture`
- locked baseline snapshot in `doc/PLAN_SEQUENCE.md` for:
  - indexed-read 4k mixed indices (`p50=38353us`, `p95=38913us`, success);
  - point-update 4k (`p50=37132us`, `p95=37519us`, currently traps);
  - nested-update 64x64 (`p50=60874us`, `p95=62130us`, currently traps);
  - AoC-style `iterative_grid_pruning_after_render`
    (`p50=199182us`, `p95=209906us`, success, total=43).
- next work is now M6-1 boundary definition, then M6-2 chunk-aware read.

M6-1 boundary-definition checkpoint (2026-04-12):
- shared index/update ownership is now explicit in
  `crates/goby-wasm/src/gen_lower/backend_ir.rs`:
  - `xs[i]` and canonical `list.get` both route to `BackendIntrinsic::ListGet`;
  - `xs[i] := v` routes via `CompExpr::AssignIndex` and lowers through shared
    path-copy mechanics (`ListGet` descent + `ListSet` ascent).
- `doc/PLAN_SEQUENCE.md` now marks M6-1 complete.
- next work is M6-2 chunk-aware indexed-read implementation.

M6-2 checkpoint (2026-04-12, progress update):
- `BackendIntrinsic::ListGet` chunk-aware index decomposition now uses
  CHUNK_SIZE-aware bit operations (`>> 5`, `& 31`) in
  `crates/goby-wasm/src/gen_lower/emit.rs` while preserving existing tag/bounds
  checks and out-of-range trap behavior.
- added large indexed-read regressions in
  `crates/goby-wasm/src/runtime_rr_tests.rs`:
  - `m6_2_indexed_read_4k_mixed_indices_executes_without_trap`
  - `m6_2_indexed_read_surface_and_stdlib_get_match_on_multi_chunk_list`
- verification snapshot:
  - `cargo test -p goby-wasm m6_2_indexed_read -- --nocapture`: green.
  - `cargo test -p goby-wasm m6_0_baseline_index_update_workloads -- --ignored --nocapture`:
    indexed-read remains near baseline (`p50=36858us`, `p95=37768us`);
    locked M6-2 5x gate is not yet met, so M6-2 stays open.

M6-3 checkpoint (2026-04-13, progress update):
- `BackendIntrinsic::ListSet` in `crates/goby-wasm/src/gen_lower/emit.rs` was
  rewritten from full-list copy to chunk-local immutable update:
  - header pointer table copy;
  - touched-chunk copy;
  - single-element patch in replacement chunk.
- added M6 follow-up runtime fixtures in
  `crates/goby-wasm/src/runtime_rr_tests.rs`:
  - `m6_3_point_update_4k_executes_without_trap` (currently `#[ignore]`)
  - `m6_3_nested_update_64x64_executes_without_trap` (currently `#[ignore]`)
- measurement snapshot:
  - `cargo test -p goby-wasm m6_0_baseline_index_update_workloads -- --ignored --nocapture`
    still reports trap-bearing point/nested workloads (`E-RUNTIME-TRAP` at
    `goby!main`), so M6-3 remains open pending trap root-cause isolation.

## Recently Completed

- **Sequence-backed List M4** (complete, 2026-04-11). Re-founded list pattern
  matching on shared sequence-view boundaries in Wasm emission. Added/locked
  chunk-boundary and empty-tail regressions
  (`rr4_repeated_head_tail_decomposition_crosses_chunk_boundaries`,
  `rr4_exact_length_pattern_crosses_chunk_boundary_and_matches`,
  `rr4_head_tail_pattern_binds_empty_tail_for_single_item_list`) and documented
  honest repeated-pattern performance language in `doc/LANGUAGE_SPEC.md`.

- **Sequence-backed List M2** (complete, 2026-04-10). Evaluated Candidates A/B/C
  via static complexity + allocator-pressure analysis. Locked direction as
  Candidate B (Chunked Sequence). §6.6 workload matrix and success bar recorded.
  Candidate A rejected (O(n) prepend/split, harmful in RR-4). Candidate C
  rejected (O(n/32) indexed read and update). See `doc/PLAN_SEQUENCE.md`.

- **Sequence-backed List M1** (complete, 2026-04-10). Rewrote `List` description
  in `doc/LANGUAGE_SPEC.md` as surface-semantic (not linked-list identity).
  list patterns described as sequence views. Indexed access noted as intended-
  practical with no current complexity guarantee. `doc/PLAN.md` preamble updated
  to register `doc/PLAN_SEQUENCE.md`.

- **Sequence-backed List M0** (complete, 2026-04-10). Product contract locked
  in `doc/PLAN_SEQUENCE.md`: `List` is the user-facing surface; runtime direction
  locked toward sequence-backed form; success bar defined as "practical for
  ordinary scripts"; optimization-boundary policy adopted from §4.3/§4.8.

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

- **Resolved**: which direct-call forms to accept after `tail_analysis`? —
  local alias chains to known top-level declarations are now covered (M5).
- **Resolved**: how far to extend the dispatcher boundary? — locked at M7;
  current published guarantee covers statically resolvable direct tail calls
  among known top-level declarations on the compiled Wasm path.
- **Resolved**: CHUNK_SIZE choice for current Candidate B runtime —
  locked at `32` for M3 (`crates/goby-wasm/src/gen_lower/emit.rs`).
- How much should stdlib traversal optimization be expressed in sequence
  intrinsics versus iterator/effect lowering? (M5 scope.)
- What is the minimum explicit intrinsic/lowerer surface for `List` that keeps
  stdlib readable? (M5 scope.)

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/PLAN_SEQUENCE.md`](PLAN_SEQUENCE.md) — active List redesign roadmap (M5 next)
- [`doc/PLAN_TCO.md`](PLAN_TCO.md) — TCO plan (all milestones complete)
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
