# Goby Project State Snapshot

Last updated: 2026-04-26 (Perceus M7 complete; §1 goal satisfied)

## Current Focus

**Perceus M0–M7 complete. §1 goal satisfied.**

`examples/refcount_reuse_loop.gb` reports `total_bytes=108704
peak_bytes=108704 free_list_hits=0 reuse_hits=5000`, below the
`< 200 KiB` budget. The bump-only allocator has been replaced by a
refcount + free-list model; all active documentation updated to reflect
this. M7 closes the Perceus roadmap.

What landed:
- Step 7-a / 7-a.5: `mut ys = xs; ys[i] := v` gets an `AssignIndex`
  reuse token when `xs` is an M4.5-`Owned` parameter. GeneralLower runs
  Perceus with imported stdlib declarations visible, and
  `__goby_list_length` is modelled as a borrowed-list intrinsic so
  `length(xs)` no longer inserts the stale `Dup(xs)` that kept runtime
  rc at 2.
- Step 7-c measurement correction: after header reuse fired, `iters=0`
  still allocated ~149 MiB, proving the remaining budget blocker was
  not `AllocReuse` chunk bumping. The actual sources were
  `build 0 4096 []` (`[k, ..acc]` tail-recursive prepend accumulator)
  and `xor_fold` (`[x, ..rest]` tail pattern). GeneralLower now lowers
  the build shape through a builder-backed loop and lowers the
  self-recursive list-case integer fold through `ListFold`, avoiding
  suffix-list allocation.

M6 Step 4 (2026-04-26, landed):
- `AssignIndex.reuse_token` reshaped to `Option<AssignIndexReuse { root_token, levels: Vec<Option<SizeClass>> }>` (commit 4f9f701).
- `perceus_reuse::SizeOrCell::FreshList { outer, inner }` records inner shape only when every outer-list element is a syntactically fresh, non-aliased list literal of the same shape (`is_list_shape` filter blocks Tuple/Record/String inner shapes that cannot anchor a List path) (commit b00eab1).
- `lower_assign_index_reuse` consumes `levels` and emits `ListSetInPlace` for ascent levels `i < reuse_depth`; inner levels need no extra `RefCountDropReuse` / `AllocReuse` because `ListGet` does not bump the inner header's refcount (commit 91360ca).

Verification:
- `cargo fmt --all --check` green.
- `cargo test --workspace` green (goby-core 757 passed +3 new perceus_reuse tests; goby-wasm 699 passed +4 new lower/runtime tests).
- New focused tests:
  - perceus_reuse: `assign_index_fresh_nested_literal_records_inner_class`, `assign_index_shared_inner_var_falls_back`, `assign_index_first_none_stops_reuse`.
  - gen_lower::lower::tests: `lower_assign_index_two_levels_uniform_inner_reuses_all_levels`, `lower_assign_index_two_levels_inner_none_after_outer_reuse_uses_listset`.
  - compile_tests: `nested_assign_index_fresh_literal_reuses_inner_in_place`, `nested_assign_index_shared_inner_falls_back`.

Next actions (ordered, per PLAN_PERCEUS §M6 "Step 7 execution plan"):

1. **Step 7-a: done.** Seed `mut ys = xs` from the parameter's outer size class
   in `comp_alloc_size_or_cell`, **gated on M4.5 `Owned` classification
   only**. Reason: smallest precondition for reuse to fire on the
   benchmark; restricting to `Owned` keeps the §3.4 uniqueness invariant
   intact. Verify `reuse_hits > 0` on `refcount_reuse_loop.gb` (do not
   expect `total_bytes` to clear 200 KiB yet).
2. **Step 7-a.5: done.** Model `__goby_list_length` as a borrowed-list
   intrinsic and run Perceus with imported stdlib declarations visible in
   GeneralLower, so `length(xs)` no longer inserts `Dup(xs)` before the
   reuse site.
3. **Step 7-b: done.** Re-measured:
   `cargo run -p goby-cli -- run --debug-alloc-stats --max-memory-mb 16 examples/refcount_reuse_loop.gb`
   reports `total_bytes=149354768 peak_bytes=149354768 free_list_hits=0
   reuse_hits=5000`.
4. **Step 7-c: done via measured source fix.** The measured blocker was
   build/fold allocation, not `AllocReuse` chunk bumping. Added
   specialized lowering for tail-recursive prepend-accumulator list
   builders and self-recursive list-case integer folds.
5. **Step 7-d: done.** Un-ignore/enable the `total_bytes < 200 * 1024` assertion in
   `wasm_exports_and_smoke.rs::refcount_reuse_loop_example_compiles`.
6. **Step 5 (`stdlib/goby/list.gb` `set` rewrite) deferred to
   post-acceptance.** Reason: the benchmark does not exercise
   `list.set`, and PLAN already states Step 4 alone cannot close the
   budget — i.e. acceptance depends on 7-a/7-c, not Step 5. Deferring
   isolates any stdlib ripple from the reuse plumbing landing and
   keeps `alloc_baseline.txt` deltas attributable.

Earlier history (kept for context):

M6 Step 4 WIP (2026-04-25, 作業中):
- `lower_assign_index_reuse` を `depth > 1` に拡張 (lower.rs)。
  - root は `RefCountDropReuse` → outer `ListSetInPlace` → `AllocReuse(Retain)` で header reuse。
  - inner levels は引き続き `ListSet` copy-on-write。
  - これにより inner list alias の value semantics を保ったまま outer header だけ reuse する。
  - full per-level inner reuse / dynamic fallback は未実装で、Step 4 の残り。
- `perceus_reuse::comp_alloc_class` が pure `Let` / `LetMut` で包まれた initializer の最終 allocation class を拾うよう修正。
  - nested list literal lowering は ANF let を含むため、これがないと `LetMut xs = [[...], ...]` の size class が seed されない。
- Tests added:
  - `perceus_reuse::tests::assign_index_unique_with_anf_wrapped_initializer_inserts_reuse`
  - `gen_lower::lower::tests::lower_assign_index_two_levels_with_reuse_reuses_outer_only`
  - `compile_tests::nested_assign_index_unique_reuses_outer_only`

Verification so far:
- `cargo test -p goby-core perceus_reuse::tests::assign_index_unique_with_anf_wrapped_initializer_inserts_reuse`
- `cargo test -p goby-wasm gen_lower::lower::tests::lower_assign_index_two_levels_with_reuse_reuses_outer_only`
- `cargo test -p goby-wasm compile_tests::nested_assign_index_unique_reuses_outer_only`
- `cargo fmt --all --check`
- `cargo check`
- `cargo test --workspace`

Next actions:
- Finish Step 4 per-level reuse for inner unique lists, including fallback when a level is shared.

M6 Step 3 WIP (2026-04-25, 作業中):
- `AssignIndex.reuse_token` の型を `Option<String>` → `Option<(String, SizeClass)>` に変更 (ir.rs)。
  - size class を backend まで伝搬するため必要。
- `BackendAllocInit::Retain` variant を追加 (backend_ir.rs):
  - ペイロードを再初期化せず refcount=1 だけリセットする `alloc_reuse` ケース。
  - `emit_alloc_reuse` の `Retain` ケース: `emit_alloc_reuse_payload` + `emit_push_tagged_ptr(TAG_LIST)` のみ。
- `lower_assign_index_reuse` 関数を実装 (lower.rs):
  - `RefCountDropReuse` → `ListSetInPlace` → `AllocReuse(Retain)` シーケンスを生成。
- `insert_reuse_seq` 内の `Assign` rebind 後 `live_sizes` を更新するよう修正 (perceus_reuse.rs)。
- `required_heap_base_spill_count` の `BackendAllocInit::Retain` を `1` に修正 (emit.rs):
  - `AllocReuse(Retain)` も `emit_alloc_reuse_payload` の `object_ptr` spill local を使うため、
    `0` のままだと scratch i32 local が不足し wasm validation error になる。

**Resolved blocker (Step 3 WIP):**

Three integration tests fail with wasm validation error:
  `lm_single_level_list_assign_updates_element`, `lm_multiple_assigns_to_different_indices`,
  `lm_value_semantics_no_aliasing`

Error: `CodegenError { message: "wasm load: failed to compile: wasm[0]::function[9]::main" }`

Root cause:

1. **Stack shape verified correct** — traced `lower_assign_index_reuse` instruction sequence:
   - `LoadLocal root` → `RefCountDropReuse{token_local}` (consumes 1, stores token; net: 0)
   - `LoadLocal root` + `LoadLocal/const index` + rhs computation → `ListSetInPlace` (consumes 3, pushes 1)
   - `StoreLocal root` (consumes 1; net: 0)
   - `AllocReuse{Retain}` (produces 1) → `Drop` (consumes 1; net: 0)
   - `I64Const(unit)` (net: +1 — function result)
   Stack balance looks correct.

2. **`BackendAllocInit::Retain` in `emit_alloc_reuse`** — added arm calls `emit_alloc_reuse_payload`
   then `emit_push_tagged_ptr(object_ptr, TAG_LIST, pw)`. This mirrors the TupleLit pattern.
   The `emit_alloc_reuse_payload` if/else branches both store a ptr into `object_ptr` before
   returning, so `object_ptr` is valid when `emit_push_tagged_ptr` reads it.

3. **Scratch local count for `Retain`** — `required_heap_base_spill_count` returned 0 for
   `Retain`, but `emit_alloc_reuse` computes `object_ptr =
   hs.scratch.i32_base + HELPER_SCRATCH_I32 + heap_base_depth` and stores through it.
   `Retain => 1` fixes under-allocation of scratch i32 locals.

Verification now passes:
- `cargo test -p goby-wasm --test wasm_exports_and_smoke lm_`
  - `lm_single_level_list_assign_updates_element`
  - `lm_multiple_assigns_to_different_indices`
  - `lm_value_semantics_no_aliasing`
  - `lm_nested_two_level_list_assign`
- `cargo fmt --all --check`
- `cargo check`
- `cargo test --workspace`

M6 Step 1 (2026-04-25, landed):
- `perceus_reuse.rs` に `AssignIndex` reuse-site 認識を追加。
- `SizeOrCell` enum, `comp_contains_conservative_abort` ヘルパ追加。
- `insert_reuse_seq` で `Assign` rebind 後 `live_sizes` 更新 (NOTE(step3) コメント削除済み)。
- 5 本のテスト追加。

M5 Step 10 (2026-04-25, previous):
- §3.3 `alloc_reuse` / `drop_reuse` セマンティクスは Step 7/9 で inline 発行済み。
- 5 本の correctness test (perceus_reuse + compile_tests)。

**Next actions:**
- Finish M6 Step 4 inner per-level reuse/fallback.
- M6 completion → Step 11 acceptance gate (`total_bytes < 200 KiB`).

See `doc/PLAN_PERCEUS.md` §M4 "As-shipped scope note" for the full M4 scope,
and the M5 section for the current step-by-step progress.

- Free-list head table in linear memory (`HEAP_BASE` 56 → 408), `SizeClass` enum,
  `emit_alloc_with_flag` (free-list pop + bump fallback), `emit_free_list_push`.
- `__goby_drop` Wasm function with full per-type child-drop:
  - TAG_CHUNK: drops each item slot, pushes chunk to `SizeClass::Chunk` free-list
  - TAG_LIST: reads n_chunks, synthesises TAG_CHUNK-tagged ptrs and recurses
  - TAG_TUPLE: reads arity word, drops each element slot
  - TAG_CELL: drops contained value, pushes to Cell free-list
  - TAG_RECORD / TAG_CLOSURE: `freed_bytes` accounting only (arity not in
    payload at runtime; full child-drop deferred to M4 with layout change)
- `free_list_hits` counter wired; `peak_bytes` tracked via `freed_bytes`.
- `EmitOptions::expose_perceus_test_exports` flag added; emits
  `__test_alloc_list_1chunk` / `__test_drop_ptr` / `__goby_drop` exports for
  the acceptance test.
- Acceptance test `drop_frees_unique_list_and_subsequent_alloc_gets_free_list_hit`
  passes: allocates 1-chunk list, drops it, re-allocates, asserts
  `GLOBAL_FREE_LIST_HITS_OFFSET > 0`.
- M4 groundwork landed on 2026-04-20:
  - shared IR now has `CompExpr::Dup` / `CompExpr::Drop`
  - IR formatting/validation and traversal helpers understand those nodes
  - general Wasm lowering maps them to backend refcount ops
  - emitter now generates module-local `__goby_dup` alongside `__goby_drop`
  - Perceus pipeline-order assertion helper added and called from general-lower
    and runtime-entry drivers
- M4 conservative slice landed on 2026-04-21:
  - `goby-core::perceus::run_perceus_passes` classifies every function
    parameter and fresh-heap `let` as `Owned`, and inserts `Drop`/`Dup` in
    the unambiguous cases:
    - `let x = <fresh-heap>` with `use_count == 0`: `Drop(x)` at the top
      of the body. Any other use count leaves the binding alone — a
      single mid-body consume (e.g. `Call(process, [x])`) would otherwise
      collide with a post-body Drop and double-free.
    - unused `case` pattern bindings: `Drop(pat_var)` at arm entry
    - owned bindings live across a `WithHandler` body: `Dup` before the
      handler boundary (Case pattern binds and Handle clause params are
      excluded from the live-across set)
    - `Call`-site: owned args transfer ownership to the callee; the caller
      emits no post-call `Drop` for those args
    - owned parameters: `Drop` only for params that are never referenced
      in the body
  - 11 perceus unit tests cover pipeline wiring, fresh-heap Drop placement,
    nested-Let / Seq-Call double-free regressions, call-site transfer,
    partial application, closure capture, case-arm pattern drop, and
    WithHandler live-across Dup
  - the rewrite is wired into general-lower for user decls and stdlib-loaded
    aux decls
  - intentionally deferred to M4.5 (borrow inference):
    - parameter last-use analysis (requires borrow/consume marks on
      `Var` occurrences; intrinsics like `length`, `list_get` are borrow
      positions in today's backend, so uniform "every use consumes"
      produces use-after-free)
    - If/Case branch balancing on non-pattern bindings
    - Dup for non-last uses in multi-use scopes
    - `perceus_loop_residency` gate (M3's
      `drop_frees_unique_list_and_subsequent_alloc_gets_free_list_hit`
      remains the current residency proxy)
- M4.5 borrow-classifier slice landed on 2026-04-21:
  - `ownership_classify_module` now performs a module-level parameter
    fixpoint: all params start `Borrowed`, and returned params, heap-stored
    params, lambda captures, effect args, unknown call args, and args passed
    to `Owned` callee params demote to `Owned`.
  - pure uses through `BinOp`, `ListGet`, `TupleProject`, interp reads, `If`
    conditions, `Case` scrutinees, `Dup`, and `Resume` remain borrowed.
  - `drop_insert` skips parameter `Drop`/`Dup` for `Borrowed` params and can
    now restore a post-body `Drop` for fresh owned `let` bindings whose uses
    are known borrows, preserving the body result through a generated temp.
  - focused Perceus tests grew from 11 to 16, covering pure borrowed params,
    borrowed-call owner drops, owned call transfer, unknown-call
    conservatism, and WithHandler Dup skipping for borrowed params.
- M4.5 let-alias slice landed on 2026-04-22:
  - the ownership classifier now tracks simple `let alias = Var(source)`
    chains back to their parameter owner during a declaration traversal.
  - returning or consuming an alias demotes the original parameter to `Owned`,
    which prevents callers from inserting an erroneous post-call `Drop` when
    ownership actually transferred through the alias.
  - alias-bound locals inherit the owner class when the source is an owned
    parameter, allowing existing drop insertion to reason about borrowed alias
    uses without treating the alias expression itself as a fresh heap value.
  - focused Perceus tests grew from 16 to 18, covering return-through-alias
    and unknown-call-consume-through-alias ownership transfer.
- M4.5 completion slice landed on 2026-04-22:
  - `drop_insert` now handles owned parameter bodies with only borrowed uses
    by preserving the body result through a temp, then dropping the parameter.
  - If/Case branch balancing now drops owned bindings on branches that do not
    consume them when a sibling branch does consume them; borrow-only branches
    preserve their result before the Drop.
  - borrow evidence is conservative for `If` conditions, `ListGet`,
    `TupleProject`, interpolation, and scalar operators because source-level
    param types are often erased to `?` in IR; this prevents early Drop on
    runtime-borrowed list/string values.
  - `let value` expressions that consume an owned binding now get a pre-value
    `Dup` when the binding remains live in the `let` body.
  - repeated owned arguments in one consuming call get a pre-call `Dup` for
    each non-last consuming occurrence.
  - recursive SCC demotion through call edges is pinned by a focused Perceus
    test; the existing module fixpoint propagates ownership from the returning
    decl through the mutually recursive caller.
  - `crates/goby-wasm/tests/alloc_baseline.rs` and
    `alloc_baseline.txt` add the M4.5 allocation regression gate for the
    currently GeneralLowered examples `fold.gb`, `hof_fold_print.gb`, and
    `refcount_reuse_loop.gb`. `list_case.gb` is currently `NotRuntimeIo`, so
    it is excluded until it can emit debug alloc stats.
  - focused Perceus tests grew from 18 to 22.

**Perceus M5** — intra-block pairing + tail-call reuse IR complete (Step 8).
Step 9 (wasm ABI wiring) **complete (2026-04-25)**: 9-a/9-b/9-c/9-d landed in commit `50cc7bf8`.
The runtime compile test `cross_call_reuse_hidden_param_increments_reuse_hits` proves
hidden-param cross-call reuse works at the Wasm ABI layer (`reuse_hits >= 2`).
`refcount_reuse_loop.gb` still does not hit the path (`reuse_hits=0`) because its `step`
starts with `if`; full convergence requires Step 10 (runtime helpers) and M6 (`mut` via reuse).
Keep `list_case.gb` alloc-stats coverage on the follow-up list.

- M5 IR scaffold started on 2026-04-23:
  - shared IR now has `AllocInit`, `CompExpr::DropReuse`, and
    `CompExpr::AllocReuse`, with formatting/validation support.
  - `crates/goby-core/src/perceus_reuse.rs` adds the first `insert_reuse`
    pass skeleton and focused tests for same-block drop-to-allocation pairing
    and effect-boundary non-pairing.
  - follow-up in the same M5 scaffold tightened the pass so pairing only
    fires when the dropped value has a known reusable `SizeClass` and the
    following allocation has the identical class; mismatched/unknown drops
    remain ordinary `Drop`.
  - Wasm backend lowering/emission now accepts the reuse IR:
    `DropReuse` lowers to a backend drop-reuse op that returns a raw payload
    token only for dynamically unique heap values, and `AllocReuse` lowers to
    token-first allocation for tuple/record/single-chunk-list initializers
    with free-list fallback by static `SizeClass`.
  - The new emitter smoke test validates a hand-built reuse backend-IR module
    with `wasmparser`; this also fixed the previously-dormant
    `emit_alloc_with_flag` memory64 stack discipline before pass wiring.
  - The pass is intentionally not wired into `run_perceus_passes` yet; runtime
    execution coverage and pass wiring remain the next M5 slice.
- M5 pass wiring landed on 2026-04-24:
  - `run_perceus_passes` now invokes `perceus_reuse::insert_reuse` as its final
    per-decl pass, matching the §3.8 `ownership_classify → drop_insert →
    reuse_pair` ordering already asserted by `assert_perceus_pipeline_order`.
  - Runtime behavior is unchanged because the Wasm backend still lowers
    `DropReuse` / `AllocReuse` via the fallback (ordinary drop / ordinary alloc)
    path; existing workspace tests remain green without baseline updates.
  - The pass now runs on every module that reaches the Perceus pipeline, so the
    next M5 slice can focus on backend reuse semantics (`__goby_drop_reuse` /
    `__goby_alloc_reuse`) and the `refcount_reuse_loop` allocation budget.
- M5 Step 8 (tail-call cross-call reuse IR pass) landed on 2026-04-24:
  - `ir.rs` now carries `Call.reuse_token: Option<String>` and
    `IrDecl.reuse_param: Option<String>` with validation.
  - `perceus_reuse.rs` adds `first_alloc_class`, `rewrite_callee_first_alloc`,
    and `insert_tail_reuse_module`: detects `Drop x` immediately before a tail
    `Call f` where size classes match, rewrites to `DropReuse x as tok`,
    threads `reuse_token = Some(tok)` into the `Call`, and marks the callee
    decl with `reuse_param = Some(tok)`, rewriting its first reachable alloc
    to `AllocReuse`.
  - `run_perceus_passes` wired: `insert_tail_reuse_module` now runs after
    intra-block `insert_reuse`.
  - Backend fallback lowering: `reuse_token` on `Call` is ignored (standard
    call ABI); `reuse_param` decl emits a `i64.const 0` local for the hidden
    token so `AllocReuse` falls through to the normal alloc path. No behavior
    change; `reuse_hits` counter stays 0 until Step 9 ABI wiring.
  - 11 unit tests in `perceus_reuse::tests` all green; full workspace suite
    green (goby-core 746, goby-wasm 689, goby-cli 67).

---

## Perceus M1 — what is already in place

The groundwork commit `bdf7d327 "Add closed literal hoisting groundwork"`
landed the structural pieces for M1:

- `crates/goby-core/src/closed_literals.rs` — `is_closed_literal` /
  `collect_closed_literals`, conservatively accepting `ListLit` / `TupleLit`
  / `RecordLit` whose leaves are scalar literals or nested closed aggregates
  (no `Var`, no spread tail, no `BinOp`).
- `crates/goby-wasm/src/gen_lower/emit.rs` — `STATIC_REFCOUNT_SENTINEL =
  u64::MAX`, `StaticHeapValue` intern pool, `alloc_static_list / tuple /
  record`, sentinel refcount write at the header slot.
- `crates/goby-wasm/src/gen_lower/lower.rs` — detects closed literals in a
  `Value` position and emits `WasmBackendInstr::PushStaticHeap` instead of
  per-call allocation.
- `examples/refcount_reuse_loop.gb` — normative goal program per
  `doc/PLAN_PERCEUS.md` §1.1 (length 4096, 5000 iterations).
- `crates/goby-wasm/tests/wasm_exports_and_smoke.rs` — two integration tests:
  `refcount_reuse_loop_example_parses` (active) and
  `refcount_reuse_loop_example_compiles` (now active — M1 acceptance harness
  landed on 2026-04-18).

## Perceus M1 prerequisite landed on 2026-04-18: bitwise XOR (`^`)

The normative goal program uses bitwise XOR:

```goby
xor_fold : List Int -> Int -> Int
xor_fold xs acc =
  case xs
    [] -> acc
    [x, ..rest] -> xor_fold rest (acc ^ x)
```

`doc/PLAN_PERCEUS.md` §1.1 marks this source as **normative** — implementers
must not "equivalent"-rewrite it, because the acceptance checksum is pinned
to the exact allocation and evaluation pattern. `^` was therefore added
end-to-end before resuming M1 proper.

## Perceus M1 acceptance harness landed on 2026-04-18

Root cause of the previous stack-overflow/hang: modules with non-main
user-defined declarations (e.g. `step`, `build`, `xor_fold`) were
incorrectly classified as `NotRequiringRuntimeCapability` by the general-lower
gate in `gen_lower/mod.rs`, because the gate only checked `main`'s direct body
for `AssignIndex` / Lambda / etc. The fix adds `has_non_main_user_decls` as an
additional gate condition so programs with helper functions are always routed
through the general-lower path (rather than the interpreter fallback, which
would attempt to evaluate `step initial 0 5000` at compile time).

## Immediate Next Actions

1. **Perceus M5 Step 10:** emit `__goby_alloc_reuse` / `__goby_drop_reuse` runtime helpers
   per `doc/PLAN_PERCEUS.md` §3.3 with correct `peak_bytes` accounting; light up
   the M5 correctness tests (`reuse_fires_on_unique_list_update`,
   `reuse_falls_through_when_shared`, `reuse_not_across_perform_effect`,
   `reuse_not_across_with_handler`, `tail_call_reuse_passes_token`).
2. **Perceus M6** (`mut` lowering via reuse) — once Step 10 lands, lowering
   `AssignIndex` on `mut xs` through `DropReuse` should drop `refcount_reuse_loop.gb`'s
   alloc dramatically and unlock Step 11 acceptance (`total_bytes < 200 KiB`).
3. Extend `tooling/` syntax highlight definitions to cover `^` (tracked as a
   TODO under `doc/PLAN.md` §4.2.1).

## Verification snapshot (2026-04-21, M4 conservative slice landed)

- `cargo fmt --all --check` — pass.
- `cargo check` — pass.
- `cargo test --workspace` — pass (goby-core 708, goby-wasm 686, goby-cli 54, all green).
- `cargo test -p goby-core --lib perceus` — 9 passed.

## Verification snapshot (2026-04-21, M4.5 borrow-classifier slice)

- `cargo fmt --all --check` — pass.
- `cargo check` — pass (existing `goby-wasm::size_class` dead-code warnings).
- `cargo test -p goby-core --lib perceus` — pass (16 passed).
- `cargo test --workspace` — pass (goby-core 717, goby-wasm 686, goby-cli 54, all green).
- devflow step gate (`cargo fmt --all --check`; `cargo check`; `cargo test --workspace`) — pass.

## Verification snapshot (2026-04-22, M4.5 let-alias slice)

- `cargo fmt --all --check` — pass.
- `cargo test -p goby-core --lib perceus` — pass (18 passed).
- devflow step gate (`cargo fmt --all --check`; `cargo check`; `cargo test --workspace`) — pass
  (existing `goby-wasm::size_class` dead-code warnings).

## Verification snapshot (2026-04-22, M4.5 completion slice)

- `cargo fmt --all --check` — pass.
- `cargo test -p goby-core --lib perceus` — pass (22 passed).
- `cargo test -p goby-wasm alloc_baseline` — pass.
- devflow step gate (`cargo fmt --all --check`; `cargo check`; `cargo test --workspace`) — pass
  (existing `goby-wasm::size_class` dead-code warnings).

## Verification snapshot (2026-04-25, M5 Step 9 wasm ABI wiring complete)

- `cargo test -p goby-wasm --lib reuse_hits` — pass
  (`drop_reuse_unique_increments_reuse_hits` + `cross_call_reuse_hidden_param_increments_reuse_hits`).
- `cargo run -p goby-cli -- run --debug-alloc-stats examples/refcount_reuse_loop.gb`
  → `total_bytes=155954768 peak_bytes=155954768 free_list_hits=0 reuse_hits=0`
  (intentional: normative `step` body needs M6 `mut` lowering to reach reuse path).
- alloc baseline 更新は Step 11 acceptance まで保留。

## Verification snapshot (2026-04-24, M5 reuse pass wiring)

- `cargo fmt --all --check` — pass.
- `cargo test -p goby-core --lib perceus` — pass (25 passed, includes the
  existing `perceus_reuse` focused tests now reached through the module
  pipeline).
- `cargo test --workspace` — pass (goby-core 738, goby-wasm 688, goby-cli 54,
  all green; existing `goby-wasm::size_class` /
  `run_wasm_bytes_capturing_stderr` dead-code warnings remain).

## Verification snapshot (2026-04-23, M5 backend reuse lowering slice)

- `cargo fmt --all --check` — pass.
- `cargo check` — pass (existing `goby-wasm::size_class` dead-code warnings).
- `cargo test -p goby-wasm gen_lower::lower::tests::lower_drop_reuse_and_alloc_reuse_emit_backend_ops`
  — pass.
- `cargo test -p goby-wasm gen_lower::emit::tests::emit_refcount_reuse_ops_produce_valid_wasm`
  — pass.
- `cargo test -p goby-wasm` — pass.
- devflow step gate (`cargo fmt --all --check`; `cargo check`; `cargo test --workspace`) — pass
  (existing `goby-wasm::size_class` / `run_wasm_bytes_capturing_stderr` dead-code warnings).

## Verification snapshot (2026-04-20, M4 first insertion slice)

- `cargo fmt --all --check` — pass.
- `cargo check` — pass.
- `cargo test` — pass (workspace green).

## Verification snapshot (2026-04-18, M2 debug-alloc-stats slice)

- `cargo fmt --all` — pass.
- `cargo check --all-targets` — pass.
- `cargo test` — pass (workspace green).
- `cargo test -p goby-cli --test cli_integration run_command_debug_alloc_stats_emits_stats_line_for_general_lowered_program`
  — pass.

## Verification snapshot (2026-04-18, M1 harness)

- `cargo fmt --all` and `cargo check --all-targets` — workspace green.
- `cargo test -p goby-core` — 697 pass, 0 failed, 2 ignored.
- `cargo test -p goby-wasm` (lib + integration, no --ignored) — all pass, 3 ignored.
- `cargo test -p goby-wasm refcount_reuse_loop_example_compiles` — pass (un-ignored).
- `cargo test -p goby-wasm refcount_reuse_loop_example_parses` — pass.

## Architecture State

| Layer | Status |
|---|---|
| Parser | Stable (`^` added) |
| Resolver | Stable |
| Typechecker | Stable (`^`: Int × Int → Int) |
| IR (`ir.rs`) | Stable (`IrBinOp::BitXor` present) |
| IR lowering (`ir_lower.rs`) | Stable |
| Wasm backend | memory64 complete; Perceus M0–M7 complete; refcount + chunk-free-list + reuse pipeline live |
| Effect handlers | Non-tail / multi-resume still produces `BackendLimitation` |
| GC / reclamation | Refcount + chunk free-list + `__goby_drop`; top-of-heap bump remains as backing allocation path for large/variable-size values (headers, Tuple, Record); Perceus M0–M7 complete; §1 goal program runs under 200 KiB |
