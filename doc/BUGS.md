## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs:

- *(none)*

Resolved bugs:

- **2026-05-08.** `WasmBackendInstr::AllocFloatBox` and
  `WasmBackendInstr::AllocMutableCell` shared two limitations
  (raised by CodeRabbit during the Track Float review):

  1. Both lowered to `emit_alloc_from_top` only, so freed Cell /
     TAG_FLOAT boxes returned to `FREE_LIST_SLOT_CELL` by `__goby_drop`
     were never consumed on alloc — every alloc grew the heap top.
  2. Both reused `HS_AUX_PTR` as the result-pointer scratch, so
     `init_instrs` / `bits_instrs` could not nest a further heap
     allocation. The constraint was a hidden invariant enforced only
     by the lowering pass.

  Fix: both arms (and the float-arithmetic helper
  `emit_alloc_float_box_with_bits_local`) now route through
  `emit_alloc_with_flag(SizeClass::Cell, …)`, which pops a recycled
  payload from the Cell free-list slot before falling back to bump
  allocation. The result pointer for the literal arms moved to the
  depth-indexed spill `HELPER_SCRATCH_I32 + heap_base_depth` (mirroring
  `CreateClosure` / `AllocReuse`), and inner `*_instrs` are routed
  through `emit_instrs_with_heap_depth(.., heap_base_depth + 1, ..)` so
  a nested allocation cannot clobber the outer pointer.
  `required_heap_base_spill_count_instr` returns `1 + child` for both
  arms, matching the new spill usage.

  Regression coverage:

  - `crates/goby-wasm/src/gen_lower/emit.rs::tests::alloc_mutable_cell_reserves_heap_spill_slot_at_each_depth`
    and `…::alloc_float_box_reserves_heap_spill_slot_at_each_depth`
    pin the spill-count contract (the exact bug Codex flagged in
    plan review).
  - `cargo nextest run -p goby-wasm` stays green at 876 tests.

- **2026-05-10.** Calling stdlib `int.parse` indirectly through a HOF
  callback (for example `list.map xs to_i`, where `to_i` handles
  `invalid_integer` around `int.parse d`) failed wasm emission.

  Root cause: the original resolver path loaded stdlib decls outside
  their owning module, so `invalid_integer value` in `stdlib/goby/int.gb`
  was classified as a local call instead of
  `PerformEffect{StringParseError, invalid_integer, ...}`. After that
  resolver-side gap was fixed, the remaining codegen gap was that the
  per-decl handler rewrite could not propagate a caller's
  `invalid_integer` handler scope into the stdlib `parse` decl when
  `to_i` was invoked indirectly through `list.map`.

  Fix: stdlib decl loading now resolves each decl in its full module
  context, own-module effect declarations are registered during
  resolver metadata collection, and handled `int.parse` calls are
  lowered locally in `effect_handler_lowering.rs`. The lowering routes
  the parse attempt through the internal `__goby_int_parse_maybe`
  backend/host intrinsic, then invokes the user's `invalid_integer`
  handler clause only when the intrinsic returns tagged `Unit`.

  Regression coverage:

  - `crates/goby-wasm/src/runtime_output_tests.rs`:
    `hof_callback_int_parse_invalid_integer_executes_via_compiled_wasm`
    is no longer ignored and pins `"1"` / `"x"` through `list.map` to
    `1` / `-1` on the compiled-wasm path.
  - Existing direct-call and boundary regressions for `int.parse`
    remain green.

- **2026-05-09.** Stdlib `int.parse` could not be compiled because its
  `minimum_int : Int` constant declared the i64 boundary value
  `-9223372036854775808`, which is outside Goby's 60-bit `Int`
  representable range (`[-2^59, 2^59 - 1]`). `encode_int` rejected the
  literal and the module failed with
  `classification error: integer -9223372036854775808 is outside the 60-bit representable range`.

  Surfaced after Track HF (BUGS.md 2026-05-08, fixed 2026-05-09):
  the cond/scrutinee scope fix let `int.parse`'s body reach wasm
  emission for the first time, and emission then tripped this
  pre-existing literal-range issue.

  Root cause: `stdlib/goby/int.gb` declared `minimum_int =
  -9223372036854775808` (i64 `MIN_VALUE`), copied from a parser model
  written against an i64 integer type. Goby's `Int` is 60-bit tagged
  with range `[-2^59, 2^59 - 1]`, so the literal was unrepresentable
  and `encode_int` rejected it during wasm lowering.

  Fix (BUGS.md Possible fix 1): re-align the stdlib boundary
  constants to the Goby `Int` 60-bit boundary. The `int.parse`
  algorithm itself is unchanged because it already uses the same
  comparison shape (`acc < minimum_int_div_10`,
  `acc == minimum_int_div_10 && digit > 8`,
  `acc == minimum_int`). The new constants:

  - `minimum_int_div_10 = -57646075230342348` (Rust/Goby truncating
    integer division `INT_MIN / 10` toward zero, remainder `-8`).
  - `minimum_int = -576460752303423488` (`-(1 << 59)`, Goby `Int`
    minimum).

  Decimal strings whose value falls outside `[-2^59, 2^59 - 1]` now
  delegate to `StringParseError.invalid_integer`, matching the
  language's number model. This is documented in
  `doc/LANGUAGE_SPEC.md` §3 (`Int` literal range) and the
  `goby/int.parse` stdlib note.

  Regression coverage:

  - `crates/goby-wasm/src/runtime_output_tests.rs`:
    - `stdlib_int_parse_direct_call_executes_via_compiled_wasm`
      pins `"42"` `"-7"` `"x"` → `42` `-7` `-1` (handler resume) on
      the compiled-wasm path.
    - `stdlib_int_parse_boundaries_execute_via_compiled_wasm`
      pins INT_MIN / INT_MAX success and INT_MAX+1 / INT_MIN-1
      `invalid_integer` fall-through on the compiled-wasm path.
  - The HOF repro from the original BUGS.md (`to_i` passed to
    `list.map`) now compiles past `encode_int` but trips a separate
    `unknown local 'invalid_integer'` defect that is unrelated to the
    boundary literal. That follow-up is the new BUGS.md 2026-05-10
    entry above.

- **2026-05-01.** A function whose body is a `case` over a `List` pattern
  that returns the head-element binder corrupted later string operations in
  the caller.

  Status of the original repro: the literal program in the historical
  entry used the old `...rest` spread syntax, which is a parse error
  under the current parser. With the corrected `..rest` syntax, the
  Int reduction (`head_or [7, 8] -> 7`) and the full `solve.gb`
  driver both pass on the pre-fix `0146b47` baseline, so the
  literal `Unit`-return symptom is no longer reproducible.

  However, triage surfaced a closely-related defect on the same code
  path:

  ```goby
  import goby/stdio

  head_or : List String -> String
  head_or xs =
    case xs
      [] -> ""
      [x, ..rest] -> x

  main : Unit -> Unit can Print
  main =
    v = head_or ["a", "b"]
    println "v:${v}"
  ```

  Pre-fix observed: `va` (the literal `:` between prefix and binder is
  lost). Variants:

  - `println "p:${v}:s"` printed `p:a9s` (interior literal mangled).
  - A `println "before"; println "v:${s}"` pair printed
    `\nv:a` (the earlier `before` line was clobbered).

  Int-typed analogue (`head_or : List Int -> Int`) was unaffected,
  because Int values are 60-bit unboxed and not refcounted. Inline
  `case ["a","b"] [...] [x, y] -> x` directly in `main` (not via a
  callee function) was also unaffected.

  Root cause: in `crates/goby-core/src/perceus.rs`,
  `classify_case_pattern_bindings` and the `Case` arm of
  `return_ownership_comp` classified every list-pattern `Bind`
  binder as `OwnershipClass::Owned`. The wasm backend
  (`crates/goby-wasm/src/gen_lower/emit.rs`,
  `BackendCasePattern::ListPattern` arm) lowers a head
  `BackendListPatternItem::Bind(name)` via
  `emit_chunked_load_const + LocalSet`, with no `__goby_dup` — so
  the binder is actually a *projection borrow* into the scrutinee
  cons-cell. The `Owned` classification had two consequences:
  (a) `drop_unused_pattern_bindings` emitted a `Drop` for an
  unused binder (e.g. `[x, ..rest] -> x` ⇒ `Drop(rest)`), which
  decremented the scrutinee's projected refcount and corrupted the
  source list; (b) the surrounding decl was classified as
  `Owned`-returning, so the caller treated the returned binder as a
  fresh allocation and inserted Drops at use sites that broke the
  string-builder concat path.

  Fix: list-pattern `Bind` binders (head and tail) are classified
  `OwnershipClass::Borrowed` in both
  `classify_case_pattern_bindings` and the new
  `collect_pattern_bindings_with_class_for_return` helper consumed by
  `return_ownership_comp::Case`. Ctor pattern args remain `Owned`.

  This is a stop-gap. The longer-term direction is to materialise
  list-pattern Binds as truly-owned references (either via emit-side
  `__goby_dup` after `LocalSet`, or via an IR-level
  `ProjectAndOwn` primitive). See `doc/PLAN.md` §4.5d Track LB for
  the deferred refactor and its acceptance criteria. An attempt to
  prepend an arm-entry `Dup` in Perceus during this fix produced a
  runtime trap on `head_or ["a", "b"]` even after sequencing the
  Dups after the unused-binder Drop calculation; that interaction
  (likely with `balance_case_branch_drops` or scrutinee-side
  accounting) needs investigation when Track LB opens.

  Regression coverage:

  - `crates/goby-wasm/src/runtime_output_tests.rs`:
    - `head_or_string_list_pattern_binder_return_preserves_interpolation_literals`
      — pins `"v:${v}"` ⇒ `"v:a\n"`.
    - `head_or_string_list_pattern_binder_return_preserves_prefix_and_suffix_literals`
      — pins `"p:${v}:s"` ⇒ `"p:a:s\n"`.
    - `head_or_string_list_pattern_binder_return_does_not_corrupt_earlier_println`
      — pins that a leading `println "before"` is not clobbered.
    - `head_or_string_list_pattern_binder_return_survives_local_temporary_source_list`
      — borrowed-escape probe (Codex pass-1 concern): the source
      list is a local temporary in the wrapper function and the
      returned head Bind survives the wrapper's drop. Currently
      green; if a future change makes this regress we want to know
      immediately. Track LB will fix the underlying soundness.
    - `tail_bind_return_from_list_pattern_prints_correctly` —
      `[_x, ..rest] -> rest` returns the freshly chunked sub-list
      and prints `["b"]` from `["a", "b"]`. Documents that the
      stop-gap leaks the fresh tail container (no Drop emitted, see
      §4.5d Track LB) but the elements remain legible.
    - `closure_capturing_list_pattern_head_bind_prints_correctly_when_invoked`
      — closure-capture probe (Codex pass-2 concern):
      `make_head xs = case xs [] -> fn _ -> "" [x, ..rest] -> fn _ -> x`
      returns an owned closure capturing a borrowed head Bind, and
      the closure is invoked from main after `make_head` returns.
      Currently green; pinned so any regression surfaces immediately.
  - `crates/goby-core/src/perceus.rs::tests`:
    - `case_list_pattern_bind_binders_classified_as_borrowed_no_drop_inserted`
      — pins that `case xs { [] -> 0 ; [x, ..rest] -> x }` produces
      neither `drop x` nor `drop rest` in the lowered IR. Replaces
      the old `case_heap_bound_pattern_drops_unused_bindings`, whose
      "expect drop rest" assertion was the unsound pattern this fix
      removes.

  Known stop-gap limitations (deferred to PLAN.md §4.5d Track LB):

  - Returning a list-pattern Bind makes the surrounding decl
    `Borrowed`-returning. The probe above
    (`..._survives_local_temporary_source_list`) shows current
    code paths happen to keep the source-list refcounts high
    enough to avoid an observable dangling read, but the type
    system does not enforce this. A future call shape that drops
    the source list eagerly while the borrowed projection is
    still in use could regress; Track LB lifts this to a real
    soundness guarantee.
  - Tail Binds returned from a list-pattern arm are now classified
    `Borrowed`, so the freshly-chunked tail container produced by
    `emit_case_bind_tail_list` is not Drop-eligible at the call
    site. This leaks the container header (and chunk header) until
    the surrounding allocation arena is reset. Acceptable for the
    immediate fix; Track LB will materialise tail Binds as truly
    owned and remove the leak.
  - A returned closure / lambda that captures a list-pattern head
    Bind is `Owned`-returning at the decl level (the closure is a
    fresh owned value), but the closure's environment slot for the
    binder is still a projection borrow into the source list. The
    closure-capture probe in the regression suite currently passes
    on every shape exercised, but the soundness still rides on the
    same implicit keep-alive of the source list that the direct
    return case relies on. Track LB acceptance therefore explicitly
    includes "binder captured into a returned closure / HOF
    callback remains valid after source-list cleanup".

- **2026-05-08.** Passing a function whose body referenced a top-level
  binding inside an `If` cond / `Case` scrutinee / non-trivial HOF call
  argument made wasm emission fail with
  `gen_lower/emit: unknown local '<top-level binding>'`. The original
  repro shape was `to_i = with invalid_integer ... in int.parse d`
  passed as a callback to `list.map`, where `int.parse`'s body
  references stdlib-internal `minimum_int` / `minimum_int_div_10` in
  `if acc < minimum_int_div_10` etc.

  Root cause (Track HF, doc/PLAN.md 4.7a): four
  `lower_value(...)`-via-`lower_value` callsites in
  `crates/goby-wasm/src/gen_lower/lower.rs` discarded the surrounding
  `aliases` / `bindings` / `known_decls` context, so a bare
  `Var(top-level)` in those positions was lowered as `LoadLocal` instead
  of the unit-arg `DeclCall` form used everywhere else. Affected
  positions:
  - `lower.rs:2688` — `CompExpr::If` cond
  - `lower.rs:3373` — `lower_case` scrutinee
  - `lower.rs:3779` / `3794` / `3796` — three `lower_value_as_arg`
    fallbacks (Var non-effect-op, GlobalRef non-effect-op, composite
    catch-all)

  An additional pre-existing shadowing bug was found in the same
  function: `lower_value_as_arg`'s known-decl arm (`lower.rs:3768`) did
  not check `!bindings.is_bound(name)`, so a local that shadowed a
  top-level decl could be promoted to a `PushFuncHandle` of the
  top-level function.

  Fix: route the four discarding callsites through
  `lower_value_ctx(..., aliases, bindings, known_decls)` and add the
  `!bindings.is_bound(name)` shadowing guard to the known-decl arm.

  Regression coverage:
  - Direct IR-level unit tests in `gen_lower::lower::tests`:
    `track_hf_if_cond_var_in_known_decls_lowers_as_decl_call`,
    `track_hf_case_scrutinee_var_in_known_decls_lowers_as_decl_call`,
    `track_hf_arg_composite_fallback_resolves_top_level_var_as_decl_call`,
    plus three shadowing-preservation guards
    (`*_var_shadowed_by_local_*`) covering If / Case / arg-position
    `lower_value_as_arg`.
  - Emit-path integration tests in `gen_lower::tests`:
    `track_hf_general_lower_emits_for_user_aux_with_top_level_bool_in_if_cond`
    and `track_hf_general_lower_emits_for_user_aux_with_top_level_in_case_scrutinee`
    — both verified red pre-fix (`unknown local 'flag'` /
    `unknown local 'mode'`) and green post-fix.

  Note: the BUGS.md original repro program (`to_i` + `list.map` calling
  stdlib `int.parse`) now passes wasm emission but trips a separate,
  pre-existing bug — `minimum_int = -9223372036854775808` is outside
  Goby's Int 60-bit representable range, so `encode_int` rejects the
  literal. That stdlib follow-up is **not** part of Track HF and should
  be tracked separately.

- **2026-05-07.** `goby-wasm` lib test `tests::fold_m5_string_accumulator`
  hung indefinitely (CPU-bound). `git bisect` between the test's
  introducing commit (`b5f2f62`) and `main` identified `4d53981`
  ("Add list append stdlib helper") as the first bad commit.

  Root cause: the test defines a local `append : String -> Int -> String`,
  and `4d53981` added a stdlib helper `append : List a -> List a -> List a`
  in `stdlib/goby/list.gb`. `build_stdlib_export_map` in
  `crates/goby-wasm/src/gen_lower/mod.rs` populated `known_decls` with
  every export of every (transitively) imported stdlib module without
  checking for collisions with user declarations. The resolver still
  picked the user's `append` for typecheck, but wasm lowering bound the
  `fold` callback to the stdlib `append` (= `__goby_list_concat`). At
  runtime the string accumulator was reinterpreted as a list header, the
  decoded chunk count was effectively unbounded, and `__goby_list_concat`
  spun forever copying.

  Fix: `build_stdlib_export_map` now drops any stdlib declaration whose
  name collides with a non-`main` user declaration, restoring the
  resolver's user-first convention on the codegen side. A unit test
  (`gen_lower::tests::user_decl_shadows_stdlib_export_in_export_map`)
  guards the invariant.

  Triage tooling: `.config/nextest.toml` now enforces a per-test
  slow-timeout (~65s) so a future regression that emits an infinite-loop
  wasm body fails as a timeout instead of hanging the suite under
  `cargo nextest run`.

- **2026-04-30.** Perceus M10 was marked complete, but the original
  138×138 real-world driver acceptance shape still exhausted Wasm memory.

  Confirmed repro:

  1. Use the `real_world_driver.gb` shape from
     `crates/goby-wasm/tests/fixtures/alloc-baseline/real_world_driver.gb`,
     but read `lines = read_lines ()` instead of the committed 6×10
     hard-coded grid.
  2. Run a 138×138 stdin grid under:
     `goby run --debug-alloc-stats --max-memory-mb 256 <program.gb>`.

  Result was `E-MEMORY-EXHAUSTION`. The failure also reproduced with a
  138×138 all-`.` grid and with the default 1 GiB module ceiling, so the
  current 6×10 baseline fixture and 20×20 focused compile test did not cover
  the intended Perceus acceptance condition.

  Initial reduction:

  - The focused Perceus tests still pass.
  - The committed `alloc_baseline` fixture passes, but it is too small.
  - A reduced `list.map lines graphemes` shape over 138 input lines also
    exhausts memory, so the next investigation should start around the
    read-lines/graphemes/list-map boundary and then re-run the full driver.

  Fixes (Perceus M11):

  1. Host imports that return escaping Goby strings/lists now allocate in the
     refcounted heap instead of the monotonic host bump arena, and generated
     Wasm reloads the shared heap cursor/floor after host calls.
  2. The legacy host bump cursor is zero-initialized when the bump arena is not
     used, so floor clamping no longer forces every later Wasm allocation to
     grow memory.
  3. Fold-prepend lowering now handles non-empty accumulators and
     case-recursive reverse-prepend helpers, avoiding repeated list-spread
     copies in the full `flatten` driver.

  Acceptance:

  - `perceus_m11_list_map_graphemes_138_lines_runs_under_256mib` passes.
  - `perceus_m11_real_world_driver_138_grid_runs_under_256mib` passes with a
    138×138 all-`.` stdin grid and `max_pages=4096` (256 MiB).

- **2026-04-30.** `goby run` exhausted the 1 GiB Wasm memory limit on a
  multi-round driver that repeatedly rebuilt a flat grid. Root causes
  were DI-1 (declaration return-ownership not reaching IR, so
  tail-recursive helper call results stayed `Borrowed`) and DI-2
  (`insert_drop_at_tail` demoted tail calls to non-tail temps when
  inserting `Drop` for the last use of a callee argument).

  Fixes (Perceus M10):

  1. **DI-1 (Option B):** return-ownership inference
     (`classify_decl_return_ownership`) inserted between
     `ownership_classify` and `drop_insert`. Functions whose body IR
     proves they return a freshly allocated / uniquely owned value
     are now classified `Owned` at every call site, enabling reuse
     and freeing of intermediate lists in tail-recursive helpers.
  2. **DI-2:** `insert_drop_at_tail` now preserves the tail-call
     shape for direct `Var` callee calls (C2) by emitting
     `Dup(name); Drop(name); Call(...)` instead of wrapping the call
     in a temporary. `GlobalRef` calls, runtime intrinsics, and
     indirect/closure calls (C3) keep the conservative temp-wrap
     form.
  3. **`list.map` / `__goby_list_map` ownership:** the result is
     classified `Owned` when the visible callback is proven to
     return owned values, so `rows = list.map lines graphemes` is
     droppable before the next round.
  4. **`ListGet` projection-borrow liveness:** the parent list
     remains live while any projected child reference is live,
     preventing use-after-free when `row2 = list.get rolls 2` is
     followed by operations on `row2`.

  Acceptance:

  - `perceus_real_world_driver_drops_intermediates_and_reuses_per_round`
    passes (`total_bytes=37016`, `reuse_hits=200`,
    `free_list_hits>0`).
  - `alloc_baseline.txt` fixture
    `alloc-baseline/real_world_driver.gb` (6×10 hard-coded grid)
    passes at ceiling `150049`.
  - Full quality gate `cargo test --workspace --release` is green.
