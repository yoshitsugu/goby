## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs:

- **2026-05-01.** A function whose result is a `case` over a list pattern can
  return `Unit` or the empty-list-arm result instead of evaluating the matching
  non-empty list arm on the current `goby run` path.

  Confirmed repro:

  Save the following complete program as `solve.gb`:

  ```goby
  import goby/list (push)
  import goby/stdio

  parse_inner : List String -> List String -> List String -> Bool -> (List String, List String) can Print
  parse_inner lines ranges ids ranges_end =
    println "l2:${lines}"
    case lines
      [] -> (ranges, ids)
      [x, ...xs] ->
        if x == ""
          parse_inner xs ranges ids True
        else
          if ranges_end
            new_ids = push ids x
            parse_inner xs ranges new_ids ranges_end
          else
            new_ranges = push ranges x
            parse_inner xs new_ranges ids ranges_end

  parse : List String -> (List String, List String) can Print
  parse lines =
    println "l:${lines}"
    parse_inner lines [] [] False

  main : Unit -> Unit can Print, Read
  main =
    lines = read_lines ()
    ranges_and_ids = parse(lines)
    println "a:${ranges_and_ids}"
  ```

  Run it with this stdin:

  ```text
  3-5
  10-14
  16-20
  12-18

  1
  5
  8
  11
  17
  32
  ```

  Command:

  ```sh
  cat sample | goby run solve.gb
  ```

  Observed output:

  ```text
  parsed and typechecked 3 declarations from solve.gb
  l:["3-5", "10-14", "16-20", "12-18", "", "1", "5", "8", "11", "17", "32"]
  a:Unit
  ```

  Expected behavior:

  - `parse_inner` should execute, so at least one `l2:...` line should be
    printed.
  - `ranges_and_ids` should be the tuple returned by `parse_inner`, not `Unit`.

  Smaller reduction:

  ```goby
  import goby/stdio

  head_or : List Int -> Int
  head_or xs =
    case xs
      [] -> 0
      [x, ...rest] -> x

  main : Unit -> Unit can Print
  main =
    v = head_or [7, 8]
    println "v:${v}"
  ```

  Observed:

  ```text
  v:0
  ```

  Expected:

  ```text
  v:7
  ```

  Notes:

  - The original `solve.gb` also had an extra discarded recursive call before
    the `if` in the `[x, ...xs]` arm. Removing that call is correct, but it is
    not the root cause of this bug.
  - Inline `case` examples such as `examples/list_case.gb` still pass, so the
    current failure appears to involve list-pattern `case` inside a called
    function returning a value.

- **2026-05-08.** `WasmBackendInstr::AllocFloatBox` and
  `WasmBackendInstr::AllocMutableCell` share two limitations that should
  be addressed together as a follow-up cell-allocation refactor (raised
  by CodeRabbit during the Track Float review):

  1. Both lower to `emit_alloc_from_top` only, so freed Cell / TAG_FLOAT
     boxes returned to the size-class free list by `__goby_drop` are not
     reused — every alloc grows the heap top. `__goby_dup` /
     `__goby_drop` already learn TAG_FLOAT and reuse the Cell free-list
     slot for accounting (`gen_lower/value.rs`), but the allocation
     side never consumes it.
  2. Both reuse `HS_AUX_PTR` as the result-pointer scratch and
     explicitly require the `init_instrs` / `bits_instrs` to **not**
     contain a nested heap allocation (would clobber the freshly
     allocated payload pointer). The constraint is documented inline
     and currently respected by every caller (`init_instrs` is a
     single `I64Const` / `LoadLocal`; `bits_instrs` ditto), but a
     future caller that needs a nested allocation would break it.

  Why this is a single follow-up rather than two Float-only fixes:
  the two opcodes share the same 8-byte payload shape and the same
  scratch-slot pattern. Fixing only `AllocFloatBox` would create a
  divergence between Cell and Float allocation that is hard to
  justify mechanically. The right shape is a single emit helper that
  (a) checks the size-class free list before falling back to top
  allocation, and (b) spills the payload pointer to a reserved spill
  slot so nested-allocating `*_instrs` are safe — then both
  `AllocMutableCell` and `AllocFloatBox` route through it.

  Code pointers:
  - `crates/goby-wasm/src/gen_lower/emit.rs` (`AllocMutableCell` ~L3662,
    `AllocFloatBox` ~L3701, helper `emit_alloc_float_box_with_bits_local`
    ~L5063, scratch slot constant `HS_AUX_PTR` L102).
  - `crates/goby-wasm/src/gen_lower/value.rs` (TAG_FLOAT free-list
    accounting in `__goby_drop`).

- **2026-05-09.** Stdlib `int.parse` cannot be compiled because its
  `minimum_int : Int` constant declares the i64 boundary value
  `-9223372036854775808`, which is outside Goby's 60-bit `Int`
  representable range. `encode_int` rejects the literal and the
  module fails with
  `classification error: integer -9223372036854775808 is outside the 60-bit representable range`.

  Surfaced after Track HF (BUGS.md 2026-05-08, fixed 2026-05-09):
  the cond/scrutinee scope fix lets `int.parse`'s body reach wasm
  emission for the first time, and emission then trips this
  pre-existing literal-range issue. Tests that exercise `int.parse`
  via a HOF callback (the original BUGS.md 2026-05-08 minimal
  program — `to_i = with invalid_integer ... in int.parse d` passed
  to `list.map`) now stop here instead of at the unknown-local emit
  error.

  Confirmed repro:

  ```goby
  import goby/list (map, push)
  import goby/int as int
  import goby/stdio

  to_i : String -> Int
  to_i d =
    with
      invalid_integer i -> resume -1
    in
      int.parse d

  main : Unit -> Unit can Print
  main =
    xs = push (push [] "1") "2"
    ys = map xs to_i
    println "done"
  ```

  Observed:

  ```text
  classification error: integer -9223372036854775808 is outside the 60-bit representable range
  ```

  Triage notes:

  - `stdlib/goby/int.gb` lines 21–25 declare both `minimum_int_div_10
    = -922337203685477580` (within 60-bit range) and `minimum_int =
    -9223372036854775808` (out of range). The latter is the offending
    literal.
  - `int.parse`'s `minimum_int` reference is used to detect the
    `Int.MIN_VALUE` boundary case (`if acc == minimum_int` on line
    69) so a positive overflow on negation is rejected as
    `invalid_integer`.
  - Goby's `Int` is a 60-bit tagged value, so the i64
    `-9223372036854775808` literal is not representable as an `Int`
    constant. The stdlib boundary check itself is misaligned with
    the language's number model.

  Possible fixes (not yet decided):

  1. Drop the boundary check from `int.parse` and document that it
     accepts inputs that overflow the 60-bit range as
     `invalid_integer`. Requires updating `int.parse` to detect
     overflow earlier (e.g. by comparing against the Goby-Int
     boundary `2^59 - 1` / `-2^59` rather than i64 `MIN_VALUE`).
  2. Introduce a 64-bit unboxed integer literal form usable inside
     stdlib but not directly exposed as `Int`. Larger language
     change.
  3. Express `minimum_int` as a runtime-computed value (e.g. derived
     from `minimum_int_div_10 * 10 - 8` once `int.parse` itself is
     correct). Avoids the literal entirely.

  Either way, the fix is stdlib-side and language-design-touching;
  it is intentionally out of scope for Track HF.

Resolved bugs:

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
