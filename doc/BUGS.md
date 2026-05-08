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

- **2026-05-08.** Passing an effect-handler-wrapping function as a value
  to a higher-order function (e.g. `list.map`) makes wasm emission fail
  with `gen_lower/emit: unknown local '<top-level binding>'`, even though
  the wrapper is fully effect-resolved at the type level (no `can` clause)
  and is therefore expected to be interchangeable with an ordinary pure
  function.

  Confirmed repro:

  Save the following complete program (no external file needed) and run
  `goby run` on it:

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
  runtime error: gen_lower/emit: unknown local 'minimum_int'
  ```

  Expected: the program prints `done`. `to_i : String -> Int` carries no
  residual effect after the `invalid_integer` handler resolves
  `StringParseError`, so it should be usable wherever a plain
  `String -> Int` is accepted, including as the callback to `list.map`.

  Triage notes:

  - Calling `to_i` directly (`to_i "42"`) lowers and runs fine.
  - A pure helper that references a top-level binding from another module
    (no effect handler) and is passed to `map` lowers fine, so the trigger
    requires the combination of (a) an effect handler in the body and
    (b) the function being used as a first-class value via a higher-order
    call site.
  - The unresolved name in the error (`minimum_int` /
    `minimum_int_div_10`) is a top-level value declaration in
    `stdlib/goby/int.gb` that `int.parse`'s body references. The emit
    error originates in `crates/goby-wasm/src/gen_lower/emit.rs:667`
    (`Locals::get`), which means the lowering path for the indirect-call
    callee is walking the body of the wrapped function and resolving
    those names against the per-function locals scope instead of the
    module/top-level scope used by the original declaration's lowering.
  - The aoc-style program that surfaced this in practice (a
    `parse`/`parse_inner` pair feeding `to_i` through `list.map` with a
    real stdin) trapped at runtime with `E-RUNTIME-TRAP` from
    `__tail_group_dispatch_0`, but `goby run` on the minimal program
    above already fails before execution at the emit stage, which is the
    canonical form to debug against.

  Fix scope (do not patch ad-hoc): the underlying defect is the
  indirect-call lowering path resolving names in the wrapped function's
  body against the wrong scope. The repro happens to surface through
  `String -> Int` and `int.parse`'s `minimum_int` reference, but the
  same shape will break for any function of the form
  "`with <handler> in <call to a stdlib/other-module function whose body
  references its own top-level bindings>`" once it is passed as a value
  to a higher-order callback. The fix must restore correct scope
  resolution for the indirect-call wrapper in general — i.e. it should
  not special-case `int.parse`, `minimum_int`, or the `String -> Int`
  signature, and the regression test should cover at least one
  additional shape (different module, different effect, non-`Int`
  return) to lock in the general behavior.

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

Resolved bugs:

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
