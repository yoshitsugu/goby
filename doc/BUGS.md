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

Resolved bugs:

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
