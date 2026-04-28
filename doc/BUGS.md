## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs:

- `goby run` exhausts the 1 GiB Wasm memory limit on a multi-round
  driver that repeatedly rebuilds a flat grid, even though every
  per-round mutation goes through the `mut ys = xs; each idxs (fn k -> ys[k] := v)`
  shape that Perceus M6/M8 was supposed to cover. M8 closed the
  immediate `reuse_hits=0` regression for the minimal driver shape,
  but the full real-world program — many helpers, several distinct
  intermediate lists per round, tail-recursive driver — still blows
  past 1 GiB. `--debug-alloc-stats` on a small input reports
  `peak_bytes == total_bytes` and `free_list_hits=0`, i.e. nothing
  ever returns to the chunk free-list during the run. Only the
  in-place `AssignIndex` reuse fires (`reuse_hits` matches the number
  of mutated cells); every other intermediate list — `acc` chains in
  the tail-recursive helpers, the per-round `removed_indices`,
  the per-cell 8-element `around_diffs` literal, the `flatten` /
  `reverse_*_acc` cons spines — is allocated and never freed.

  Repro (no external dependency beyond the standard `goby/*` modules):

  ```gb
  import goby/list (each, fold, length)
  import goby/string (graphemes)
  import goby/stdio

  threshold : Int
  threshold = 4

  check_around_rolls : List String -> Int -> Int -> Int -> Int -> Bool can Print
  check_around_rolls rolls x y max_x max_y =
    if rolls[y * max_x + x] == "@"
      around_diffs = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
      result =
        fold around_diffs 0 (fn result d ->
          diff_y = y + d.1
          diff_x = x + d.0
          if 0 <= diff_y && diff_y < max_y && 0 <= diff_x && diff_x < max_x
            if rolls[diff_y * max_x + diff_x] == "@"
              result + 1
            else
              result
          else
            result
        )
      result < threshold
    else
      False

  reverse_positions_acc : List Int -> List Int -> List Int can Print
  reverse_positions_acc xs acc =
    case xs
      [] -> acc
      [x, ..rest] -> reverse_positions_acc rest [x, ..acc]

  reverse_positions : List Int -> List Int can Print
  reverse_positions xs =
    reverse_positions_acc xs []

  count_valid_roll_acc : List String -> Int -> Int -> Int -> Int -> List Int -> Int -> (List Int, Int) can Print
  count_valid_roll_acc rolls x y max_x max_y acc count =
    if y >= max_y
      (reverse_positions acc, count)
    else
      if x >= max_x
        count_valid_roll_acc rolls 0 (y + 1) max_x max_y acc count
      else
        if check_around_rolls rolls x y max_x max_y
          count_valid_roll_acc rolls (x + 1) y max_x max_y [y * max_x + x, ..acc] (count + 1)
        else
          count_valid_roll_acc rolls (x + 1) y max_x max_y acc count

  count_valid_roll : List String -> Int -> Int -> Int -> Int -> (List Int, Int) can Print
  count_valid_roll rolls x y max_x max_y =
    count_valid_roll_acc rolls x y max_x max_y [] 0

  update_rolls : List String -> List Int -> List String can Print
  update_rolls rolls removed_indices =
    mut new_rolls = rolls
    each removed_indices (fn idx ->
      new_rolls[idx] := "."
      ()
    )
    new_rolls

  flatten_acc : List (List String) -> List String -> List String can Print
  flatten_acc rows acc =
    case rows
      [] -> acc
      [row, ..rest] ->
        new_acc =
          fold row acc (fn a c -> [c, ..a])
        flatten_acc rest new_acc

  reverse_strings_acc : List String -> List String -> List String can Print
  reverse_strings_acc xs acc =
    case xs
      [] -> acc
      [x, ..rest] -> reverse_strings_acc rest [x, ..acc]

  flatten : List (List String) -> List String can Print
  flatten rows =
    reversed = flatten_acc rows []
    reverse_strings_acc reversed []

  check : List String -> Int -> Int -> Int -> Int can Print
  check rolls max_x max_y result =
    counted = count_valid_roll rolls 0 0 max_x max_y
    removed_indices = counted.0
    count = counted.1
    if count > 0
      new_rolls = update_rolls rolls removed_indices
      check new_rolls max_x max_y (result + count)
    else
      result

  main : Unit -> Unit can Print, Read
  main =
    lines = read_lines ()
    nlines = length lines
    rows = list.map lines graphemes
    max_y = length rows
    row0 = rows[0]
    max_x = length row0
    rolls = flatten rows
    nrolls = length rolls
    result = check rolls max_x max_y 0
    println "${result}"
  ```

  Input shape: stdin is a rectangular grid of single-character rows
  drawn from `{'@', '.'}`. Each round, `count_valid_roll` collects
  every `@` cell whose `@`-neighbour count is below `threshold`, and
  `update_rolls` blanks those cells. The driver loops until a round
  removes nothing.

  Run:

  ```sh
  goby run --debug-alloc-stats repro.gb < grid.txt
  ```

  Observed on a small (10×10) input:

  ```text
  alloc-stats: total_bytes=333576 peak_bytes=333576 free_list_hits=0 reuse_hits=9
  ```

  Observed on the original (~138×138) input: `runtime error: memory
  exhausted [E-MEMORY-EXHAUSTION]` under the 1024 MiB default ceiling.

  Expected: `peak_bytes` bounded by a small multiple of the live
  working set (`rolls` plus one or two cons spines), and
  `free_list_hits` growing roughly linearly with the number of rounds
  times the number of intermediate lists released per round. The
  `reuse_hits=9` result on the 10×10 grid (one per blanked cell) is
  evidence that the `update_rolls` borrow-update reuse from M8 is
  firing as intended; the bug is in the rest of the program.

  Hypotheses for what is missing (in order of likely impact):

  1. **Tail-recursive self-call drops.** `check`, `count_valid_roll_acc`,
     `reverse_positions_acc`, `reverse_strings_acc`, and `flatten_acc`
     all pass an old `rolls` / `acc` / `xs` argument as the first
     argument of a recursive call to themselves with a freshly
     allocated successor in another argument position (`new_rolls`,
     `[x, ..acc]`, etc). For the old binding to be reused or freed, the
     last-use `Drop` must be inserted *before* the recursive call so
     refcount can drop to zero ahead of the new allocation. The
     `free_list_hits=0` observation is consistent with these drops not
     being inserted on the tail-recursion edge — every iteration
     pushes a fresh chunk and the previous one stays at refcount ≥ 1
     until the whole call chain unwinds (and `total_bytes ==
     peak_bytes` says even unwinding is not returning chunks to the
     free-list).

  2. **HOF-laundered last-use.** `flatten_acc` builds its successor
     `acc` inside `fold row acc (fn a c -> [c, ..a])`. The closure's
     `a` parameter is the only consumer of `acc`, but Perceus's
     ownership classifier likely sees `fold` as borrowing `acc`
     because the callee is a higher-order function whose body it
     cannot inspect at the call site. Same shape inside
     `check_around_rolls` (`fold around_diffs 0 ...`) for the
     per-cell 8-element literal. If `around_diffs` is reclassified
     `Borrowed` for this reason, the literal leaks once per
     `check_around_rolls` call.

  3. **Tuple-returned ownership.** `count_valid_roll` returns
     `(List Int, Int)`. The `List Int` half is consumed by
     `update_rolls` but the tuple is destructured through `counted.0`
     / `counted.1`. If projection out of a tuple does not transfer
     `Owned` to the projected component, `removed_indices` may be
     classified `Borrowed` and never dropped after `update_rolls`
     returns.

  4. **`mut ys = xs` reuse vs. an upstream borrow.** This is the
     same family of bug M8 closed for the minimal repro, but the full
     program adds extra borrowers between the binding and the
     mutation (notably `count_valid_roll rolls ...` immediately
     before `update_rolls rolls removed_indices` in `check`). M8's
     fix may be sound for the literal `count_marks; update_xs` shape
     but not for this nested driver — worth re-checking that
     `reuse_hits` scales with the number of rounds (it currently
     reports 9 = total cells blanked, not 1 per round, which is what
     M8 was sized to deliver).

  Notes:
  - M8 was marked complete on the strength of an acceptance benchmark
    that exercised the linear `mut ys = xs; ys[i] := v` shape. The
    Perceus project memory already records that the M6 acceptance
    program "did not cover the real-world driver shape"; this entry
    is the analogous miss one milestone later.
  - Before opening a fix milestone, the first useful step is an IR
    dump on the repro to identify *which* `Drop` insertions are
    actually present, rather than guessing among the four
    hypotheses above.
