## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs:

- None currently tracked.

Resolved on 2026-04-27:

- `goby run` exhausts the 1 GiB Wasm memory limit on a tail-recursive
  driver that updates a list through `mut ys = xs; each idxs (fn k -> ys[k] := v; ())`
  inside a helper, when the same list is also borrowed in the driver
  (e.g. by a read-only traversal) before the helper call. The Perceus
  reuse path that M6 added (`each + AssignIndex` → `ListSetInPlace`,
  Owned-parameter `mut ys = xs` seeding) does not fire under this
  call shape, and `--debug-alloc-stats` reports `reuse_hits=0` with
  `total_bytes == peak_bytes` (i.e. nothing is freed during the loop).

  Repro:

  ```gb
  import goby/list (length, each)
  import goby/stdio

  count_marks : List Int -> Int -> Int -> Int
  count_marks xs i acc =
    n = length xs
    if i == n
      acc
    else
      if xs[i] == 1
        count_marks xs (i + 1) (acc + 1)
      else
        count_marks xs (i + 1) acc

  update_xs : List Int -> List Int -> List Int
  update_xs xs idxs =
    mut ys = xs
    each idxs (fn k ->
      ys[k] := 0
      ()
    )
    ys

  build : Int -> Int -> List Int -> List Int
  build k n acc =
    if k == n
      acc
    else
      build (k + 1) n [k % 2, ..acc]

  build_idxs : Int -> Int -> List Int -> List Int
  build_idxs k n acc =
    if k == n
      acc
    else
      build_idxs (k + 1) n [k, ..acc]

  drive : List Int -> List Int -> Int -> Int
  drive xs idxs iters =
    if iters == 0
      count_marks xs 0 0
    else
      c = count_marks xs 0 0
      new_xs = update_xs xs idxs
      drive new_xs idxs (iters - 1)

  main : Unit -> Unit can Print
  main =
    xs = build 0 1000 []
    idxs = build_idxs 0 50 []
    result = drive xs idxs 200
    println "${result}"
  ```

  Run:

  ```sh
  goby run --debug-alloc-stats repro.gb
  ```

  Previous result (small scale, completes):

  ```text
  alloc-stats: total_bytes=37016 peak_bytes=37016 free_list_hits=0 reuse_hits=0
  475
  ```

  Fixed result: the focused runtime test
  `compile_tests::perceus_real_world_driver_borrow_then_update_reuses_and_frees`
  reports `reuse_hits=200` and `total_bytes=37016` for the same scale.
  The fix classifies last-use `mut ys = xs` sources as consumed without
  adding a `LetMut` alias, resolves known `Var` callees for borrowed-call
  ownership, propagates reuse metadata into lambda callback bodies, and
  wraps cell-promoted `each` roots in `drop_reuse` / `alloc_reuse(Retain)`.

  Notes:
  - At larger scales (e.g. `xs` of 19 000 elements with several hundred
    iterations — the AoC2025 day 4 part 2 shape that surfaced this) the
    same program runs out of memory under the default 1024 MiB ceiling.
  - Removing the `count_marks xs ...` borrow before the `update_xs xs ...`
    call routes the program through a different lowering path (not
    `GeneralLowered`) and `--debug-alloc-stats` is not applicable, so
    the borrow is the load-bearing part of the repro: the `mut ys = xs`
    seed in `update_xs` only re-uses the parameter's chunk when the
    caller has not already taken a Borrowed reference to it earlier in
    the same basic block.
  - `peak_bytes == total_bytes` and `free_list_hits == 0` together
    indicate that drop is not running on the per-iteration intermediate
    lists either. This is a second symptom (likely related: the
    parameter ownership classification used by `drop_insert` is the
    same input the reuse pass reads), tracked together with the reuse
    miss.
  - The fix lives in the reuse plumbing and the `each + AssignIndex`
    cell-root reuse wrapper. Tracked as Perceus M8 in
    `doc/PLAN_PERCEUS.md`.

Resolved on 2026-04-14:

- `goby run` exhausted Wasm memory when a tail-recursive driver repeatedly
  updated a list-of-lists through nested `AssignIndex` inside an `each`
  callback (50 × 50 grid, 5000 iterations).

  Fix: the Wasm lowering now recognises the `each xs (fn i -> root[..] := rhs)`
  pattern via `lower_list_each_mutating_assign` and emits `ListSetInPlace`
  (direct write into the existing chunk — no header or chunk allocation) instead
  of copy-on-write `ListSet`. The recogniser fires when: (1) the callback body
  is a single `AssignIndex` statement returning `Unit`, (2) `root` is not a
  top-level declaration, and (3) `rhs` does not mention `root`.
  Also added `BackendIntrinsic::ListSetInPlace` to `needs_helper_state` so that
  the scratch-slot pool is allocated for functions that use this intrinsic.
  Verified by `runtime_output_tests::each_assign_index_in_place_bugs_md_minimal_repro_completes`.

- `goby run` could exhaust Wasm memory for named-callback list-building chains
  that repeatedly returned `[x, ..acc]` through stdlib `fold`.

  Repro:

  ```gb
  import goby/list ( fold )
  import goby/stdio

  build : Int -> List Int can Print
  build n =
    if n == 0
      []
    else
      rest = build (n - 1)
      [n, ..rest]

  prepend : List Int -> Int -> List Int can Print
  prepend acc x =
    [x, ..acc]

  main : Unit -> Unit can Print, Read
  main =
    _lines = read_lines ()
    seed = build 20000
    xs = fold seed [] prepend
    println "${xs[0]}"
  ```

  Current result:

  ```text
  1
  ```

  Notes:
  fixed by rewriting supported named/local callback `fold` shapes into the same
  inline reverse-fold lowering boundary used by the earlier callback builder
  slice, eliminating the stdlib `fold` + `ListConcat` chain for these prepend
  builders.

Resolved on 2026-04-09:

- `goby run` could exhaust Wasm memory for an inline `fold` callback that
  repeatedly returned `[x, ..acc]`.

  Repro:

  ```gb
  import goby/list ( fold )
  import goby/stdio

  build : Int -> List Int can Print
  build n =
    if n == 0
      []
    else
      rest = build (n - 1)
      [n, ..rest]

  main : Unit -> Unit can Print, Read
  main =
    _lines = read_lines ()
    seed = build 20000
    xs =
      fold seed [] (fn acc x ->
        [x, ..acc]
      )
    println "${xs[0]}"
  ```

  Current result:

  ```text
  1
  ```

  Notes:
  fixed by lowering the inline empty-accumulator `fold` callback builder shape
  to a dedicated reverse traversal over the source list, eliminating the
  callback-side `ListConcat` chain in `main`.

Resolved on 2026-04-09:

- `goby run` could exhaust Wasm memory for a small runtime-`Read` program that recursively
  builds a list via spread, even when the stdin payload is small and otherwise irrelevant.

  Repro:

  ```gb
  import goby/stdio

  build : Int -> List Int can Print
  build n =
    if n == 0
      []
    else
      rest = build (n - 1)
      [n, ..rest]

  main : Unit -> Unit can Print, Read
  main =
    _lines = read_lines ()
    xs = build 5000
    println "${xs[0]}"
  ```

  Run:

  ```sh
  printf 'x\n' | goby run repro.gb
  ```

  Current result:

  ```text
  5000
  ```

  Notes:
  fixed by lowering the self-recursive `[x, ..rest]` builder shape to a
  builder-backed loop in the Wasm general-lowering path, avoiding repeated
  `ListConcat` copies of the recursive tail.

Resolved on 2026-04-07:

- `goby run` failed for rooted list updates on a mutable binding captured by a lambda/closure.
  Fix: `lower_assign_index` now reads/writes through the cell when root is cell-promoted;
  `PerformEffect` and `Call` (effect target) arg lowering now uses `lower_value_ctx` so that
  cell-promoted bindings in interpolated expressions (`println("${b[0]}")`) resolve correctly.
  Verified by `examples/closure_mut_list.gb` (→ 16) and `examples/closure_mut_nested_list.gb` (→ 99).

Resolved on 2026-04-06:

- nested `List` reads, index interpolation, and mutable rooted updates under `goby run`.
