## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs:

- `goby run` can exhaust Wasm memory for a small runtime-`Read` program that recursively
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
  runtime error: runtime error: memory exhausted [E-MEMORY-EXHAUSTION]: allocation exceeded the configured Wasm memory limit
  ```

  Notes:
  the failure reproduces without any Advent of Code-specific logic. The key shape is
  `Read`-driven runtime execution plus recursive `[x, ..rest]` list construction.

Resolved on 2026-04-07:

- `goby run` failed for rooted list updates on a mutable binding captured by a lambda/closure.
  Fix: `lower_assign_index` now reads/writes through the cell when root is cell-promoted;
  `PerformEffect` and `Call` (effect target) arg lowering now uses `lower_value_ctx` so that
  cell-promoted bindings in interpolated expressions (`println("${b[0]}")`) resolve correctly.
  Verified by `examples/closure_mut_list.gb` (→ 16) and `examples/closure_mut_nested_list.gb` (→ 99).

Resolved on 2026-04-06:

- nested `List` reads, index interpolation, and mutable rooted updates under `goby run`.
