## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs:

- `goby run` can still exhaust Wasm memory for named-callback list-building
  chains that repeatedly return `[x, ..acc]` through stdlib `fold`, even after
  the self-recursive builder bug and the inline callback fold bucket were fixed.

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
  runtime error: memory exhausted [E-MEMORY-EXHAUSTION]: allocation exceeded the configured Wasm memory limit
  ```

  Notes:
  the RR-4 lowering now handles the inline `fold seed [] (fn acc x -> [x,
  ..acc])` builder shape, but named callback paths still route through ordinary
  stdlib `fold` + `ListConcat`.

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
