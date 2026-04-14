## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs:

- `goby run` exhausts Wasm memory when a tail-recursive driver repeatedly
  returns a list-of-lists that was updated through nested `AssignIndex` inside
  an `each` callback, even though each individual update is well-formed and
  the "current" grid is the only one that is logically reachable.

  Minimal repro (independent of any specific input file — reproduces at
  50 × 50 grid with 5000 loop iterations):

  ```gb
  import goby/list (each)
  import goby/stdio

  build_row : Int -> List String can Print
  build_row n =
    if n == 0
      []
    else
      rest = build_row (n - 1)
      ["@", ..rest]

  build_grid : Int -> List (List String) can Print
  build_grid n =
    if n == 0
      []
    else
      rest = build_grid (n - 1)
      [build_row 50, ..rest]

  build_indices : Int -> List Int can Print
  build_indices n =
    if n == 0
      []
    else
      rest = build_indices (n - 1)
      [n - 1, ..rest]

  clear_all : List (List String) -> List Int -> List (List String) can Print
  clear_all rolls indices =
    mut new_rolls = rolls
    each indices (fn i ->
      new_rolls[i][i] := "."
      ()
    )
    new_rolls

  loop : List (List String) -> List Int -> Int -> Int can Print
  loop rolls indices n =
    if n == 0
      0
    else
      next = clear_all rolls indices
      loop next indices (n - 1)

  main : Unit -> Unit can Print, Read
  main =
    _lines = read_lines ()
    rolls = build_grid 50
    indices = build_indices 50
    r = loop rolls indices 5000
    println "${r}"
  ```

  Run:

  ```sh
  printf 'x\n' | goby run repro.gb
  ```

  Current result:

  ```text
  runtime error: memory exhausted [E-MEMORY-EXHAUSTION]: allocation
  exceeded the configured Wasm memory limit; consider reducing recursive
  list-spread construction or other large intermediate allocations
  ```

  Scaling down the driver (e.g. `build_grid 20` with `loop ... 100`) completes
  normally, so the failure is driven by repeated rounds, not a single
  pathological call.

  Analysis:
  `lower_assign_index` (`crates/goby-wasm/src/gen_lower/lower.rs:1661`) lowers
  `new_rolls[i][i] := "."` as a descent via `ListGet` followed by an ascent of
  copy-on-write `ListSet` calls — one fresh list per nesting level. Inside
  `each indices (fn ... -> ... := ...)` that rebuilds the whole outer grid
  and the touched inner row for every index in the list. The tail call
  `loop next indices (n - 1)` then hands a freshly rebuilt grid to the next
  round while the prior `rolls` value is still bound in the caller frame,
  so intermediate copies accumulate until the driver unwinds. Neither the
  2026-04-07 cell-promoted fix nor the 2026-04-09 inline/reverse-fold builder
  fixes reach this shape, because the hot path is `each` + nested
  `AssignIndex` under a tail-recursive driver rather than a list-spread
  `fold` or a self-recursive `[x, ..rest]` builder.

  Likely fix direction:
  teach the Wasm lowering to recognise the `each xs (fn i -> root[..] := rhs)`
  pattern (or more generally, repeated `AssignIndex` on the same `mut` root
  inside a loop) and emit in-place `ListSet` on a single shared backing list
  rather than threading copy-on-write results through the `mut` binding.
  Alternatively, reclaim the previous grid once the tail call consumes it,
  so steady-state memory does not grow with the number of rounds.

Resolved on 2026-04-09:

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
