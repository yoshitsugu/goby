## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs:

- `goby run` fails for rooted list updates on a mutable binding captured by a lambda/closure.
  Reproduction:
  ```goby
  import goby/list

  main : Unit -> Unit can Print
  main =
    a = [1, 2, 3]
    mut b = [10, 20, 30]
    list.each a (fn i ->
      b[0] := b[0] + i
      ()
    )
    println("${b[0]}")
  ```
  Current behavior:
  `goby run` fails with `runtime error: gen_lower/emit: unknown local 'b'`.
  Boundary:
  plain rooted mutable-list updates are already routed and executed correctly, but the
  `GeneralLowered` Wasm lowering path still mishandles `AssignIndex` when the mutable root is a
  closure-captured shared cell rather than a directly declared local.

Resolved on 2026-04-06:

- nested `List` reads, index interpolation, and mutable rooted updates under `goby run`.
