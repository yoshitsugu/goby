## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs: none.

Resolved on 2026-04-07:

- `goby run` failed for rooted list updates on a mutable binding captured by a lambda/closure.
  Fix: `lower_assign_index` now reads/writes through the cell when root is cell-promoted;
  `PerformEffect` and `Call` (effect target) arg lowering now uses `lower_value_ctx` so that
  cell-promoted bindings in interpolated expressions (`println("${b[0]}")`) resolve correctly.
  Verified by `examples/closure_mut_list.gb` (→ 16) and `examples/closure_mut_nested_list.gb` (→ 99).

Resolved on 2026-04-06:

- nested `List` reads, index interpolation, and mutable rooted updates under `goby run`.
