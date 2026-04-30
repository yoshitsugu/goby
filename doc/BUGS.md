## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

Open bugs:

- **2026-04-30.** Perceus M10 was marked complete, but the original
  138×138 real-world driver acceptance shape still exhausts Wasm memory.

  Confirmed repro:

  1. Use the `real_world_driver.gb` shape from
     `crates/goby-wasm/tests/fixtures/alloc-baseline/real_world_driver.gb`,
     but read `lines = read_lines ()` instead of the committed 6×10
     hard-coded grid.
  2. Run a 138×138 stdin grid under:
     `goby run --debug-alloc-stats --max-memory-mb 256 <program.gb>`.

  Result: `E-MEMORY-EXHAUSTION`. The failure also reproduces with a
  138×138 all-`.` grid and with the default 1 GiB module ceiling, so the
  current 6×10 baseline fixture and 20×20 focused compile test do not cover
  the intended acceptance condition from `doc/PLAN_PERCEUS.md`.

  Initial reduction:

  - The focused Perceus tests still pass.
  - The committed `alloc_baseline` fixture passes, but it is too small.
  - A reduced `list.map lines graphemes` shape over 138 input lines also
    exhausts memory, so the next investigation should start around the
    read-lines/graphemes/list-map boundary and then re-run the full driver.

Resolved bugs:

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
