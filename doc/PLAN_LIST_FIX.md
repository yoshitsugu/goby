# PLAN_LIST_FIX — Resolve the `each` + nested `AssignIndex` memory blowup

Transient plan. Closes the open bug in `doc/BUGS.md` ("`goby run` exhausts
Wasm memory when a tail-recursive driver repeatedly returns a list-of-lists
updated through nested `AssignIndex` inside an `each` callback"). Delete
this file once all milestones land and the BUGS.md entry moves to the
resolved section.

Long-term structural questions (type-level uniqueness, alternate list
representations, CLI surface evolution, embedder portability) are out of
scope here and recorded in `doc/PLAN.md` §3.1 and §3.2.

## User-facing requirements driving this plan

1. **Scaling.** Performance must not degrade super-linearly in the amount
   of data the program touches.
2. **Ceiling.** The runtime must not abort with `E-MEMORY-EXHAUSTION`
   before the host's resource budget is actually close to exhausted.

Track A closes the reported bug; Track B ensures that working sets
larger than the current emitter-baked 64 MiB cap also run to completion.

## Background (current implementation, as of 2026-04-14)

- IR node: `CompExpr::AssignIndex { root, path, value }` at
  `crates/goby-core/src/ir.rs:149`. Created by
  `crates/goby-core/src/ir_lower.rs:990`.
- Lowering: `lower_assign_index` at
  `crates/goby-wasm/src/gen_lower/lower.rs:1661` — descent via `ListGet`,
  ascent via copy-on-write `ListSet`.
- `ListSet` helper: `emit_list_set_helper` at
  `crates/goby-wasm/src/gen_lower/emit.rs:5450`. Copies header table
  and the touched chunk.
- `each` / `fold` lowering: goes through `BackendIntrinsic::ListFold`
  (`backend_ir.rs:138`). `each` is expressed as `fold` that discards
  the accumulator. Prepend-builder callbacks are already specialised
  via `lower_supported_inline_list_fold_prepend_builder`
  (`lower.rs:1303`-ish). **The `each` + nested `:=` shape is not yet
  specialised.**
- Memory ceiling: `DEFAULT_WASM_MEMORY_CONFIG` at
  `crates/goby-wasm/src/memory_config.rs:37` — `initial_pages: 4`,
  `max_pages: 1024` (64 MiB), `host_bump_reserved_bytes: 49_152`.
  `memory_type()` at `memory_config.rs:26` sets `memory64: false`.
- Host ceiling enforcement: `ensure_linear_memory_capacity`
  (`crates/goby-wasm/src/wasm_exec.rs:639`) compares against
  `memory_config.max_pages` and sets `RUNTIME_ERROR_MEMORY_EXHAUSTION`
  (code at `wasm_exec.rs:56`). Host-side byte accounting is `u32`
  throughout.
- CLI entry: `crates/goby-cli/src/main.rs` — `run()` at line 169,
  `parse_args` at 319/323, `execute_wasm` at 413.
- Execution environment targeted by this plan: `wasmtime` only.
  `wasmtime` 43.0.1 (in-tree) supports memory64 at Tier 1.

## Goals (measurable)

1. **G0 — Reported bug resolved.** The minimal repro committed to
   `doc/BUGS.md` (50 × 50 grid, 5000 iterations, executed via
   `printf 'x\n' | goby run repro.gb`) completes without
   `E-MEMORY-EXHAUSTION` under the default CLI settings and prints
   `0`. Both repros are wired into the automated test suite so the
   regression cannot silently return. G0 is the primary success
   condition for this plan; G1–G3 are the structural properties that
   make G0 durable.
2. **G1 — Scaling.** For a program that performs *K* cell-updates on
   an *R × C* grid through `each xs (fn v -> root[..] := rhs)`, peak
   steady-state allocation is O(R + C), independent of *K*. Verified
   by an `examples/` regression that OOM's on `main` but completes
   after M1.
3. **G2 — Ceiling.** `E-MEMORY-EXHAUSTION` fires at the *lower* of
   `--max-memory-mb` (or `GOBY_MAX_MEMORY_MB`) and the CLI's built-in
   default (1 GiB). The emitter contributes no hardcoded ceiling below
   that. Verified by a CLI smoke test.
4. **G3 — Preserved semantics.** `:=`, `each`, list equality, list
   pattern matching, and existing diagnostic wording / spans are
   unchanged on the existing test suite. Verified by `cargo test` and
   `examples/` stdout comparison.

## Non-goals (local to this plan)

- No new list representation, no type-system changes, no runtime
  uniqueness bits, no wasmer support, no surface-syntax changes.

## Track A — In-place lowering for the `each` + `:=` shape

Syntactic recognition at the IR level. Narrow by design: when a
structural solution from `doc/PLAN.md` §3.1 lands, delete the matcher
in one commit.

**Recognised shape (IR):**

```
CompExpr::Call {
  callee: GlobalRef { module: "list", name: "fold" }  // or the Var/alias equivalents
                                                      // and the bare-name forms
                                                      // already handled around
                                                      // lower.rs:1300
  args: [
    <source list>,                  // arg 0: the list being iterated
    <unit or literal accumulator>,  // arg 1: accumulator discarded by each
    <callback lambda>,              // arg 2: (acc, v) -> body
  ]
}
```

where `<callback lambda>` is a direct `fn acc v -> body` whose body
has the sequential shape `<AssignIndex root[..] := rhs> ; acc` (where
`;` is the IR's sequence/block form used today by `each`'s stdlib
expansion), and where:

- `root` is a `mut` binding whose declaration is visible in the
  enclosing scope of the `fold` call;
- `root` is not captured by any **other** lambda in the enclosing
  function body;
- the RHS `rhs` does not itself read `root` through a second path that
  could alias (conservative: reject if `root` appears textually inside
  `rhs`).

**Lowering:** emit the descent `ListGet` chain for `root[..]`, then a
single `ListSetInPlace` intrinsic that overwrites the element inside
the existing chunk. Skip the ascent / `StoreLocal { name: root }`. The
callback still returns `acc` so `ListFold`'s contract is preserved.

**Fallback:** any condition fails → today's lowering unchanged.

**Documented limitation:** recognition is syntactic, so a semantically
equivalent rewrite (manual recursion instead of `each`, explicit
`let`-binding of `root` mid-body) silently loses the optimization.
This is accepted. Do not pre-emptively expand the matcher; grow it
only in response to a *second* real-program report.

## Track B — Move the ceiling out of the emitter

Independent of the bug (Track A alone closes the repro). Exists so
that working sets larger than 64 MiB become a runtime policy question
rather than a compile-time cap.

- Emitter switches to `MemoryType { memory64: true, … }`; address-typed
  operations migrate from i32 to i64. Tagged values on the operand
  stack are already i64 — only linear-memory addresses change.
- The compiled-in `max_pages` ceiling is removed in favour of CLI
  policy: `--max-memory-mb <N>` (env fallback `GOBY_MAX_MEMORY_MB`),
  default 1 GiB, `0` means "defer to host / `wasmtime::StoreLimits`
  default only."
- Forced-trap tests that rely on `max_pages == initial_pages` stay;
  they are re-anchored to memory64 page counts, not deleted.

## Milestones

Each milestone is an independent shippable unit reviewed through the
`codex-reviewed-stepwise-dev-flow` skill. Cost labels: S = hours,
M = day(s), L = week(s).

### M1 — In-place lowering (cost: S-M)

#### M1.1 — Add the `ListSetInPlace` backend intrinsic (S)

- [ ] `crates/goby-wasm/src/gen_lower/backend_ir.rs`: add variant
      `BackendIntrinsic::ListSetInPlace` immediately after `ListSet`.
      Document signature `(list: i64, index: i64, value: i64) -> i64`
      (returns the same list handle for compatibility with `ListSet`
      stack shape).
- [ ] Update `BackendIntrinsic::arity` (`backend_ir.rs:150`) to return
      `3` for the new variant.
- [ ] Add a doc comment explaining that the result list aliases the
      input list header — callers must already have ruled out sharing
      via the pattern-match gate in `lower_assign_index`.

#### M1.2 — Emit the in-place helper (M)

- [ ] `crates/goby-wasm/src/gen_lower/emit.rs`: add
      `emit_list_set_in_place_helper` next to `emit_list_set_helper`
      (line 5450). Share the validation prelude (tag checks, negative
      index rejection, bounds check) with the existing helper by
      extracting a private `emit_list_set_prelude` if duplication is
      meaningful; otherwise just duplicate for clarity.
- [ ] After bounds check, compute `dst_chunk_ptr` and the word offset
      for the element, store `value_i64` directly. **Do not** allocate
      a new header, do not copy the header table, do not copy the
      chunk.
- [ ] Leave the input list pointer on the result stack unchanged.
- [ ] Wire dispatch: `emit_intrinsic` at
      `gen_lower/emit.rs:2987` area — add a `BackendIntrinsic::ListSetInPlace
      => emit_list_set_in_place_helper(...)` arm.
- [ ] Add a byte-level unit test under the pattern of
      `emit_list_set_produces_valid_wasm` (`emit.rs:7931`) that
      confirms the helper produces valid Wasm and its output list
      header pointer equals the input pointer.

#### M1.3 — Pattern-recognition in `lower_assign_index` (M)

- [ ] Pattern-detection lives at the `fold`-call site (not in
      `lower_assign_index`), because the detection needs to see the
      `fold` callee, its three args, and the callback body together.
      Add a helper `lower_supported_inline_list_fold_mutating_each`
      next to `lower_supported_inline_list_fold_prepend_builder`
      (`lower.rs` around line 1303). Mirror its call sites: the
      intrinsic-recognised branch (line 1303), the `GlobalRef` branch
      (line 1336), and the bare-name `Var` branch (line 1396).
- [ ] The helper:
  - confirms arg 2 is a direct lambda `fn acc v -> body`;
  - walks `body` to find the shape `<AssignIndex> ; acc` (or the IR's
    equivalent `Seq`/`Block` + trailing `Var(acc)`);
  - extracts `root`, `path`, `rhs` from the `AssignIndex`;
  - verifies `root` is a `mut` binding in the enclosing scope (use
    `aliases` / `bindings` already threaded through
    `lower_comp_inner`);
  - verifies `root` does not appear textually inside `rhs` (reuse
    the existing `closure_capture` free-variable walker at
    `crates/goby-core/src/closure_capture.rs:296` to collect
    `ValueExpr::Var` occurrences; reject if any equal `root`);
  - verifies the other lambdas in the enclosing function body do not
    capture `root`. (Start conservative: reject if the enclosing
    declaration contains any other `CompExpr::Lambda` that has
    `root` in its captured set. `closure_capture.rs:451` already
    tracks captures via `AssignIndex`, extend reading the
    captured-set at the enclosing declaration.)
  - on success, emits: source list on the stack, accumulator on the
    stack, and a *rewritten* callback that uses `ListSetInPlace` on
    `root` descended via `ListGet`, then pushes `acc`. The fold
    intrinsic itself is still `ListFold`; only the callback body is
    rewritten.
- [ ] If any check fails, return `Ok(None)` and fall through to
      today's lowering.
- [ ] The rewritten callback must still honour the `mut`-binding
      cell-promotion contract from the 2026-04-07 fix: if `root` is
      `CellPromoted` the descent reads and the in-place write go
      through `LoadCellValue` (reads only — the in-place write does
      not need `StoreCellValue` because the cell still holds the
      same header pointer). Assert this in a unit test.

#### M1.4 — Tests and examples (S)

- [ ] Unit test in `gen_lower/lower.rs` (near the existing
      `lower_assign_index_*` tests at line 4197+): the recognised IR
      for
      ```
      mut r = xs;
      each indices (fn i ->
        r[i][i] := "."
        ()
      )
      ```
      lowers to an emitted plan that contains exactly one
      `ListSetInPlace` per surface `:=` and **zero** `ListSet`. A
      near-miss variant (callback returns `r` instead of `()`) still
      lowers to `ListSet`.
- [ ] Runtime test (host-level): execute a small program equivalent
      to the `doc/BUGS.md` minimal repro at a scale that would OOM
      pre-fix (e.g. 50 × 50 × 5000) under the *current* 64 MiB
      ceiling, and assert the expected output. Add under
      `crates/goby-wasm/src/runtime_output_tests.rs` following the
      existing pattern.
- [ ] Example: copy the minimal repro from `doc/BUGS.md` (adjusted
      down to a size that completes in a few hundred ms) into
      `examples/each_assign_index_in_place.gb`. Ensure the example
      harness picks it up.
- [ ] **G0 verification — bug repro (automated):** add a host-level
      test that runs the exact BUGS.md minimal repro at its full
      50 × 50 × 5000 size under the default ceiling and asserts
      (a) exit success, (b) stdout equals `0`, (c) no
      `E-MEMORY-EXHAUSTION`. Place it with the other
      `runtime_output_tests.rs` cases so it runs in `cargo test`.
**Exit criteria (G0, G1 partial within the 64 MiB ceiling):** the
BUGS.md minimal repro regression test passes under `cargo test`;
`examples/each_assign_index_in_place.gb` completes; peak Wasm pages
do not grow with iteration count; `cargo test` green overall.

### M2 — CLI ceiling surface (cost: S-M, wasm32, pre-memory64)

User-facing knob lands first so that behaviour and diagnostics are
settled before the memory64 surgery.

#### M2.1 — Config split (S)

- [ ] `crates/goby-wasm/src/memory_config.rs`: introduce
      `TEST_MEMORY_CONFIG` (same values as the current
      `DEFAULT_WASM_MEMORY_CONFIG`) and rename the runtime-facing one
      to `RUNTIME_MEMORY_CONFIG` with `max_pages: 16_384` (= 1 GiB).
      Keep `DEFAULT_WASM_MEMORY_CONFIG` as a deprecated alias of
      `TEST_MEMORY_CONFIG` for any remaining call sites; plan to
      delete the alias in M2.4.
- [ ] Update `default_memory_config_locks_bounded_growth_defaults`
      test (`memory_config.rs:48`) to cover both configs.
- [ ] Grep for every `DEFAULT_WASM_MEMORY_CONFIG` use and classify:
      test harness → `TEST_MEMORY_CONFIG`; runtime entry → pick up
      from CLI resolution (see M2.2).

#### M2.2 — CLI flag and env fallback (S)

- [ ] `crates/goby-cli/src/main.rs`: extend `CliArgs` / `parse_args`
      (line 319) with `max_memory_mb: Option<u32>`. Accept
      `--max-memory-mb <N>`; reject negative / non-numeric with a
      clear error.
- [ ] Add resolution helper `resolve_max_memory_mb(cli: Option<u32>)
      -> MaxMemoryPolicy` that returns one of:
      `Bounded(pages)`, `DeferToHost`. Precedence: CLI > env
      (`GOBY_MAX_MEMORY_MB`) > default (1 GiB). `0` ⇒ `DeferToHost`.
- [ ] Thread the policy into `execute_wasm` (`main.rs:413`), then
      into the `WasmMemoryConfig` passed to the emitter and into
      `wasmtime::StoreLimits`. The `StoreLimits` cap must match the
      `MemoryType::maximum` when `Bounded`.

#### M2.3 — Improved trap body (S)

- [ ] `crates/goby-wasm/src/wasm_exec.rs:56`: keep the diagnostic
      code `E-MEMORY-EXHAUSTION`. Replace the static string with a
      formatted body that reports:
      "configured ceiling: N MiB (source: cli|env|default|host),
      high-water mark: ≈M MiB".
- [ ] Plumb the config into `set_runtime_error_once` call sites at
      `wasm_exec.rs:645`, `wasm_exec.rs:661`, `wasm_exec.rs:667`
      so that the body can be constructed lazily at error time.
      Simplest: store the config snapshot + high-water counter in
      `WasiP1Ctx` (or a sibling struct in the store) and read from
      the error formatter.
- [ ] Mirror the updated string literal at `emit.rs:7782`.

#### M2.4 — Tests (S)

- [ ] CLI smoke test (new integration test under `crates/goby-cli`):
      run a program with `--max-memory-mb=16` that would OOM at
      16 MiB but succeed at the default 1 GiB; assert exit code and
      that the trap body contains both "configured ceiling: 16 MiB"
      and a sensible high-water mark.
- [ ] Env-var test: same program with unset CLI flag and
      `GOBY_MAX_MEMORY_MB=16` produces the identical trap body.
- [ ] Default-reachable test: allocate ≈100 MiB under no flags;
      assert it completes. (Still within wasm32's 4 GiB, so no
      memory64 needed yet.)
- [ ] Keep `heap_only_recursive_tuple_allocation_grows_past_initial_pages`
      (`lib.rs:1985`) and
      `host_string_concat_grows_linear_memory_past_initial_pages`
      (`wasm_exec.rs:1191`) green by pointing them at
      `TEST_MEMORY_CONFIG`.

**Exit criteria:** `cargo test` green; `--max-memory-mb` actually
moves the trap boundary up and down; the 1 GiB default is reachable
from the CLI.

### M3 — memory64 proof-of-concept (cost: M)

Smallest vertical slice. Behind an internal Rust feature flag
(`cfg(feature = "wasm_memory64")`) or an internal `bool` on
`WasmMemoryConfig`; not yet default.

#### M3.1 — Skeleton toggle (S)

- [ ] Add `WasmMemoryConfig::memory64: bool` (default `false`). Set
      `MemoryType::memory64` from it at `memory_config.rs:30`.
- [ ] Enable `wasmtime::Config::wasm_memory64(true)` unconditionally
      (it is backward-compatible with wasm32 modules).

#### M3.2 — One example under memory64 (S)

- [ ] Pick `examples/hello.gb` or a similarly trivial example. Add a
      test variant in `runtime_output_tests.rs` that constructs a
      `WasmMemoryConfig { memory64: true, .. }` and runs the example
      end-to-end.
- [ ] Audit what address-typed emit sites this example actually
      exercises. Switch **only those** to `*64` opcodes for now via
      a `ptr_*` helper stub (the full helper comes in M4.1).
- [ ] Expect this step to produce invalid modules for non-trivial
      examples — that is fine; other examples stay on wasm32 until
      M4.

#### M3.3 — Host-refusal semantics validation (S)

- [ ] Synthetic test: set `--max-memory-mb=0` and a
      `wasmtime::StoreLimits::memory_size(32 * 1024 * 1024)`. Run a
      program that tries to grow past 32 MiB. Assert the failure
      arrives as a trap (not a Rust panic, not a process abort).
- [ ] If `wasmtime` returns a non-trap failure path (e.g.
      `memory.grow` → -1), record the decision in `doc/PLAN.md` §3.2
      that `--max-memory-mb=0` must still translate to a finite
      `StoreLimits` cap inside the CLI. Update M2.2's resolver
      accordingly before M4.

#### M3.4 — Address-site inventory (S)

- [ ] Produce the full checklist of address-typed emit sites in
      `gen_lower/emit.rs`. Categories: `I32Load*`, `I32Store*`,
      `I32Const` used as an address or offset, `I32Add`/`I32Sub`
      applied to addresses, `MemoryFill`, `MemoryCopy`, `MemorySize`,
      `MemoryGrow`.
- [ ] Separately list the same categories in `wasm_exec.rs` — host
      side: `u32` address locals, `u32::try_from` truncations,
      `WASM_PAGE_BYTES` multiplications.
- [ ] Commit the checklist as `doc/PLAN_LIST_FIX_M4_SITES.md` (or
      inline at the end of this file if short). M4 walks it
      top-to-bottom.

**Exit criteria:** one example runs under memory64; the address-site
inventory exists; host-refusal behaviour documented.

### M4 — Full emitter migration to memory64 (cost: L)

Execute the M3 inventory. Memory64 becomes the default; the wasm32
path is retired.

#### M4.1 — `ptr_*` helper layer (S)

- [ ] Introduce `fn ptr_load(offset: u64) -> Instruction<'static>`,
      `fn ptr_store(offset: u64)`, `fn ptr_const(addr: u64)`,
      `fn ptr_add() -> [Instruction; 1]`, `fn ptr_size() ->
      Instruction` in a new module
      `crates/goby-wasm/src/gen_lower/ptr.rs`. They read the target
      address width from a `PtrWidth` enum threaded through
      `HeapEmitState`.
- [ ] Unit tests: each helper emits the expected `I32*` in 32-bit
      mode and `I64*` in 64-bit mode.

#### M4.2 — Route every inventoried site through the helpers (M)

- [ ] Walk the M3.4 checklist. For each category:
  - `I32Load*` / `I32Store*` on linear memory → `ptr_load` /
    `ptr_store`.
  - `I32Const <addr>` → `ptr_const`.
  - `I32Add` on addresses → `ptr_add`.
  - `MemoryFill` / `MemoryCopy` → emit the size argument via the
    correct `i32`/`i64` per `PtrWidth`.
  - `MemorySize` / `MemoryGrow` → produce/consume `i64` when
    `memory64`.
- [ ] **Value-level i32 arithmetic unrelated to addresses must not
      change** (chunk indices, payload counts, tag bits).
- [ ] After each sub-walk, `cargo test -p goby-wasm`. If red, revert
      the sub-walk before moving on.

#### M4.3 — Scratch layout (S)

- [ ] In `HeapEmitState::scratch` rename/repartition so that
      address-typed slots are explicitly i64 (or are typed via a
      small enum). Update the consumers in `emit_list_set_helper`
      and siblings (the `s_src_header`, `s_dst_chunk_ptr`,
      `s_word_off` family).
- [ ] Document the new layout in the doc comment on
      `HeapEmitState`.

#### M4.4 — Host side (`wasm_exec.rs`) (S)

- [ ] `current_linear_memory_bytes` (`wasm_exec.rs:633`):
      return `u64`, not `u32`.
- [ ] `ensure_linear_memory_capacity` and callers: take `required_end:
      u64`, compare against `max_pages` as `u64`.
- [ ] Replace all `u32::try_from(bytes)` truncations on addresses
      with `u64` arithmetic; only at host-import boundaries that
      interact with host data structures do we narrow.

#### M4.5 — Flip the default (S)

- [ ] `memory_config.rs`: `RUNTIME_MEMORY_CONFIG.memory64 = true`;
      `memory_type()` emits `memory64: true` when set. Remove the
      M3.1 internal toggle.
- [ ] Retire the wasm32 code path for the runtime-facing config.
      `TEST_MEMORY_CONFIG` may stay wasm32 so that the forced-trap
      tests continue to exercise the original path on small
      allocations; alternatively, re-anchor those tests to wasm64
      page counts and drop the wasm32 path entirely. Decision
      criterion: pick the option that keeps the test intent clearest;
      if unsure, keep wasm32 for tests.

#### M4.6 — Tests (S)

- [ ] Re-anchor `heap_only_recursive_tuple_allocation_grows_past_initial_pages`
      (`lib.rs:1985`) and
      `host_string_concat_grows_linear_memory_past_initial_pages`
      (`wasm_exec.rs:1191`) to the config chosen in M4.5. Verify
      they still raise `E-MEMORY-EXHAUSTION`.
- [ ] New positive test: allocate a single list of > 64 MiB under
      the default `RUNTIME_MEMORY_CONFIG`. Confirm success.
- [ ] Re-run the full `cargo test` matrix. Existing program stdout
      must be byte-identical (module bytes will differ).

#### M4.7 — Comment / doc audit (S)

- [ ] `rg '4\s*GiB|64\s*MiB|wasm32|max_pages' crates/ doc/` (the
      search, not the tool). Update comments / docstrings /
      diagnostic bodies that promise wasm32 limits. Replace with
      "host-limited (memory64)" phrasing.

**Exit criteria (G2, G3):** `cargo test` green; existing program
stdout unchanged; a > 64 MiB allocation completes under defaults;
`--max-memory-mb` still bounds as specified.

### M5 — Invariant and documentation sync (cost: S)

- [ ] `doc/LANGUAGE_SPEC.md`: update if any wording references the
      Wasm memory target.
- [ ] `doc/STATE.md`: record locked decisions (memory64 default,
      1 GiB default ceiling, in-place lowering for `each` + `:=`) and
      next steps per `AGENTS.md` restart-safety.
- [ ] `doc/PLAN.md`: mark any §3 items this plan settled;
      §3.1 / §3.2 remain for the unresolved structural questions.
- [ ] `doc/BUGS.md`: confirm the G0 regression test added in M1.4
      is green on the current `main`, then move the open entry to
      the resolved section with date and a one-line note pointing
      at M1. The entry must not be moved unless the regression test
      exists and passes.
- [ ] Run the `goby-invariants` skill checklist before the final
      commit of the track.
- [ ] **Delete this file (`doc/PLAN_LIST_FIX.md`).**

**Exit criteria:** invariant gate clean; BUGS.md entry closed;
`doc/PLAN_LIST_FIX.md` removed from the tree.

## Dependency graph

```
M1 (in-place)  ──► independent; closes the reported bug.
M2 (CLI)       ──► independent; ships on wasm32.
M3 (mem64 PoC) ──► depends on M2 (CLI policy used to exercise ceilings).
M4 (mem64)     ──► depends on M3.
M5 (docs)      ──► depends on M1..M4.
```

M1 and M2 can run in either order or in parallel. M3 → M4 → M5 is
strict.

## Quality gate (per milestone boundary)

- [ ] `cargo fmt`
- [ ] `cargo check`
- [ ] `cargo test`
- [ ] No regression in diagnostic wording / spans except where
      explicitly listed in the milestone.
