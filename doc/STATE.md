# Goby Project State Snapshot

Last updated: 2026-05-08.

## Current Focus

**Track GU (generic user-defined types) — spec freeze complete (GU-D1).**
`doc/PLAN_GU.md` §3.8 surface text is now in `doc/LANGUAGE_SPEC.md`
(commit `9c715e5`, 2026-05-08): the new union/record grammar (with type
parameters and variant args) is documented in §2 alongside the alias
form, and the constructor-pattern subsection (bare and `TypeName.Ctor`
forms) is documented in §3 next to list patterns. `doc/PLAN_IR.md` WB-2B
"Out of scope" note is updated to drop the now-incorrect "do not exist
in the current language spec" claim. Next phase is **GU-S1: destructive
AST swap** — `crates/goby-core/src/ast.rs` is reshaped to the §3.1/§3.2
final form; the build is intentionally broken at the GU-S1 commit and
GU-S2 walks the layers in dependency order to repair it.

Track PC remains queued; it cannot start before GU-X2 (the closed-form
green check). The earlier reference to `tmp/pc.md` is dropped — the
design lock is now carried by `doc/PLAN_PC.md` §2 and `doc/PLAN_GU.md`
§3 directly.

## Known Red / Green State

Green:

- `cargo test -p goby-core --lib`: 917 passed / 2 ignored.
- `cargo check --workspace`: warning-free.
- `cargo test -p goby-lsp`: 56 passed / 0 failed.
- `cargo nextest run -p goby-wasm -E 'not test(fold_m5_string_accumulator)'`:
  the regular wasm suite passes (856 / 12 skipped).

Red / ignored:

- Pre-existing `#[ignore]`d perceus / compile_tests entries (see
  `doc/BUGS.md` for any per-case detail).
- `goby-wasm` lib test `tests::fold_m5_string_accumulator` is a known
  CPU-bound hang on this checkout; track it via `doc/BUGS.md` and skip
  it in nextest runs.

## Next Step

**Primary (active):**

- **Track GU (generic user-defined types) — GU-S1 next.** GU-D0 design
  freeze and GU-D1 spec freeze complete (commits up to `9c715e5`,
  2026-05-08). The next step is GU-S1 (destructive AST swap in
  `crates/goby-core/src/ast.rs` to the §3.1/§3.2 final shape; the
  build is intentionally broken at this phase and repaired layer by
  layer in GU-S2). See `doc/PLAN_GU.md` §6 for the full phase list.

**Queued behind GU:**

- **Track EX: case exhaustiveness checking** (`doc/PLAN.md` §4.5c).
  New track locked 2026-05-08. Lifts non-exhaustive `case` from
  Track GU's interim runtime trap to a compile-time error. Hard
  prerequisite for Track PC. Cannot start before GU-X2 closes.
- **Track PC: Parser combinator on algebraic effects**
  (`doc/PLAN.md` §4.6, `doc/PLAN_PC.md`). Hard-depends on **GU-X2
  AND EX-S1**; cannot start before both close. PC-2 additionally
  depends on §3.3 multi-shot / branch-local state.
- **Track RP: relative-path imports** (`doc/PLAN.md` §4.5b). PC-P0
  pre-flight consumes RP-3; RP can land in parallel with GU/EX.

**PC blockers (orthogonal to GU/RP):**

- **§3.3 multi-shot classification + branch-local state surface**: the
  only remaining hard PC-2 blocker.

**Other queued tracks (lower priority):**

- Track OOB (out-of-bounds handling polish; `doc/PLAN.md` §4.5).
- Track D D5/D6 follow-ups (`goby lint`; `doc/PLAN.md` §4.1).
- Track RR-6 limit tuning (`doc/PLAN.md` §4.5 RR).

**Parallel known-red cleanup (lowest priority):**

- **Grapheme-track roadmap-label hygiene**: residual `e4_` / `e5_` /
  `e6_` test prefixes and comment-internal `E4` / `E5` / `E6`
  references survive in `crates/goby-wasm/src/lib.rs`,
  `crates/goby-wasm/src/compile_tests.rs`, and
  `crates/goby-wasm/tests/wasm_exports_and_smoke.rs`. Rewrite per
  `doc/PLAN.md` §2 (locked 2026-03-25) when a grapheme-related track
  is opened or as a standalone hygiene PR.
- Pre-existing typecheck regressions in 8 example files
  (`case_arm_block.gb`, `function_reference.gb`, `list_set.gb`,
  `list_spread.gb`, `mut.gb`, `string_graphemes.gb`, `tco.gb`,
  `to_integer.gb`) reproduce on the `975863e` baseline. Triage
  separately with a `doc/BUGS.md` entry per case.
