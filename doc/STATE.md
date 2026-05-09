# Goby Project State Snapshot

Last updated: 2026-05-09.

## Current Focus

**Track GU (generic user-defined types) — parser layer complete (GU-S2a).**
`parser_top.rs::parse_type_declaration_line` now parses the new
generic-type grammar from `doc/PLAN_GU.md` §3.8 (type parameters on
union/record declarations, positional union variant args including
nested type applications such as `Cons(a, List a)`), with
disambiguation between `Box(a)` (single-variant union, no pipe) and
`Box(value: a)` (record). `parser_pattern.rs::parse_case_pattern`
recognises bare and qualified constructor patterns
(`Ctor`, `Ctor x _`, `TypeName.Ctor x`) with binder identifier rules
that match existing list-pattern conventions. `parser_util.rs` adds
`split_type_header`, `contains_top_level_colon`, and
`has_balanced_delimiters`. `cargo build -p goby-core` still fails,
with errors now localised to five files (ast.rs is only mentioned by
hint, plus formatter.rs, ir_lower.rs, typecheck_build.rs,
typecheck_types.rs). Next phase is **GU-S2b: formatter + lib.rs
re-exports** — re-emit type declarations and constructor patterns in
the new shape and refresh re-exports.

Track PC remains queued; it cannot start before GU-X2 (the closed-form
green check). The earlier reference to `tmp/pc.md` is dropped — the
design lock is now carried by `doc/PLAN_PC.md` §2 and `doc/PLAN_GU.md`
§3 directly.

## Known Red / Green State

Green (last measured before the GU-S1 destructive AST swap, baseline
commit `df57c32`):

- `cargo test -p goby-core --lib`: 917 passed / 2 ignored.
- `cargo check --workspace`: warning-free.
- `cargo test -p goby-lsp`: 56 passed / 0 failed.
- `cargo nextest run -p goby-wasm -E 'not test(fold_m5_string_accumulator)'`:
  the regular wasm suite passes (865 / 11 skipped after Track HF, 2026-05-09).

**Currently broken (intentional, GU-S1 window only)**:

- `cargo build -p goby-core` fails. Errors are localised to six §4
  walk-list files (ast.rs, formatter.rs, ir_lower.rs, parser_top.rs,
  typecheck_build.rs, typecheck_types.rs) and are repaired by GU-S2.
  All other check commands (`-p goby-core --lib`, `-p goby-wasm`,
  `-p goby-lsp`, `--workspace`) are blocked by this until GU-S2 ends.

Red / ignored:

- Pre-existing `#[ignore]`d perceus / compile_tests entries (see
  `doc/BUGS.md` for any per-case detail).
- `goby-wasm` lib test `tests::fold_m5_string_accumulator` is a known
  CPU-bound hang on this checkout; track it via `doc/BUGS.md` and skip
  it in nextest runs.

## Next Step

**Primary (active):**

- **Track GU (generic user-defined types) — GU-S2b next.** GU-D0 / GU-D1
  / GU-S1 / GU-S2a complete. The next step is GU-S2b: re-emit type
  declarations and constructor patterns in `crates/goby-core/src/formatter.rs`
  to match the new shape, and refresh re-exports in
  `crates/goby-core/src/lib.rs` (notably `UnionVariant` /
  `CtorPatternArg`). Downstream typecheck / IR / wasm layers remain
  broken until later GU-S2 sub-tasks land. See `doc/PLAN_GU.md` §6
  for the full phase list.

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
