# Goby Project State Snapshot

Last updated: 2026-05-07 (Track EP closed; Track PC (`doc/PLAN.md` §4.6
Parser Combinator) is the new active line, gated only by §3.3 multi-shot
classification + branch-local state surface.)

## Current Focus

**Track PC: Parser Combinator on Algebraic Effects**
(`doc/PLAN.md` §4.6).

Track PC is the next concrete forcing function for the Goby effect
system: a small, idiomatic parser-combinator surface that uses effects
as the composition mechanism rather than decoration. With effect-row
polymorphism landed (2026-05-07), the only remaining hard PC-2
prerequisite is **§3.3 multi-shot classification + branch-local state
surface**: `alt` and `many` need either a sanctioned multi-shot path or
an explicit branch-local state construct to roll the input cursor back
without violating ordinary-`mut` semantics.

PC execution phases are described in `doc/PLAN.md` §4.6:

- **PC-0**: Surface / protocol / backtracking lock (no implementation).
- **PC-1**: Cursor-threaded MVP without multi-shot — produces fixtures
  that exercise HOF effect propagation and basic combinators.
- **PC-2**: Multi-shot `alt` / `many` once §3.3 unblocks.
- **PC-3**: Stdlib polish, error-reporting, examples.

PC-0 should land first (design lock), in parallel with §3.3
classification work.

## Known Red / Green State

Green:

- `cargo test -p goby-core --lib`: 903 passed / 2 ignored.
- `cargo nextest run -p goby-wasm`: 787 passed / 11 skipped, ~15s wall
  (`fold_m5_string_accumulator` now passes after the
  `build_stdlib_export_map` fix; see `doc/BUGS.md`).
- `cargo check --workspace`: warning-free.
- All previously-green Perceus, TCO, List, and effect-row acceptance
  tests remain green.

Red / ignored:

- Pre-existing `#[ignore]`d perceus / compile_tests entries from the M10
  closure remain ignored; see `doc/PLAN.md` §4.2.

## Next Step

**Primary:**

1. **PC-0 design lock** (`doc/PLAN.md` §4.6): record the chosen `Parser`
   shape, effect protocol, and backtracking story in
   `doc/LANGUAGE_SPEC.md` (or a dedicated stdlib design note). Add
   motivating examples (number parser, JSON-like literal, simple
   arithmetic-expression parser) as failing/aspirational fixtures.
2. **§3.3 multi-shot classification + branch-local state surface**
   (parallelizable with PC-0): the remaining hard PC-2 blocker.

**Other queued tracks** (lower priority unless a concrete pull exists):

- **Track Float** (`doc/PLAN.md`): floating-point support. Self-contained.
- **Track OOB** (`doc/PLAN.md`): out-of-bounds handling polish.
- **Track D D5/D6 follow-ups** (`goby lint`).
- **Track RR-6 limit tuning**.

**Parallel known-red cleanup (lower priority than the next track):**

- Pre-existing typecheck regressions in 8 example files
  (`case_arm_block.gb`, `function_reference.gb`, `list_set.gb`,
  `list_spread.gb`, `mut.gb`, `string_graphemes.gb`, `tco.gb`,
  `to_integer.gb`) surface during a `goby check` loop on `examples/*.gb`.
  These predate the row-polymorphism work (reproduce on the `975863e`
  baseline) and should be triaged separately, ideally with a
  `doc/BUGS.md` entry per case; formatter idempotence does not catch
  them.

Effect-row follow-ups deferred (not blocking PC):

- method-call (`.foo`) / pipeline (`|>`) callback paths bypass
  `validate_call_chain` and therefore the row-mismatch / row-leak
  diagnostics. Reproducers using ordinary `Expr::Call` are covered.
- `Expr::Block`'s ordinary-call validation does not thread
  per-statement `local_env` updates the way
  `check_unhandled_effects_in_expr` does. Goby's surface lacks
  `{ ... }` block syntax, so this is not currently reachable from user
  source.

## Recently Closed (Reference Only)

- **Effect row polymorphism** (commits 2026-05-07): row-polymorphic
  callback signatures (`can ..., {e}`) propagate effects through stdlib
  HOFs (`each` / `map` / `fold`) and user-defined HOFs. Closed callback
  rows reject effectful lambdas; diagnostic wording distinguishes
  "missing effect in closed row" from "row variable cannot be unified"
  (LANGUAGE_SPEC §5). 13 dedicated acceptance tests in
  `crates/goby-core/src/typecheck.rs` (`ep3_*`) plus 7 from earlier
  phases pin the contract. Key pieces: row representation with closed /
  open / row-variable forms (`unify_effect_rows`,
  `apply_row_substitution`); `infer_expr_effects` /
  `infer_curried_lambda_body_effects` /
  `infer_call_effects_at_site` for callback row inference; the
  decoupling of `decl_can_ops` (decl `can` permits) from `covered_ops`
  (lexically `with`-discharged) so callback effects surface to the row
  variable instead of being silently swallowed by an outer `can`.
- **Track E (Perceus)**: M0–M11 complete. Durable design lives in
  `doc/PLAN.md` §4.2; runtime-allocator unification, `LetMut`-aware
  `return_ownership_value`, and the 138×138 stdin acceptance test all
  shipped. Re-open conditions and won't-fix items are recorded in the
  same section.
- **Generic TCO (RR-5)**: published contract in
  `doc/LANGUAGE_SPEC.md` §4.1.
- **Sequence-backed `List`**: published contract in
  `doc/LANGUAGE_SPEC.md`.
- **WB-4C lexical handler metadata** (effect operation identity slice):
  shared IR carries `EffectOpId`; handler-clause lowering and legality
  analysis use `effect + op` identity when available. Lexical target
  records for `WithHandler` / `PerformEffect` remain a follow-up slice.
- **CLAUDE.md test-runner guidance** (`355ba02`, 2026-05-07): captures
  the recommendation to reach for `cargo nextest run -p goby-wasm` on
  the full wasm suite (per-process parallelism dodges wasmtime's
  internal locking) with a `cargo test` fallback when nextest isn't
  installed.
- **BUGS.md** (`6cf184a`, 2026-05-07): records `goby-wasm` lib test
  `tests::fold_m5_string_accumulator` as a pre-existing CPU-bound hang
  on the pre-row-polymorphism baseline. Use
  `cargo nextest run -p goby-wasm -E 'not test(fold_m5_string_accumulator)'`
  while triaging unrelated work.
