# Goby Project State Snapshot

Last updated: 2026-05-07 (EP-1b parse_can_clause landed)

## Current Focus

Track EP (effect row polymorphism, `doc/PLAN.md` §4.6) is the active line.

- **EP-0** (semantics lock): complete (commit `2c71294`, 2026-05-07).
  Surface `can E1, E2, {e}` and Rémy/Leijen-style row unification rules
  are recorded in `doc/LANGUAGE_SPEC.md` §5.
- **EP-1a** (internal scaffolding): complete (commit `5371b4e`, 2026-05-07).
  `EffectRow`, `RowVarId`, `RowSubst`, `CanClause` are added to
  `crates/goby-core/src/typecheck_env.rs` ahead of consumers; item-local
  `#[allow(dead_code)]` suppresses lib-profile warnings until EP-1b/c
  wire them in.
- **EP-1b** (parser wiring): complete (2026-05-07).
  - `parse_can_clause(annotation) -> Result<Option<CanClause>, ParseCanError>`
    and `find_top_level_can_keyword_index` added to
    `crates/goby-core/src/typecheck_annotation.rs`. Detection is
    parenthesis-depth aware so callback `can` clauses do not shadow the
    outer one.
  - `validate_effect_clause`, `build_required_effects_map`,
    `ops_from_can_clause`, `extract_residual_effects`,
    `types::strip_effect`, `typecheck_validate.rs::strip_effect_clause`,
    and `goby-wasm::planning::parse_effect_clause_effects` all route
    through the new helper. The wasm planner uses a tolerant public
    wrapper `fixed_effects_from_can_clause`.
  - Surface accepts `can E1, E2`, `can E1, {e}`, `can {e}`, `can {}`;
    rejects `can {e}, {f}` (multi row var), `can {Foo}` / `can {_}`
    (row var must be lowercase ASCII identifier), and `can {}, X`
    / `can {}, {}` (`{}` cannot be combined or repeated).
  - Row variables are recorded in `CanClause` but **not** propagated
    into `Ty::Fun` yet — that is EP-1c's responsibility.
  - **Known follow-up (EP-1c)**: callback annotations such as
    `(Int -> Int can Ghost) -> Int` still skip nested-`can`
    validation (long-standing gap, not introduced by EP-1b). The fix
    requires `Ty::Fun` to carry per-position effect rows, so it will
    ride along with the EP-1c structural change.
- **EP-1c** (next): extend `Ty::Fun` with `effects: EffectRow` (default
  closed-empty) across all construction/match sites, and validate
  nested callback `can` clauses recursively at the same time.
- **EP-1d**: integrate `unify_effect_rows` into `unify_types_with_subst`.
- **EP-1e**: typecheck regression tests for callback row variables and
  freshening independence.
- **EP-2**: retrofit stdlib HOFs (`each` / `map` / `fold`) with
  row-polymorphic signatures and add user-facing examples.

Other active tracks remain queued in `doc/PLAN.md` §4 (Track D `goby
lint` D5/D6 follow-ups, Track Float, Track OOB, Track RR-6 limit
tuning, Track PC parser combinator — gated by EP).

## Recently Closed (Reference Only)

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

## Known Red / Green State

Green:

- `cargo test -p goby-core --lib`: 830 tests (EP-1a 9 + EP-1b parser 23
  + EP-1b typecheck 5 + duplicate-brace 1 over the prior baseline), all
  green; `cargo check --workspace` warning-free.
- `cargo test -p goby-wasm --lib planning::tests`: 16/16 green,
  including the new `parse_effect_clause_effects_drops_row_variable_and_explicit_empty`
  guard.
- All previously-green Perceus, TCO, and List acceptance tests remain
  green.

Red / ignored:

- None for the EP track.
- Pre-existing `#[ignore]`d perceus / compile_tests entries from the
  M10 closure remain ignored; see `doc/PLAN.md` §4.2 for re-open
  conditions.

## Next Step

Implement EP-1c: extend `Ty::Fun` with `effects: EffectRow` (defaulting
to closed-empty) across the construction and match sites in
`typecheck_*`, plumb `CanClause -> EffectRow` at annotation conversion,
and start validating nested callback `can` clauses recursively (the
EP-1b follow-up Codex pass-1 surfaced). Unification of effect rows
itself stays deferred to EP-1d.
