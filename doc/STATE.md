# Goby Project State Snapshot

Last updated: 2026-05-07 (EP-1c Ty::Fun.effects landed)

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
  - Row variables are recorded in `CanClause`; propagation into
    `Ty::Fun` lands in EP-1c (below).
  - The callback nested-`can` validation gap (e.g.
    `(Int -> Int can Ghost) -> Int` previously slipping past validate)
    is closed in EP-1c via recursive segment validation.
- **EP-1d** (next): integrate `unify_effect_rows` and replace the
  EP-1c "ignore effects" placeholders. See **Next Step** below.
- **EP-1e**: typecheck regression tests for callback row variables and
  freshening independence (likely folded into EP-1d).
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

- `cargo test -p goby-core --lib`: 854 tests (EP-1c added 24:
  `from_can_clause` 4, render 5, annotation lift 7, callback validate
  4, unify/are_compatible "ignore" 3, doubly-parenthesized 1) all
  green; `cargo check --workspace` warning-free.
- `cargo test -p goby-wasm --lib planning::tests`: 16/16 green.
- All previously-green Perceus, TCO, and List acceptance tests remain
  green.

Red / ignored:

- None for the EP track.
- Pre-existing `#[ignore]`d perceus / compile_tests entries from the
  M10 closure remain ignored; see `doc/PLAN.md` §4.2 for re-open
  conditions.

## Next Step

Implement EP-1d: introduce `unify_effect_rows` (closed/closed,
closed/open, open/open with occurs check per LANGUAGE_SPEC §5) and wire
it into `unify_types_with_subst`'s `Ty::Fun` branch and into
`TypeEnv::are_compatible`. Replace the EP-1c "effects: _" placeholders
in the unify code paths and the documenting "ignore effects" tests.
Extend `instantiate_ty_with_fresh_type_vars` with a row-var mapping so
independent call sites of a row-polymorphic function get distinct fresh
row variables (the TODO comment left in EP-1c).
