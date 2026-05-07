# Goby Project State Snapshot

Last updated: 2026-05-07 (Track EP Phase 1 closed; CodeRabbit follow-ups landed)

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
    `Ty::Fun` lands in EP-1c.
- **EP-1c** (Ty::Fun extension + nested validate): complete (commit
  `c70dcd8`, 2026-05-07).
  - `Ty::Fun` carries an `effects: EffectRow` field, propagated through
    every construction / destructuring site in `typecheck_*` (resolve_alias,
    apply_type_substitution, instantiate_ty_with_fresh_type_vars, partial
    application, callback rest, branch merging).
  - `EffectRow::from_can_clause` lifts the surface `CanClause` into the
    internal row.
  - `ty_from_annotation` is now string-based and threads a shared type-hole
    counter through nested function segments. It peels matching outer
    parens and lifts each segment's `can` clause into its own
    `Ty::Fun.effects`.
  - `validate_type_annotation` recurses into each function segment, closing
    the EP-1b follow-up gap for callback annotations with malformed or
    unknown effects.
  - `typecheck_render` appends ` can ...` only when the row is non-empty
    (snapshot compatibility).
  - `typecheck_build::build_type_env` and
    `typecheck_validate::ty_from_import_annotation` route through
    `ty_from_annotation` so global / imported function types preserve
    their `can` clauses.
  - `TypeEnv::are_compatible` and `unify_types_with_subst` initially
    ignored the `effects` field; that placeholder is removed in EP-1d.
- **EP-1d** (unification + freshening): complete (commit `0da40c7`,
  2026-05-07; CodeRabbit follow-ups in `48c030a`).
  - `unify_effect_rows` implements the LANGUAGE_SPEC §5 rules (closed-closed,
    closed-open, open-open same-var, open-open distinct vars with occurs
    check). `apply_row_substitution` walks tails through `RowSubst` with a
    `visited: HashSet<RowVarId>` to detect direct and indirect cycles.
  - `apply_type_substitution` and `unify_types_with_subst` are now
    row-aware; `RowSubst` is threaded alongside `TypeSubst` through
    `match_function_argument_type`, `infer_callback_arg_ty`,
    `infer_lambda_ty_against_expected`, `resolve_function_value_ty`, and
    the call-validation chain.
  - `instantiate_ty_with_fresh_type_vars` freshens `RowVarId` per call site
    via a dedicated `row_mapping`; same-named row variables within one
    instantiation share the same fresh tail.
  - `TypeEnv::are_compatible` recurses through `Ty::Fun` and `Ty::Con`
    args using a single shared `RowSubst`, and seeds `next_id` past any
    `__goby_fresh_row_N` already present in the inputs to avoid collisions.
  - `unify_open_with_open` rolls back the first bind when the second
    occurs check fails so callers never observe a half-applied row
    substitution.
  - `branch_types_compatible` / `merge_branch_type` now require equal
    residual rows on `Ty::Fun` branches; mismatched function rows
    collapse the merged type to `Ty::Unknown` rather than silently
    dropping effects.
  - LANGUAGE_SPEC §5 is updated with the open-open *same row variable*
    rule that the implementation already follows (`S1 == S2`, no fresh
    binding).
  - CodeRabbit follow-ups (`48c030a`):
    `typecheck_effect_usage` now renders diagnostics from the
    pre-unification snapshot so a partially-mutated `subst` cannot
    leak into the message; `ty_from_import_annotation` documents its
    "non-function imports cannot carry `can`" invariant via
    `debug_assert`; renderer test comment for the ambiguous nested-can
    shape rewritten to match what the renderer actually emits.
- **EP-1e**: folded into EP-1d (callback freshening, partial application,
  collision avoidance covered by tests).
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

- `cargo test -p goby-core --lib`: 872 tests (EP-1d added 18 over the
  EP-1c baseline: 13 unify_effect_rows unit tests, 2 freshening tests,
  3 reverted-and-rewritten EP-1c "ignore" placeholders converted to
  EP-1d acceptance/rejection asserts, plus a fresh-row collision guard);
  `cargo check --workspace` warning-free.
- `cargo test -p goby-wasm --lib planning::tests`: 16/16 green.
- All previously-green Perceus, TCO, and List acceptance tests remain
  green.

Red / ignored:

- None for the EP track.
- Pre-existing `#[ignore]`d perceus / compile_tests entries from the
  M10 closure remain ignored; see `doc/PLAN.md` §4.2 for re-open
  conditions.

## Next Step

Implement EP-2: retrofit stdlib HOFs (`each` / `map` / `fold`) with
row-polymorphic signatures (e.g. `map : List a -> (a -> b can {e}) ->
List b can {e}`) and add user-facing examples. The EP-1d unification
machinery is in place to make these signatures actually propagate
callback effects to the caller's `can` clause.
