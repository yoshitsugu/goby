# Goby Project State Snapshot

Last updated: 2026-05-07 (Track EP — EP-3 closes Track EP. `doc/PLAN.md`
§4.6 EP-0 → EP-3 all marked complete; the next active line is selected
from the queued tracks in §4 — see "Next Step".)

## Current Focus

Track EP (effect row polymorphism, `doc/PLAN.md` §4.6) is **complete
(2026-05-07)**. EP-3 landed in two commits:

- **EP-3 Step 2** (`9e8a336`): root-cause fix that splits decl-level
  `decl_can_ops` (permitted by surrounding `can`) from
  with-handler-derived `covered_ops` (lexically discharged). Lambda body
  effect inference reachable via `CallContext.covered_ops` now sees only
  the with-derived set, so callback effects bind the row variable as
  LANGUAGE_SPEC §5 requires; `check_unhandled_effects_in_expr` unions the
  two internally so existing decl-level "can permits this op" behaviour
  is unchanged. `validate_call_chain`'s `Expr::With` branch propagates
  handler-derived covered ops into `CallContext` so a closed callback row
  inside `with H in take_cb (fn ...)` is matched against the residual
  effects rather than the raw lambda body.
- **EP-3 Step 4** (`a083528`): diagnostic refinement — the existing
  "callback effect row mismatch" line now appends a classification hint
  produced by `classify_effect_row_mismatch`. The three mismatching
  closed/closed shapes (extras-only, missing-required-only, disjoint) and
  the row-variable shape each emit distinct wording so LANGUAGE_SPEC §5
  line 283-284's "missing effect in closed row" vs "row variable cannot
  be unified" distinction is observable. The equal closed/closed case is
  unreachable here because the unifier accepts it.

13 EP-3 acceptance tests in `crates/goby-core/src/typecheck.rs` pin the
contract (5 partial-discharge, 3 generic+row, 5 diagnostics).

### EP history (closed phases)

- **EP-0** semantics lock: complete (`2c71294`, 2026-05-07).
- **EP-1a** internal scaffolding: complete (`5371b4e`, 2026-05-07).
- **EP-1b** parser wiring: complete (2026-05-07).
- **EP-1c** Ty::Fun extension + nested validate: complete (`c70dcd8`).
- **EP-1d** unification + freshening: complete (`0da40c7`; CodeRabbit
  follow-ups in `48c030a`). LANGUAGE_SPEC §5 carries the final unification
  rules; `unify_effect_rows`, `apply_row_substitution`, and row freshening
  are wired into `unify_types_with_subst` / `TypeEnv::are_compatible` and
  `instantiate_ty_with_fresh_type_vars`.
- **EP-2 Step 1** (`538ea56`, 2026-05-07): pure helper
  `infer_expr_effects(expr, env, effect_map, required_effects_map, covered_ops)`
  added in `crates/goby-core/src/typecheck_effect.rs` with 10 unit tests
  covering direct and qualified op calls, nested-lambda containment, handler
  discharge (inline / qualified / handler-value), known-callee discharge,
  local-binding shadowing, and pure bodies.
- **EP-2 Step 2** (`2dd8ffe`, 2026-05-07): stdlib `each` / `map` / `fold`
  signatures retrofitted to LANGUAGE_SPEC §5's row-polymorphic shape:
  - `each : List a -> (a -> Unit can {e}) -> Unit can {e}`
  - `map : List a -> (a -> b can {e}) -> List b can {e}`
  - `fold : List a -> b -> (b -> a -> b can {e}) -> b can {e}`
  LANGUAGE_SPEC §7 builtin notes and the historical `List.fold` entry in
  `doc/PLAN.md` were synced in the same commit.
- **EP-2 Step 3a** (`bf0b0c9`, 2026-05-07): `infer_lambda_ty_against_expected`
  now seeds `provided.effects` from `infer_expr_effects(...)` and the
  `validate_call_chain` lambda branch threads the resulting outermost row
  through `unify_effect_rows`. A `CallContext` bundles
  `effect_map / required_effects_map / covered_ops` and is plumbed through
  `check_ordinary_call_arg_types_in_expr` /
  `check_ordinary_call_arg_types_in_stmt` / `infer_callback_arg_ty`. Closed
  callback rows now reject effectful lambdas per LANGUAGE_SPEC §5.
- **EP-2 Step 3b** (`1a43446`, 2026-05-07):
  - `infer_curried_lambda_body_effects` aggregates the body row of a
    possibly-curried lambda by walking through nested `Expr::Lambda`
    layers, so a flat callback annotation (`(b -> a -> b can {e})`)
    receives the inner body's effects when the user writes
    `fn acc -> fn x -> ...`.
  - `infer_call_effects_at_site` (in `typecheck_call.rs`) instantiates the
    callee at a *fully-applied* call site, unifies argument rows (lambdas
    via the curried-aggregation path, named functions via
    `match_function_argument_type`), and returns the resolved fixed effect
    set. Partial applications return `None`.
  - `check_unhandled_effects_in_expr` (`typecheck_effect_usage.rs`) calls
    `infer_call_effects_at_site` on every `Expr::Call`; any effect not
    fully covered by `covered_ops` becomes a "callback effect `<name>` is
    not handled by any enclosing `with` scope" diagnostic.
  - 7 acceptance tests in `crates/goby-core/src/typecheck.rs` assert the
    EP-2 contract:
    1. `each xs (fn n -> emit(n))` without `can Log` is rejected.
    2. Same with `can Log` typechecks.
    3. `map xs (fn n -> emit(n))` propagates / fails depending on `can`.
    4. `fold xs 0 (fn acc -> fn x -> acc + emit(x))` with `can Log` passes.
    5. Same without `can Log` is rejected.
    6. `fold xs 0` partial application typechecks without any `can`.
    7. A handler inside the lambda discharges its own effect; the
       surrounding declaration needs no `can`.
- **EP-2 Step 4** (`1f34991`, 2026-05-07): user-facing example added.
  - `examples/hof_effect.gb`: walks through `each` / `map` / `fold`
    with `Log` callbacks, showing inline-lambda, named-callback, and
    curried-lambda (`fn acc -> fn x -> ...`) shapes; the wrappers
    surface `can Log` and `main` discharges it through a `with` handler.
  - `goby check` passes; `goby run` is gated by the existing
    "handler lowering does not yet support lambdas inside
    handler-lowered code" backend limitation, recorded in the file's
    header comment (same pattern as `read.gb`).
  - `crates/goby-core/src/formatter.rs` adds `idempotent_hof_effect`
    so the file is covered by the formatter idempotence battery.
  - `examples/README.md` "Stdlib And Effects" lists the new entry.
- **EP-2 Step 5** (2026-05-07): `doc/PLAN.md` §2.3 / §4.6 / §4.7 synced
  with the EP-1/EP-2 completion (motivating example moved to the present
  tense, EP-1 / EP-2 phase entries marked `(complete, 2026-05-07)`,
  Track PC blocking-dependency note records EP-2 as satisfied so the only
  remaining PC-2 prerequisite is the §3.3 multi-shot / branch-local state
  work). No source changes.
- **EP-3 Step 2** (`9e8a336`, 2026-05-07): root-cause fix that splits the
  decl-level `decl_can_ops` (permitted by surrounding `can`) from
  with-handler-derived `covered_ops` (lexically discharged). Lambda body
  effect inference reachable via `CallContext.covered_ops` now sees only
  the with-derived set; `check_unhandled_effects_in_expr` unions the two
  internally so existing decl-level "can permits this op" behaviour is
  unchanged. `validate_call_chain`'s `Expr::With` branch propagates
  handler-derived covered ops into `CallContext`, lifting the spurious
  closed-callback-row mismatch when an outer `with H in take_cb (fn ...)`
  already discharges some of the lambda's effects. 10 acceptance tests:
  - `ep3_partial_discharge_log_handler_handles_log_propagates_print` (P-1+)
  - `ep3_partial_discharge_log_handler_without_outer_print_can_is_rejected` (P-1−)
  - `ep3_partial_discharge_callback_log_handled_print_propagates` (P-2+)
  - `ep3_partial_discharge_callback_print_not_handled_is_rejected` (P-2−)
  - `ep3_partial_discharge_with_handler_in_call_chain_lifts_callback_effects`
    (Codex Pass1 regression — `with` on the call site path)
  - `ep3_generic_pure_callback_with_no_can_passes` (G-1)
  - `ep3_generic_effectful_callback_requires_matching_can` (G-2 +/−)
  - `ep3_generic_two_independent_map_calls_dont_interfere` (G-3 fresh-row)
  - `ep3_diag_closed_callback_rejects_extra_effect_lambda` (D-1)
  - `ep3_diag_two_callback_hof_with_conflicting_effects_is_rejected`
    (D-2 — named functions; inline 2-lambda hits a parser limitation)
- **EP-3 Step 4** (`a083528`, 2026-05-07): diagnostic refinement —
  `classify_effect_row_mismatch` produces a trailing hint that
  distinguishes LANGUAGE_SPEC §5 line 283-284's "missing effect in closed
  row" from "row variable cannot be unified". The base
  "callback effect row mismatch" line is unchanged so external assertions
  matching only the prefix still hold. The existing D-1 (extras-only)
  acceptance test is tightened to require the new substring, and 3
  additional tests pin missing-required, disjoint, and row-variable
  wording independently. The equal closed/closed case never reaches the
  classifier (the unifier accepts it).

### Related same-day cleanup (non-EP work)

- **CLAUDE.md test-runner guidance** (`355ba02`, 2026-05-07): captures the
  recommendation to reach for `cargo nextest run -p goby-wasm` on the full
  wasm suite (per-process parallelism dodges wasmtime's internal locking)
  with a `cargo test` fallback when nextest isn't installed. Independent
  process-tooling change, not part of the EP track itself.
- **BUGS.md** (`6cf184a`, 2026-05-07): records `goby-wasm` lib test
  `tests::fold_m5_string_accumulator` as a pre-existing CPU-bound hang
  reproduced on the EP-1d baseline. Use
  `cargo nextest run -p goby-wasm -E 'not test(fold_m5_string_accumulator)'`
  while triaging unrelated work.

## Known Red / Green State

Green:

- `cargo test -p goby-core --lib`: 903 passed / 2 ignored (EP-3 Step 2
  added 10 acceptance tests, Step 4 added 3 wording tests).
- `cargo nextest run -p goby-wasm`: 787 passed / 11 skipped, ~15s wall
  (`fold_m5_string_accumulator` now passes after the
  `build_stdlib_export_map` fix; see `doc/BUGS.md`).
- `cargo check --workspace`: warning-free.
- All previously-green Perceus, TCO, and List acceptance tests remain
  green.

Red / ignored:

- Pre-existing `#[ignore]`d perceus / compile_tests entries from the M10
  closure remain ignored; see `doc/PLAN.md` §4.2.

## Next Step

Track EP closed. Remaining queued tracks (`doc/PLAN.md` §4) — choose by
priority and any external pull:

1. **§3.3 multi-shot classification + branch-local state surface.** Now
   the only hard prerequisite for §4.7 PC-2 (parser combinator on
   algebraic effects).
2. **Track Float** (`doc/PLAN.md`): floating-point support. Self-contained.
3. **Track OOB** (`doc/PLAN.md`): out-of-bounds handling polish.
4. **Track D D5/D6 follow-ups** (`goby lint`).
5. **Track RR-6 limit tuning**.

**Parallel known-red cleanup (lower priority than the next track):**

- Pre-existing typecheck regressions in 8 example files
  (`case_arm_block.gb`, `function_reference.gb`, `list_set.gb`,
  `list_spread.gb`, `mut.gb`, `string_graphemes.gb`, `tco.gb`,
  `to_integer.gb`) surface during a `goby check` loop on `examples/*.gb`.
  These predate EP-2 Step 4 (reproduce on the `975863e` baseline) and
  should be triaged separately, ideally with a `doc/BUGS.md` entry per
  case; formatter idempotence does not catch them.

Track-EP follow-ups not blocking closure (recorded under §4.6 EP-3 in
PLAN): method-call / pipeline callback paths bypass `validate_call_chain`
and `Expr::Block` does not thread per-statement `local_env` updates the
way `check_unhandled_effects_in_expr` does. The block case is currently
unreachable from Goby surface syntax (no `{ ... }` block expression).

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
