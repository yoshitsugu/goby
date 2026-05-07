# Goby Project State Snapshot

Last updated: 2026-05-07 (Track EP — EP-2 Step 4 landed: user-facing
`examples/hof_effect.gb` exercising row-polymorphic effect propagation
through `each` / `map` / `fold`, with formatter-idempotence coverage)

## Current Focus

Track EP (effect row polymorphism, `doc/PLAN.md` §4.6) is the active line.
EP-2 ("HOF effect propagation") is the in-flight milestone.

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

### EP-2 progress (this milestone)

- **EP-2 Step 1** (`538ea56`, 2026-05-07): pure helper
  `infer_expr_effects(expr, env, effect_map, required_effects_map, covered_ops)`
  added in `crates/goby-core/src/typecheck_effect.rs` with 10 unit tests
  covering direct and qualified op calls, nested-lambda containment, handler
  discharge (inline / qualified / handler-value), known-callee discharge,
  local-binding shadowing, and pure bodies.
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
- **EP-2 Step 3b** (2026-05-07):
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
- **EP-2 Step 4** (2026-05-07): user-facing example added.
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

## Known Red / Green State

Green:

- `cargo test -p goby-core --lib`: 890 passed / 2 ignored (EP-2 Step 1
  added 10, Step 3b added 7, Step 4 added 1 formatter idempotence test).
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

1. EP-2 Step 5 / wrap-up: mark EP-2 complete in `doc/PLAN.md` §4.6, then
   move on to EP-3 (diagnostics polish, partial discharge interaction).
2. Pre-existing typecheck regressions in 8 example files
   (`case_arm_block.gb`, `function_reference.gb`, `list_set.gb`,
   `list_spread.gb`, `mut.gb`, `string_graphemes.gb`, `tco.gb`,
   `to_integer.gb`) surface during a `goby check` loop on `examples/*.gb`.
   These predate EP-2 Step 4 (reproduce on the `975863e` baseline) and
   should be triaged separately; formatter idempotence does not catch
   them.

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
