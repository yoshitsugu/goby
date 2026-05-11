# Goby Project State Snapshot

Last updated: 2026-05-11 (Track GU-S3 generic-record support (GR) landed: `Box(value: 42) : Box Int`, `Pair(first: 1, second: "x") : Pair Int String`, `v.value : Int` for `v : Box Int`, and nested `v.value : Maybe Int` for `v : Box (Maybe Int)` all type-check. Shared-type-param mismatch like `Same a = Same(left: a, right: a)` against `Same(left: 1, right: "x")` is rejected. Union side (CA-1..CA-3b + CP) + record side (GR) together close PLAN_GU §6 GU-S3 generic-application/access surface; queued next is constructor-name ambiguity resolution).

## Current Focus

**Track GU (generic user-defined types) — GU-S3 mostly closed
on the typecheck side.** GU-S2 (parser → formatter → IR → wasm
placeholder) landed earlier; on top of that, GU-S3 has now
absorbed D-6 (`freshen_type_scheme` extraction), the union ctor
application split (CA-1/CA-2/CA-3a/CA-3b), constructor-pattern
binder inference (CP), and generic-record support (GR-1/GR-2/GR-3),
all on 2026-05-11. As a result, generic ctor applications
(`Just 42 : Maybe Int`, `Box(value: 42) : Box Int`, `Pair 1 "x"
: Pair Int String`), generic field access (`v.value : Int` for
`v : Box Int`), and generic ctor patterns (`case xs Just(x) -> x
+ 1` binding `x : Int`) all type-check; shared-type-param
mismatches at both call sites and record literals (`Same(left:
1, right: "x")` against `Same a = Same(left: a, right: a)`) are
rejected. The IR / wasm side still routes generic ctor
construction and ctor patterns through `UnsupportedCasePattern` /
`LowerError::UnsupportedForm` — runtime support is GU-S4. The
next typecheck-side sub-task in PLAN_GU §6 GU-S3 is constructor-
name ambiguity resolution (qualified > scrutinee-pinned > local-
shadows-imported > diagnostic), followed by cross-module imports
of generic unions and records via
`inject_imported_type_constructors`.

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

**All-green (post GU-S3 D-6 `freshen_type_scheme` extraction, 2026-05-11)**:

- `cargo test -p goby-core --lib`: 948 passed / 2 ignored (up from 942
  thanks to four helper-contract tests and two
  `instantiate_handler_clause_signature` direct tests).
- `cargo nextest run -p goby-wasm`: 876 passed / 11 skipped (unchanged
  from the 2026-05-10 baseline; the D-6 refactor does not touch wasm).
- `cargo test -p goby-lsp`: 56 passed.
- `cargo check --workspace`: warning-free.

**All-green (post GU-S3 CA-1 `Expr::Call` call-site unify, 2026-05-11)**:

- `cargo test -p goby-core --lib`: 950 passed / 2 ignored (up from 948
  via two new `typecheck_check::tests` AST-level pins of partial-
  application and invalid-arg call paths; two pre-existing tests
  rewritten to match the new precise behaviour).
- `cargo test -p goby-lsp`: 56 passed.
- `cargo check --workspace`: warning-free.
- `cargo nextest run -p goby-wasm`: not re-run for CA-1 — the change is
  pure typecheck-side and emit-side baselines remain valid until CA-3b.

**All-green (post GU-S3 CA-2 `unifies_with_annotation`, 2026-05-11)**:

- `cargo test -p goby-core --lib`: 962 passed / 2 ignored (up from 950
  via 12 new `typecheck_unify::tests` rigid/flexible / Handler / effect-
  row-seed pins for the new helper).
- `cargo test -p goby-lsp`: 56 passed.
- `cargo check --workspace`: warning-free.
- `cargo nextest run -p goby-wasm`: not re-run for CA-2 — typecheck-side
  only.

**All-green (post GU-S3 CA-3a `next_id` plumbing, 2026-05-11)**:

- `cargo test -p goby-core --lib`: 964 passed / 2 ignored (up from 962
  via two new `typecheck_check::tests` plumbing pins — env seed from
  locals + globals, and shared `next_id` advance through `Expr::Call`).
- `cargo test -p goby-lsp`: 56 passed.
- `cargo check --workspace`: warning-free.
- `cargo nextest run -p goby-wasm`: not re-run for CA-3a — pure
  plumbing.

**All-green (post GU-S3 CA-3b union ctor application, 2026-05-11)**:

- `cargo test -p goby-core --lib`: 975 passed / 2 ignored (up from
  964; 11 new `typecheck_check::tests` acceptance + regression pins
  for generic union ctor application — `Just 42 : Maybe Int`,
  `Nothing : Maybe Int`, two independent `Just` call sites in one
  module, nested `Just Nothing : Maybe (Maybe Int)`, two-parameter
  `Pair 1 "x" : Pair Int String`, qualified `Maybe.Just 42`,
  qualified nullary `Maybe.Nothing`, non-generic regression x2
  (`Red : Color`, `Box 42 : Box`), wrong-arg-type reject, and a
  documenting test for the pre-existing wrong-arity silent-pass
  behaviour). CA-1's `erase_fresh_type_vars_to_unknown`
  compatibility step is removed in the same commit; CA-2
  `unifies_with_annotation` + CA-3a `next_id` plumbing make it
  unnecessary.
- `cargo test -p goby-lsp`: 56 passed.
- `cargo check --workspace`: warning-free.
- `cargo nextest run -p goby-wasm -E 'not test(fold_m5_string_accumulator)'`:
  875 passed / 12 skipped — the generic ctor registration change
  reaches the wasm backend boundary (`UnsupportedCasePattern` /
  `LowerError::UnsupportedForm` paths are untouched), no regression.

**All-green (post GU-S3 CP constructor-pattern binder inference, 2026-05-11)**:

- `cargo test -p goby-core --lib`: 986 passed / 2 ignored (up from
  975; 11 new `typecheck_check::tests` for the new CP path:
  module-level acceptance for `Just(x) -> x + 1` against
  `Maybe Int` / `from_some Just(x) -> x` against `Maybe String` /
  `Pair(x, _)` against `Pair Int String`, plus AST-level pins for
  the `x : Int` binder, wildcard non-binding, non-generic
  `Box(x)` regression, unknown-ctor fallback, wrong-arity
  fallback, qualified `Maybe.Just(x)`, two `next_id` plumbing
  regressions (scrutinee-fresh seed-jump + caller-counter
  advance from Codex pass-1/pass-2 reviews)).
- `cargo test -p goby-lsp`: 56 passed.
- `cargo check --workspace`: warning-free.
- `cargo nextest run -p goby-wasm -E 'not test(fold_m5_string_accumulator)'`:
  875 passed / 12 skipped — CP is typecheck-side only, no wasm
  layer touched.

**All-green (post GU-S3 GR generic-record support, 2026-05-11)**:

- `cargo test -p goby-core --lib`: 996 passed / 2 ignored (up from
  986; 10 new `typecheck_check::tests` — generic record ctor
  application against single / multi type-parameter records,
  Call-rewrite path for single-field records, field access on
  generic and nested-generic records, non-generic record
  regressions x2, AST-level `Ty::Con { args: [resolved] }` pin,
  wrong-type-field reject, and Codex pass-1 regression for the
  shared-type-param mismatch (`Same(left: 1, right: "x")` against
  `Same a = Same(left: a, right: a)`)).
- `cargo test -p goby-lsp`: 56 passed.
- `cargo check --workspace`: warning-free.
- `cargo nextest run -p goby-wasm -E 'not test(fold_m5_string_accumulator)'`:
  875 passed / 12 skipped — GR is typecheck-side only, the
  generic record ctor lowering remains under `UnsupportedForm`
  / `UnsupportedCasePattern` until GU-S4 lands.

Red / ignored:

- Pre-existing `#[ignore]`d perceus / compile_tests entries (see
  `doc/BUGS.md` for any per-case detail).
- `goby-wasm` lib test `tests::fold_m5_string_accumulator` is a known
  CPU-bound hang on this checkout; track it via `doc/BUGS.md` and skip
  it in nextest runs.

## Next Step

**Primary (active):**

- **Track GU-S3 in progress.** Bug-fix interlude closed 2026-05-10
  (BUGS.md open list empty). The first GU-S3 sub-task — D-6 (extract
  `freshen_type_scheme` and migrate the existing effect-side
  `instantiate_handler_clause_signature` to use it, behaviour
  unchanged) — landed 2026-05-11 (commit `5cf0947`). The shared
  helper lives in `crates/goby-core/src/typecheck_unify.rs` and is
  the single declaration-side template freshener that the remaining
  GU-S3 sub-tasks (union ctor application, ctor-pattern type
  checking, generic-record ctor application, generic-record field
  access, cross-module imports of generic types) route through.

- **GU-S3 sub-tasks completed 2026-05-11.** Union ctor application
  (CA-1/CA-2/CA-3a/CA-3b), constructor-pattern binder inference (CP),
  and generic-record support (GR-1/GR-2/GR-3) have all landed. The
  per-commit landed summaries below stay as the load-bearing
  reference for the shared helpers each step introduced; the next
  active sub-task is constructor-name ambiguity resolution (see the
  "sub-task ordering" bullet further below).

  - **CA-1** ✅ landed: Rewrite `typecheck_check.rs::infer_expr_ty`
    `Expr::Call` arm to use call-site unify via the new `pub(crate) fn
    infer_call_result_ty` entry in `typecheck_call.rs`. Initially
    shipped with a temporary `erase_fresh_type_vars_to_unknown`
    compatibility step (removed in CA-3b). Two pre-existing tests
    (`no_sugar_for_multi_field_constructor`,
    `typechecks_int_to_string_as_named_map_callback`) were rewritten
    against the now-precise call-site behaviour.
  - **CA-2** ✅ landed: `typecheck_unify::unifies_with_annotation` +
    helpers (`is_flexible_fresh_ty_var`, `max_fresh_ty_id_in_ty`).
    Treats `Ty::Var` whose name starts with `__goby_fresh_ty_` as
    flexible (bindable both sides) and any other `Ty::Var` as rigid
    (equality only). `Ty::Unknown` stays as a wildcard. `Handler`
    annotation special-case (`Con("Handler", [..])` ~ `Ty::Handler`)
    is preserved symmetrically. `typecheck_stmt.rs::check_declared_return_type`
    switched from `env.are_compatible` to the new helper; `next_id`
    is seeded from both `max_fresh_ty_id_in_ty` and `max_fresh_row_id`
    so `unify_effect_rows`-created row vars do not collide. Existing
    `are_compatible` callers untouched.
  - **CA-3a** ✅ landed: `infer_expr_ty` now takes `&mut next_id` and
    threads it through every recursive arm + `infer_block_expr_ty`.
    `check_expr` wrapper seeds via the new
    `typecheck_unify::next_fresh_ty_id_seed(env)` (locals + Resolved
    globals walk, one-past-max contract covering both `__goby_fresh_ty_*`
    and `__goby_fresh_row_*`). `infer_call_result_ty` takes `&mut next_id`
    so the call resolver shares the same counter as the surrounding
    `Expr::Call` arm. Internal callers `resolve_function_value_ty`,
    `infer_lambda_ty_against_expected`, `validate_call_chain`,
    `infer_call_effects_at_site`, and `infer_expr_binding_ty` were
    migrated. Pure plumbing, behaviour unchanged (existing 962 tests
    still pass).
  - **CA-3b** ✅ landed: `inject_type_constructors` no longer maps
    generic union ctors to `Ty::Unknown`; the real `Ty::Fun { params:
    arg_types, result: result_template }` template (or the bare
    `result_template` for nullary ctors) is registered, exactly like
    the non-generic case. `typecheck_check.rs::infer_expr_ty` Var
    and Qualified arms route ctor lookups through a new
    `lookup_and_maybe_freshen_ctor` helper, which calls
    `freshen_type_scheme(&[ty], next_id)` only when
    `is_ctor_binding(name)` is true *and* the template still contains
    a `Ty::Var` (so non-generic ctors stay no-op). To prevent the
    call-site resolver from re-freshening the already-freshened
    template, `instantiate_ty_with_fresh_type_vars`'s `Ty::Var(name)`
    arm becomes idempotent on the `__goby_fresh_ty_` prefix.
    `TypeEnv::is_ctor_binding(name)` detects ctor sources via shared
    constants `CTOR_SOURCE_PREFIX` / `CTOR_SOURCE_SUFFIX` (used by
    both `typecheck_build::ctor_source` and the detector), preserving
    the locals-shadowing-wins behaviour modeled on `is_effect_op`.
    CA-1's `erase_fresh_type_vars_to_unknown` compatibility step is
    removed — `unifies_with_annotation` (CA-2) and shared `next_id`
    (CA-3a) make leaked fresh names bind correctly at annotation
    sites.
  - **CP** ✅ landed: `env_with_case_pattern_bindings` (with a new
    `*_using` entry that threads the caller's `&mut next_id`) drives
    constructor-pattern binders through
    `resolve_ctor_pattern_binder_tys` — scheme freshen
    `[result_template, ...arg_types]` + `unify_types_with_subst`
    against scrutinee + `apply_type_substitution`. New
    `TypeEnv::lookup_union_variant(type_qualifier, ctor)` (sorted
    first-match on bare lookup) backs the variant lookup. Wildcard
    binders, unknown ctors, and wrong-arity all fall back to
    `Ty::Unknown` to preserve the legacy tolerant walker. `Expr::Case`
    arm passes the same `next_id` into both the pattern walker and
    the arm-body inference so pattern-binder freshens and arm-body
    generic ctor freshens cannot collide.
  - **GR (GR-1/GR-2/GR-3)** ✅ landed: Record-side analogue of
    CA-3b + CP. `inject_type_constructors` Record arm drops the
    `is_generic -> Ty::Unknown` placeholder (GR-1).
    `Expr::RecordConstruct` is now `infer_record_construct_ty`:
    scheme freshen `[result_template, ...field_tys_sorted_by_name]`
    + per-field `unify_types_with_subst` + `apply_type_substitution`,
    so `Box(value: 42)` returns `Ty::Con { name: "Box", args: [Int] }`
    (GR-2). `Expr::Qualified` field access resolves through
    `resolve_record_field_ty`, applying a `type_params -> args`
    substitution to the declared field template so `v.value` on
    `v : Box Int` returns `Int` (GR-3). The ambiguity walker
    detects shared-type-param mismatches across fields via
    `find_generic_record_field_mismatch` (Codex pass-1 follow-up).
    The old `TypeEnv::record_field_ty` is removed in favour of the
    new helper.

  An adjacent existing-behaviour quirk surfaced during D-6 Pass 2
  remains open: when `clause_name` is already qualified (`Eff.op`),
  `instantiate_handler_clause_signature` performs an `Eff.Eff.op`
  lookup. Not a regression; flagged for a later GU-S3 / resume-typing
  sub-task.

- **Bug-fix interlude (history).** Track GU was paused before GU-S3
  until every open BUGS.md entry was fixed and covered by a
  regression test, so a failure spotted during GU-S3/S4 could be
  attributed to the new code rather than to pre-existing layer
  noise. Status (now closed):
  1. **2026-05-01 case-over-list-pattern function-result bug** —
     **CLOSED 2026-05-09** as a stop-gap that classifies list-
     pattern Bind binders as `Borrowed` in Perceus. Three end-to-
     end regressions pinned in `runtime_output_tests.rs` plus a
     layer-level pin in `perceus::tests`. Long-term refactor
     tracked under PLAN.md §4.5d Track LB (truly-owned binders via
     emit-side or IR-level `Dup`).
  2. **2026-05-09 stdlib `int.parse` `minimum_int` literal range**
     — **CLOSED 2026-05-10** by re-aligning `stdlib/goby/int.gb`
     boundary constants to the Goby Int 60-bit range
     (`minimum_int = -576460752303423488`,
     `minimum_int_div_10 = -57646075230342348`). Two compiled-wasm
     regressions pinned in `runtime_output_tests.rs`
     (`stdlib_int_parse_direct_call_executes_via_compiled_wasm`,
     `stdlib_int_parse_boundaries_execute_via_compiled_wasm`).
     Spec note added in `doc/LANGUAGE_SPEC.md` §3 (`Int` literal
     range) and the `goby/int.parse` stdlib paragraph. The separate
     HOF-callback effect-handler scoping defect that surfaced after
     this fix is closed as entry #3 below.
  3. **2026-05-10 HOF-callback `int.parse` `unknown local
     'invalid_integer'`** — **CLOSED 2026-05-10**. The resolver-side
     fix resolves stdlib decls in their owning-module context and
     registers own-module effect declarations. The codegen-side fix
     keeps `with invalid_integer ... in int.parse value` local to the
     handler-lowered callback by routing the parse attempt through the
     internal `__goby_int_parse_maybe` backend/host intrinsic and
     invoking the user's handler clause only when the intrinsic returns
     tagged `Unit`. The HOF regression
     `hof_callback_int_parse_invalid_integer_executes_via_compiled_wasm`
     is no longer ignored and passes.
  4. **2026-05-08 `AllocFloatBox` / `AllocMutableCell` shared
     refactor** — **CLOSED 2026-05-10**. Both arms (and the
     `emit_alloc_float_box_with_bits_local` helper) now route through
     `emit_alloc_with_flag(SizeClass::Cell, …)`, so freed Cell /
     TAG_FLOAT slots returned to `FREE_LIST_SLOT_CELL` by `__goby_drop`
     are consumed before bump. The result pointer for the literal
     arms moved to the depth-indexed spill `HELPER_SCRATCH_I32 +
     heap_base_depth`, mirroring `CreateClosure` / `AllocReuse`, so
     nested allocations inside `init_instrs` / `bits_instrs` are no
     longer a hidden constraint. `required_heap_base_spill_count_instr`
     returns `1 + child` for both arms to match. Two unit tests in
     `gen_lower::emit::tests` pin the spill-count contract.

- **Track GU-S3 sub-task ordering (post union + CP + GR).**
  D-6 `freshen_type_scheme` extraction, **union ctor application**
  (CA-1/CA-2/CA-3a/CA-3b), **constructor-pattern binder inference** (CP),
  and **generic-record support** (GR-1/GR-2/GR-3) are all complete
  (2026-05-11). The remaining queued sub-tasks, in the order they will
  be picked up: constructor-name ambiguity resolution (qualified >
  scrutinee-pinned > local-shadows-imported > ambiguity diagnostic);
  cross-module imports of generic unions and records (via
  `inject_imported_type_constructors`); generic-record field access
  on global / function-return receivers (locals-only short-cut today).
  See `doc/PLAN_GU.md` §6 GU-S3 for the deliverable list. Deferred
  CA-3b / CP / GR follow-ups (tracked in `progress-log.md`): Pipeline
  (`42 |> Just`) / MethodCall ctor lookup, ctor-as-value (`map xs Just`),
  expected-type-driven bare ctor disambiguation, generic ctor arity /
  wrong-arg diagnostic wording (currently silent-passes on extra args),
  pattern-side wrong-arity diagnostic (currently silent fallback to
  `Ty::Unknown`), PC-shaped function-typed record fields with effect
  rows.

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

- Track LB: list-pattern Bind binders as truly-owned references
  (`doc/PLAN.md` §4.5d). New track locked 2026-05-09 as the long-
  term follow-up to the BUGS.md 2026-05-01 stop-gap. Independent
  of GU/EX/PC; can land whenever bandwidth allows. Acceptance
  reverts the `Borrowed`-classification stop-gap once head/tail
  Binds are materialised as truly owned references.
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
