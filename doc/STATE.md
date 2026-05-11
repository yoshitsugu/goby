# Goby Project State Snapshot

Last updated: 2026-05-11 (Track GU-S3 union ctor application sub-tasks CA-1 + CA-2 + CA-3a landed; `infer_expr_ty` now threads `next_id` through call resolver / lambda body / case arms via the new `typecheck_unify::next_fresh_ty_id_seed`. CA-3b pending).

## Current Focus

**Track GU (generic user-defined types) — GU-S2 closed.** All eight
GU-S2 sub-tasks (S2a parser, S2b formatter, S2c no-op resolved
form, S2d typecheck data layer, S2e typecheck walker hooks, S2f IR
layer, S2g stdlib resolver no-op, S2h wasm backend) have landed.
The new generic-type AST flows through every layer; constructor
patterns parse, format, type-check (binders as `Ty::Unknown` until
the freshening pass), lower into a placeholder
`IrCasePattern::Ctor` that perceus / closure-capture pattern
walkers carry, and the wasm backend routes them through
`UnsupportedCasePattern` / `LowerError::UnsupportedForm` so generic
union construction is not yet runtime-executable. Single bare
`type Wrap = String` keeps its alias meaning; `type Box a = Box(a)`
is a single-variant generic union; `type S = S(xs: List (String)`
fails at parse time via the new RHS balanced-delimiter check.
Next phase is **GU-S3: typecheck semantics for parametric types**
(`freshen_type_scheme` extraction, ctor-application unification,
constructor-pattern type checking, ambiguity resolution, generic
records, cross-module imports of generic unions). See
`doc/PLAN_GU.md` §6 GU-S3 for the deliverable list.

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

- **Active sub-task: union ctor application (CA-1/CA-2/CA-3a landed, CA-3b next).**
  PLAN_GU.md §6 GU-S3 acceptance items for this sub-task: `Just 42 :
  Maybe Int` typechecks, two `Just 42` / `Just "hi"` call sites in the
  same module type-check independently, arity / type mismatches
  surface through the existing function-call diagnostic path. Three
  Codex plan-review rounds (2026-05-11) exposed five design holes
  (Call arm ignores call-site unify; `are_compatible` doesn't bind
  `Ty::Var`; `next_id=0` collides for nested ctors; rigid vs flexible
  `Ty::Var` indistinguishable; `next_id` seed needs locals+globals
  walk). The sub-task is split into **4 commits**:

  - **CA-1** ✅ landed: Rewrite `typecheck_check.rs::infer_expr_ty`
    `Expr::Call` arm to use call-site unify via the new `pub(crate) fn
    infer_call_result_ty` entry in `typecheck_call.rs`. Includes a
    temporary `erase_fresh_type_vars_to_unknown` compatibility step
    (preserves the old curry-chain `Ty::Unknown` escape until CA-2 /
    CA-3b land flexible-`__goby_fresh_ty_*` annotation unification).
    Two pre-existing tests (`no_sugar_for_multi_field_constructor`,
    `typechecks_int_to_string_as_named_map_callback`) were rewritten
    against the now-precise call-site behaviour. General precision
    improvement; no ctor registration change.
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
  - **CA-2**: Add a `unifies_with_annotation` helper that binds
    only flexible (`__goby_fresh_ty_*` prefix) `Ty::Var`, leaves
    rigid `a` rigid. Swap `typecheck_stmt.rs` return-type comparison
    to it. `are_compatible` untouched.
  - **CA-3a**: Thread `next_id: &mut usize` through `infer_expr_ty`
    and through `typecheck_call.rs::resolve_function_value_ty` so a
    single inference walk shares one counter. `check_expr` wrapper
    seeds `next_id = next_fresh_ty_id_seed(env)` (locals + globals
    walk, one-past-max contract). Plumbing only, behaviour unchanged.
  - **CA-3b**: Drop the `is_generic -> Ty::Unknown` placeholder in
    `inject_type_constructors` Union arm. Add Var/Qualified-arm
    ctor-template freshen in `infer_expr_ty`. Make
    `instantiate_ty_with_fresh_type_vars` idempotent on
    `__goby_fresh_ty_*` prefix to avoid double-freshen. Share a
    `CTOR_SOURCE_PREFIX/SUFFIX` constant between build.rs and a new
    `TypeEnv::is_ctor_binding(name)`. Tests pin the acceptance
    shapes (`Just 42`, `Nothing`, nested `Just Nothing`,
    `Pair 1 "x"`, two `Just`s in one module, `[Nothing, Just 1]`,
    non-generic regressions, qualified ctors, wrong-type/arity
    reject).

  Planning artefacts:
  `~/.claude/workspaces/home_yoshitsugu_src_github.com_yoshitsugu_goby/implementation-plan.md`
  and `progress-log.md`. Execution starts in the next session from
  CA-1, one commit per dev-flow Step 3-7 cycle. Acceptance bar is
  `cargo test -p goby-core --lib` green at each commit, baseline
  948 passed / 2 ignored.

  Deferred follow-ups (out of scope for this sub-task; tracked in
  `progress-log.md`): Pipeline (`42 |> Just`) / MethodCall ctor
  lookup, ctor-as-value (`map xs Just`), case-pattern ctor binder
  inference, generic record support, cross-module imports of
  generic unions, ambiguity validation, IR-lowering generic-ctor
  unsupported path, expected-type-driven bare ctor disambiguation,
  generic ctor arity / wrong-arg diagnostic wording.

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

- **Track GU-S3 sub-task ordering (post D-6).** D-6
  `freshen_type_scheme` extraction is complete; **union ctor
  application** is the in-flight sub-task (split into CA-1/CA-2/
  CA-3a/CA-3b, see above; planning complete, implementation
  pending). Queued after union ctor application: constructor-
  pattern type-check with binder inference, generic-record ctor
  application, generic-record field access (including PC-shaped
  function-typed fields with effect rows), constructor-name
  ambiguity resolution (qualified > scrutinee-pinned >
  local-shadows-imported > ambiguity diagnostic), cross-module
  imports of generic unions and records. See `doc/PLAN_GU.md` §6
  GU-S3 for the deliverable list.

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
