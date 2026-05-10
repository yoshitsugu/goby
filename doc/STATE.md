# Goby Project State Snapshot

Last updated: 2026-05-11 (Track GU-S3 D-6 `freshen_type_scheme` extraction complete; effect-side call site migrated, behaviour unchanged).

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
  unchanged) — landed 2026-05-11. The shared helper lives in
  `crates/goby-core/src/typecheck_unify.rs` and is the single
  declaration-side template freshener that the remaining GU-S3
  sub-tasks (union ctor application, ctor-pattern type checking,
  generic-record ctor application, generic-record field access,
  cross-module imports of generic types) route through. Next pick is
  **union constructor application** through `freshen_type_scheme`
  (`typecheck_call.rs` ctor lookup path); see `doc/PLAN_GU.md` §6
  GU-S3 for the deliverable list.

  An adjacent existing-behaviour quirk surfaced during Pass 2: when
  `clause_name` is already qualified (`Eff.op`),
  `instantiate_handler_clause_signature` will perform an
  `Eff.Eff.op` lookup. Not a D-6 regression; flagged here so a
  later GU-S3 / resume-typing sub-task can address it deliberately
  rather than as a drive-by.

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
  `freshen_type_scheme` extraction is the only completed sub-task;
  the rest of GU-S3 is still ahead. Route every union constructor
  application, constructor-pattern type-check, generic-record
  constructor application, and generic-record field access through
  the shared helper. Constructor-name ambiguity resolution
  (qualified > scrutinee-pinned > local-shadows-imported >
  ambiguity diagnostic) and cross-module imports of generic unions
  also land in GU-S3. See `doc/PLAN_GU.md` §6 GU-S3 for the
  deliverable list.

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
