# Goby Project State Snapshot

Last updated: 2026-05-09.

## Current Focus

**Track GU (generic user-defined types) — typecheck data layer complete (GU-S2c+S2d).**
GU-S2c (`resolved.rs`) was verified as a no-op: the resolved form
holds `CasePattern` as a struct field and never destructures
`TypeDeclaration::Union/Record`, so the new shapes flow through
unchanged. GU-S2d extended `RecordTypeInfo` with `type_params` and
`constructor`, introduced `UnionTypeInfo` / `UnionVariantInfo`,
added a `union_types` field on `TypeEnv` (keyed by **union type name**
to keep storage unambiguous when ctor names overlap) plus a
`TypeEnv::empty()` helper, and rewrote `inject_type_constructors`
and `validate_type_declarations` to walk the new shapes. Generic
constructors register their global symbols as `Ty::Unknown` so
unfreshened `Ty::Var(...)` templates cannot leak into unification
before GU-S3 lands `freshen_type_scheme`; non-generic unions and
records keep their previous direct `Ty::Con` / `Ty::Fun` signatures.
`cargo build -p goby-core` (lib + tests) is green except for one
remaining error in `ir_lower.rs:660` (`CasePattern::Ctor`
non-exhaustive), which is GU-S2f's responsibility. Next phase is
**GU-S2e: typecheck walkers** — extend the exhaustive `match` arms
over `CasePattern` and the record / ctor lookup paths in
`typecheck_validate.rs`, `typecheck_call.rs`, `typecheck_check.rs`,
`typecheck_branch.rs`, `typecheck_diag.rs` to recognise the new
`Ctor` variant and the extended `RecordTypeInfo` shape (data only;
no fresh-instantiation yet).

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

**Currently broken (intentional, GU-S2 window)**:

- `cargo build -p goby-core` (lib + tests) reports one remaining
  error: `ir_lower.rs:660` — `CasePattern::Ctor` non-exhaustive. This
  is GU-S2f (IR layer) territory and is repaired before GU-S2 closes.
  All other check commands (`-p goby-core --lib`, `-p goby-wasm`,
  `-p goby-lsp`, `--workspace`) remain blocked by this single
  compile error until GU-S2f lands.

Red / ignored:

- Pre-existing `#[ignore]`d perceus / compile_tests entries (see
  `doc/BUGS.md` for any per-case detail).
- `goby-wasm` lib test `tests::fold_m5_string_accumulator` is a known
  CPU-bound hang on this checkout; track it via `doc/BUGS.md` and skip
  it in nextest runs.

## Next Step

**Primary (active):**

- **Track GU (generic user-defined types) — GU-S2e next.** GU-D0 / GU-D1
  / GU-S1 / GU-S2a / GU-S2b / GU-S2c (no-op) / GU-S2d complete. The
  next step is GU-S2e: walker-side typecheck repair across
  `typecheck_validate.rs`, `typecheck_call.rs`, `typecheck_check.rs`,
  `typecheck_branch.rs`, `typecheck_diag.rs` — every exhaustive
  `match` over `CasePattern` recognises the new `Ctor` variant, and
  every record constructor / field-access call site reads from the
  extended `RecordTypeInfo` (treating `type_params` as empty for
  existing records, no freshening yet). Downstream IR layer
  (ir_lower.rs E0004) is still broken and is GU-S2f's responsibility.
  See `doc/PLAN_GU.md` §6 for the full phase list.

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
