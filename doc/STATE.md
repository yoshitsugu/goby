# Goby Project State Snapshot

Last updated: 2026-05-10 (post bug-fix interlude entry #3 partial close).

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

**All-green (post bug-fix interlude #3 partial close, 2026-05-10)**:

- `cargo test -p goby-core --lib`: 942 passed / 2 ignored.
- `cargo nextest run -p goby-wasm -E 'not test(fold_m5_string_accumulator)'`:
  872 passed / 13 skipped (up from 12 thanks to the new
  `hof_callback_int_parse_invalid_integer_executes_via_compiled_wasm`
  ignored regression that pins the codegen-side follow-up tracked
  under BUGS.md 2026-05-10).
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

- **Bug-fix interlude before GU-S3.** Track GU is paused until
  every open BUGS.md entry is fixed and covered by a regression
  test, so a failure spotted during GU-S3/S4 can be attributed to
  the new code rather than to pre-existing layer noise. Each fix
  ships with a regression test (unit, integration, or fixture as
  appropriate); the interlude is "done" only when BUGS.md's open
  list is empty and the regressions are pinned. Status:
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
     range) and the `goby/int.parse` stdlib paragraph. The HOF
     repro now compiles past `encode_int` but trips a separate
     `unknown local 'invalid_integer'` HOF-callback effect-handler
     scoping defect, captured as the new BUGS.md 2026-05-10 entry.
  3. **2026-05-10 HOF-callback `int.parse` `unknown local
     'invalid_integer'`** — **PARTIAL: resolver-side fix landed
     2026-05-10**. Two changes: (a) `goby-wasm` stdlib decl loading
     now resolves each decl in its full owning-module context via
     `lower_stdlib_decl_via_module`, and (b) `goby-core::resolved`
     registers a module's own `effect_declarations` into
     `bare_effect_ops` / `qualified_effect_ops`. Together these
     turn `invalid_integer value` inside stdlib `parse` into a
     proper `PerformEffect{StringParseError, invalid_integer, …}`
     instead of the `Call(Var("invalid_integer"), …)` that produced
     the original `unknown local` error. A side fix in
     `effect_handler_lowering.rs` pads `Unit -> _` effect calls
     whose ir-level args were stripped of Unit so handler clauses
     keep their declared arity. Codegen-side follow-up (handler
     scope propagation across HOF callee boundary into stdlib
     decls) remains open and is tracked as the BUGS.md 2026-05-10
     entry's "codegen-side gap" with the staged decl-clone pass
     described there. The HOF regression is pinned as
     `hof_callback_int_parse_invalid_integer_executes_via_compiled_wasm`
     with `#[ignore]`; the acceptance signal is removing that
     `#[ignore]`.
  4. **2026-05-08 `AllocFloatBox` / `AllocMutableCell` shared
     refactor** — open. Wasm allocator refactor, last in the
     interlude.

### Handoff for entry #3 codegen-side follow-up (2026-05-10)

This subsection is a detailed brief for whoever picks up the
codegen-side gap of BUGS.md 2026-05-10. It records the failing
shape, the hypotheses already explored, what was tried and *why
it did not land*, and the open design questions. The current
attempt was paused after wasm validation could not be passed; a
fresh investigation is welcome to either fix the staged
decl-clone approach or replace it with a different pipeline
shape.

**Symptom that remains red.**
`crates/goby-wasm/src/runtime_output_tests.rs::hof_callback_int_parse_invalid_integer_executes_via_compiled_wasm`
is `#[ignore]`d. The reproducing program is:

```goby
import goby/list (map, push)
import goby/int as int
import goby/stdio

to_i : String -> Int
to_i d =
  with
    invalid_integer i -> resume -1
  in
    int.parse d

main : Unit -> Unit can Print
main =
  xs = push (push [] "1") "x"
  ys = map xs to_i
  println (int.to_string ys[0])
  println (int.to_string ys[1])
```

Expected output: `1\n-1\n`. The handler should catch
`StringParseError.invalid_integer` and resume with `-1` for the
non-numeric input.

**Why direct call works but HOF call does not.**
`println (int.to_string (to_i "42"))` (no list / no callback)
already passes today. That test
(`stdlib_int_parse_direct_call_executes_via_compiled_wasm`) is
fully evaluated by the static-output / native-fallback path —
the emitted wasm is ~136 bytes (literally `println "42"`) and
never executes `int.parse`'s body at all. As soon as `to_i` is
passed as a callback to `list.map`, the program no longer fits
the static-output gate (it has a value-dependent control flow)
and is forced through the general-lower path, where the gap
shows up.

**Where the resolver-side fix already landed.**
After commit `9e70286` (this commit):
- `goby_core::resolved::ResolverMetadata::collect` registers a
  module's own `effect_declarations` into `bare_effect_ops` /
  `qualified_effect_ops`.
- `gen_lower/mod.rs::lower_stdlib_decl_via_module` resolves
  each stdlib decl in its full owning-module context, so
  `int.parse`'s body lowers `invalid_integer value` as
  `PerformEffect{StringParseError, invalid_integer, [Var("value")]}`
  rather than `Call(Var("invalid_integer"), [Var("value")])`.

  This means the resolver-side gap is closed. Reproducing the
  bug now hits the codegen-side gap instead of the wasm-emit
  "unknown local" message. The previous "unknown local
  'invalid_integer'" is no longer the failure shape; it is
  superseded by the wasm-validation issue described below.

**Pipeline view at the point of the remaining gap.**
1. `ir_lower::lower_module(user_module)` →
   `IrModule { decls: [main, to_i] }`. `to_i.body` is
   `Seq { stmts: [Dup{Var(d)}], tail: WithHandler { Handle{[invalid_integer i -> Resume(IntLit(-1))]}, body: Call(GlobalRef{int, parse}, [Var(d)]) } }`.
2. `lower_stdlib_decl_via_module` populates the perceus input
   with stdlib decls including `parse`. `parse.body` carries
   the resolved `PerformEffect{StringParseError, invalid_integer, [Var(value)]}`
   sites at three tail-position branches plus its own inner
   `WithHandler { Handle{[yield grapheme _ -> ...]}, body: __goby_string_each_grapheme value }`
   block.
3. Perceus runs over the merged module and inserts Drop / Dup
   per decl independently.
4. Per-decl `effect_handler_lowering::lower_safe_handlers_in_comp`
   rewrites `WithHandler / PerformEffect / Resume` away, but it
   only sees clauses *textually inside* the body it is rewriting.
   `to_i`'s body has the `with invalid_integer ...` wrapper but
   no `PerformEffect{invalid_integer}` inside (the matching
   PerformEffect lives in `parse`'s body). `parse`'s body is
   rewritten in isolation, so its three
   `PerformEffect{invalid_integer}` sites have no active scope
   and stay as `PerformEffect`. They then reach
   `lower_comp_inner::CompExpr::PerformEffect` which only handles
   the builtin Print / Read effects via `backend_effect_op` and
   surfaces `UnsupportedForm`.

The execution-plan layer wraps that into the user-facing
"main body uses one-shot tail-resumptive effect handlers, but
the current direct-call lowering path is not implemented yet"
diagnostic; do not be misled by the wording — the actual
failure is the unhandled non-builtin `PerformEffect` reaching
wasm emit.

**Approach attempted and paused: pre-Perceus decl-clone +
clause substitution.** A standalone module
`crates/goby-wasm/src/gen_lower/handler_specialization.rs`
sketched (and removed in `9e70286` because it could not be
landed) an `IrModule -> IrModule` pass that:
1. Walks each user decl body tracking active handler scopes.
2. When a `Call(GlobalRef/Var(target_name), args)` sits under
   at least one scope and `decl_map[target_name].body` contains
   a `PerformEffect{eff, op, ..}` whose `(eff, op_short)`
   matches an active clause, it generates a fresh
   `target__spN` decl whose body is the target's body with
   each matching `PerformEffect` rewritten as
   `Let { p0_renamed = a0; ...; <clause body w/ Resume(e) → Value(e)> }`
   (alpha-renaming clause params with a unique suffix to avoid
   collision with the target's binders).
3. The original Call site is rewritten to
   `Call(Var("target__spN"), args)`.
4. The cloned decl is registered in `known_decls` and lowered
   like any other aux decl (`source_decl_lookup_name` strips the
   `__sp` suffix to find the base for `type_annotation` lookup).
5. Both `lower_aux_decl`'s int-fold / int-scan / list-builder
   matchers were guarded with a `body_had_handler =
   has_handler_rewrite_entrypoints(body)` flag so they skip the
   specialty patterns when the body went through handler
   rewriting (they were spuriously matching the inlined parse
   shape and producing a `loop i64` wrapper with mismatched
   stack effects).

Three unit tests passed inside this module:
- `emits_specialized_clone_when_call_under_handler_performs_active_op`
- `does_not_specialize_when_call_is_outside_active_handler`
- `does_not_specialize_when_op_does_not_match_clause`

**Why it did not land — wasm validation failure modes seen in order:**
1. *Initial wiring*: regression now compiled but
   `wasmtime --invoke main` reported
   `Invalid input WebAssembly code at offset 2061: type
   mismatch: values remaining on stack at end of block`
   inside `function[10] <to_i>`. This was traced to the
   inlined-into-caller variant where `to_i` had the entire
   parse body inside its `WithHandler`; the BinOp::Eq emit's
   nested `if (empty)` blocks plus nested mutable-state
   updates produced a stack shape that the validator
   rejected.
2. *After switching to the decl-clone variant*: the failure
   moved to `function[11] <parse__sp0>` at a similar offset,
   showing the same kind of stack-shape mismatch. Inspecting
   the wasm with `wasm-objdump` showed an `if (empty)` from
   the BinOp::Eq emit at offset `0x710` whose else branch
   left an i64 on the stack — but this is the *standard*
   BinOp::Eq emit shape that other Eq-bearing decls use
   without trouble, so the mismatch is more likely a
   side-effect of the surrounding control-flow structure
   than a bug in BinOp::Eq itself.
3. *Drop-fix attempt*: when the clause body does not
   reference its param (e.g. `invalid_integer i -> resume -1`
   with `i` unused), the substituted `Let { i__spN = arg;
   <body w/o ref to i__spN> }` leaks the arg's refcount
   because Perceus already accounted for the original
   `PerformEffect{eff, op, [arg]}` as if `arg` were
   consumed. Adding a `Seq { Drop(i__spN); <body> }` wrapper
   inside `substitute_perform_effects` made the unit tests
   pass but did not change the wasm-validation outcome.

**Hypotheses worth probing, ordered by promise:**
- The decl-clone substitutes `Resume(e)` with `Value(e)` on
  the assumption that the substituted position is a one-shot
  tail-resumptive call. Inside `parse` that is true at every
  matching site (the three `invalid_integer value` calls are
  in tail position of nested `if`/`if`/`if` chains), but
  Perceus then runs over the cloned decl and may insert
  Drops based on tail-position assumptions that no longer
  hold — for instance, `value__sp0` / `value` is used in the
  tail by some branches and dropped in others, and the
  substitution itself does not contribute to the live-set in
  the same way as the original `PerformEffect`. A targeted
  experiment would be: dump the post-Perceus IR of
  `parse__sp0` and compare branch-by-branch live sets
  against pre-substitution `parse`'s IR.
- The `if i64` blocks at the substitution sites might need
  explicit tail-position normalisation. After substitution
  the if-arms that previously produced "no value" (because
  the PerformEffect transferred control) now produce a
  concrete i64. Whether Perceus's branch-balancing pass
  sees that correctly is unverified.
- Handler-rewrite-after-Perceus may be the wrong order
  altogether for this case. An alternative shape is
  *decl-clone before Perceus, but also bypass per-decl
  handler rewriting on the cloned body* (since substitution
  already fully eliminates `PerformEffect` for the matching
  ops, the remaining inner WithHandler in the clone — for
  yield in parse's case — is the only handler structure
  left and is already handled correctly per decl).
- The third option, taken last, is to lift handler rewriting
  to run **after** Perceus on the user-decl side and run
  Perceus a second time over user decls only after rewrite.
  That is more invasive but isolates the ownership-graph
  reasoning to one fully-resolved shape.

**Files touched by the staged attempt (not in current tree).**
The decl-clone module and the `body_had_handler` skip in
`lower_aux_decl` were removed before the resolver-side fix
was committed. The pre-fix
`crates/goby-wasm/src/gen_lower/handler_specialization.rs` is
recoverable from this conversation's history; reproducing it
from scratch is a small task (≈700 lines including unit
tests). The expected wiring point in
`gen_lower/mod.rs::lower_module_to_instrs` is between
`perceus_input` population and
`goby_core::perceus::run_perceus_passes(&perceus_input)`.

**What "done" looks like.**
- `hof_callback_int_parse_invalid_integer_executes_via_compiled_wasm`
  is no longer `#[ignore]`d and passes.
- `cargo nextest run -p goby-wasm` stays green.
- `cargo test -p goby-core --lib` stays green.
- The BUGS.md 2026-05-10 entry is moved into the *Resolved
  bugs* section with the codegen-side mechanism described.
- This handoff subsection in `doc/STATE.md` is removed.

- **Track GU resumes at GU-S3 once the bug-fix interlude is clear.**
  The GU-S3 plan is unchanged: extract `freshen_type_scheme` (with
  the existing effect-side call site migrated first), then route
  every constructor application, constructor-pattern type-check,
  and generic-record field access through the same helper.
  Constructor-name ambiguity resolution (qualified > scrutinee-
  pinned > local-shadows-imported > ambiguity diagnostic) and
  cross-module imports of generic unions also land in GU-S3. See
  `doc/PLAN_GU.md` §6 GU-S3 for the deliverable list.

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
