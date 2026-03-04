# Goby `@embed` Migration Plan

Status: Planned
Owner: Goby core/tooling/runtime track
Last updated: 2026-03-04

## 1. Purpose

This document defines the concrete implementation procedure for the `@embed`
semantics update locked in `doc/PLAN.md` on 2026-03-04.

The update changes `@embed` from an effect-name marker into a stdlib-only
default-effect-handler declaration:

- canonical syntax: `@embed <EffectName> <HandlerName>`
- example: `@embed Print __goby_embeded_effect_stdout_handler`

## 2. Locked Requirements

The implementation must satisfy all of the following:

1. Legacy `@embed effect <EffectName>` is rejected by parser.
2. `@embed` is accepted only in stdlib sources.
3. `@embed` requires `effect <EffectName>` in the same module.
4. Handler name must match `__goby_embeded_effect_*`.
5. Declared embedded handler target must exist in known intrinsic set.
6. During `main` validation only, unresolved effects may be accepted if they
   have stdlib `@embed` default handlers.
7. Runtime dispatch precedence is preserved:
   explicit user handler (`with` / `with_handler`) wins over embedded default.

## 3. Non-Goals

- Generalizing `@embed` for user modules or third-party libraries.
- Adding multiple default handlers per effect.
- Reworking overall effect system semantics beyond this migration.
- Removing existing non-embed stdlib intrinsic behavior unrelated to this change.

## 4. Current Baseline (Before This Plan)

- Parser currently accepts both:
  - `@embed <EffectName>`
  - `@embed effect <EffectName>` (legacy compatibility)
- AST `EmbedDecl` stores only `effect_name`.
- Resolver/typechecker treat embedded metadata as effect-name list, not
  effect-to-handler mapping.
- `stdlib/goby/stdio.gb` uses `@embed Print`.
- Intrinsic namespace policy for `__goby_*` is already enforced, but embedded
  handler intrinsic target wiring is not complete under the new model.

## 5. Execution Plan

## Phase 0: Guardrails and Branch Hygiene

1. Create a small migration branch.
2. Run baseline checks before edits:
   - `cargo check`
   - `cargo test`
3. Record baseline failures (if any) and do not mix unrelated fixes into this plan.

Exit criteria:

- Clean baseline understanding and reproducible pre-change state.

## Phase 1: Parser + AST Contract Update

Files:

- `crates/goby-core/src/ast.rs`
- `crates/goby-core/src/parser.rs`
- parser tests in `crates/goby-core/src/parser.rs`

Steps:

1. Extend `EmbedDecl` to store both:
   - `effect_name: String`
   - `handler_name: String`
   - keep `line`.
2. Replace `parse_embed_line` contract:
   - parse exactly two identifiers after `@embed`.
   - return structured result `(effect_name, handler_name)`.
3. Remove legacy compatibility path for `@embed effect ...`.
4. Add parser diagnostics:
   - missing handler name
   - malformed effect name
   - malformed handler name
   - explicit legacy-form rejection message
5. Update parser tests:
   - positive: `@embed Print __goby_embeded_effect_stdout_handler`
   - negative: `@embed Print`
   - negative: `@embed effect Print`
   - negative: invalid handler identifier.

Exit criteria:

- New syntax parses.
- Legacy syntax fails with stable diagnostic.
- AST captures handler target.

## Phase 2: Resolver Metadata Migration

Files:

- `crates/goby-core/src/stdlib.rs`
- unit tests in `crates/goby-core/src/stdlib.rs`

Steps:

1. Replace `embedded_effects: Vec<String>` with structured metadata, for example:
   - `embedded_defaults: Vec<EmbeddedDefaultHandlerDecl>`
   - where each item has `effect_name` + `handler_name`.
2. Update embedded metadata collection and duplicate detection logic:
   - duplicate by `effect_name` remains rejected.
3. Keep resolver error surface stable where possible:
   - update error text only when necessary.
4. Update resolver tests to assert both effect and handler values.

Exit criteria:

- Resolver returns effect-to-handler embedding metadata.
- Duplicate behavior remains consistent.

## Phase 3: Typechecker Rule Update (`@embed` Validation)

Files:

- `crates/goby-core/src/typecheck.rs`
- related tests in same file

Steps:

1. Update embed validation logic for new AST shape.
2. Keep existing rules:
   - stdlib-only gate (context-aware)
   - duplicate embedded effect rejection
   - local `effect` declaration required
3. Add handler-name policy check:
   - must start with `__goby_embeded_effect_`.
4. Add embedded handler intrinsic existence check:
   - initial allowed set includes `__goby_embeded_effect_stdout_handler`.
5. Extend known intrinsic helper(s) so this embedded handler name is recognized.
6. Update tests:
   - accepts valid stdlib embed declaration with handler
   - rejects invalid handler namespace
   - rejects unknown embedded handler intrinsic
   - keeps current stdlib-path and missing-effect checks.

Exit criteria:

- All embed declarations are validated against new handler policy.
- Unknown or malformed handler targets fail early in typecheck.

## Phase 4: Main Effect Relaxation with Embedded Defaults

Files:

- `crates/goby-core/src/typecheck.rs`
- optional support plumbing in resolver access paths
- tests in `crates/goby-core/src/typecheck.rs`

Steps:

1. Build effective embedded-default map from:
   - local module embeds
   - imported stdlib embed metadata.
2. Apply relaxation only during `main` entry validation:
   - if `main` can-clause effect has default embedded handler, allow unresolved
     handler coverage at entry boundary.
3. Keep all non-`main` declarations unchanged:
   - unresolved effects without enclosing handlers remain errors.
4. Add tests:
   - `main : Unit -> Unit can Print` accepted with embedded default available
   - non-main unresolved effect still rejected
   - unresolved non-embedded effect on `main` still rejected.

Exit criteria:

- `main` receives scoped relaxation only for embedded-default effects.

## Phase 5: Runtime/Intrinsic Wiring

Files:

- `crates/goby-wasm/src/lib.rs`
- runtime tests in same file

Steps:

1. Add intrinsic implementation entry for:
   - `__goby_embeded_effect_stdout_handler`
2. Implement fallback behavior for `Print` when:
   - no explicit active handler resolves the operation, and
   - embedded default mapping provides stdout handler target.
3. Preserve precedence:
   - explicit `with` / `with_handler` dispatch first
   - embedded default fallback second.
4. Add runtime tests:
   - explicit handler overrides default
   - fallback default handles print when explicit handler is absent.

Exit criteria:

- Runtime behavior aligns with locked default-handler model.

## Phase 6: Stdlib Source and Docs Sync

Files:

- `stdlib/goby/stdio.gb`
- `examples/README.md` (if outdated syntax is shown)
- `doc/PLAN_STANDARD_LIBRARY.md` (if outdated syntax remains)

Steps:

1. Update stdio declaration:
   - from `@embed Print`
   - to `@embed Print __goby_embeded_effect_stdout_handler`
2. Remove or mark obsolete legacy syntax references.
3. Keep README high-level and avoid deep spec duplication.

Exit criteria:

- Repository examples/docs do not present removed embed syntax as valid.

## Phase 7: Validation and Release Checklist

Run at repository root:

1. `cargo fmt`
2. `cargo check`
3. `cargo test`
4. `cargo clippy -- -D warnings`

If failures occur:

1. Confirm whether failure is caused by this migration or pre-existing.
2. Fix migration regressions in the same branch.
3. Do not introduce unrelated behavioral changes.

Final checklist:

1. Parser rejects legacy `@embed effect ...`.
2. Typechecker enforces handler namespace + intrinsic existence.
3. `main` embed-default relaxation is scoped correctly.
4. Runtime fallback to embedded default handler works.
5. Explicit handler precedence is preserved.
6. Stdlib/docs/tests are synchronized.

## 6. Test Matrix

Parser:

- accept `@embed Print __goby_embeded_effect_stdout_handler`
- reject `@embed Print`
- reject `@embed effect Print`
- reject invalid handler token

Typechecker:

- reject `@embed` outside stdlib root (context-aware path)
- reject duplicate embedded effects
- reject missing local `effect` declaration
- reject non-`__goby_embeded_effect_*` handler
- reject unknown embedded handler intrinsic
- accept valid stdlib embed declaration

Main effect behavior:

- accept `main can Print` when stdlib embed default exists
- reject `main can <UnknownEffect>`
- reject non-main unresolved effect without handler

Runtime:

- explicit user handler dispatch still works
- embedded default stdout handler is used only when explicit one is absent

## 7. Risk Log

1. Metadata shape migration risk:
   - resolver/typechecker call sites may assume `Vec<String>`.
   - mitigation: migrate in one commit with compiler-guided refactor.
2. Runtime precedence regression risk:
   - fallback may accidentally bypass explicit handler.
   - mitigation: add direct precedence regression tests first.
3. Diagnostic drift risk:
   - tests may rely on exact message fragments.
   - mitigation: stabilize new messages and update tests intentionally.

## 8. Suggested Commit Slicing

1. Parser/AST migration + parser tests.
2. Resolver metadata migration + resolver tests.
3. Typechecker embed validation update + tests.
4. Main relaxation semantics + tests.
5. Runtime intrinsic wiring + runtime tests.
6. Stdlib/doc sync + full validation run.

This slicing keeps reviewable diffs small and isolates behavior changes.
