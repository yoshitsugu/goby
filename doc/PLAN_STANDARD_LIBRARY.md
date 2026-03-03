# Goby Standard Library Foundation Plan

Status: Step 0-12 + ExtraStep A/B complete
Owner: Goby core/tooling track
Last updated: 2026-03-03

## 1. Purpose

This document defines the implementation plan for establishing a file-based
standard library foundation in Goby, while preserving current behavior and
developer workflows.

The immediate target is import/typecheck infrastructure, not full runtime
replacement.

## 2. Scope

In scope:

- Introduce a canonical in-repo stdlib source layout.
- Resolve `import goby/...` from `.gb` source files.
- Keep current built-in import behavior as compatibility fallback during migration.
- Add a stdlib-only `@embed` annotation for effect embedding declarations.
- Preserve existing import diagnostics quality.
- Add regression coverage for file-based stdlib import behavior.

Out of scope (later phases):

- Full runtime replacement of built-in operations with stdlib Goby code.
- Package manager / remote dependency resolution.
- Cross-repo stdlib distribution.
- General `@embed` support for user code or third-party libraries.

## 3. Current Baseline

Today, import resolution is built-in and hardcoded in typechecker logic:

- `validate_imports` and `inject_imported_symbols` use `builtin_module_exports`.
- Supported modules are fixed (`goby/string`, `goby/list`, `goby/env`).
- `print` is currently treated as a language/runtime builtin rather than a stdlib module API.
- Runtime behavior for some stdlib operations is still hardcoded in `goby-wasm`.

Implication:

- Import behavior works, but no file-based stdlib source pipeline exists yet.
- Language-level stdlib evolution is tightly coupled to compiler code edits.

## 4. Target Architecture

Introduce a dedicated stdlib resolver layer in `goby-core`:

- Input: module path (`goby/string`, etc.) + stdlib root directory.
- Resolution: module path -> source file path (`stdlib/goby/string.gb`).
- Parse/type extraction: read exported symbols and their type signatures.
- Output: resolved module export map for typechecker import validation/injection.

Compatibility rule during migration:

- Try file-based stdlib resolution first.
- If unavailable, fall back to legacy built-in export table.
- Keep a temporary builtin bridge for `print` until `goby/stdio` runtime parity is complete.

`@embed` rule:

- `@embed` is accepted only in stdlib sources under `stdlib/`.
- `@embed` is rejected in user modules and non-stdlib libraries.
- Embedded effects are treated as runtime-provided capabilities that do not need
  user-space effect declaration boilerplate in `main`.

## 5. Canonical Layout

Planned repository layout:

- `stdlib/goby/string.gb`
- `stdlib/goby/list.gb`
- `stdlib/goby/env.gb`
- `stdlib/goby/stdio.gb`

Path mapping rule:

- `import goby/<name>` resolves to `<stdlib_root>/goby/<name>.gb`.

## 6. Phased Implementation Plan

### Phase A: Resolver Skeleton (Type Information Only)

Deliverables:

- New module: `crates/goby-core/src/stdlib.rs`.
- Resolver API (tentative):
  - `StdlibResolver::new(root: PathBuf)`
  - `resolve_module(module_path: &str) -> Result<ResolvedStdlibModule, StdlibResolveError>`
- `ResolvedStdlibModule` includes:
  - normalized module path,
  - exported symbol -> type map (typechecker-facing representation).

Behavior:

- Parse resolved `.gb` module.
- Collect top-level declarations with type annotations as exported symbols.
- Ignore runtime body semantics in this phase.

Diagnostics:

- `ModuleNotFound`
- `ReadFailed`
- `ParseFailed`
- `DuplicateExport`
- `ExportTypeMissing` (if stricter export policy is chosen)

### Phase B: Typechecker Integration

Deliverables:

- Route `validate_imports` through resolver-backed module export lookup.
- Route `inject_imported_symbols` through the same resolver-backed source.
- Keep `builtin_module_exports` as fallback path for compatibility.

Rules:

- `ImportKind::Plain`, `Alias`, `Selective` behavior remains unchanged.
- Unknown module/symbol diagnostics remain stable and explicit.
- Existing collision/ambiguity handling via `GlobalBinding::Ambiguous` remains unchanged.

### Phase C: Seed File-Based Stdlib Modules

Deliverables:

- Add initial stdlib files under `stdlib/goby/`.
- Include stable export signatures matching current built-in module contracts.
- Add `goby/stdio` module that exposes standard I/O functions (including `print`).
- Add stdlib examples of embedded effect declarations, e.g. `@embed Print`.

Notes:

- Initial bodies may be minimal; type signatures are the critical bridge for import/typecheck.
- Keep runtime behavior unchanged unless a function is explicitly migrated.

Initial `goby/stdio` API contract (minimum):

- `print : String -> Unit can Print` (or equivalent effect-aware signature accepted by Goby).

Initial embedded declaration model for stdio:

- Stdlib module may include embedded effect declarations such as:
  - `effect Print`
  - `@embed Print`
  - embedded operation signatures consumed by `goby/stdio.print`.
- Goal: keep `Print` capability wiring inside stdlib/runtime bridge instead of
  requiring user modules to declare effect internals.

Deferred additions:

- `println`, `eprint`, `read_line`.

### Phase D: CLI Wiring for Stdlib Root

Deliverables:

- Pass stdlib root into typecheck/import resolution flow.
- Default root: repository-local `stdlib/`.

Optional extension:

- Environment override `GOBY_STDLIB_ROOT`.
- Future CLI flag `--stdlib-root`.

### Phase E: Diagnostics and UX Hardening

Deliverables:

- Improve import error messages with attempted path details.
- Keep machine-readable stability for editor tooling.
- Ensure ambiguous import diagnostics identify competing sources clearly.
- Add explicit diagnostics for invalid `@embed` usage:
  - used outside stdlib root,
  - malformed embed target,
  - duplicate embedded effect names.

### Phase F: Runtime Migration Preparation (Separate Execution Track)

Deliverables:

- Inventory hardcoded runtime built-ins in `goby-wasm`.
- Define per-function migration checklist from hardcoded runtime path to stdlib-backed path.
- Define explicit migration path from builtin `print` to `goby/stdio.print`.

Not executed in this plan unless explicitly started.

Planned `print` migration sequence:

1. Keep builtin `print` as runtime primitive while introducing `goby/stdio.print`.
2. Make `goby/stdio` import path first-class in docs/examples.
3. Keep compatibility so legacy bare `print` still works during migration.
4. Introduce stdlib-only `@embed Print` declaration path and wire runtime bridge through it.
5. After parity and adoption, reduce direct compiler/runtime special-casing.
6. Decide policy: retire bare builtin `print` or keep as permanent compatibility alias.

## 7. Incremental Step-by-Step Execution Plan

This section is the operational sequence. Each step is intentionally small and
should be completed (code + tests + docs) before moving to the next.

Step 0: Baseline lock

- Add/confirm baseline tests for current import behavior and bare `print`.
- No behavior change.
- Exit criteria: `cargo check/test/clippy` green on unchanged behavior.

Step 1: Add stdlib resolver module shell

- Add `crates/goby-core/src/stdlib.rs` with data types and no integration.
- Add unit tests for path mapping only.
- Exit criteria: resolver compiles, no call sites changed.

Step 2: Implement file resolution + parse path

- Implement module-path -> file-path resolution and file loading.
- Parse resolved `.gb` and return declaration/type metadata.
- Exit criteria: resolver tests cover success + module-not-found + parse-failed.

Step 3: Export map extraction

- Build symbol->type export map from parsed stdlib module.
- Add duplicate-export and missing-type tests.
- Exit criteria: resolver returns stable export maps for fixture modules.

Step 4: Integrate import validation (read path only)

- Switch `validate_imports` to use resolver first, then builtin fallback.
- Keep symbol injection path unchanged in this step.
- Exit criteria: unknown-module/symbol diagnostics remain stable.

Step 5: Integrate symbol injection

- Switch `inject_imported_symbols` to resolver-first, builtin fallback.
- Keep ambiguity/collision behavior unchanged.
- Exit criteria: existing import collision tests remain green.

Step 6: Seed stdlib files (`string/list/env`)

- Add initial files under `stdlib/goby/` with current signatures.
- No runtime migration yet.
- Exit criteria: imports resolve from files in normal repo layout.

Step 7: Add `stdlib/goby/stdio.gb`

- Add `print` signature in `goby/stdio`.
- Add typecheck test for `import goby/stdio ( print )`.
- Exit criteria: stdio import path is usable in typechecker.

Step 8: Introduce stdlib-only `@embed` parsing gate

- Add parser/typechecker support for `@embed` declarations.
- Enforce path restriction: allowed only when module source is under stdlib root.
- Exit criteria: stdlib fixture accepted; user-module fixture rejected.

Step 9: `@embed Print` stdio bridge metadata

- Wire embedded `Print` declaration metadata into stdio planning path.
- Keep bare builtin `print` behavior intact.
- Exit criteria: no runtime regression; bridge metadata visible to compiler stages.

Step 10: CLI stdlib root wiring

- Pass stdlib root to resolver/typecheck flow.
- Default to repo `stdlib/`; optional env override.
- Exit criteria: CLI tests cover default and invalid-root error paths.

Step 11: Diagnostic hardening

- Improve resolver/import/embed diagnostics (attempted path, context).
- Exit criteria: snapshot tests for key diagnostics.

Step 12: print migration handoff checkpoint

- Document active behavior:
  - `goby/stdio.print` available,
  - bare `print` compatibility preserved,
  - `@embed` restricted to stdlib.
- Exit criteria: checkpoint recorded in `doc/STATE.md`, next runtime step unblocked.
## 8. Test Strategy

### Unit Tests (`goby-core`)

- Resolver:
  - resolves existing stdlib module by path,
  - reports module-not-found cleanly,
  - reports duplicate exports deterministically.
- Typecheck import integration:
  - `import goby/string` works from file-based resolver,
  - `import goby/stdio` resolves `print` symbol with expected type,
  - selective import unknown symbol fails with expected message,
  - collision ambiguity behavior remains unchanged.
  - `@embed` declaration is rejected outside stdlib modules.

### Integration/Regression

- Existing `examples/import.gb` still typechecks and runs.
- Existing import-related tests remain green.
- Add stdio regression:
  - `import goby/stdio ( print )` typechecks,
  - bare `print` compatibility path remains functional until migration completion.
  - stdlib `@embed Print` fixture is accepted only from stdlib path.

### Quality Gates

- `cargo check`
- `cargo test`
- `cargo clippy -- -D warnings`

## 9. Migration Guardrails

- Do not break existing built-in module behavior during transition.
- Keep fallback path until file-based stdlib parity is confirmed.
- Avoid widening scope into package/dependency resolution.
- Keep `README.md` user-facing and high-level; put implementation details in `doc/`.

## 10. Risks and Mitigations

Risk: import behavior drift between file-based and built-in fallback.
Mitigation: table-driven parity tests for shared modules/symbols.

Risk: ambiguous symbol diagnostics regress during resolver integration.
Mitigation: retain current global symbol insertion and ambiguity logic.

Risk: runtime/typecheck mismatch while stdlib is type-only.
Mitigation: keep runtime built-ins as source of truth until explicit runtime migration.

## 11. Execution Checkpoints

Checkpoint 1:

- Resolver module added, tests for successful/failed module resolution.

Checkpoint 2:

- Typechecker imports use resolver (with fallback), existing tests green.

Checkpoint 3:

- `stdlib/goby/*.gb` seed modules committed, `examples/import.gb` regression green.
- `stdlib/goby/stdio.gb` added and covered by import/typecheck regression.
- `@embed Print` declaration sample added under stdlib and validated.

Checkpoint 4:

- CLI stdlib-root wiring complete and documented.

Checkpoint 5:

- Diagnostics hardening complete; plan marked ready for runtime migration handoff.

Checkpoint 6:

- `print` migration track validated:
  - `goby/stdio.print` path is available and tested,
  - compatibility bridge behavior is documented in `doc/STATE.md`.
  - stdlib-only `@embed` restriction is enforced by parser/typechecker diagnostics.

## 12. Definition of Done

- File-based stdlib import resolution for `goby/...` is implemented and tested.
- Legacy fallback remains available and verified during transition.
- Existing import examples and tests pass unchanged.
- `goby/stdio` is available as stdlib module, and `print` migration status is explicitly tracked.
- `@embed` is supported for stdlib modules only and rejected elsewhere.
- Progress and follow-up work are tracked in `doc/STATE.md`.

## ExtraStep Candidates (Post Step12)

This section tracks follow-up adjustments discovered after Step12 completion.
These items are intentionally staged as additional work, not retroactive changes
to Step0-12.

### ExtraStep A: stdio embed model alignment (Print default handler embedding)

Problem statement:

- Current `stdlib/goby/stdio.gb` uses:
  - `@embed effect Print`
  - `print : String -> Unit can Print`
  - body placeholder `value |> print`
- Intended model is:
  - explicit effect declaration in stdio module,
  - embedding declaration for the effect's default runtime handler.

Target shape (illustrative):

```gb
effect Print
  print : String -> Unit can Print

@embed Print
```

Follow-up tasks:

1. Update `@embed` syntax model from `@embed effect <Name>` to `@embed <Name>`
   (or add compatibility support for both during transition).
2. Update parser/typechecker diagnostics accordingly.
3. Add validation rule:
   - `@embed X` requires an in-module `effect X` declaration.
4. Update `stdlib/goby/stdio.gb` to the effect+embed form above.
5. Keep resolver metadata (`embedded_effects`) behavior consistent.
6. Add regression coverage for:
   - parse acceptance of `@embed Print`,
   - rejection when embedded effect is not declared in module,
   - `goby/stdio` import path behavior unchanged.

Decision (2026-03-03):

- Canonical syntax is `@embed <EffectName>`.
- Legacy `@embed effect <EffectName>` remains accepted temporarily for compatibility.
- Compatibility-removal gate:
  - remove legacy `@embed effect <EffectName>` acceptance only in a dedicated
    follow-up change after stdlib/module fixtures are fully migrated.

Execution checklist (incremental):

- [x] A1. Baseline lock: current gates pass before edits (`cargo check`, `cargo test -p goby-core`).
- [x] A2. Spec lock in this plan: decide compatibility policy for legacy `@embed effect <Name>`.
- [x] A3. Parser accepts canonical `@embed <Name>`.
- [x] A4. Parser compatibility behavior implemented per A2 (accept+deprecate or reject with clear error).
- [x] A5. AST/internal representation normalized so downstream stages are syntax-agnostic.
- [x] A6. Typechecker rule added: `@embed X` requires in-module `effect X`.
- [x] A7. Path-gate integration verified:
  - stdlib path + declared effect => accept,
  - non-stdlib path => reject,
  - stdlib path + missing effect => reject.
- [x] A8. Resolver `embedded_effects` extraction remains stable across accepted embed syntax.
- [x] A9. `stdlib/goby/stdio.gb` migrated to canonical model (`effect Print` + `@embed Print`).
- [x] A10. Diagnostics hardened for malformed embed, unknown embedded effect, and duplicates.
- [x] A11. Docs synced (`doc/PLAN.md`, `doc/STATE.md`) with landed behavior.
- [x] A12. Final quality gates pass (`cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings`).

### ExtraStep B: stdlib intrinsic bridge naming (`__goby_<module>_<function>`)

Problem statement:

- Some stdlib APIs (for example string length) are difficult to implement
  faithfully in pure Goby at current language/runtime capability level.
- Current placeholder implementations (for example `length value = 0`) should
  be replaced with explicit runtime bridge calls.

Proposed convention:

- Introduce reserved intrinsic names:
  - `__goby_<module>_<function>`
  - example: `__goby_string_length : String -> Int`
- Stdlib modules can call these intrinsics as a temporary runtime bridge.
- User modules should not define or call `__goby_*` names directly
  (restriction policy to be enforced in parser/typechecker/lints as follow-up).

Illustrative stdlib usage:

```gb
length : String -> Int
length value = __goby_string_length value
```

Staging guidance:

1. Add only minimal intrinsics needed for parity-critical stdlib APIs.
2. Keep APIs that can be expressed with future language features
   (for example iteration + algebraic effects with resume) on a path toward
   pure Goby stdlib implementations.
3. Track each intrinsic as technical debt with an explicit retirement target.

Near-term candidate split:

- Intrinsic-backed first:
  - `string.length` (runtime primitive likely needed short-term).
  - `env.fetch_env_var` (host environment access requires runtime boundary).
- Deferred for language-feature implementation:
  - `string.split` (possible pure stdlib implementation once iteration/resume
    support is available).

Additional intrinsic example:

```gb
fetch_env_var : String -> String
fetch_env_var name = __goby_env_fetch_env_var name
```

Decision (2026-03-03):

- First intrinsic set is locked to:
  - `__goby_string_length : String -> Int`
  - `__goby_env_fetch_env_var : String -> String`
- Restriction policy (current phase):
  - user modules are hard-rejected for `__goby_*` declaration/call usage,
  - stdlib modules may call only the known intrinsic set above,
  - unknown `__goby_*` names in stdlib are rejected with explicit diagnostics.

Execution checklist (incremental):

- [x] B1. Baseline lock: current gates pass before edits (`cargo check`, `cargo test`).
- [x] B2. Spec lock in this plan:
  - confirm first intrinsic set (`__goby_string_length`, `__goby_env_fetch_env_var`),
  - lock temporary user restriction policy scope (diagnostic-only vs hard reject).
- [x] B3. Reserve intrinsic namespace policy:
  - parser/typechecker/runtime convention for names matching `__goby_*`,
  - clarify whether user definitions/calls are rejected now or warned/deferred.
- [x] B4. Typechecker symbol modeling:
  - add intrinsic function types to type environment when referenced by stdlib modules,
  - keep imported API surface unchanged (`goby/string.length`, `goby/env.fetch_env_var`).
- [x] B5. Runtime bridge implementation:
  - map `__goby_string_length` to runtime string-length primitive,
  - map `__goby_env_fetch_env_var` to existing host env boundary behavior.
- [x] B6. Stdlib module migration:
  - update `stdlib/goby/string.gb` `length` to call `__goby_string_length`,
  - update `stdlib/goby/env.gb` `fetch_env_var` to call `__goby_env_fetch_env_var`.
- [x] B7. Resolver/import parity checks:
  - ensure resolver-first import behavior still matches expected symbol/type contracts,
  - ensure builtin fallback behavior remains intact for missing stdlib files.
- [x] B8. Diagnostics hardening:
  - clear runtime/typecheck errors for unknown intrinsic usage,
  - explicit messages for disallowed user-space `__goby_*` usage (if hard reject is chosen).
- [x] B9. Regression tests (`goby-core` + `goby-wasm` + CLI integration where relevant):
  - positive tests for `string.length` and `env.fetch_env_var` through stdlib entry points,
  - negative tests for malformed/unknown intrinsic calls,
  - compatibility tests for existing bare `print` and stdlib imports.
- [x] B10. Performance/behavior sanity:
  - verify no unintended path regression in native/fallback mode selection,
  - ensure runtime output parity for affected examples/tests.
- [x] B11. Docs synced (`doc/PLAN.md`, `doc/STATE.md`) with intrinsic bridge policy and limits.
- [x] B12. Final quality gates pass (`cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings`).

### ExtraStep C: `string.split` stdlib implementation via Iterator + grapheme intrinsic

Problem statement:

- `string.concat` was retired and string interpolation is now the canonical string
  composition mechanism.
- `string.split` is still runtime-builtin backed and should move toward stdlib-driven
  behavior.
- `Iterator` effect declarations are now available in stdlib and can be used as the
  stream-like execution model.

Decisions (2026-03-03):

- New intrinsic name:
  - `__goby_string_each_grapheme : String -> Unit can Iterator`
- Processing model:
  - stream-like internal execution using `Iterator.yield`,
  - public API remains `List String` for now.
- Implementation direction:
  - choose **B** strategy: add minimal list/string primitives needed to build
    `split` in stdlib (instead of keeping `string.split` as permanent builtin).

Required pre-lock decisions before code:

1. `split` compatibility semantics:
   - whether empty segments are preserved (recommended: preserve, Rust-compatible),
   - behavior for empty delimiter (recommended: grapheme-wise split).
2. Grapheme definition:
   - Unicode Extended Grapheme Cluster (recommended).
3. Minimal helper primitive set for stdlib implementation:
   - `__goby_string_eq : String -> String -> Bool`
   - `__goby_list_push_string : List String -> String -> List String`
   - keep intrinsic scope minimal and stdlib-only.

Execution checklist (incremental):

- [ ] C1. Spec lock in this plan:
  - lock split edge-case semantics (empty delimiter, consecutive delimiters, leading/trailing delimiters),
  - lock grapheme definition and test fixtures.
- [ ] C2. Typechecker intrinsic model expansion:
  - allow `__goby_string_each_grapheme`, `__goby_string_eq`, `__goby_list_push_string` in stdlib only,
  - keep user-space `__goby_*` hard rejection unchanged.
- [ ] C3. Runtime bridge implementation:
  - implement `__goby_string_each_grapheme` with iterator-effect dispatch,
  - implement `__goby_string_eq` and `__goby_list_push_string`.
- [ ] C4. Stdlib iterator/string implementation:
  - implement `split` in `stdlib/goby/string.gb` using Iterator-driven processing,
  - remove dependency on runtime builtin `string.split(...)`.
- [ ] C5. Runtime builtin retirement:
  - remove direct runtime handling path for `string.split` method call.
- [ ] C6. Regression coverage:
  - add edge-case tests for split semantics and Unicode grapheme behavior,
  - add import/example parity tests (including `examples/import.gb` behavior).
- [ ] C7. Docs sync:
  - update `doc/PLAN.md` and `doc/STATE.md` with final intrinsic set and split ownership.
- [ ] C8. Final quality gates:
  - `cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings`.
