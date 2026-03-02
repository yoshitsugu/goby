# Goby Standard Library Foundation Plan

Status: Draft (execution plan)
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
- Preserve existing import diagnostics quality.
- Add regression coverage for file-based stdlib import behavior.

Out of scope (later phases):

- Full runtime replacement of built-in operations with stdlib Goby code.
- Package manager / remote dependency resolution.
- Cross-repo stdlib distribution.

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

Notes:

- Initial bodies may be minimal; type signatures are the critical bridge for import/typecheck.
- Keep runtime behavior unchanged unless a function is explicitly migrated.

Initial `goby/stdio` API contract (minimum):

- `print : String -> Unit can Print` (or equivalent effect-aware signature accepted by Goby).

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
4. After parity and adoption, reduce direct compiler/runtime special-casing.
5. Decide policy: retire bare builtin `print` or keep as permanent compatibility alias.

## 7. Test Strategy

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

### Integration/Regression

- Existing `examples/import.gb` still typechecks and runs.
- Existing import-related tests remain green.
- Add stdio regression:
  - `import goby/stdio ( print )` typechecks,
  - bare `print` compatibility path remains functional until migration completion.

### Quality Gates

- `cargo check`
- `cargo test`
- `cargo clippy -- -D warnings`

## 8. Migration Guardrails

- Do not break existing built-in module behavior during transition.
- Keep fallback path until file-based stdlib parity is confirmed.
- Avoid widening scope into package/dependency resolution.
- Keep `README.md` user-facing and high-level; put implementation details in `doc/`.

## 9. Risks and Mitigations

Risk: import behavior drift between file-based and built-in fallback.
Mitigation: table-driven parity tests for shared modules/symbols.

Risk: ambiguous symbol diagnostics regress during resolver integration.
Mitigation: retain current global symbol insertion and ambiguity logic.

Risk: runtime/typecheck mismatch while stdlib is type-only.
Mitigation: keep runtime built-ins as source of truth until explicit runtime migration.

## 10. Execution Checkpoints

Checkpoint 1:

- Resolver module added, tests for successful/failed module resolution.

Checkpoint 2:

- Typechecker imports use resolver (with fallback), existing tests green.

Checkpoint 3:

- `stdlib/goby/*.gb` seed modules committed, `examples/import.gb` regression green.
- `stdlib/goby/stdio.gb` added and covered by import/typecheck regression.

Checkpoint 4:

- CLI stdlib-root wiring complete and documented.

Checkpoint 5:

- Diagnostics hardening complete; plan marked ready for runtime migration handoff.

Checkpoint 6:

- `print` migration track validated:
  - `goby/stdio.print` path is available and tested,
  - compatibility bridge behavior is documented in `doc/STATE.md`.

## 11. Definition of Done

- File-based stdlib import resolution for `goby/...` is implemented and tested.
- Legacy fallback remains available and verified during transition.
- Existing import examples and tests pass unchanged.
- `goby/stdio` is available as stdlib module, and `print` migration status is explicitly tracked.
- Progress and follow-up work are tracked in `doc/STATE.md`.
