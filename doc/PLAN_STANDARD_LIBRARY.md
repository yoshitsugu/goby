# Goby Standard Library Remaining Work Plan

Status: active follow-up only
Owner: Goby core/runtime track
Last updated: 2026-03-24

## 1. Scope

Most of the original stdlib-foundation plan is already implemented. This
document now keeps only the remaining work that is still relevant for active
development.

This is a focused companion plan to `doc/PLAN.md`, not a separate roadmap.

Current remaining stdlib track:

- finish moving `goby/string.split` to stdlib-driven behavior,
- remove the remaining direct runtime builtin path for `string.split`,
- lock tests/docs around the final ownership boundary.

Everything else from the earlier stdlib bootstrap plan should be treated as
completed historical work and recovered from git history if needed.

## 2. Current State

Already true today (updated 2026-03-24):

- file-based stdlib import resolution exists,
- stdlib-only `@embed` is implemented,
- intrinsic bridge names for current stdlib support exist,
- `__goby_string_each_grapheme` and `__goby_list_push_string` are implemented
  and fully wired: `graphemes(text)[N]` executes through the Goby-owned Wasm
  runtime (GeneralLowered path) with correct Unicode EGC output,
- `String == String` works through the normal `==` operator,
- `stdlib/goby/string.gb` already contains iterator-driven `split` paths for:
  - empty delimiter (`split_with_empty_delimiter`),
  - single-grapheme delimiter (`split_with_single_delimiter`).
- the backend intrinsic `StringSplit` and fused `SplitEachPrint`/`SplitGetPrint`
  instructions exist for the byte-level split path used by general Wasm lowering;
  these are independent of the stdlib `string.split` call.

Still not finished:

- multi-grapheme delimiters still depend on the runtime `string.split(...)`
  builtin path (the `else` branch of `split` in `stdlib/goby/string.gb`),
- stdlib state/type plumbing still needs cleanup before the final `split`
  implementation is fully consolidated:
  - `stdlib/goby/string.gb` now typechecks successfully,
  - `parts: []` style state initialization now converges in `List String`
    constructor/state-update flows,
  - outer `mut` locals now remain visible inside stdlib-style `with ... in`
    bodies,
  - duplicated iterator state declarations are still present across stdlib modules,
- the runtime builtin branch cannot yet be deleted.

## 3. Locked Behavior

These semantics remain locked while finishing the work:

- preserve empty segments,
- preserve leading and trailing empty segments,
- empty delimiter means grapheme-wise split,
- grapheme definition is Unicode Extended Grapheme Cluster,
- empty input returns `[]` for current compatibility.

## 4. Remaining Milestones

- [ ] C4. Stdlib iterator/string implementation
  - finish `split` in `stdlib/goby/string.gb` using Iterator-driven processing,
  - remove dependency on runtime builtin `string.split(...)`.

- [ ] C5. Runtime builtin retirement
  - remove direct runtime handling path for `string.split` method call.

- [ ] C6. Regression coverage
  - add split edge-case tests and Unicode grapheme behavior tests,
  - add parity coverage for import/example paths that depend on `split`.

- [ ] C7. Docs sync
  - update active docs with final split ownership and intrinsic set.

- [ ] C8. Final quality gates
  - `cargo fmt`
  - `cargo check`
  - `cargo test`
  - `cargo clippy -- -D warnings`

## 5. Step-By-Step Execution

- [x] C4-S1. Unblock stdlib state type declaration
  - update the typechecker/local-binding path so `stdlib/goby/string.gb` passes under the
    current language/runtime architecture.
  - completed 2026-03-24:
    - `GraphemeState(... parts: [] ...)` now typechecks as `List String` in the relevant
      constructor/state-update flows,
    - `grapheme_count` local mutable binding (`mut n = 0`) no longer fails through nested
      stdlib-style `with ... in` bodies,
    - focused regression coverage added for both shapes,
    - `cargo run -p goby-cli -- check stdlib/goby/string.gb` now succeeds.

- [ ] C4-S2. Stabilize iterator state contract in stdlib
  - keep `GraphemeState` in `stdlib/goby/iterator.gb` as the canonical shared
  state shape,
  - make `stdlib/goby/string.gb` import and use that shared shape only,
  - remove duplicated local declarations once C4-S1 is done.
  - exit criteria:
    - no duplicated `Iterator` / `GraphemeState` declarations across stdlib modules.

- [ ] C4-S3. Implement multi-grapheme delimiter path in stdlib
  - add iterator-driven matcher state for:
    - current token buffer,
    - delimiter-match progress,
    - output parts list.
  - preserve the locked split semantics from section 3.
  - exit criteria:
    - `split` no longer calls runtime `string.split(...)` for any delimiter case.

- [ ] C4-S4. Remove stdlib fallback dependency and harden behavior tests
  - add stdlib-level split cases in `goby-wasm` runtime-output tests:
    - empty delimiter,
    - single-grapheme delimiter,
    - multi-grapheme delimiter,
    - consecutive / leading / trailing delimiter cases,
    - Unicode grapheme cases including emoji-family clusters.
  - add parity coverage for `examples/import.gb` behavior with the stdlib split path.
  - exit criteria:
    - tests prove stdlib `split` behavior matches the locked semantics.

- [ ] C4-S5. Mark C4 done and prepare C5 handoff
  - update active docs/state:
    - C4 complete,
    - runtime `string.split` branch is now legacy-only pending deletion.
  - exit criteria:
    - only C5 remains between current state and builtin retirement.

## 6. Definition Of Done

This remaining stdlib track is complete when all of the following are true:

- `goby/string.split` is fully implemented in stdlib for all delimiter cases,
- no direct runtime builtin branch remains for `string.split`,
- regression tests cover the locked split semantics and Unicode grapheme cases,
- active planning/state docs reflect the final ownership boundary,
- quality gates pass.
