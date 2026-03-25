# Goby Standard Library Remaining Work Plan

Status: active follow-up only
Owner: Goby core/runtime track
Last updated: 2026-03-25 (C4-S4 partial)

## 1. Scope

Most of the original stdlib-foundation plan is already implemented. This
document now keeps only the remaining work that is still relevant for active
development.

This is a focused companion plan to `doc/PLAN.md`, not a separate roadmap.

Current remaining stdlib track:

- finish moving `goby/string.split` to stdlib-driven behavior,
- remove the remaining direct runtime builtin path for `string.split`,
- lock tests/docs around the final ownership boundary.

This track is intentionally narrower than `doc/PLAN_IR.md`:

- `PLAN_IR` owns making composed runtime-`Read` shapes reach `GeneralLowered`
  end-to-end through the existing backend IR and Wasm emitter.
- this document owns only the final semantics/ownership handoff for `goby/string.split`.
- in particular, `list.map`, `list.each`, and `goby/string.graphemes` lowering convergence
  are not split-track milestones here except where they are needed as parity coverage for
  stdlib `split`.

Everything else from the earlier stdlib bootstrap plan should be treated as
completed historical work and recovered from git history if needed.

## 2. Current State

Already true today (updated 2026-03-25):

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
- at least one composed-path regression using stdlib `split` in a higher-order pipeline
  (`split -> map(graphemes) -> list.get -> each`) now exists through the `PLAN_IR` WB-3-M7
  acceptance shape and its variants.
- the backend intrinsic `StringSplit` and fused `SplitEachPrint`/`SplitGetPrint`
  instructions exist for the byte-level split path used by general Wasm lowering;
  these are independent of the stdlib `string.split` call.

Still not finished:

- runtime builtin retirement is still pending:
  - the legacy direct runtime handling path for `string.split` still exists,
  - behavior-hardening coverage for the final stdlib-owned split path is not complete,
- docs/state still need the final ownership sync once the remaining split tests land.

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

- [x] C4-S2. Stabilize iterator state contract in stdlib
  - keep `GraphemeState` in `stdlib/goby/iterator.gb` as the canonical shared
  state shape,
  - make `stdlib/goby/string.gb` import and use that shared shape only,
  - remove duplicated local declarations once C4-S1 is done.
  - completed 2026-03-25:
    - `type GraphemeState = ...` now lives in `stdlib/goby/iterator.gb`,
    - `stdlib/goby/string.gb` imports `Iterator` and `GraphemeState` selectively from
      `goby/iterator`,
    - imported shared record constructors now flow through the normal type-environment path,
    - `cargo run -p goby-cli -- check stdlib/goby/string.gb` succeeds after the move.
  - exit criteria:
    - no duplicated `Iterator` / `GraphemeState` declarations across stdlib modules.

- [x] C4-S3. Implement multi-grapheme delimiter path in stdlib
  - add stdlib-owned grapheme-aware matcher state for:
    - current token buffer,
    - delimiter-match progress,
    - output parts list.
  - preserve the locked split semantics from section 3.
  - completed 2026-03-25:
    - `stdlib/goby/string.gb` now handles multi-grapheme delimiters through stdlib-owned grapheme-list matching,
    - the stdlib `split` definition no longer calls runtime `string.split(...)` for any delimiter case,
    - focused Wasm execution coverage proves leading / consecutive / trailing empty-segment preservation for a representative multi-grapheme delimiter case,
    - `cargo run -p goby-cli -- check stdlib/goby/string.gb` still succeeds after the rewrite.
  - exit criteria:
    - `split` no longer calls runtime `string.split(...)` for any delimiter case.

- [x] C4-S4. Remove stdlib fallback dependency and harden behavior tests (complete, 2026-03-25)
  - completed (commit a9f85fa3):
    - `split text ""` Wasm trap fixed: `lower_comp_inner` redirects `split text ""` to
      `StringGraphemesList` (same as `graphemes text`) in both GlobalRef and Var call paths.
    - `StringSplit` intrinsic retained for non-empty delimiter (stdlib `split_with_*` functions
      use record field access which `emit.rs` does not support — full stdlib routing deferred).
    - `stdlib/goby/string.gb`: `split`/`split_multi_parts`/`split_with_multi_delimiter`
      refactored to use `let` bindings before `if` conditions (required by `ir_lower.rs`).
    - stdlib transitive closure mechanism added to `lower_module_to_instrs` (for user aux decls
      that call stdlib functions).
    - tests added: empty-delimiter ASCII (`split "abc" ""`), empty-delimiter emoji ZWJ,
      single-char delimiter comma, leading/trailing empty segments.
  - completed in this slice:
    - `examples/import.gb` compile-path parity test (classified as `NotRuntimeIo`, compiles to valid Wasm),
    - `split "" ""` → `[]` edge-case execution test,
    - multi-grapheme delimiter Unicode EGC execution test.
  - exit criteria:
    - tests prove stdlib `split` behavior matches the locked semantics.

- [x] C4-S5. Mark C4 done and prepare C5 handoff
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
