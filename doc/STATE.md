# Goby Project State Snapshot

Last updated: 2026-04-05

## Current Focus

The next implementation target is [`doc/PLAN_ERROR.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_ERROR.md).

Current intent:

- lock the initial typed-diagnostic migration boundary explicitly
- preserve the rule that `goby-core` owns typed diagnostic spans while CLI/LSP
  only render them
- add parity coverage for ordinary-call typed mismatches whose blamed argument
  already owns a precise AST span

Current slice status:

- `doc/PLAN_EXPORT_EMBED.md` is closed unless a regression reopens it.
- TD0 boundary lock is now explicit in `doc/PLAN_ERROR.md`:
  - the initial typed-diagnostic migration set is limited to ordinary-call
    argument mismatches whose blamed argument already owns a precise AST span
  - literal/interpolated/list/tuple/block argument expressions remain deferred
    until expression-span ownership is widened honestly
- Ordinary-call typed mismatch parity is regression-locked for the current
  bounded subset:
  - `goby-core` tests assert token-aligned spans for bare, qualified,
    later-argument, and partially-applied remainder mismatches on bound
    variable arguments
  - CLI fixture/rendering parity is locked for the bounded representative case
  - LSP range parity is locked for ordinary and qualified cases, including a
    UTF-16 conversion case on a line containing emoji before the blamed token
- Remaining TD work is to widen honest expression-span ownership so the
  representative literal-argument case can gain the same snippet/range quality.

## Locked Decisions

- `goby-core` remains the sole owner of typed diagnostic spans and messages.
- The current TD slice is intentionally bounded to argument expressions that
  already own precise AST spans.
- Do not fabricate pseudo-precise spans for string/int/bool literals or other
  span-less expression shapes just to satisfy frontend fixture quality.
- CLI and LSP parity should be added only after the core span source is honest.

## Immediate Next Steps

- Continue `doc/PLAN_ERROR.md` from the bounded TD1 frontier:
  - decide whether to widen `Expr` span ownership for literals/interpolated/list/tuple/block
    expressions, or to add a narrower argument-slice ownership layer
  - once that ownership exists, move the canonical `f "a"` ordinary-call
    mismatch fixture onto snippet/range parity
- Keep avoiding symbol-specific typed-diagnostic branches such as special-casing
  `print`, `println`, or one fixture function name.

## Architecture State

- Resolved-form → shared IR boundary is stable.
- Wasm backend lowering design remains locked in [`doc/PLAN_IR.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_IR.md).
- Current runtime memory work is closed unless future runtime-lifetime evidence justifies reopening GC / reclamation planning.
- Known backend limitation still in scope:
  - non-tail / multi-resume handlers produce `BackendLimitation`

## Key Entry Points

- [`doc/PLAN.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN.md) — top-level roadmap
- [`doc/PLAN_ERROR.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_ERROR.md) — active typed-diagnostic plan
- [`doc/LANGUAGE_SPEC.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/LANGUAGE_SPEC.md) — current language behavior
- [`crates/goby-core/src/typecheck_call.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-core/src/typecheck_call.rs) — ordinary-call typed mismatch diagnostics
- [`crates/goby-core/src/typecheck_span.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-core/src/typecheck_span.rs) — span selection helpers
- [`crates/goby-cli/src/main.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-cli/src/main.rs) — CLI diagnostic rendering
- [`crates/goby-lsp/src/main.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-lsp/src/main.rs) — LSP diagnostic range parity
