# Goby Project State Snapshot

Last updated: 2026-04-05

## Current Focus

The next implementation target is [`doc/PLAN_ERROR.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_ERROR.md).

Current intent:

- continue `doc/PLAN_ERROR.md` after closing TD2 typed-argument parity
- preserve the rule that `goby-core` owns typed diagnostic spans while CLI/LSP
  only render them
- keep widening span ownership only where the parser can honestly point at the
  whole blamed argument expression

Current slice status:

- Ordinary-call typed mismatch parity is now locked on the canonical literal
  fixture as well as the previously covered bound-variable / qualified /
  later-argument / partially-applied cases:
  - single-line parser-owned literal / interpolated / list / tuple expressions
    now carry honest whole-expression spans via a transparent AST wrapper
  - `goby-core` keeps owning the resulting span; CLI/LSP still only render it
  - `goby check` / `goby run` parity is locked for the direct `f "a"` fixture
  - LSP range parity remains locked for ordinary and qualified cases, including
    UTF-16 conversion on a line with emoji before the blamed token
- TD2 typed-argument parity is now also locked for effect-op and `resume`
  argument mismatches where the blamed argument expression already has honest
  parser-owned span support:
  - effect-op mismatch diagnostics now point at the mismatched argument
    expression in `goby-core` and render aligned snippets/ranges in CLI/LSP
  - `resume` mismatch diagnostics now do the same, including handler clause
    body cases after rebasing clause parsed-body spans to declaration-body
    coordinates before source-file conversion
- The remaining deferred ownership gap inside typed mismatches is now
  multiline/body-relative expression shapes that still lack honest direct
  ownership, primarily block arguments and any future equivalent forms.

## Locked Decisions

- `goby-core` remains the sole owner of typed diagnostic spans and messages.
- Single-line parser-owned literal / interpolated / list / tuple expressions may
  use honest whole-expression spans for typed diagnostics.
- Handler clause parsed-body diagnostics may rebase body-local spans to
  declaration-body coordinates inside `goby-core` before final source-file
  conversion when that mapping is structurally known.
- Do not fabricate pseudo-precise spans for multiline block arguments or other
  body-relative shapes that still lack direct file-relative ownership.
- CLI and LSP parity should be added only after the core span source is honest.

## Immediate Next Steps

- Continue `doc/PLAN_ERROR.md` after TD2:
  - audit which remaining typed diagnostic families still already know an honest
    blame site versus which still lack source ownership
  - keep multiline block-argument ownership deferred until there is a credible
    file-relative span source rather than widening by frontend guesswork
- Keep deferring multiline block-argument ownership until there is a credible
  file-relative span source rather than widening by frontend guesswork.

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
