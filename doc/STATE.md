# Goby Project State Snapshot

Last updated: 2026-04-05

## Current Focus

The next implementation target is [`doc/PLAN_ERROR.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_ERROR.md).

Current intent:

- close ordinary-call typed-diagnostic parity on the canonical literal fixture
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
- The remaining deferred ownership gap inside ordinary typed mismatches is now
  multiline/body-relative expression shapes, primarily block arguments.

## Locked Decisions

- `goby-core` remains the sole owner of typed diagnostic spans and messages.
- Single-line parser-owned literal / interpolated / list / tuple expressions may
  use honest whole-expression spans for typed diagnostics.
- Do not fabricate pseudo-precise spans for multiline block arguments or other
  body-relative shapes that still lack direct file-relative ownership.
- CLI and LSP parity should be added only after the core span source is honest.

## Immediate Next Steps

- Continue `doc/PLAN_ERROR.md` at TD2:
  - audit effect-op argument mismatches and `resume` mismatches that can now
    benefit from the widened parser-owned expression spans
  - add parity regressions only where `goby-core` already knows the blamed
    argument expression honestly
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
