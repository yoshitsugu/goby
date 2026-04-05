# Goby Project State Snapshot

Last updated: 2026-04-05

## Current Focus

The next implementation target is [`doc/PLAN_EXPORT_EMBED.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_EXPORT_EMBED.md).

Current intent:

- make `@embed` a `goby/prelude`-only feature
- move both `Print` and `Read` ownership into `goby/stdio`
- derive implicit `Print` / `Read` availability from prelude `@embed` metadata
  rather than from hard-coded name tables or duplicated stdlib declarations

Current slice status:

- M1 semantics lock is complete in `doc/LANGUAGE_SPEC.md` and `doc/PLAN.md`.
- The shared typecheck-facing metadata path is in place:
  - stdlib resolver now tracks visible imported effects and prelude-selected
    embedded effect exports with provenance
  - embed validation now requires a visible effect in `goby/prelude`
  - implicit `main` effect/default-handler and bare-op injection now flow from
    prelude embed metadata instead of same-module embed assumptions
- The resolved-name migration is complete:
  - `resolved.rs` now derives implicit bare effect-op names from prelude embed
    metadata and explicit qualified effect-op names from visible imported effects
  - fixed `Print` / `Read` tables are no longer the source of implicit effect-op resolution
- Stdlib ownership has been moved to the intended end-state layout:
  - `stdlib/goby/stdio.gb` is now the canonical declaration site for both
    `Print` and `Read`
  - `stdlib/goby/prelude.gb` now contains only `import goby/stdio` plus `@embed`
    declarations for the implicit surface
- Wasm/runtime parity is preserved after the ownership move:
  - fallback runtime effect-op visibility now follows effective runtime imports
    transitively, so bare `print` / `read` still resolve through
    `goby/prelude -> goby/stdio`
  - general lowering now also seeds stdlib export lookup from the effective
    implicit-import surface rather than from explicit user imports only
- Regression/doc closure is complete for the export/embed change:
  - examples now describe the current `goby/stdio` ownership model rather than
    the old planned split layout
  - LSP parity covers both implicit `Read` and explicit `goby/stdio` usage
  - `doc/PLAN_EXPORT_EMBED.md` is now at completion-state for this slice

## Locked Decisions

- `goby/prelude` remains the only implicit-import entrypoint.
- `@embed` is only valid in `goby/prelude`.
- `goby/stdio` is the canonical owner of both `Print` and `Read`.
- All operations of an embedded effect become bare names automatically when that
  effect is part of the implicit prelude surface.
- Implementation should follow the long-term ownership model directly; do not
  add compatibility layers that preserve the old split design.

## Immediate Next Steps

- Treat
  [`doc/PLAN_EXPORT_EMBED.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_EXPORT_EMBED.md)
  as closed unless a regression reopens it.
- Pick the next top-level slice from
  [`doc/PLAN.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN.md)
  rather than continuing to add compatibility churn around export/embed.
- Keep avoiding symbol-specific bridging such as
  `if effect == "Print"` / `if effect == "Read"` in any follow-up cleanup.

## Architecture State

- Resolved-form → shared IR boundary is stable.
- Wasm backend lowering design remains locked in [`doc/PLAN_IR.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_IR.md).
- Current runtime memory work is closed unless future runtime-lifetime evidence justifies reopening GC / reclamation planning.
- Known backend limitation still in scope:
  - non-tail / multi-resume handlers produce `BackendLimitation`

## Key Entry Points

- [`doc/PLAN.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN.md) — top-level roadmap
- [`doc/PLAN_EXPORT_EMBED.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_EXPORT_EMBED.md) — current target plan
- [`doc/LANGUAGE_SPEC.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/LANGUAGE_SPEC.md) — current language behavior
- [`crates/goby-core/src/stdlib.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-core/src/stdlib.rs) — stdlib resolver
- [`crates/goby-core/src/typecheck_validate.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-core/src/typecheck_validate.rs) — import / embed validation and implicit-prelude plumbing
- [`crates/goby-core/src/typecheck_phase.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-core/src/typecheck_phase.rs) — known-effect and embedded-default phase wiring
- [`crates/goby-core/src/resolved.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-core/src/resolved.rs) — bare / qualified name resolution behavior
