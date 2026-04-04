# Goby Project State Snapshot

Last updated: 2026-04-05

## Current Focus

The next implementation target is [`doc/PLAN_EXPORT_EMBED.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_EXPORT_EMBED.md).

Current intent:

- make `@embed` a `goby/prelude`-only feature
- move both `Print` and `Read` ownership into `goby/stdio`
- derive implicit `Print` / `Read` availability from prelude `@embed` metadata
  rather than from hard-coded name tables or duplicated stdlib declarations

## Locked Decisions

- `goby/prelude` remains the only implicit-import entrypoint.
- `@embed` is only valid in `goby/prelude`.
- `goby/stdio` is the canonical owner of both `Print` and `Read`.
- All operations of an embedded effect become bare names automatically when that
  effect is part of the implicit prelude surface.
- Implementation should follow the long-term ownership model directly; do not
  add compatibility layers that preserve the old split design.

## Immediate Next Steps

- Complete M1 in [`doc/PLAN_EXPORT_EMBED.md`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/doc/PLAN_EXPORT_EMBED.md) by locking the spec wording in `doc/LANGUAGE_SPEC.md`.
- Implement the shared metadata path before changing stdlib file layout.
- Avoid symbol-specific bridging such as `if effect == "Print"` / `if effect == "Read"` during the migration.
- After the compiler path is shared, move `Print` and `Read` declarations into `stdlib/goby/stdio.gb` and keep `stdlib/goby/prelude.gb` responsible only for imports plus `@embed`.

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
