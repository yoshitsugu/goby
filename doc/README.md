# Goby Documentation Guide

Use this directory as follows:

- `LANGUAGE_SPEC.md`: active language specification (latest user-visible syntax/semantics).
- `PLAN.md`: active top-level planning/roadmap reference.
- `PLAN_IR.md`: detailed architecture plan for the shared typed IR track.
- `PLAN_STANDARD_LIBRARY.md`: remaining stdlib follow-up plan (`string.split` stdlib migration and runtime builtin retirement).
- `STATE.md`: restart-safe execution snapshot (latest milestones, open items, next actions).
- `BUGS.md`: known issues and their status.

Recommended reading order:

1. `LANGUAGE_SPEC.md`
2. `PLAN.md`
3. `PLAN_IR.md` or `PLAN_STANDARD_LIBRARY.md` when working in those areas
4. `STATE.md`
5. `BUGS.md` (when fixing defects)

Archive policy:

- Historical completed plans are stored under `doc/old/` and are not part of normal workflow.

Maintenance rules:

- When syntax/semantics change, update `LANGUAGE_SPEC.md` in the same change.
- When syntax changes, verify syntax highlighting impact and update relevant
  tooling files as needed:
  - `tooling/syntax/textmate/goby.tmLanguage.json` (canonical)
  - `tooling/vscode-goby/syntaxes/goby.tmLanguage.json` (synced copy)
  - `tooling/emacs/goby-mode.el`
  - `tooling/vim/syntax/goby.vim`
- Update `PLAN.md` when planning status/migration steps/decisions change.
- Update specialized plan documents (`PLAN_IR.md`, `PLAN_STANDARD_LIBRARY.md`) when their
  tracked scope changes.
- At meaningful milestones, update `STATE.md`.
- Keep `README.md` high-level; keep detailed spec text in this directory.
