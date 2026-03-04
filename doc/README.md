# Goby Documentation Guide

Use this directory as follows:

- `LANGUAGE_SPEC.md`: active language specification (latest user-visible syntax/semantics).
- `PLAN.md`: active planning/roadmap reference (migration steps, future decisions, execution plans).
- `STATE.md`: restart-safe execution snapshot (latest milestones, open items, next actions).
- `BUGS.md`: known issues and their status.

Recommended reading order:

1. `LANGUAGE_SPEC.md`
2. `PLAN.md`
3. `STATE.md`
4. `BUGS.md` (when fixing defects)

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
- At meaningful milestones, update `STATE.md`.
- Keep `README.md` high-level; keep detailed spec text in this directory.
