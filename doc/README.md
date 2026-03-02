# Goby Documentation Guide

Use this directory as follows:

- `PLAN.md`: active language design reference (source of truth for current and next decisions).
- `STATE.md`: restart-safe execution snapshot (latest milestones, open items, next actions).
- `BUGS.md`: known issues and their status.

Recommended reading order:

1. `PLAN.md`
2. `STATE.md`
3. `BUGS.md` (when fixing defects)

Archive policy:

- Historical completed plans are stored under `doc/old/` and are not part of normal workflow.

Maintenance rules:

- When syntax/semantics change, update `PLAN.md` first.
- At meaningful milestones, update `STATE.md`.
- Keep `README.md` high-level; keep detailed spec text in this directory.
