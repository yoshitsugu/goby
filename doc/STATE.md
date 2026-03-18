# Goby Project State Snapshot

Last updated: 2026-03-18

## Current Focus

- `doc/PLAN.md` now has Active Track F for general Wasm lowering of runtime `Read` programs.
- Priority direction is to replace shape-specific runtime-I/O Wasm emitters with a general
  lowering/execution path, not to keep extending recognizer-driven support case by case.

## Immediate Next Steps

1. Lock Phase F1 architecture:
   choose the execution representation and lowering boundary for general effectful runtime programs.
2. Define the Wasm-side runtime value/memory model needed for `Read.read`, `string.split`,
   `List String`, and `Print`.
3. Re-evaluate whether any temporary bridge is needed only after the Phase F1/F2 design is written down.

## Decisions To Carry Forward

- Runtime `Read` support should move toward general Wasm lowering rather than more
  ad-hoc `RuntimeIoPlan` pattern additions.
- Temporary breakage is acceptable while moving toward the cleaner long-term backend design.

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split` and retire the runtime builtin path
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset
- `PLAN.md` D6b-ts: Tree-sitter grammar after D6c

## Restart Notes

- `doc/PLAN.md` is the roadmap reference.
- Add a new focused save-point here when the next development slice starts.
