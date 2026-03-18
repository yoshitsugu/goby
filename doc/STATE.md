# Goby Project State Snapshot

Last updated: 2026-03-18

## Current Focus

- Track F F1 is complete: `doc/wasm_runtime_architecture.md` locked, fixture files committed.
- Next milestone is F2: value representation stub (`gen_lower/value.rs` with `RtValue` tagged-i64).

## Immediate Next Steps

1. F2: create `crates/goby-wasm/src/gen_lower/` module with:
   - `mod.rs`: skeleton + `GeneralLowerer` entry point stub
   - `value.rs`: `RtValue` tagged-i64 encoding for Unit/Int/Bool/String/List
   - Unit tests for encode/decode round-trips
2. F2 must confirm or revise the tentative helper ABI in §6 of the architecture doc.
3. F3 follows after F2: general lowering for `Read.read` + `Print.print/println`.

## Decisions To Carry Forward

- Architecture is locked in `doc/wasm_runtime_architecture.md`; all F2+ code must follow it.
- `gen_lower/` must not import from `runtime_io_plan.rs`.
- Helper ABI in §6 is tentative until F2 confirms.
- F3 fixture (`tests/track-f/f3_print_read.gb`) currently routes through `DynamicWasiIo(Echo)`;
  F3 goal is to route through the general lowering path instead.

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split` and retire the runtime builtin path
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset
- `PLAN.md` D6b-ts: Tree-sitter grammar after D6c

## Restart Notes

- `doc/PLAN.md` is the roadmap reference.
- `doc/wasm_runtime_architecture.md` is the architecture reference for Track F.
- Add a new focused save-point here when the next development slice starts.
