# Goby Project State Snapshot

Last updated: 2026-04-15

## Current Focus

**PLAN_LIST_FIX M2 — CLI ceiling surface**

M1 (in-place lowering) is complete as of 2026-04-14.

M2.0 + M2.2 complete as of 2026-04-15:
- `RUNTIME_MEMORY_CONFIG` (1 GiB) / `TEST_MEMORY_CONFIG` (64 MiB) split in `memory_config.rs`
- All emitted Wasm modules now declare 1 GiB `maximum` (was 64 MiB)
- `--max-memory-mb <N>` flag and `GOBY_MAX_MEMORY_MB` env var wired to CLI
  - Both runtime-stdin path and file-based wasmtime path honour the ceiling
  - `execute_wasm` passes `-Wmax-memory-size=<bytes>` to wasmtime ≥ v15

**Next: M2.3 — improve the OOM trap message**

Remaining M2 sub-steps:
3. **M2.3** — improve the trap message to report "configured ceiling: N MiB"
4. **M2.4** — tests: OOM repro with lowered ceiling, `--max-memory-mb` parse tests

## Architecture State

| Layer | Status |
|---|---|
| Parser | Stable |
| Resolver | Stable |
| Typechecker | Stable |
| IR (`ir.rs`) | Stable |
| IR lowering (`ir_lower.rs`) | Stable |
| Wasm backend | Stable. `ListSetInPlace` added for each+AssignIndex in-place path |
| Effect handlers | Non-tail / multi-resume produces `BackendLimitation` |
| GC / reclamation | Out of scope. Bump allocator only |
