# Goby Project State Snapshot

Last updated: 2026-04-15

## Current Focus

**PLAN_LIST_FIX M2 — CLI ceiling surface**

M1 (in-place lowering) is complete as of 2026-04-14.

**M2 complete as of 2026-04-15:**
- M2.0: `RUNTIME_MEMORY_CONFIG` (1 GiB) / `TEST_MEMORY_CONFIG` (64 MiB) split; emitter updated
- M2.2: `--max-memory-mb <N>` flag and `GOBY_MAX_MEMORY_MB` env var wired to CLI
  - Both runtime-stdin and file-based wasmtime paths honour the ceiling
  - `execute_wasm` passes `-Wmax-memory-size=<bytes>` to wasmtime ≥ v15
- M2.3: OOM trap message now appends "(configured ceiling: N MiB; use --max-memory-mb to raise it)"
- M2.4: `--max-memory-mb` parse tests + `resolve_memory_config` unit tests added

**Next: M3 — memory64 proof-of-concept**

Current state: all cargo tests green; M2 exit criteria met.

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
