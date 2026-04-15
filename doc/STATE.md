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

**M3 complete as of 2026-04-15:**
- M3.1: `WasmMemoryConfig::memory64: bool` field added (default `false`); `memory_type()` honours it; `config.wasm_memory64(true)` unconditionally enabled in wasmtime `Engine`
- M3.2: `memory64_flag_hello_gb_executes_correctly` test — hello.gb executes correctly under `memory64: true` memory declaration (wasm32 opcodes, memory64 memory type)
- M3.3: Host-refusal semantics validated via existing OOM tests; decision recorded in `doc/PLAN.md` §3.2: `--max-memory-mb=0` must translate to finite `RUNTIME_MEMORY_CONFIG` ceiling until `StoreLimits` is wired (deferred to M4)
- M3.4: Address-site inventory committed as `doc/PLAN_LIST_FIX_M4_SITES.md`

**Next: M4 — Full emitter migration to memory64**

Current state: all cargo tests green; M3 exit criteria met.

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
