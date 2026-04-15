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

**M4 in progress:**
- M4.1 (ptr.rs helpers) + M4.1b (i64 scratch locals): complete
- M4.2.0 (HeapEmitState.pw() shorthand): complete
- M4.2.1 (backend.rs): excluded — all I32Load/Store are WASI Preview 1 iovec (always i32)
- M4.2.2 (emit_chunked_* in emit_instrs): complete
- M4.2.3 (all remaining I32Load*/I32Store* in emit.rs): complete — 0 I32Load/I32Store remain

Next: M4.3 (address-typed I32Const → ptr_const), M4.4 (I32Add/Sub on addresses), M4.5 (MemoryGrow/MemorySize), M4.6 (flip memory64 default)

Current state: all cargo tests green; M4.2 exit criteria met.

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
