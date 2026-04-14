# Goby Project State Snapshot

Last updated: 2026-04-15

## Current Focus

**PLAN_LIST_FIX M2 — CLI ceiling surface**

M1 (in-place lowering) is complete as of 2026-04-14:
- `BackendIntrinsic::ListSetInPlace` + `emit_list_set_in_place_helper` landed
- `lower_list_each_mutating_assign` recognises `list.each xs (fn v -> root[..] := rhs)` at the GlobalRef call site and emits `ListFold` with the in-place callback
- `needs_helper_state` updated for `ListSetInPlace`
- G0 verified: `each_assign_index_in_place_bugs_md_minimal_repro_completes` passes at 50×50×5000 under the default 64 MiB ceiling
- BUGS.md open entry moved to resolved

**Next: M2 — expose `--max-memory-mb` / `GOBY_MAX_MEMORY_MB` on the CLI**

Current state (before M2):
- Hard-coded ceiling: `DEFAULT_WASM_MEMORY_CONFIG.max_pages = 1024` (64 MiB) in `crates/goby-wasm/src/memory_config.rs`
- CLI entry: `crates/goby-cli/src/main.rs` — `run()` ~line 169, `parse_args` ~line 319, `execute_wasm` ~line 413
- No user-facing knob exists yet

M2 sub-steps (from PLAN_LIST_FIX.md):
1. **M2.1** — split `DEFAULT_WASM_MEMORY_CONFIG` into a const floor and a runtime-resolved config
2. **M2.2** — add `--max-memory-mb` flag and `GOBY_MAX_MEMORY_MB` env fallback to CLI
3. **M2.3** — improve the trap message to mention the flag when memory is exhausted
4. **M2.4** — tests: run the BUGS.md repro with a raised ceiling, assert it completes

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
