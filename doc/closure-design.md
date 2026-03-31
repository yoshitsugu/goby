# Closure Representation Design — WB-3B

**Status:** SUPERSEDED by `doc/PLAN_CLOSURE_CAPTURE.md` (2026-03-31)
**Original revision:** 1.1 (locked 2026-03-26)
**Scope:** Design only. This document has been superseded by `doc/PLAN_CLOSURE_CAPTURE.md`,
which changes the mutable-capture direction (see § 5 note below). Refer to
`doc/PLAN_CLOSURE_CAPTURE.md` and `doc/LANGUAGE_SPEC.md` for the current intended semantics.
The low-level Wasm layout details in §§ 3–4 and 6–9 remain as a useful implementation
reference, but the closure-capture semantic decisions in §§ 5 and 10 have been superseded.

---

## 1. Overview and Motivation

WB-3A (the current general-lowering path) supports only **non-capturing lambdas**: lambdas
whose bodies reference no variables from the enclosing scope. Capturing lambdas — where the
lambda body references outer `let` / `mut` bindings — currently fall back to the interpreter
path and produce a precise diagnostic (H4).

H5 locks the design for closure representation so that WB-3B can implement closure lowering
without making ad-hoc decisions about memory layout or calling conventions.

The target program is `examples/bugs/runtime_read_captured_lambda.gb`:

```goby
import goby/list ( each )

helper : Unit -> Int
helper =
  mut total = 0
  each [1, 2] (fn x -> total := total + x)
  total

main : Unit -> Unit can Print, Read
main =
  _ = read()
  println "${helper()}"
```

---

## 2. Current State (WB-3A)

### Non-capturing lambda lowering

1. `lower_value_as_arg` detects free variables via `comp_has_free_var`.
2. If no free variables: the lambda body is lowered and lifted as `__lambda_N` aux decl.
3. `PushFuncHandle { decl_name }` emits `i64.const encode_func_handle(table_slot)`.
4. `IndirectCall` dispatches via `call_indirect table[0] type=(i64)->i64`.

### Tagged-i64 value representation

All runtime values are encoded as tagged `i64` words:

| Tag  | Type    | Payload (lower 60 bits)                    |
|------|---------|--------------------------------------------|
| 0x0  | Unit    | 0                                          |
| 0x1  | Int     | 60-bit two's complement                    |
| 0x2  | Bool    | 0 = false, 1 = true                        |
| 0x3  | String  | u32 pointer to `(len: i32, bytes...)`      |
| 0x4  | List    | u32 pointer to `(len: i32, items: [i64]...)` |
| 0x5  | Func    | u32 funcref table slot index               |
| 0x6  | Tuple   | u32 pointer to `(len: i32, items: [i64]...)` |
| 0x7  | Record  | u32 pointer to `(ctor_tag: i64, fields: [i64]...)` |
| 0x8  | **Closure** | u32 pointer to closure record (this design) |
| 0x9–0xF | (reserved) | —                                   |

### Memory layout

```
Linear memory:
  [0,   8)  iovec descriptor (for WASI fd_write)
  [8,  16)  nwritten counter
  [16, ∞)   heap — bump-allocated, no GC, arena lifetime
```

### Funcref table

Single Wasm `funcref` table (table index 0). Each aux decl occupies one slot.
Slot N holds the Wasm function index of the N-th aux decl.

---

## 3. Closure Value Representation

### Tag assignment

`TAG_CLOSURE = 0x8` (first available tag after existing 0x0–0x7 assignments).

### Closure record in linear memory

A closure is heap-allocated using the existing bump allocator. Its layout is:

```
Offset  Size   Field
──────  ────   ────────────────────────────────────────────
0       4      func_table_slot: u32   — funcref table slot of the closure wrapper
4       4      env_size: u32          — number of captured values (N)
8       8*N    captured[0..N]: i64    — captured values (tagged i64, by value)
```

Total size: `8 + 8 * N` bytes, aligned to 8 bytes.

### Closure tagged-i64

```
bits 63–60: 0x8 (TAG_CLOSURE)
bits 59–32: 0 (unused; reserved for future use)
bits 31–0:  u32 pointer to closure record in linear memory
```

### Encoding/decoding (to be added to value.rs in WB-3B)

```rust
pub(crate) const TAG_CLOSURE: u8 = 0x8;

pub(crate) fn encode_closure_ptr(ptr: u32) -> i64 {
    ((TAG_CLOSURE as i64) << 60) | (ptr as i64)
}
```

For decoding the pointer, reuse the existing `decode_payload_ptr` in `value.rs` (which already
applies the `0xFFFF_FFFF` mask). **Do not add a separate `decode_closure_ptr` function.**

Note on bits 32–59: heap pointers will always be < 2^32 (within 4 GiB), so bits 32–59 are
always zero for valid closure records. The mask `0xFFFF_FFFF` is therefore correct and sufficient.

---

## 4. Call Convention

### Design choice: two type indices (Option B)

**Decision:** Non-closure aux decls keep the current `(i64) -> i64` signature.
Closure wrapper functions use a new `(i64, i64) -> i64` signature (arg, env_ptr).

This avoids changing all existing non-closure call sites and IndirectCall emission.

### Non-closure call (unchanged)

```
stack before IndirectCall: [..., arg: i64, callee: i64_with_TAG_FUNC]
decode: slot = lower_32(callee)
emit:   call_indirect table[0] type=(i64)->i64
```

### Closure call (new path)

```
stack before ClosureCall: [..., arg: i64, callee: i64_with_TAG_CLOSURE]
decode: closure_ptr     = lower_32(callee)           — extract heap pointer
load:   func_table_slot = i32.load(closure_ptr + 0)  — read slot from record
emit:   call_indirect table[0] type=(i64,i64)->i64
                       with args (arg, closure_ptr)
```

The closure wrapper receives `closure_ptr` as the second argument and reads captures
from `closure_ptr + 8 + 8*i` for capture index `i` (the record's `env_size` field at
offset 4 is available for bounds-checking but is not required for the dispatch itself).

### New backend IR instruction (to be added in WB-3B)

```rust
/// Indirect call to a closure (TAG_CLOSURE value).
/// Stack before: [..., arg: i64, callee_closure: i64]
/// Extracts closure_ptr from callee, reads func_table_slot from record,
/// and emits call_indirect with sig (i64, i64) -> i64.
ClosureCall,
```

### IndirectCall tag dispatch (to be added in WB-3B)

The current `IndirectCall` instruction assumes `TAG_FUNC`. A new
`TaggedCall` instruction (or a runtime tag-check branch) is needed:

```
if tag(callee) == TAG_FUNC    → IndirectCall (existing)
if tag(callee) == TAG_CLOSURE → ClosureCall  (new)
```

Exact dispatch strategy (static analysis vs runtime branch) is deferred to WB-3B.

### ListEach / ListMap and TAG_CLOSURE (WB-3B scope)

`ListEach` and `ListMap` backend IR instructions currently hardcode
`call_indirect type=(i64)->i64`. When the callback is a `TAG_CLOSURE` value,
this will type-mismatch.

**Decision:** Handling `TAG_CLOSURE` callbacks in `ListEach`/`ListMap` is explicitly
**WB-3B scope**. Options include:
- New `ListEachClosure`/`ListMapClosure` variants with two-arg dispatch.
- A unified `TaggedListEach`/`TaggedListMap` that inspects the tag at runtime.

Until WB-3B, capturing lambdas passed to `each`/`map` remain `UnsupportedIrForm`.

---

## 5. Mutable Local Capture — SUPERSEDED

> **Note (2026-03-31):** The snapshot semantics described here have been superseded by
> `doc/PLAN_CLOSURE_CAPTURE.md`. The locked semantic target is now **shared-cell semantics**
> for `mut` captures, not value-copy at closure creation time.
> See `doc/LANGUAGE_SPEC.md` § 3 "Closure semantics" for the current specification.
> The original snapshot decision and the write-capture restriction below are no longer the
> intended direction.

~~**Decision:** Captured variables are captured **by value at closure creation time**.~~

~~Mutations to a mutable local after closure creation do **not** affect the closure.~~
~~The closure captures the value of the local at the moment the closure value is produced.~~

Original snapshot example (no longer the spec — shown for historical context only):
```goby
mut x = 1
f = fn y -> x + y   ← OLD: would capture x = 1 as snapshot
x := 42
f(0)               ← OLD: would return 1 (snapshot); SPEC: should return 43 (shared cell)
```

~~**Restriction:** Lambdas that **assign to** a captured mutable local are not supported
in WB-3B.~~ (This restriction is superseded — write capture is an explicit goal of Track CC.)

---

## 6. Funcref Table Strategy

All callables (non-capturing lambdas and closure wrappers) share the single funcref table
(table index 0).

Closure wrappers are added to the table in the same way as lambda aux decls. The only
difference is their Wasm function type signature: `(i64, i64) -> i64` vs `(i64) -> i64`.

The funcref table must support both `call_indirect` type indices. Two type entries are
registered in the Wasm type section:

```
type 0: (i64) -> i64        — non-closure aux decls (existing)
type 1: (i64, i64) -> i64   — closure wrappers (new)
```

`call_indirect` uses the appropriate type index based on the callee tag.

---

## 7. Closure Creation Sequence (Wasm Instructions)

When `fn x -> body` is lowered as a **closure** (has free variables `v0, v1, ...`):

1. **Allocate** `8 + 8*N` bytes from the heap bump allocator → `closure_ptr: i32`.
2. **Store func_table_slot**: `i32.store(closure_ptr + 0, wrapper_table_slot)`.
3. **Store env_size**: `i32.store(closure_ptr + 4, N)`.
4. **Store captured values**:
   - For each captured variable `vi` (in declaration order):
     `i64.store(closure_ptr + 8 + 8*i, value_of(vi))`.
5. **Encode and push**: `i64.const encode_closure_ptr(closure_ptr)`.

This corresponds to a new `WasmBackendInstr::CreateClosure { wrapper_decl: String, captures: Vec<String> }`.

---

## 8. Fallback / Runtime Parity

The interpreter runtime already handles capturing lambdas via `captured_locals` and
`captured_callables`. The Wasm path must produce identical output for the same input.

**Parity verification target (existing test):**

```
crates/goby-wasm/src/runtime_behavior_tests.rs::typed_mode_matches_fallback_for_lambda_closure_capture
```

This test currently runs the program through the interpreter fallback and asserts
the output is `"4142\n"`. When WB-3B lands, a parallel test must verify the Wasm
general-lowered path produces the same output.

**New tests to add in Track CC (revised from WB-3B scope):**
- `closure_capture_general_lowered_matches_interpreter` — same program, Wasm path
- ~~`closure_capture_mutable_snapshot_semantics`~~ — **obsolete**: snapshot semantics are
  superseded; Track CC tests will instead verify shared-cell `mut` capture behavior

**Existing test disposition:**
The existing `typed_mode_matches_fallback_for_lambda_closure_capture` test asserts output
`"4142"` using a `let` binding (`base = 40`). For immutable captures, snapshot semantics and
shared-cell semantics produce identical results, so this test is not expected to change when
`mut`-capture semantics are fixed in CC1+. It exercises real closure-capture parity logic
and should be kept; it does not need to be updated as part of the `mut`-capture fix.

---

## 9. Implementation Phases (WB-3B Scope)

The following changes are **deferred to WB-3B** and are **not part of H5**:

1. `value.rs`: Add `TAG_CLOSURE`, `encode_closure_ptr`; reuse `decode_payload_ptr` for decode.
2. `backend_ir.rs`: Add `CreateClosure`, `ClosureCall` instructions.
3. `lower.rs`:
   - Change `lower_value_as_arg` for capturing lambdas → emit `CreateClosure`.
   - Add a new selective check (`comp_has_free_assign`) that rejects only lambdas that
     *assign to* a captured mutable local (e.g. `Assign { name }` where `name` is free).
     The existing `comp_has_free_var` check blocks **all** capturing lambdas and must be
     relaxed so that read-only captures are allowed while write captures remain rejected.
4. `lower.rs` / `gen_lower/mod.rs`:
   - Add `ClosureAuxDecl` struct (or extend `LambdaAuxDecl` with
     `env_param_name: Option<String>`) to carry the second parameter for closure wrappers.
   - `LambdaAuxDecl.param_name: String` is currently single-param; closure wrappers need
     `(arg, closure_ptr)`. **Decision:** Add a new `ClosureAuxDecl { wrapper_name, param_name, env_param_name, instrs }` rather than extending `LambdaAuxDecl`, to keep the two paths separate.
5. `emit.rs`:
   - Add closure wrapper function emission (sig `(i64, i64) -> i64`).
   - Add `CreateClosure` emission (heap alloc + stores).
   - Add `ClosureCall` emission (tag dispatch + `call_indirect` with type 1).
   - Register type 1 `(i64, i64) -> i64` in the type section when closures are present.
   - Update `ListEach`/`ListMap` or add closure variants (see Section 4).
6. `gen_lower/mod.rs`: Update `UnsupportedIrForm` gate for capturing lambdas
   (see `doc/PLAN_CLOSURE_CAPTURE.md` for the superseded direction — write captures
   are now an explicit goal, not a permanent restriction).
7. Tests: Add parity and shared-cell `mut`-capture tests. (The "snapshot-semantics" test name
   referenced in earlier drafts is superseded — see § 8 for the updated disposition.)

---

## 10. Known Constraints and TODOs

| Constraint | Detail |
|------------|--------|
| Tag exhaustion | 4-bit tag space: 0x0–0x8 used, 0x9–0xF reserved (7 remaining). Future types must allocate from this pool. |
| env_ptr 32-bit limit | closure_ptr is a u32 heap offset. Heap must stay within 4 GiB (safe in practice). |
| Single-param lambda | `LambdaAuxDecl.param_name` is a single `String`. Multi-param lambdas use currying. Closure + currying interaction is WB-3B scope. |
| No GC | Closures are bump-allocated with arena lifetime (no freed until module execution ends). Long-lived programs creating many closures will leak. Acceptable for now. |
| Mutable reference capture | ~~Reference semantics (shared mutable cell) is not supported. Only value capture (snapshot) is designed here.~~ **SUPERSEDED**: shared-cell semantics are now the locked target (see `doc/PLAN_CLOSURE_CAPTURE.md`). |
| Nested closures | A closure returning a closure is possible in principle (closure wrapper emits `CreateClosure`). Correct scope chain is WB-3B scope. |
| `call_indirect` type dispatch | Whether dispatch is static (known at lower time) or dynamic (runtime tag check) is deferred to WB-3B. Both options preserve the `call_indirect` type safety guarantee. |
