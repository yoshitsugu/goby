## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

### 1. `list.length` can still fail code generation in runtime-`Read` programs

Minimal repro:

- [`examples/bugs/runtime_read_list_length_codegen.gb`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/examples/bugs/runtime_read_list_length_codegen.gb)

How to reproduce:

```sh
printf 'x\n' | cargo run -q -p goby-cli -- run examples/bugs/runtime_read_list_length_codegen.gb
```

Current result:

```text
codegen error: general lowering unsupported: stdlib declaration could not be lowered to backend IR
```

Analysis:

- This program only uses `goby/list.length` on a literal list through a small helper.
- The failure happens before Wasm execution, so this is not a user-program runtime bug.
- The current general-lowering path still does not reliably support the stdlib `length`
  declaration in runtime-`Read` programs, even though the source program is simple and
  well-typed.
- This should either lower successfully or report a more precise unsupported feature than the
  generic `stdlib declaration could not be lowered to backend IR`.

### 2. `list.length` can compile but then trap at runtime on `map ... graphemes` results

Minimal repro:

- [`examples/bugs/runtime_read_map_graphemes_length_trap.gb`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/examples/bugs/runtime_read_map_graphemes_length_trap.gb)

How to reproduce:

```sh
printf 'x\n' | cargo run -q -p goby-cli -- run examples/bugs/runtime_read_map_graphemes_length_trap.gb
```

Current result:

```text
runtime error: execution: error while executing at wasm backtrace:
    0: ...
```

Analysis:

- `map lines graphemes` itself is not the direct problem: indexing the resulting nested list and
  iterating it with `each` both work in smaller repros.
- The crash appears when `goby/list.length` consumes that nested list.
- `goby/list.length` is implemented in stdlib via list-pattern recursion:
  `case xs; [] -> 0; [x, ..xxs] -> 1 + length xxs`.
- That points to the current Wasm bug being in the lowered `ListPattern` / tail-list handling
  path rather than in `graphemes` or nested-list indexing themselves.
- The most suspicious ownership boundary is the Wasm emitter path for list-pattern tail rebuilding
  in [`crates/goby-wasm/src/gen_lower/emit.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-wasm/src/gen_lower/emit.rs).

Relationship between the two bugs:

- Both failures involve `goby/list.length`, but they are currently observable as two distinct
  user-facing bugs:
  - one is an early codegen rejection,
  - the other compiles and then traps in Wasm at runtime.
- They may share a deeper root cause in the general-lowering / list-pattern implementation, but
  that has not been fully proven yet.

### 3. H8 follow-up: non-value binary operands still fail in runtime-`Read` helpers

Minimal repro:

- [`examples/bugs/runtime_read_non_value_binop_operand.gb`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/examples/bugs/runtime_read_non_value_binop_operand.gb)

How to reproduce:

```sh
printf 'x\n' | cargo run -q -p goby-cli -- run examples/bugs/runtime_read_non_value_binop_operand.gb
```

Current result:

```text
codegen error: general lowering unsupported: user-defined declaration could not be lowered to backend IR
```

Underlying lowering error:

```text
binary operator right operand must be a pure value expression in shared IR today
```

Analysis:

- The original gap was not specific to recursion; the real issue was that shared IR only accepted
  already-pure value expressions as binary-operator operands.
- A helper call on the right-hand side of `+`, as in `1 + count (n - 1)`, therefore failed before
  backend lowering even though the source program was otherwise valid.
- This is the minimal repro for Track H / H8 and shows that the remaining gap is still active in
  the current toolchain.
- The compiler should ANF-lower the recursive call into a temporary before constructing
  `ValueExpr::BinOp`, instead of collapsing the failure into the generic
  `user-defined declaration could not be lowered to backend IR` message.
