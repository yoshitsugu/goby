## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

### 1. Runtime-`Read` programs reject helper lambdas that capture outer locals

Minimal repro: [examples/bugs/runtime_read_captured_lambda.gb](/home/yoshitsugu/src/github.com/yoshitsugu/goby/examples/bugs/runtime_read_captured_lambda.gb)

Repro command:

```sh
cat /dev/null | cargo run -p goby-cli -- run examples/bugs/runtime_read_captured_lambda.gb
```

Observed result:

```text
codegen error: runtime I/O program shape is currently unsupported: no dynamic Wasm lowering exists and it is outside the temporary interpreter-bridge subset
```

Expected result:

- The program should either compile and run, or fail with a specific diagnostic that points to unsupported closure capture in the lambda body.

Analysis:

- The helper lambda `|x| -> total := total + x` captures the outer mutable local `total`.
- Current Wasm general lowering rejects lambdas with free variables because WB-3A has no closure representation yet.
- This rejection happens during lowering of non-`main` declarations referenced by a runtime-`Read` program.
- Once that lowering returns `None`, runtime-I/O classification falls through to `Unsupported`, which hides the real cause behind a generic runtime-I/O error.

Relevant implementation points:

- Closure capture rejection: [crates/goby-wasm/src/gen_lower/lower.rs](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-wasm/src/gen_lower/lower.rs#L774)
- Auxiliary declaration lowering gate: [crates/goby-wasm/src/gen_lower/mod.rs](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-wasm/src/gen_lower/mod.rs#L654)
- Generic runtime-I/O fallback to `Unsupported`: [crates/goby-wasm/src/runtime_io_plan.rs](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-wasm/src/runtime_io_plan.rs#L320)

Why this is a bug:

- The observed error message is misleading. The failing shape is not fundamentally "runtime I/O"; the real unsupported feature is closure capture inside a helper lambda.
- The same helper without `Read` does not surface the same error path, so the runtime-I/O classification currently masks a more precise compiler limitation.

### 2. Tuple member access in a general-lowered runtime program is emitted as an unknown local

Minimal repro: [examples/bugs/runtime_read_tuple_member.gb](/home/yoshitsugu/src/github.com/yoshitsugu/goby/examples/bugs/runtime_read_tuple_member.gb)

Repro command:

```sh
cat /dev/null | cargo run -p goby-cli -- run examples/bugs/runtime_read_tuple_member.gb
```

Observed result:

```text
runtime error: gen_lower/emit: unknown local 'pair.0'
```

Expected result:

- The program should print `1`, or the compiler should reject tuple member access before code generation with a precise unsupported-feature diagnostic.

Analysis:

- The expression `pair.0` is tuple member access on a local tuple value.
- In the current general-lowering path, that access survives long enough to be treated like a local named `pair.0` instead of lowering to an actual tuple projection.
- The emitter then fails because no Wasm local with that name exists.

Relevant implementation points:

- Tuple member access is still represented through qualified/member syntax on the AST side and is handled dynamically in the interpreter/runtime path, but not lowered correctly for the Wasm general path.
- The runtime interpreter has explicit tuple-member handling for `Expr::Qualified { receiver, member, .. }`: [crates/goby-wasm/src/runtime_resolver.rs](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-wasm/src/runtime_resolver.rs#L636)

Why this is a bug:

- This is neither a user error nor a valid runtime failure mode; it is a backend lowering bug.
- The backend should not leak internal pseudo-local names like `pair.0` into emitter diagnostics.
