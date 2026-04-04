## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

### Dynamic String Equality Fails In GeneralLowered Runtime

- Status: confirmed on 2026-04-04
- Scope: `Read` programs classified as `GeneralLowered`, especially `read_lines ()` combined with `string.graphemes`
- Symptom: a dynamically produced string such as `rows[0][0]` prints as `@`, but `rows[0][0] == "@"` evaluates to `False`
- Expected: string equality should compare string contents, so identical text should yield `True`
- Likely cause: the GeneralLowered Wasm `Eq` path compares tagged values directly, which is correct for immediate values but degrades to pointer identity for strings
- Minimal repro test: `execute_runtime_module_with_stdin_compares_dynamic_grapheme_strings_by_contents` in [`crates/goby-wasm/src/lib.rs`](/home/yoshitsugu/src/github.com/yoshitsugu/goby/crates/goby-wasm/src/lib.rs)
