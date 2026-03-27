## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

*(No active bugs — all Track H follow-ups were resolved in commits 6d13891e and 17dfd076.)*

---

### Resolved: Track H Follow-ups (resolved 2026-03-27)

The three bugs below were confirmed fixed. Their repro files remain under `examples/bugs/` as
regression fixtures.

#### 1. `list.length` codegen failure in runtime-`Read` programs

**Repro:** `examples/bugs/runtime_read_list_length_codegen.gb`

**Was:**
```text
codegen error: general lowering unsupported: stdlib declaration could not be lowered to backend IR
```

**Root cause:** `stdlib/goby/list.gb` の `1 + length xxs` が BinOp の right operand に
non-value expression を持つため、`ir_lower` の `lower_value_required` でエラーになっていた。

**Fix:** H3 (ANF lowering for non-value binary operands) で自動解決。commit `6d13891e`

#### 2. `list.length` runtime trap on `map ... graphemes` results

**Repro:** `examples/bugs/runtime_read_map_graphemes_length_trap.gb`

**Was:**
```text
runtime error: execution: error while executing at wasm backtrace: ...
```

**Root cause:** `import goby/list (length)` と `import goby/string (graphemes)` を同時使用すると
`build_stdlib_export_map` の transitive import 処理順により `goby/string.length` が
`goby/list.length` を上書きし、`length` aux decl が `StringLength` intrinsic に誤解決されていた。

**Fix:** 直接インポートを transitive imports より先に処理するよう修正。commit `17dfd076`

#### 3. Non-value binary operands fail in runtime-`Read` helpers

**Repro:** `examples/bugs/runtime_read_non_value_binop_operand.gb`

**Was:**
```text
codegen error: general lowering unsupported: user-defined declaration could not be lowered to backend IR
```

**Root cause:** `ir_lower` の `try_lower_value` 内 BinOp ハンドラが `lower_value_required` で
non-value オペランド（再帰呼び出しなど）を拒否していた。

**Fix:** `lower_binop_anf` を新設し、non-value オペランドを ANF Let でホイスト。commit `6d13891e`
