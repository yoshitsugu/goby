# Goby Project State Snapshot

Last updated: 2026-03-17

## Recently Completed

- Track D6b — Neovim plugin (goby.nvim): **completed** (2026-03-17)
  - `tooling/nvim/` に Lua プラグインを新規作成
  - `lua/goby/{init,lsp,format,util}.lua`: setup() + LSP + fmt-on-save
  - nvim-lspconfig 優先 / vim.lsp.start fallback (Neovim >= 0.8)
  - Cargo.toml 上昇探索でバイナリを解決; augroup clear=true; filetype guard
  - `b:undo_ftplugin`; `&&`, `<`, `>` 演算子と負数リテラルのシンタックス追加
- Track D6a — VS Code extension: **completed** (2026-03-17)
  - `tooling/vscode-goby/` を LSP client (vscode-languageclient) + fmt-on-save で拡張
  - esbuild bundle; binary resolution (設定 → PATH → workspace `target/`)
  - 同時保存ガード・double-save ループ防止・`applyEdit` awaited
  - `npm run package` → `vscode-goby-0.1.0.vsix`
- Track D3b-fix — Local binding hover バグ修正: **completed** (2026-03-17)
  - `body_relative_col` 追加 + `hover_at` にカラム範囲ガード (同行 RHS 誤判定を修正)
  - `infer_local_bindings` が `Expr::With` body の binding を収集するよう拡張
  - 480 goby-core tests, 42 goby-lsp tests pass
- Track D4 — `goby fmt`: **completed** (2026-03-17)
  - `format_module(module: &Module) -> String` in goby-core (comment-drop)
  - `goby fmt <file>` (in-place) / `goby fmt --check <file>` (exit 1 if not formatted)
  - 17 idempotency tests over examples/; 3 CLI integration tests
- Track D3b — Local binding hover: **completed** (2026-03-17)
  - `infer_local_bindings` + `LocalBindingSymbol { name, body_relative_line, body_relative_col, ty_str }`
  - Known limitations: use-site hover (Expr::Var spans unpopulated), Handler/Block bindings deferred
- Track D3a — Symbol index + hover/go-to-definition: **completed** (2026-03-17)
  - `SymbolIndex` / `build_symbol_index()` in `goby-core/src/symbol_index.rs`
  - `textDocument/hover`: `name : annotation`; `textDocument/definition`: declaration Location
- Track D2b — multi-error collection: **completed** (2026-03-17)
  - `typecheck_module_collect` / `typecheck_module_collect_with_context`
  - goby-cli / goby-lsp が全エラーを一括報告
- Track D2a — `goby-lsp` crate (diagnostics): **completed** (2026-03-16)
  - LSP lifecycle, DocumentStore, `span_to_lsp_range()`
- Track D — Developer Tooling Foundation (D1a-ii〜D1d): **completed** (2026-03-16)
  - D1a-ii: declaration-level AST spans
  - D1a-iii: Stmt/Expr identifier spans (Expr::Var span field追加; population は deferred)
  - D1b: unified `Diagnostic` type
  - D1c: TypecheckError span population
  - D1d: Ruby/Elm-style range-underline CLI display
- Track F — List Index Access `l[i]`: **completed** (2026-03-16)
  - F1a–F4 全フェーズ完了。詳細は PLAN.md §4.6。

## Current Focus

- Next: **Track D6c** — Shared grammar asset
  - VS Code の `goby.tmLanguage.json` と Neovim の `syntax/goby.vim` を共通ソースから生成
- **Track D5** (`goby lint`) は構文が固まるまで保留

## Next Steps (execution order)

1. ~~D1a-i〜D1d~~ **(completed)**
2. ~~D2a: goby-lsp crate~~ **(completed)**
3. ~~D2b: multi-error collection~~ **(completed)**
4. ~~D3a: symbol index, top-level hover/go-to-def~~ **(completed)**
5. ~~D3b: local binding hover~~ **(completed)**
6. ~~D4: goby fmt~~ **(completed)**
7. D5: goby lint — **保留** (構文が固まり次第着手)
8. ~~D6a: VS Code extension~~ **(completed)**
9. ~~D6b: Neovim plugin~~ **(completed)**
10. D6c: Shared grammar asset — **次**
11. D6b-ts: Tree-sitter grammar (D6c 後)

## Locked Decisions Carried Forward

- Runtime-I/O ownership stays split:
  - `runtime_io_plan.rs` for classification/planning
  - `backend.rs` for dynamic WASI stdin/stdout lowering helpers
  - CLI as path selector only
- `goby run` must follow planner classification, not codegen error text
- No planner-bypassing runtime-I/O branching in `crates/goby-wasm/src/lib.rs`
- `InterpreterBridge` remains only as a temporary extension point; current reachable surface is empty
- `fetch_env_var` reads the compiler-process environment at compile time.
  Open policy question tracked by `TODO(F-sweep)` in `lower.rs`.

## Current Runtime-I/O Boundary

Dynamic (`DynamicWasiIo`):

- echo shapes for `read()` / `read_line()`
- local alias / forwarded alias echo variants
- trailing static print suffixes after echo
- `read` + `split(text, "\n")` + `each` when the callback is output-passthrough
- callback alias / delimiter alias chain variants of that split family
- trailing static print suffixes after `each`
- transformed split-callback family: `|line| -> println "${line}!"` with static prefix/suffix decorations

Unsupported:

- runtime-read transforms outside the DynamicWasiIo subset
- `decorated = "${text}!"` after `text = read()`
- mixed `read_line()` then `read()`
- repeated `read()` programs that depend on post-exhaustion behavior in codegen

NotRuntimeIo:

- complex static-output programs with bindings still fall through to fallback resolution instead of `StaticOutput` collapse
