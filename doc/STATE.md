# Goby Project State Snapshot

Last updated: 2026-03-17

## Recently Completed

- Track D1a-iii — Stmt/Expr identifier node spans: **completed** (2026-03-17)
  - `Expr::Var` → named-field variant `{ name, span: Option<Span> }`
  - `Expr::Qualified` and `Expr::Call` also gained `span: Option<Span>`
  - `Stmt::Binding/MutBinding/Assign` gained `span: Option<Span>`; `Stmt::Expr` extended to `Stmt::Expr(Expr, Option<Span>)`
  - `parse_stmts_from_lines` populates Stmt spans with body-relative `Span::point(i+1, indent+1)`
  - Expr::Var/Qualified/Call span population deferred (parse_expr works on trimmed strings with no position context)
  - 6 new Stmt span unit tests; 429 goby-core tests, 778 workspace tests pass
- Track D3a — Symbol index + hover/go-to-definition: **completed** (2026-03-17)
  - `SymbolIndex` / `build_symbol_index()` in `goby-core/src/symbol_index.rs`
  - top-level declarations → (def-line Span, type_annotation); effect members → (Span, signature)
  - `textDocument/hover`: returns `name : annotation` (PlainText) for top-level names; null otherwise
  - `textDocument/definition`: returns `Location` for declaration span; null for unknowns/stdlib
  - `word_at_position()`: UTF-16-aware identifier extraction at cursor
  - 423 goby-core tests, 31 goby-lsp tests (2 end-to-end)
- Track D2b — multi-error collection: **completed** (2026-03-17)
  - `typecheck_module_collect` / `typecheck_module_collect_with_context`: new parallel entrypoints that return `Vec<TypecheckError>` instead of stopping at first error
  - `check_declaration_bodies_collect()`: per-declaration error recovery; module-level errors remain fail-fast
  - goby-cli `check`/`run`: renders all errors joined by blank line; two-error golden fixture test added
  - goby-lsp `analyze()`: returns all per-declaration diagnostics via collect variant; two-diagnostic test added
  - 9 new goby-core unit tests (416 total), 19 goby-cli integration tests, 20 goby-lsp tests passing
- Track D2a — `goby-lsp` crate (diagnostics only): **completed** (2026-03-16)
  - New crate `crates/goby-lsp` added to workspace
  - LSP lifecycle: initialize / shutdown / exit via `lsp-server 0.7` + `lsp-types 0.95`
  - In-memory `DocumentStore` for didOpen/didChange/didClose
  - `span_to_lsp_range()`: Goby 1-indexed byte-offset spans → LSP 0-indexed UTF-16 positions
  - `floor_char_boundary` guard prevents mid-codepoint panic on approximate span cols
- Track D — Developer Tooling Foundation (D1a-ii through D1d): **completed** (2026-03-16)
  - D1a-ii: declaration-level AST node spans (Declaration.col, EffectDecl/EffectMember/HandlerClause/CaseArm spans)
  - D1b: unified `Diagnostic` type with `From<ParseError>` and `From<TypecheckError>`
  - D1c: populate spans at TypecheckError construction sites (annotation + effect decl errors)
  - D1d-0 through D1d-3: Ruby/Elm-style range-underline CLI display with `file:line:col: error:` header, gutter, and `^^^`
  - 409 goby-core tests, 21 goby-cli unit tests, 18 goby-cli integration tests passing
- Track F — List Index Access `l[i]`: **completed** (2026-03-16)
  - All phases F1a–F4 shipped and Codex-reviewed (two full review rounds).
  - See PLAN.md §4.6 for summary.

## Current Focus

- Track D3b — Local binding hover: **completed** (2026-03-17); D3b-fix **completed** (2026-03-17)
  - `infer_local_bindings(decl: &Declaration) -> Vec<LocalBindingSymbol>` in goby-core
  - Walks `parsed_body` stmts (top-level + `Expr::With` body) with a minimal `TypeEnv`
  - `LocalBindingSymbol { name, body_relative_line, body_relative_col, ty_str }` — omits `Ty::Unknown`
  - `hover_at`: column-range guard prevents false positives on same-line RHS occurrences
  - 480 goby-core tests, 42 goby-lsp tests pass
  - Known limitations: use-site hover (Expr::Var spans unpopulated), Handler clause / Expr::Block bindings deferred
- Track D4 — `goby fmt` (AST pretty-printer): **completed** (2026-03-17)
  - `format_module(module: &Module) -> String` in goby-core (Option A, comment-drop)
  - Covers all Expr/Stmt variants; round-trip-safe output (parser-compatible form)
  - `goby fmt <file>` (in-place rewrite) and `goby fmt --check <file>` (exit 1 if not formatted)
  - 17 idempotency tests over examples/; 3 CLI integration tests
  - 475 goby-core tests, 22 goby-cli tests pass
- Track D6a — VS Code extension: **completed** (2026-03-17)
  - `tooling/vscode-goby/` extended with LSP client (vscode-languageclient) + fmt-on-save
  - Binary resolution: `goby.serverPath` / `goby.executablePath` → PATH → workspace `target/`
  - `GOBY_STDLIB_ROOT` passed from `goby.stdlibRoot` setting; async fmt with 10 s timeout
  - Concurrent-save guard; double-save loop prevention; `applyEdit` awaited
  - `npm run package` produces `vscode-goby-0.1.0.vsix`
- Next: Track D6b — Neovim plugin

## Locked Decisions Carried Forward

- Runtime-I/O ownership stays split as:
  - `runtime_io_plan.rs` for classification/planning
  - `backend.rs` for dynamic WASI stdin/stdout lowering helpers
  - CLI as path selector only
- `goby run` must follow planner classification, not codegen error text
- No planner-bypassing runtime-I/O branching in `crates/goby-wasm/src/lib.rs`
- `InterpreterBridge` remains only as a temporary extension point; current reachable surface is empty
- `fetch_env_var` still reads the compiler-process environment at compile time for direct-style and effect-boundary programs.
  Open policy question remains tracked by `TODO(F-sweep)` in `lower.rs`.

## Current Runtime-I/O Boundary

Dynamic (`DynamicWasiIo`):

- echo shapes for `read()` / `read_line()`
- local alias / forwarded alias echo variants
- trailing static print suffixes after echo
- `read` + `split(text, "\n")` + `each` when the callback is output-passthrough
- callback alias / delimiter alias chain variants of that split family
- trailing static print suffixes after `each`
- transformed split-callback family: `|line| -> println "${line}!"` with static
  prefix/suffix decorations

Unsupported:

- runtime-read transforms outside the DynamicWasiIo subset
- important fixed examples:
  - `decorated = "${text}!"` after `text = read()`
  - mixed `read_line()` then `read()`
  - repeated `read()` programs that depend on post-exhaustion behavior in codegen

NotRuntimeIo:

- complex static-output programs with bindings still fall through to fallback resolution
  instead of `StaticOutput` collapse

## Next Slice

Track D2 — LSP and further tooling.

Execution order (matches PLAN.md dependency chain):

1. ~~D1a-i: Span extension and position helpers~~ **(completed)**
2. ~~D1a-ii: Declaration-level AST node spans~~ **(completed)**
3. ~~D1a-iii: Stmt/Expr identifier node spans~~ **(completed — Stmt spans populated; Expr::Var/Call/Qualified span field added, population deferred)**
4. ~~D1b: Unified `Diagnostic` type~~ **(completed)**
5. ~~D1c: TypecheckError span population~~ **(completed — annotation + effect decl sites)**
6. ~~D1d: Ruby/Elm-style range-underline error display~~ **(completed)**
7. ~~D2a: `goby-lsp` crate — diagnostics only~~ **(completed)**
8. ~~D2b: Multi-error collection (`typecheck_module` → `Vec<Diagnostic>`)~~ **(completed)**
9. ~~D3a: Symbol index, top-level hover/go-to-definition~~ **(completed)**
9b. ~~D3b: Local binding hover and stdlib definition~~ **(completed — definition-line hover only; use-site and stdlib jump deferred)**
10. D4: `goby fmt` (AST pretty-printer).
11. D5: `goby lint` (static checks).
12. ~~D6a: VS Code extension (syntax highlighting, LSP client, fmt-on-save).~~ **(completed)**
13. D6b: Neovim plugin (LSP client, fmt-on-save, optional Tree-sitter grammar).
14. D6c: Shared grammar asset (single source of truth for language definition).
