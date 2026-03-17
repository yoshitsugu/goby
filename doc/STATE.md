# Goby Project State Snapshot

Last updated: 2026-03-16

## Recently Completed

- Track D2a — `goby-lsp` crate (diagnostics only): **completed** (2026-03-16)
  - New crate `crates/goby-lsp` added to workspace
  - LSP lifecycle: initialize / shutdown / exit via `lsp-server 0.7` + `lsp-types 0.95`
  - In-memory `DocumentStore` for didOpen/didChange/didClose
  - `analyze()`: parse + typecheck → at most one `lsp_types::Diagnostic` per run
  - `span_to_lsp_range()`: Goby 1-indexed byte-offset spans → LSP 0-indexed UTF-16 positions
  - `floor_char_boundary` guard prevents mid-codepoint panic on approximate span cols
  - 19 tests: span conversion (ASCII, 2/3/4-byte UTF-8), document store, analyze, lifecycle, end-to-end
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

- Next: Track D2b — multi-error collection (`typecheck_module` → `Vec<Diagnostic>`)

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
3. D1a-iii: Stmt/Expr identifier node spans. **(deferred — requires exhaustive match audit)**
4. ~~D1b: Unified `Diagnostic` type~~ **(completed)**
5. ~~D1c: TypecheckError span population~~ **(completed — annotation + effect decl sites)**
6. ~~D1d: Ruby/Elm-style range-underline error display~~ **(completed)**
7. ~~D2a: `goby-lsp` crate — diagnostics only~~ **(completed)**
8. D2b: Multi-error collection (`typecheck_module` → `Vec<Diagnostic>`).
9. D3a/D3b: Symbol index, hover, go-to-definition.
10. D4: `goby fmt` (AST pretty-printer).
11. D5: `goby lint` (static checks).
