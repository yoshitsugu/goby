# vscode-goby

VS Code extension for the [Goby programming language](https://gitlab.com/yoshitsugu/goby) (`.gb` files).

## Features

- Syntax highlighting for all Goby keywords, types, operators, strings, and comments.
- Bracket matching and auto-closing for `()`, `[]`, and `""`.
- Line comment toggle (`#`).
- **LSP integration** via `goby-lsp`: diagnostics (squiggles), hover types, go-to-definition.
- **Format on save** via `goby fmt` (configurable).

## Prerequisites

Build the Goby toolchain from the repository root:

```sh
cargo build -p goby-lsp   # language server
cargo build -p goby-cli   # goby CLI (for fmt-on-save)
```

The binaries will be in `target/debug/`. The extension finds them automatically if you open the
repository root as your VS Code workspace, or if they are on your `PATH`.

### GOBY_STDLIB_ROOT

`goby-lsp` needs to know where the stdlib is. Set this in your shell profile:

```sh
export GOBY_STDLIB_ROOT=/path/to/goby/stdlib
```

Or configure `goby.stdlibRoot` in your VS Code settings (see below).

## Installation

### Option A: Install from VSIX (recommended for local use)

1. Build the `.vsix` from `tooling/vscode-goby/`:
   ```sh
   cd tooling/vscode-goby
   npm install
   npm run package
   ```
2. In VS Code, open the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`).
3. Run **Extensions: Install from VSIX…** and select `vscode-goby-0.1.0.vsix`.
4. Reload VS Code.

### Option B: Extension Development Host (for contributors)

1. Open `tooling/vscode-goby/` in VS Code.
2. Press `F5` to launch a new **Extension Development Host** window.
3. Open any `.gb` file in the host window.

## Settings

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `goby.serverPath` | string | `""` | Absolute path to `goby-lsp`. Leave empty to use PATH or workspace-local `target/`. |
| `goby.executablePath` | string | `""` | Absolute path to the `goby` CLI. Leave empty to use PATH or workspace-local `target/`. |
| `goby.stdlibRoot` | string | `""` | Path to the Goby stdlib root (sets `GOBY_STDLIB_ROOT`). Leave empty to inherit from shell environment. |
| `goby.formatOnSave` | boolean | `true` | Run `goby fmt` on `.gb` files when saving. |

## Commands

| Command | Description |
|---------|-------------|
| `Goby: Restart Language Server` | Stop and restart `goby-lsp` (useful after rebuilding). |

## Manual Syntax Test

See `tooling/syntax/README.md` for step-by-step instructions on verifying token scopes
using **Developer: Inspect Editor Tokens and Scopes**.

## Highlighted Token Categories

| Scope | Examples |
|-------|---------|
| `comment.line.hash.goby` | `# comment` |
| `string.quoted.double.goby` | `"hello\n"` |
| `constant.numeric.integer.goby` | `42` |
| `constant.language.goby` | `True`, `False` |
| `keyword.control.goby` | `if` `else` `case` `with` `in` `resume` `can` |
| `keyword.other.goby` | `@embed` `type` `effect` `handler` `import` `as` `mut` `fn` |
| `storage.type.goby` | `Int` `String` `Bool` `Unit` `List` |
| `entity.name.type.goby` | `MyType`, `Error`, `LogHandler` |
| `keyword.operator.goby` | `->` `\|>` `==` `=` `+` `*` `\|` `:` |
| `punctuation.goby` | `(` `)` `[` `]` `,` `.` |
