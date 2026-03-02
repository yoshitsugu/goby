# vscode-goby

Syntax highlighting for the [Goby programming language](https://gitlab.com/yoshitsugu/goby) (`.gb` files).

## Features

- Syntax highlighting for all Goby keywords, types, operators, strings, and comments.
- Bracket matching and auto-closing for `()`, `[]`, and `""`.
- Line comment toggle (`#`).

## Installation

### Option A: Install from Folder (development/local use)

1. Open VS Code.
2. Open the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`).
3. Run **Extensions: Install from VSIX…** — or use **Extensions: Install from Folder…**
   and select this `tooling/vscode-goby/` directory.
4. Reload VS Code when prompted.

### Option B: Extension Development Host (for contributors)

1. Open the `tooling/vscode-goby/` directory in VS Code.
2. Press `F5` to launch a new **Extension Development Host** window.
3. Open any `.gb` file in the host window to verify highlighting.

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
| `keyword.control.goby` | `if` `else` `case` `using` `can` |
| `keyword.other.goby` | `type` `effect` `handler` `for` `import` `as` |
| `storage.type.goby` | `Int` `String` `Bool` `Unit` `List` |
| `entity.name.type.goby` | `MyType`, `Error`, `LogHandler` |
| `keyword.operator.goby` | `->` `\|>` `==` `=` `+` `*` `\|` `:` |
| `punctuation.goby` | `(` `)` `[` `]` `,` `.` |
