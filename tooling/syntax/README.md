# Goby Syntax Highlighting

This directory contains the canonical syntax highlighting grammar for the Goby language (`.gb` files).

## Token Categories

| Scope | Description | Examples |
|-------|-------------|---------|
| `comment.line.hash.goby` | Line comments | `# ...` |
| `string.quoted.double.goby` | String literals | `"hello\n"` |
| `constant.numeric.integer.goby` | Integer literals | `42` |
| `constant.language.goby` | Boolean constants | `True`, `False` |
| `keyword.control.goby` | Control flow + effect application | `if` `else` `case` `with` `with_handler` `in` `resume` `can` |
| `keyword.other.goby` | Declarations and module keywords | `@embed` `type` `effect` `handler` `import` `as` `mut` |
| `storage.type.goby` | Built-in type names | `Int` `String` `Bool` `Unit` `List` |
| `entity.name.type.goby` | User-defined type / constructor names | `MyType`, `Error`, `LogHandler` |
| `keyword.operator.goby` | Operators | `->` `\|>` `==` `=` `+` `*` `\|` `:` |
| `punctuation.goby` | Brackets and separators | `(` `)` `[` `]` `,` `.` |

Note: declaration names (lowerCamelCase / snake_case identifiers) are not assigned a specific
scope in the regex-only grammar MVP. They render with the editor's default `variable` color.

## Directory Structure

```
tooling/syntax/
  textmate/
    goby.tmLanguage.json   # Canonical TextMate grammar (source of truth)
  testdata/
    highlight_sample.gb    # Representative snippet for manual verification
  README.md                # This file

tooling/vscode-goby/
  package.json             # VS Code extension manifest
  syntaxes/
    goby.tmLanguage.json   # Copy of the canonical grammar
  README.md                # Install instructions
```

## Manual Test Instructions (VSCode)

1. Open the repository root in VSCode.
2. Run **Extensions: Install from Folder…** and select `tooling/vscode-goby/`.
   (Or press F5 in the extension directory to launch an Extension Development Host.)
3. Open `tooling/syntax/testdata/highlight_sample.gb`.
4. Place the cursor on each token and open
   **Developer: Inspect Editor Tokens and Scopes** (Command Palette).
5. Verify that the reported scope matches the expected scope from the table above.

### Expected scopes per line in `highlight_sample.gb`

| Line content | Expected scope |
|---|---|
| `# comment …` | `comment.line.hash.goby` |
| `"hello\nworld…"` | `string.quoted.double.goby` |
| `42` | `constant.numeric.integer.goby` |
| `True` / `False` | `constant.language.goby` |
| `if` / `else` / `case` / `with` / `with_handler` / `in` / `resume` / `can` | `keyword.control.goby` |
| `@embed` / `type` / `effect` / `handler` / `import` / `as` / `mut` | `keyword.other.goby` |
| `Int` / `String` / `Bool` / `Unit` / `List` | `storage.type.goby` |
| `MyType` / `MyHandler` (UpperCamelCase) | `entity.name.type.goby` |
| `->` / `\|>` / `==` | `keyword.operator.goby` |
| `(` / `[` / `,` | `punctuation.goby` |
