# Goby Vim Syntax

Vim syntax highlighting for the [Goby programming language](https://gitlab.com/yoshitsugu/goby) (`.gb` files).

## Files

```
tooling/vim/
  syntax/goby.vim      # Syntax definitions
  ftdetect/goby.vim    # Auto-detects *.gb files as filetype goby
```

## Installation

### Manual

Copy the files into your Vim runtime path:

```sh
mkdir -p ~/.vim/syntax ~/.vim/ftdetect
cp tooling/vim/syntax/goby.vim  ~/.vim/syntax/
cp tooling/vim/ftdetect/goby.vim ~/.vim/ftdetect/
```

### vim-plug / Vundle / lazy.nvim (local plugin)

Point your plugin manager at the `tooling/vim/` directory.

**vim-plug:**
```vim
Plug '/path/to/goby/tooling/vim'
```

**lazy.nvim:**
```lua
{ dir = "/path/to/goby/tooling/vim" }
```

## Highlighted Token Categories

| Vim group | Token category | Examples |
|---|---|---|
| `Comment` | Line comments | `# comment` |
| `String` | String literals | `"hello\n"` |
| `SpecialChar` | Escape sequences | `\n` `\t` `\\` `\"` |
| `Number` | Integer literals | `42` |
| `Boolean` | Boolean constants | `True`, `False` |
| `Keyword` | Control + declaration keywords | `if` `else` `case` `with` `@embed` `type` … |
| `Type` | Built-in types + user-defined types | `Int`, `MyType` |
| `Operator` | Operators | `->` `\|>` `\|\|` `&&` `==` `<=` `>=` `=` `<` `>` `+` `-` `*` `/` `%` `!` `\|` `:` |

## Manual Test

```sh
vim -Nu NONE -c "set rtp+=tooling/vim" tooling/syntax/testdata/highlight_sample.gb
```

Move the cursor to a token and run `:echo synIDattr(synID(line('.'),col('.'),1),'name')` to inspect the syntax group.
