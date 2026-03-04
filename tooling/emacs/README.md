# goby-mode.el

Emacs major mode for the [Goby programming language](https://gitlab.com/yoshitsugu/goby) (`.gb` files).

## Features

- Syntax highlighting (`font-lock`) for all Goby token categories.
- Line comment toggle with `M-;` (`comment-start` set to `"# "`).
- Bracket matching for `()` and `[]`.
- String escape sequence highlighting (`\n`, `\t`, `\\`, `\"`).

> Indentation and electric features are not included in this MVP release.

## Installation

### Manual

```emacs-lisp
(add-to-list 'load-path "/path/to/goby/tooling/emacs")
(require 'goby-mode)
```

### use-package

```emacs-lisp
(use-package goby-mode
  :load-path "path/to/goby/tooling/emacs")
```

After loading, `goby-mode` is automatically activated for `*.gb` files via `auto-mode-alist`.

## Highlighted Token Categories

| Emacs face | Token category | Examples |
|---|---|---|
| `font-lock-comment-face` | Line comments | `# comment` |
| `font-lock-string-face` | String literals | `"hello\n"` |
| `font-lock-constant-face` | Integer literals + booleans | `42`, `True`, `False` |
| `font-lock-keyword-face` | Control keywords | `if` `else` `case` `with` `with_handler` `in` `resume` `can` |
| `font-lock-keyword-face` | Declaration keywords | `@embed` `type` `effect` `handler` `import` `as` `mut` |
| `font-lock-type-face` | Built-in types | `Int` `String` `Bool` `Unit` `List` |
| `font-lock-type-face` | User-defined types / constructors | `MyType`, `Error`, `LogHandler` |
| `font-lock-builtin-face` | Operators | `->` `\|>` `==` `=` `+` `*` `\|` `:` |

## Manual Test

```
emacs tooling/syntax/testdata/highlight_sample.gb
```

Place the cursor on a token and run `M-x describe-char` to inspect the `face` property.
See `tooling/syntax/README.md` for the full expected-scope table.
