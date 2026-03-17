# goby.nvim

Neovim plugin for the [Goby programming language](https://gitlab.com/yoshitsugu/goby) (`.gb` files).

**Requires Neovim >= 0.8.**

## Features

- Filetype detection for `.gb` files.
- Syntax highlighting (shared with the Vim plugin).
- Indentation settings (`expandtab`, `shiftwidth=2`).
- **LSP integration** via `goby-lsp`: diagnostics, hover, go-to-definition.
  - Works with or without [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig).
- **Format on save** via `goby fmt` (configurable).
- `:GobyFormat` command for manual formatting.

## Prerequisites

Build the Goby toolchain from the repository root:

```sh
cargo build -p goby-lsp   # language server
cargo build -p goby-cli   # goby CLI (for fmt-on-save)
```

Binaries will be in `target/debug/`. The plugin finds them automatically if you open the
repository root in Neovim, or if they are on your `PATH`.

### GOBY_STDLIB_ROOT

`goby-lsp` needs to know where the stdlib is. Set this in your shell profile:

```sh
export GOBY_STDLIB_ROOT=/path/to/goby/stdlib
```

Or pass it as `opts.stdlib_root` (see below).

## Installation

### lazy.nvim

```lua
{
  dir = "/path/to/goby/tooling/nvim",
  ft = "goby",
  config = function()
    require("goby").setup()
  end,
}
```

### packer.nvim

```lua
use {
  "/path/to/goby/tooling/nvim",
  config = function()
    require("goby").setup()
  end,
}
```

### Manual (runtimepath)

```lua
-- In your init.lua:
vim.opt.rtp:prepend("/path/to/goby/tooling/nvim")
require("goby").setup()
```

## Configuration

```lua
require("goby").setup({
  -- Absolute path to goby-lsp binary. Default: auto-resolve (PATH → target/).
  server_path = "",

  -- Absolute path to goby CLI binary. Default: auto-resolve (PATH → target/).
  goby_path = "",

  -- Stdlib root directory. Sets GOBY_STDLIB_ROOT. Default: inherit from env.
  stdlib_root = "",

  -- Run `goby fmt` on .gb files when saving. Default: true.
  format_on_save = true,

  -- LSP on_attach callback. Default: nil.
  on_attach = function(client, bufnr)
    -- e.g. set keymaps here
  end,

  -- LSP capabilities. Default: vim.lsp.protocol.make_client_capabilities().
  capabilities = nil,
})
```

### Example with explicit paths

```lua
require("goby").setup({
  server_path = vim.fn.expand("~/src/goby/target/debug/goby-lsp"),
  goby_path   = vim.fn.expand("~/src/goby/target/debug/goby"),
  stdlib_root = vim.fn.expand("~/src/goby/stdlib"),
  on_attach = function(_, bufnr)
    local opts = { buffer = bufnr }
    vim.keymap.set("n", "K",  vim.lsp.buf.hover,          opts)
    vim.keymap.set("n", "gd", vim.lsp.buf.definition,     opts)
    vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, opts)
  end,
})
```

## Commands

| Command | Description |
|---------|-------------|
| `:GobyFormat` | Format the current buffer with `goby fmt`. |

## Server Resolution Order

For both `goby-lsp` and `goby`:

1. `opts.server_path` / `opts.goby_path` (explicit setting)
2. `PATH` lookup (`exepath`)
3. `target/debug/<binary>` under the nearest `Cargo.toml` directory
4. `target/release/<binary>` under the nearest `Cargo.toml` directory

If none is found, a notification is shown and the feature is silently disabled.
