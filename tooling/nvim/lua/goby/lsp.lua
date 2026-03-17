--- Goby LSP client setup.
--- Supports nvim-lspconfig (preferred) or bare vim.lsp.start (fallback).

local M = {}
local util = require("goby.util")

-- ---------------------------------------------------------------------------
-- Binary resolution
-- ---------------------------------------------------------------------------

--- Resolve the goby-lsp binary.
--- Order: opts.server_path → exepath("goby-lsp") → Cargo.toml root/target/{debug,release}/goby-lsp
--- The Cargo.toml search is seeded from `buf_dir` (the directory of the file being opened),
--- so it works regardless of Neovim's current working directory.
---@param opts table
---@param buf_dir? string directory to start Cargo.toml search (defaults to getcwd)
---@return string|nil
local function resolve_server_path(opts, buf_dir)
  if opts.server_path and opts.server_path ~= "" then
    if vim.fn.executable(opts.server_path) == 1 then
      return opts.server_path
    end
    vim.notify(
      string.format("[goby] server_path '%s' is not executable; falling back.", opts.server_path),
      vim.log.levels.WARN
    )
  end

  local on_path = vim.fn.exepath("goby-lsp")
  if on_path ~= "" then
    return on_path
  end

  local search_dir = buf_dir or vim.fn.getcwd()
  local cargo_root = util.find_cargo_root(search_dir)
  if cargo_root then
    for _, sub in ipairs({ "debug", "release" }) do
      local p = cargo_root .. "/target/" .. sub .. "/goby-lsp"
      if vim.fn.executable(p) == 1 then
        return p
      end
    end
  end

  return nil
end

-- ---------------------------------------------------------------------------
-- LSP setup
-- ---------------------------------------------------------------------------

--- Build the lspconfig default_config / vim.lsp.start config table.
---@param server_path string
---@param opts table
---@return table
local function make_lsp_config(server_path, opts)
  local env = {}
  if opts.stdlib_root and opts.stdlib_root ~= "" then
    env["GOBY_STDLIB_ROOT"] = opts.stdlib_root
  end

  return {
    cmd = { server_path },
    filetypes = { "goby" },
    root_dir = function(fname)
      local ok, lutil = pcall(require, "lspconfig.util")
      if ok then
        return lutil.root_pattern("Cargo.toml")(fname) or lutil.path.dirname(fname)
      end
      local fdir = vim.fn.fnamemodify(fname, ":h")
      return util.find_cargo_root(fdir) or fdir
    end,
    single_file_support = true,
    cmd_env = next(env) ~= nil and env or nil,
  }
end

--- Set up Goby LSP via nvim-lspconfig (preferred).
---@param server_path string
---@param opts table
local function setup_with_lspconfig(server_path, opts)
  local lspconfig = require("lspconfig")
  local configs = require("lspconfig.configs")

  if not configs.goby_lsp then
    configs.goby_lsp = {
      default_config = make_lsp_config(server_path, opts),
    }
  end

  lspconfig.goby_lsp.setup({
    on_attach = opts.on_attach,
    capabilities = opts.capabilities or vim.lsp.protocol.make_client_capabilities(),
  })
end

--- Set up Goby LSP via bare vim.lsp.start (fallback, Neovim >= 0.8).
---@param server_path string
---@param opts table
local function setup_without_lspconfig(server_path, opts)
  local lsp_config = make_lsp_config(server_path, opts)

  vim.api.nvim_create_autocmd("FileType", {
    pattern = "goby",
    group = vim.api.nvim_create_augroup("GobyLsp", { clear = true }),
    callback = function(ev)
      local fname = vim.api.nvim_buf_get_name(ev.buf)
      if fname == "" then return end -- unnamed buffer; skip
      vim.lsp.start({
        name = "goby-lsp",
        cmd = lsp_config.cmd,
        root_dir = lsp_config.root_dir(fname),
        on_attach = opts.on_attach,
        capabilities = opts.capabilities or vim.lsp.protocol.make_client_capabilities(),
        cmd_env = lsp_config.cmd_env,
      })
    end,
  })
end

--- Main LSP setup entry point called from init.lua.
---@param opts table
function M.setup(opts)
  -- Resolve using getcwd() at setup time; the fallback path in setup_without_lspconfig
  -- resolves per-buffer via the autocmd callback.
  local server_path = resolve_server_path(opts)
  if not server_path then
    vim.notify(
      "[goby] goby-lsp not found. Set opts.server_path or add goby-lsp to your PATH. "
        .. "Build with: cargo build -p goby-lsp",
      vim.log.levels.INFO
    )
    return
  end

  local ok = pcall(require, "lspconfig")
  if ok then
    setup_with_lspconfig(server_path, opts)
  else
    setup_without_lspconfig(server_path, opts)
  end
end

return M
