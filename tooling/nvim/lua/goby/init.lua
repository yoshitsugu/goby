--- Goby language plugin for Neovim.
--- Requires Neovim >= 0.8.
---
--- Usage (lazy.nvim):
---   { dir = "/path/to/goby/tooling/nvim", config = function() require("goby").setup() end }
---
--- Usage (manual):
---   vim.opt.rtp:prepend("/path/to/goby/tooling/nvim")
---   require("goby").setup()

local M = {}

--- Whether setup() has already run (prevents double-registration).
local _initialized = false

--- Default option values.
local defaults = {
  --- Absolute path to the goby-lsp binary. Empty = auto-resolve.
  server_path = "",
  --- Absolute path to the goby CLI binary. Empty = auto-resolve.
  goby_path = "",
  --- Stdlib root directory (sets GOBY_STDLIB_ROOT). Empty = inherit from env.
  stdlib_root = "",
  --- Run `goby fmt` on .gb files when saving.
  format_on_save = true,
  --- on_attach callback passed to the LSP client.
  on_attach = nil,
  --- LSP client capabilities. Defaults to vim.lsp.protocol.make_client_capabilities().
  capabilities = nil,
}

--- Merge user opts into defaults (shallow).
local function merge_opts(user_opts)
  local opts = {}
  for k, v in pairs(defaults) do
    opts[k] = v
  end
  if user_opts then
    for k, v in pairs(user_opts) do
      opts[k] = v
    end
  end
  return opts
end

--- Check minimum Neovim version.
local function check_version()
  if vim.fn.has("nvim-0.8") == 0 then
    vim.notify(
      "[goby] Neovim >= 0.8 is required. Please upgrade Neovim.",
      vim.log.levels.WARN
    )
    return false
  end
  return true
end

--- Set up the Goby plugin.
---@param opts? table Optional configuration table (see defaults above).
function M.setup(opts)
  if _initialized then
    return
  end
  if not check_version() then
    return
  end
  _initialized = true

  local merged = merge_opts(opts)

  require("goby.lsp").setup(merged)
  require("goby.format").setup(merged)
end

return M
