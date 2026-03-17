--- Goby fmt-on-save and :GobyFormat command.

local M = {}
local util = require("goby.util")

--- Resolved goby binary path (set during setup, reused by :GobyFormat).
--- Note: resolved at setup time; if the binary is rebuilt, restart Neovim or
--- re-call setup() after clearing package.loaded["goby"].
M._goby_bin = nil

-- ---------------------------------------------------------------------------
-- Binary resolution
-- ---------------------------------------------------------------------------

--- Resolve the goby CLI binary.
--- Order: opts.goby_path → exepath("goby") → Cargo.toml root/target/{debug,release}/goby
--- The Cargo.toml search is seeded from `buf_dir` (directory of the current buffer),
--- so it works regardless of Neovim's cwd.
---@param opts table
---@param buf_dir? string directory to start Cargo.toml search (defaults to getcwd)
---@return string|nil
local function resolve_goby_path(opts, buf_dir)
  if opts.goby_path and opts.goby_path ~= "" then
    if vim.fn.executable(opts.goby_path) == 1 then
      return opts.goby_path
    end
    vim.notify(
      string.format("[goby] goby_path '%s' is not executable; falling back.", opts.goby_path),
      vim.log.levels.WARN
    )
  end

  local on_path = vim.fn.exepath("goby")
  if on_path ~= "" then
    return on_path
  end

  local search_dir = buf_dir or vim.fn.getcwd()
  local cargo_root = util.find_cargo_root(search_dir)
  if cargo_root then
    for _, sub in ipairs({ "debug", "release" }) do
      local p = cargo_root .. "/target/" .. sub .. "/goby"
      if vim.fn.executable(p) == 1 then
        return p
      end
    end
  end

  return nil
end

-- ---------------------------------------------------------------------------
-- Format implementation
-- ---------------------------------------------------------------------------

--- Run `goby fmt` on the file backing `bufnr`, then safely reload the buffer.
---@param bufnr? integer defaults to current buffer
function M.format(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  if not M._goby_bin then
    return -- goby binary not found; silent skip
  end

  local filepath = vim.api.nvim_buf_get_name(bufnr)
  if filepath == "" then
    return
  end

  -- Record mtime before formatting (sub-second precision via nsec).
  local mtime_before = util.mtime_val(util.fs_stat(filepath))

  vim.fn.jobstart({ M._goby_bin, "fmt", filepath }, {
    on_exit = function(_, exit_code, _)
      if exit_code ~= 0 then
        -- on_exit runs on the main Lua thread; vim.schedule not needed for notify.
        vim.notify(
          string.format("[goby] goby fmt exited with code %d", exit_code),
          vim.log.levels.WARN
        )
        return
      end

      local mtime_after = util.mtime_val(util.fs_stat(filepath))
      if mtime_after == mtime_before then
        return -- file unchanged; skip reload
      end

      -- Safely reload: set autoread + checktime, then restore autoread.
      -- vim.schedule is required here so checktime fires in a clean scheduler tick.
      vim.schedule(function()
        if not vim.api.nvim_buf_is_valid(bufnr) then return end
        local prev_autoread = vim.bo[bufnr].autoread
        vim.bo[bufnr].autoread = true
        vim.cmd("checktime " .. bufnr)
        -- Restore autoread after checktime has been processed.
        vim.schedule(function()
          if vim.api.nvim_buf_is_valid(bufnr) then
            vim.bo[bufnr].autoread = prev_autoread
          end
        end)
      end)
    end,
  })
end

-- ---------------------------------------------------------------------------
-- Setup
-- ---------------------------------------------------------------------------

--- Set up fmt-on-save autocmd and :GobyFormat command.
---@param opts table
function M.setup(opts)
  M._goby_bin = resolve_goby_path(opts)
  if not M._goby_bin then
    vim.notify(
      "[goby] goby CLI not found. Set opts.goby_path or add goby to your PATH. "
        .. "Build with: cargo build -p goby-cli",
      vim.log.levels.INFO
    )
  end

  -- :GobyFormat user command (force=true is safe on re-load/reload scenarios).
  vim.api.nvim_create_user_command("GobyFormat", function()
    M.format(vim.api.nvim_get_current_buf())
  end, { desc = "Format the current Goby buffer with goby fmt", force = true })

  -- fmt-on-save autocmd (match by filetype, not glob, for reliability).
  if opts.format_on_save then
    local aug = vim.api.nvim_create_augroup("GobyFormat", { clear = true })
    vim.api.nvim_create_autocmd("BufWritePost", {
      group = aug,
      callback = function(ev)
        if vim.bo[ev.buf].filetype == "goby" then
          M.format(ev.buf)
        end
      end,
    })
  end
end

return M
