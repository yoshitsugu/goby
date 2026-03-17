--- Shared utilities for the Goby Neovim plugin.

local M = {}

--- Walk upward from `start_dir` looking for a `Cargo.toml` file.
--- Returns the directory containing `Cargo.toml`, or nil if not found.
---@param start_dir string
---@param max_levels? integer defaults to 5
---@return string|nil
function M.find_cargo_root(start_dir, max_levels)
  local dir = start_dir
  for _ = 1, (max_levels or 5) do
    if vim.fn.filereadable(dir .. "/Cargo.toml") == 1 then
      return dir
    end
    local parent = vim.fn.fnamemodify(dir, ":h")
    if parent == dir then
      break
    end
    dir = parent
  end
  return nil
end

--- Return a comparable mtime value (sec * 1e9 + nsec) from a uv stat result.
---@param stat table|nil result of vim.uv.fs_stat or vim.loop.fs_stat
---@return number
function M.mtime_val(stat)
  if not stat then
    return 0
  end
  return stat.mtime.sec * 1000000000 + (stat.mtime.nsec or 0)
end

--- Stat a file path using vim.uv (>= 0.10) or vim.loop (0.8/0.9 fallback).
---@param path string
---@return table|nil
function M.fs_stat(path)
  local uv = vim.uv or vim.loop
  return uv.fs_stat(path)
end

return M
