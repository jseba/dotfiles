util = require("config.util")

local function set_statusline_bg(item, color)
    vim.api.nvim_set_hl(0, "GalaxyLine"..item, {bg = color})
end
local function set_statusline_fg(item, color)
    vim.api.nvim_set_hl(0, "GalaxyLine"..item, {fg = color})
end
local function set_statusline_hi(item, fg, bg)
    vim.api.nvim_set_hl(0, "GalaxyLine"..item, {fg = fg, bg = bg})
end

local M = {
    icons = {
        slant_right = "",
    },
    colors = {
    },
}

local vimode = {
    modes = {
        c      = {M.icons.ViModeConsole, "plum3"},
        ce     = {M.icons.ViModeConsole, "plum3"},
        cv     = {M.icons.ViModeConsole, "plum3"},
        i      = {M.icons.ViModeInsert, "chartreuse3"},
        ic     = {M.icons.ViModeInsert, "chartreuse3"},
        n      = {M.icons.ViModeNormal, "DarkGoldenrod2"},
        no     = {M.icons.ViModeNormal, "DarkGoldenrod2"},
        r      = {M.icons.ViModeReplace, "chocolate"},
        rm     = {M.icons.ViModeReplace, "chocolate"},
        R      = {M.icons.ViModeReplace, "purple"},
        Rv     = {M.icons.ViModeReplace, "purple"},
        s      = {M.icons.ViModeSearch, "SkyBlue2"},
        S      = {M.icons.ViModeSearch, "SkyBlue2"},
        t      = {M.icons.ViModeTerminal, "gray"},
        V      = {M.icons.ViModeVisual, "gray"},
        v      = {M.icons.ViModeVisual, "gray"},
        ["r?"] = {M.icons.ViModeReplace, "chocolate"},
        [""]  = {"🅢 ", "SkyBlue2"},
        [""]  = {" ", "gray"},
        ["!"]  = {"! ", "plum3"}
    }
}

function vimode.mode()
    local nums = {
        "❶",
        "❷",
        "❸",
        "❹",
        "❺",
        "❻",
        "❼",
        "❽",
        "❾",
        "❿",
        "⓫",
        "⓬",
        "⓭",
        "⓮",
        "⓯",
        "⓰",
        "⓱",
        "⓲",
        "⓳",
        "⓴"
    }
    local bufnr = vim.api.nvim_get_current_buf() or 0
    bufnr = nums[bufnr] or bufnr

    local vimMode = vim.api.nvim_get_mode().mode
    if vimode.modes[vimMode] == nil then
        set_statusline_bg("ViMode", "error")
        return " ? | " .. bufnr .. " "
    end

    local mode = vimode.modes[vimMode]
    set_statusline_bg("ViMode", mode[2])
    return (mode[1] or "?") .. " | " .. bufnr .. " "
end

function vimode.separator()
    local fg = "error"
    local mode = vim.api.nvim_get_mode().mode
    if vimode.modes[mode] ~= nil then
        fg = vimode.modes[mode][2]
    end

    bg = "act1"
    if not util.buffer_not_empty() then
        bg = "purple"
    end

    set_statusline_hi("ViModeSeparator", fg, bg)
    return M.icons.slant_right
end

M.providers = {
    vimode = vimode,
}

return M
