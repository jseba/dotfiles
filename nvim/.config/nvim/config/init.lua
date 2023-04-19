local M = {}

local options = {
    colorscheme = function()
        require("doom-one").load()
    end,
    icons = {
        diags = {
            Error = " ",
            Warn = " ",
        Hint = " ",
            Info = " ",
        },
        git = {
            added = " ",
            modified = " ",
            removed = " ",
        },
        kinds = {
            Array = " ",
            Boolean = " ",
            Class = " ",
            Color = " ",
            Constant = " ",
            Constructor = " ",
            Copilot = " ",
            Enum = " ",
            EnumMember = " ",
            Event = " ",
            Field = " ",
            File = " ",
            Folder = " ",
            Function = " ",
            Interface = " ",
            Key = " ",
            Keyword = " ",
            Method = " ",
            Module = " ",
            Namespace = " ",
            Null = " ",
            Number = " ",
            Object = " ",
            Operator = " ",
            Package = " ",
            Property = " ",
            Reference = " ",
            Snippet = " ",
            String = " ",
            Struct = " ",
            Text = " ",
            TypeParameter = " ",
            Unit = " ",
            Value = " ",
            Variable = " ",
        },
    },
}

function M.setup(opts)
    if not M.has() then
        require("lazy.core.util").error("lazy.vim required")
        error("exiting")
    end

    if vim.fn.argc(-1) == 0 then
        -- no explicit files to open, delay autocmds and keymaps
        vim.api.nvim_create_autocmd("User", {
            group = vim.api.nvim_create_augroup("NvimUserConfig", {clear=true}),
            pattern = "VeryLazy",
            callback = function()
                M.load("autocmds")
                M.load("keymaps")
            end,
        })
    else
        -- opening file(s) now, load autocmds and keymaps early
        M.load("autocmds")
        M.load("keymaps")
    end

    require("lazy.core.util").try(function()
        if type(M.colorscheme) == "function" then
            M.colorscheme()
        else
            vim.cmd.colorscheme(M.colorscheme)
        end
    end, {
        msg = "could not load configured colorscheme",
        on_error = function(msg)
            require("lazy.core.util").error(msg)
            -- load default built-in colorscheme
            vim.cmd.colorscheme("habamax")
        end,
    })
end

---@param range? string
function M.has(range)
    local semver = require("lazy.manage.semver")
    return semver.range(range or M.lazy_version):matches(require("lazy.core.config").version or "0.0.0")
end

---@param name "autocmds" | "options" | "keymaps"
function M.load(name)
    local util = require("lazy.core.util")
    local function _doload(mod)
        util.try(function()
            require(mod)
        end, {
            msg = "failed loading "..mod,
            on_error = function(msg)
                local info = require("lazy.core.cache").find(mod)
                if info == nil or (type(info) == "table" and #info == 0) then
                    return
                end
                util.error(msg)
            end,
        })
    end
    _load("config."..name)
    if vim.bo.filetype == "lazy" then
        -- XXX: lazy ui options may have been overwritten, reset it here
        vim.cmd([[do VimResized]])
    end
end

M.init_done = false
function M.init()
    if not M.init_done then
        M.init_done = true
        -- delay notifications until vim.notify was replaced (or after 500ms)
        require("util").lazy_notify()
        require("config").load("options")
    end
end

setmetatable(M, {
    __index = function(_, key)
        ---@cast options VimConfig
        return options[key]
    end,
})

return M
