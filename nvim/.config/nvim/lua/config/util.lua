local util = require("lazy.core.util")
local M = {}

M.root_patterns = { ".git", ".lua" }

function M.has(plugin)
    return require("lazy.core.config").plugins[plugin] ~= nil
end

function M.opts(name)
    local plugin = require("lazy.core.config").plugins[name]
    if not plugin then
        return {}
    end
    return require("lazy.core.plugin").values(plugin, "opts", false)
end

function M.fg(name)
    ---@type {foreground?:number}?
    local hl = vim.api.nvim_get_hl and vim.api.nvim_get_hl(0, { name = name })
    local fg = hl and hl.foreground
    return fg and { fg = string.format("#%06x", fg) }
end

function M.root()
    ---@type string?
    local path = vim.api.nvim_buf_get_name(0)
    path = path ~= "" and vim.loop.fs_realpath(path) or nil
    local roots = {}
    if path then
        for _, client in pairs(vim.lsp.get_active_clients({ bufnr = 0 })) do
            local workspace = client.config.workspace_folders
            local paths = workspace
                    and vim.tbl_map(function(ws)
                        return vim.uri_to_fname(ws.uri)
                    end, workspace)
                or client.config.root_dir and { client.config.root_dir }
                or {}
            for _, p in ipairs(paths) do
                local r = vim.loop.fs_realpath(p)
                if path:find(r, 1, true) then
                    roots[#roots + 1] = r
                end
            end
        end
    end
    table.sort(roots, function(a, b)
        return #a > #b
    end)
    local root = roots[1]
    if not root then
        path = path and vim.fs.dirname(path) or vim.loop.cwd()
        root = vim.fs.find(M.root_patterns, { path = path, upward = true })[1]
        root = root and vim.fs.dirname(root) or vim.loop.cwd()
    end
    ---@cast root string
    return root
end

function M.telescope(builtin, opts)
    local params = { builtin = builtin, opts = opts }
    return function()
        builtin = params.builtin
        opts = params.opts
        opts = vim.tbl_deep_extend("force", { theme = "ivy", cwd = M.root() }, opts or {})
        require("telescope.builtin")[builtin](opts)
    end
end

function M.lazy_notify()
    local notifs = {}
    local function temp_notify(...)
        table.insert(notifs, vim.F.pack_len(...))
    end

    local orig_notify = vim.notify
    vim.notify = temp_notify

    local timer = vim.loop.new_timer()
    local check = vim.loop.new_check()

    local replay = function()
        timer:stop()
        check:stop()
        if vim.notify == temp_notify then
            vim.notify = orig_notify
        end
        vim.schedule(function()
            for _, notif in ipairs(notifs) do
                vim.notify(vim.F.unpack_len(notif))
            end
        end)
    end

    check:start(function()
        if vim.notify ~= temp_notify then
            replay()
        end
    end)

    timer:start(500, 0, replay)
end

function M.on_attach(on_attach)
    vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
            local buf = args.buf
            local client = vim.lsp.get_client_by_id(args.data.client_id)
            on_attach(client, buf)
        end,
    })
end

function M.buffer_not_empty()
    if vim.fn.empty(vim.fn.expand("%:t")) ~= 1 then
        return true
    end
    return false
end

return M
