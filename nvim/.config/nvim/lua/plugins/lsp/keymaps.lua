local M = {}

local lspPrefix = "<space>;"

local function format()
    require("plugins.lsp.format").format({ force = true })
end

M.keymaps = {
    { lspPrefix .. "d", vim.diagnostic.open_float },
    { lspPrefix .. "l", "<cmd>LspInfo<cr>" },
    { "gd", "<cmd>Telescope lsp_definitions<cr>", has = "definition" },
    { "gr", "<cmd>Telescope lsp_references<cr>" },
    { "gD", vim.lsp.buf.declaration },
    { "gI", "<cmd>Telescope lsp_implementations<cr>" },
    { "gy", "<cmd>Telescope lsp_type_definitions<cr>" },
    { "K", vim.lsp.buf.hover },
    { "gK", vim.lsp.buf.signature_help, has = "signatureHelp" },
    { "<C-k>", vim.lsp.buf.signature_help, mode = "i", has = "signatureHelp" },
    { lspPrefix .. "r", vim.lsp.buf.rename, has = "rename" },
    { lspPrefix .. "f", format, has = "documentFormatting" },
    { lspPrefix .. "f", format, mode = "v", has = "documentFormatting" },
    { lspPrefix .. "a", vim.lsp.buf.code_action, mode = { "n", "v" }, has = "codeAction" },
    {
        lspPrefix .. "A",
        function()
            vim.lsp.buf.code_action({
                context = {
                    only = { "source" },
                    diagnostics = {},
                },
            })
        end,
        has = "codeAction",
    },
}

function M.on_attach(client, buffer)
    local keys = require("lazy.core.handler.keys")
    local keymaps = {}

    for _, value in ipairs(M.keymaps) do
        local keymap = keys.parse(value)
        if keymap[2] == vim.NIL or keymap[2] == false then
            keymaps[keymap.id] = nil
        else
            keymaps[keymap.id] = keymap
        end
    end

    for _, keymap in pairs(keymaps) do
        if not keymap.has or client.server_capabilities[keymap.has .. "Provider"] then
            local opts = keys.opts(keymap)
            opts.has = nil
            opts.silent = opts.silent ~= false
            opts.buffer = buffer
            vim.keymap.set(keymap.mode or "n", keymap[1], keymap[2], opts)
        end
    end
end

return M
