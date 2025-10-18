local coq = require('coq')
local lsp_signature = require('lsp_signature')
---@type vim.lsp.Config
return {
    cmd = { 'typescript-language-server', '--stdio' },
    init_options = { hostInfo = 'neovim' },
    filetypes = {
        'javascript',
        'typescript',
    },
    root_markers = {
        'tsconfig.json',
        'package.json',
        'jsconfig.json',
    },
    on_attach = function(client, bufnr)
        coq.lsp_ensure_capabilities(client)

        -- ts_ls provides `source.*` code actions that apply to the whole file
        -- these only appear if specified in `context.only`
        vim.api.nvim_buf_create_user_command(0, 'LspTypescriptSourceAction', function()
            local source_actions = vim.tbl_filter(function(action)
                return vim.startswith(action, 'source.')
            end, client.server_capabilities.codeActionProvider.codeActionKinds)

            vim.lsp.buf.code_action({
                context = {
                    only = source_actions,
                },
            })
        end, {})
    end,
}
