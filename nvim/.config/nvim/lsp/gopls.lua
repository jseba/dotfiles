---@type vim.lsp.Config
local coq = require('coq')
local lsp_signature = require('lsp_signature')
return {
    cmd = { 'gopls' },
    root_markers = {
        'go.work',
        'go.mod',
    },
    filetypes = {
        'go',
        'gomod',
        'gowork',
        'gosum',
        'gotmpl',
    },
    on_attach = function(client, bufnr)
        if not client.server_capabilities.semanticTokensProvider then
            local semantic = client.config.capabilities.textDocument.semanticTokens
            client.server_capabilities.semanticTokensProvider = {
                full = true,
                legend = {
                    tokenTypes = semantic.tokenTypes,
                    tokenModifiers = semantic.tokenModifiers,
                },
                range = true,
            }
        end
        coq.lsp_ensure_capabilities(client)
        lsp_signature.on_attach({}, bufnr)
    end,
    settings = {
        gopls = {
            workspaceFiles = {
                '**/BUILD',
                '**/WORKSPACE',
                '**/*.{bzl,bazel}',
            },
            -- env = {
            --     GOPACKAGESDRIVER = './tools/gopackagesdriver.sh'
            -- },
            directoryFilters = {
                '-bazel-bin',
                '-bazel-out',
                '-bazel-testlogs',
                '-bazel-mypkg',
                '-node_modules',
                '-.git',
                '-.vscode',
                '-.vscode-test',
            },
            gofumpt = true,
            codelenses = {
                gc_details = false,
                generate = true,
                regenerate_cgo = true,
                run_govulncheck = true,
                test = true,
                tidy = true,
                upgrade_dependency = true,
                vendor = true,
            },
            hints = {
                assignVariableTypes = true,
                compositeLiteralFields = true,
                compositeLiteralTypes = true,
                constantValues = true,
                functionTypeParameters = true,
                parameterNames = true,
                rangeVariableTypes = true,
            },
            analyses = {
                staticcheck = true,
                nilness = true,
                unusedparams = true,
                unusedwrite = true,
                useany = true,
            },
            usePlaceholders = false,
            completeUnimported = true,
            staticcheck = true,
            semanticTokens = true,
        },
    },
}

