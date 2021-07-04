require('gitsigns').setup()

local nvim_lsp = require('lspconfig')

-- Setup LSP config only after attaching to buffer
local on_attach = function (client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    -- Attach completion when setting up LSP
    require('completion').on_attach(client)

    -- Use Omnicomplete for completion (<C-x><C-o>)
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Keybindings
    local opts = { noremap=true, silent=true }
    buf_set_keymap('n', 'K',         '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', '<Leader>D', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', '<Leader>d', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', '<Leader>i', '<Cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<Leader>t', '<Cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    buf_set_keymap('n', '<Leader>r', '<Cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '<Leader>k', '<Cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', '<Leader>R', '<Cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '<Leader>a', '<Cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', '<Leader>e', '<Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    buf_set_keymap('n', '<Leader>E', '<Cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
    buf_set_keymap('n', '[d',        '<Cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']d',        '<Cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
    buf_set_keymap('n', '<Leader>f', '<Cmd>lua vim.lsp.buf.formatting()<CR>', opts)
end

-- Enable LSP servers
nvim_lsp.rust_analyzer.setup({on_attach=on_attach})
nvim_lsp.gopls.setup({on_attach=on_attach})
nvim_lsp.clangd.setup({on_attach=on_attach})

-- Setup Treesitter
local ts = require('nvim-treesitter.configs')
ts.setup {
	ensure_installed = {
		"typescript", "cpp", "jsonc",
		"lua", "c", "go", "python",
		"json", "rst", "toml", "yaml",
		"rust", "gomod", "javascript",
	},
	ignore_install = {},
	highlight = {
		enable = true,
	},
}
