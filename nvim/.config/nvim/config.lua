require('gitsigns').setup()

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
