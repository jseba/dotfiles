return {
    {
        "NTBBloodbath/doom-one.nvim",
        lazy = false, -- ensure loaded during startup
        priority = 1000,
        init = function()
            vim.g.doom_one_cursor_coloring = true
            vim.g.doom_one_terminal_colors = true
            vim.g.doom_one_italic_comments = false
            vim.g.doom_one_enable_treesitter = true
            vim.g.doom_one_transparent_background = false
            vim.g.doom_one_diagnostics_text_color = false
            vim.g.doom_one_pumblend_enable = true
            vim.g.doom_one_pumblend_transparency = 20
            vim.g.doom_one_plugin_telescope = true
        end,
        config = function()
            vim.cmd([[colorscheme doom-one]])
        end,
    },
}
