return {
    {
        "nvim-lualine/lualine.nvim",
        event = "VeryLazy",
        opts = function()
            local icons = require("config").options.icons
            return {
                icons_enabled = false,
                component_separators = "",
                section_separators = "",
                theme = "tokyonight",
                sections = {
                    lualine_a = { "mode" },
                    lualine_b = { "branch" },
                    lualine_c = {
                        {
                            "diagnostics",
                            symbols = {
                                error = icons.diagnostics.Error,
                                warn = icons.diagnostics.Warn,
                                info = icons.diagnostics.Info,
                                hint = icons.diagnostics.Hint,
                            },
                        },
                        { "filetype", icon_only = true, separator = "", padding = { left = 1, right = 0 } },
                        { "filename", path = 1, symbols = { modified = "  ", readonly = "", unnamed = "" } },
                        {
                            function()
                                return require("nvim-navic").get_location()
                            end,
                            cond = function()
                                return package.loaded["nvim-navic"] and require("nvim-navic").is_available()
                            end,
                        },
                    },
                    lualine_x = {
                        {
                            function()
                                return require("noice").api.status.command.get()
                            end,
                            cond = function()
                                return package.loaded["noice"] and require("noice").api.status.command.has()
                            end,
                            color = require("config.util").fg("Statement"),
                        },
                        {
                            function()
                                return require("noice").api.status.mode.get()
                            end,
                            cond = function()
                                return package.loaded["noice"] and require("noice").api.status.mode.has()
                            end,
                            color = require("config.util").fg("Constant"),
                        },
                        {
                            require("lazy.status").updates,
                            cond = require("lazy.status").has_updates,
                            color = require("config.util").fg("Special"),
                        },
                        {
                            "diff",
                            symbols = {
                                added = icons.git.added,
                                modified = icons.git.modified,
                                removed = icons.git.removed,
                            },
                        },
                    },
                    lualine_y = {
                        { "progress", separator = " ", padding = { left = 1, right = 0 } },
                        { "location", padding = { left = 0, right = 1 } },
                    },
                    lualine_z = {
                        function()
                            return " " .. os.date("%R")
                        end,
                    },
                },
                inactive_sections = {
                    lualine_a = {},
                    lualine_b = {},
                    lualine_c = { "filename" },
                    lualine_x = {},
                    lualine_y = {},
                    lualine_z = {},
                },
                extensions = { "lazy" },
            }
        end,
        config = function(_, opts)
            require("lualine").setup(opts)
        end,
    },
    {
        "folke/noice.nvim",
        event = "VeryLazy",
        opts = {
            lsp = {
                override = {
                    ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                    ["vim.lsp.util.stylize_markdown"] = true,
                },
            },
            presets = {
                bottom_search = true,
                command_palette = true,
                long_message_to_split = true,
            },
        },
        keys = {
            {
                "<S-Enter>",
                function()
                    require("noice").redirect(vim.fn.getcmdline())
                end,
                mode = "c",
            },
            {
                "<C-f>",
                function()
                    if not require("noice.lsp").scroll(4) then
                        return "<C-f>"
                    end
                end,
                silent = true,
                expr = true,
                mode = { "i", "n", "s" },
            },
            {
                "<C-b>",
                function()
                    if not require("noice.lsp").scroll(-4) then
                        return "<C-b>"
                    end
                end,
                silent = true,
                expr = true,
                mode = { "i", "n", "s" },
            },
        },
    },
    {
        "SmiteshP/nvim-navic",
        lazy = true,
        init = function()
            vim.g.navic_silence = true
            require("config.util").on_attach(function(client, buffer)
                if client.server_capabilities.documentSymbolProvider then
                    require("nvim-navic").attach(client, buffer)
                end
            end)
        end,
        opts = function()
            return {
                separator = " ",
                highlight = true,
                depth_limit = 5,
                icons = require("config").options.icons.kinds,
            }
        end,
    },
    {
        "nvim-tree/nvim-web-devicons",
        lazy = true,
    },
    {
        "MunifTanjim/nui.nvim",
        lazy = true,
    },
}
