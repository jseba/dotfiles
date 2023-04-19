local util = require("config.util")

return {
    {
        "nvim-telescope/telescope.nvim",
        cmd = "Telescope",
        version = false, -- no tags/releases
        keys = {
            { "<space><space>", util.telescope("git_files"), desc = "Find Files" },
            { "<space>f", util.telescope("files"), desc = "Find All Files" },
            { "<space>r", util.telescope("oldfiles"), desc = "Recent Files" },
            { "<space>b", util.telescope("buffers"), desc = "Buffers" },
            { "<space>a", util.telescope("live_grep"), desc = "Live Grep" },
            { "<space>g", util.telescope("git_status"), desc = "Git Status" },
            { "<space>l", util.telescope("git_commits"), desc = "Git Commits" },
            { "<space>h", util.telescope("help_tags"), desc = "Help" },
            { "<space>t", util.telescope("tags"), desc = "Tags" },
            { "<space>/", util.telescope("current_buffer_fuzzy_find"), desc = "Search in Current Buffer" },
            { "<space>\\", util.telescope("commands"), desc = "Commands" },
            { "<space>s", util.telescope("lsp_document_symbols",
                {
                    "Class",
                    "Function",
                    "Method",
                    "Constructor",
                    "Interface",
                    "Module",
                    "Struct",
                    "Trait",
                    "Field",
                    "Property",
                }),
                desc = "Go to Symbol in Current Buffer",
            },
            { "<space>S", util.telescope("lsp_dynamic_workspace_symbols",
                {
                    "Class",
                    "Function",
                    "Method",
                    "Constructor",
                    "Interface",
                    "Module",
                    "Struct",
                    "Trait",
                    "Field",
                    "Property",
                }),
                desc = "Go to Symbol",
            },
        },
        opts = {
            defaults = {
                prompt_prefix = " ",
                selection_caret = " ",
                mappings = {
                    i = {
                        ["<c-t>"] = function(...)
                            return require("trouble.providers.telescope").open_with_trouble(...)
                        end,
                        ["<a-t>"] = function(...)
                            return require("trouble.providers.telescope").open_selected_with_trouble(...)
                        end,
                        ["<a-i>"] = function(...)
                            util.telescope("find_files", {no_ignore=true})()
                        end,
                        ["<a-h>"] = function(...)
                            util.telescope("find_files", {hidden=true})()
                        end,
                        ["<a-p>"] = function(...)
                            return require("telescope.actions").cycle_history_prev(...)
                        end,
                        ["<a-n>"] = function(...)
                            return require("telescope.actions").cycle_history_next(...)
                        end,
                        ["<c-f>"] = function(...)
                            return require("telescope.actions").preview_scrolling_down(...)
                        end,
                        ["<c-b>"] = function(...)
                            return require("telescope.actions").preview_scrolling_up(...)
                        end,
                    },
                    n = {
                        ["q"] = function(...)
                            return require("telescope.actions").close(...)
                        end,
                    },
                },
            },
        },
    },

    {
        "echasnovski/mini.bufremove",
        keys = {
            { "<space>o", function() require("mini.bufremove").delete(0, true) end, desc = "Delete Buffer" },
        },
    },

    {
        "folke/trouble.nvim",
        cmd = { "TroubleToggle", "Trouble" },
        opts = { use_diagnostic_signs = true },
    },

    {
        "folke/todo-comments.nvim",
        cmd = { "TodoTrouble", "TodoTelescope" },
        event = { "BufReadPost", "BufNewFile" },
        config = true,
    },
}