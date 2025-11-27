return {
	{
		"nvim-telescope/telescope.nvim",
		version = false,
		dependencies = {
			{
				"jvgrootveld/telescope-zoxide",
			},
		},
		keys = function()
			-- local zoxide = require("telescope.extensions.zoxide")
			return {
				{
					"<Space><Space>",
					LazyVim.pick("files"),
				},
				{
					"<Space>/",
					require("telescope.builtin").current_buffer_fuzzy_find,
				},
				{
					"<Space>a",
					LazyVim.pick("live_grep"),
				},
				{
					"<Space>b",
					function()
						require("telescope.builtin").buffers({
							sort_mru = true,
							sort_lastused = true,
						})
					end,
				},
				{
					"<Space>g",
					require("telescope.builtin").git_commits,
				},
				{
					"<Space>l",
					require("telescope.builtin").git_bcommits,
				},
				{
					"<Space>,r",
					require("telescope.builtin").lsp_references,
				},
			}
		end,
	},
	{
		"stevearc/conform.nvim",
		opts = {
			formatters_by_ft = {
				fish = { "fish_indent" },
				proto = { "buf" },
				bzl = { "buildifier" },
			},
		},
	},
	{ "tpope/vim-repeat" },
	{ "folke/flash.nvim", enabled = false },
}
