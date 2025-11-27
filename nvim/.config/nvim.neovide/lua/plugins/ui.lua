return {
	{
		"NeogitOrg/neogit",
		lazy = true,
		dependencies = {
			"nvim-lua/plenary.nvim",
			"sindrets/diffview.nvim",
			"nvim-telescope/telescope.nvim",
		},
		cmd = "Neogit",
		keys = {
			{ "<Space>f", "<cmd>Neogit<cr>" },
		},
	},
	{
		"akinsho/bufferline.nvim",
		enabled = false,
	},
}
