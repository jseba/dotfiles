return {
	cmd = { "rust-analyzer" },
	filetypes = {
		"rust",
	},
	root_markers = {
		"Cargo.toml",
	},
	before_init = function(init_params, config)
		if config.settings and config.settings["rust_analyzer"] then
			init_params.initializationOptions = config.settings["rust_analyzer"]
		end
	end,
	capabilities = {
		experimental = {
			serverStatusNotification = true,
		},
	},
	settings = {
		rust_analyzer = {
			checkOnSave = true,
		},
	},
}
