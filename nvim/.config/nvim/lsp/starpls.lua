local coq = require("coq")
local lsp_signature = require("lsp_signature")
---@type vim.lsp.Config
return {
	cmd = { "starpls", "server", "--bazel_path=bazelisk" },
	root_markers = {
		"WORKSPACE",
		"MODULE.bazel",
	},
	filetypes = { "bzl" },
}
