return {
	{
		"saghen/blink.cmp",
		opts = {
			sources = {
				default = { "lsp", "path", "buffer" },
				completion = {
					documentation = {
						auto_show = false,
					},
					ghost_text = {
						enabled = true,
					},
					trigger = {
						show_on_keyword = false,
						show_on_insert_on_trigger_character = false,
						show_on_accept_on_trigger_character = false,
						show_on_trigger_character = true,
					},
				},
			},
		},
	},
	{
		"neovim/nvim-lspconfig",
		---@type vim.diagnostic.Opts
		diagnostics = {
			jump = {
				float = true,
			},
			underline = true,
			update_in_insert = false,
			virtual_text = false,
			virtual_lines = {
				current_line = true,
			},
		},
		opts = {
			servers = {
				gopls = {
					root_markers = { "go.work", "go.mod" },
					filetypes = { "go", "gomod", "gowork", "gosum", "gotmpl" },
					settings = {
						gopls = {
							workspaceFiles = {
								"**/BUILD",
								"**/WORKSPACE",
								"**/*.{bzl,bazel}",
							},
							-- env = {
							-- 	GOPACKAGESDRIVER = '../tools/gopackagesdriver.sh',
							-- },
							directoryFilters = {
								"-bazel-bin",
								"-bazel-out",
								"-bazel-testlogs",
								"-bazel-mypkg",
								"-node_modules",
								"-.git",
								"-.vscode",
								"-.vscode-test",
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
				},
				rust_analyzer = {
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
				},
				starpls = {
					cmd = { "starpls", "server", "--bazel_path=bazelisk" },
				},
				lua_ls = {
					settings = {
						Lua = {
							diagnostics = {
								globals = { "require", "LazyVim" },
							},
						},
					},
				},

				ts_ls = {},
				jsonls = {},
				taplo = {},
				yamlls = {},
				gitlab_ci_ls = {},
				vacuum = {},
				buf_ls = {},
			},
		},
	},
	{
		"yetone/avante.nvim",
		keys = function()
			return {
				{ "<Space>;a", "<cmd>AvanteAsk<cr>" },
				{ "<Space>;c", "<cmd>AvanteChat<cr>" },
				{ "<Space>;f", "<cmd>AvanteFocus<cr>" },
				{ "<Space>;h", "<cmd>AvanteHistory<cr>" },
				{ "<Space>;m", "<cmd>AvanteModels<cr>" },
				{ "<Space>;n", "<cmd>AvanteChatNew<cr>" },
				{ "<Space>;p", "<cmd>AvanteSwitchProvider<cr>" },
				{ "<Space>;r", "<cmd>AvanteRefresh<cr>" },
				{ "<Space>;s", "<cmd>AvanteStop<cr>" },
				{ "<Space>;t", "<cmd>AvanteToggle<cr>" },
				{ "<Space>;x", "<cmd>AvanteClear<cr>" },
			}
		end,
		opts = {
			input = {
				provider = "snacks",
				provider_opts = {
					title = "Avante Input",
					icon = " ",
				},
			},
			instructions_file = "AGENTS.md",
			mode = "legacy",
			provider = "opencode",
			providers = {
				opencode = {
					endpoint = "https://opencode.cloudflare.dev",
					model = "claud-sonnent-4-5",
					timeout = 30000,
				},
			},
			{ "<Space>;c", "<cmd>AvanteChat<cr>" },
			{ "<Space>;c", "<cmd>AvanteChat<cr>" },
			{ "<Space>;c", "<cmd>AvanteChat<cr>" },
			{ "<Space>;c", "<cmd>AvanteChat<cr>" },
			{ "<Space>;c", "<cmd>AvanteChat<cr>" },
		},
	},
}
