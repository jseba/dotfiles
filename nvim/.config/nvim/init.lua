local v = vim
local a = vim.api
local c = vim.cmd
local o = vim.opt

local map = v.keymap.set
local Plug = v.fn['plug#']

v.call('plug#begin')
Plug('echasnovski/mini.nvim')
Plug('nvim-lua/plenary.nvim')
Plug('nvim-lualine/lualine.nvim')
Plug('nvim-tree/nvim-web-devicons')
Plug('nvim-treesitter/nvim-treesitter')
Plug('nvim-treesitter/nvim-treesitter-context')
Plug('nvimtools/none-ls.nvim')
Plug('neovim/nvim-lspconfig')
Plug('VonHeikemen/lsp-zero.nvim')
Plug('NTBBloodbath/doom-one.nvim')
Plug('folke/tokyonight.nvim')
Plug('sainnhe/edge')
v.call('plug#end')

o.autoindent = true
o.autoread = false
o.autowrite = true
o.backspace = 'indent,eol,start'
o.belloff = 'all'
o.breakindent = true
o.clipboard = 'unnamedplus'
o.cmdheight = 2
o.complete = '.,w,b,u,t,d'
o.completeopt = 'longest,menuone,noinsert'
o.confirm = true
o.cursorline = true
o.diffopt:append({ vertical = true })
o.expandtab = true
o.formatoptions = "qrn1j"
o.gdefault = true
o.hidden = true
o.history = 1000
o.hlsearch = true
o.ignorecase = true
o.incsearch = true
o.joinspaces = false
o.laststatus = 2
o.linespace = 0
o.list = true
o.listchars = { tab = "  ", trail = "-", extends = "#", nbsp = "." }
o.modeline = true
o.mouse = "a"
o.number = false
o.pumblend = 10
o.pumheight = 10
o.scrolljump = 5
o.scrolloff = 3
o.sessionoptions = { "buffers", "curdir", "tabpages", "winsize" }
o.shiftround = true
o.shiftwidth = 4
o.shortmess:append({
  f = true,
  i = true,
  l = true,
  m = true,
  n = true,
  r = true,
  x = true,
  o = true,
  O = true,
  t = true,
  T = true,
})
o.showmatch = false
o.showmode = false
o.sidescroll = 1
o.sidescrolloff = 10
o.signcolumn = "yes"
o.smartcase = true
o.smartindent = true
o.softtabstop = 4
o.spell = false
o.spelllang = { "en" }
o.splitbelow = true
o.splitright = true
o.startofline = false
o.swapfile = false
o.synmaxcol = 200
o.tabstop = 4
o.termguicolors = true
o.textwidth = 140
o.timeoutlen = 300
o.title = true
o.undofile = true
o.undolevels = 10000
o.updatetime = 200
o.whichwrap = "b,s,h,l,<,>,[,]"
o.wildmenu = true
o.wildmode = "longest:full,full"
o.wrap = false

o.cinoptions:append("N-s") -- don't indent namespace
o.cinoptions:append("g0") -- don't indent C++ public/private
o.cinoptions:append(":-s") -- don't indent case labels
o.cinoptions:append("E-s") -- don't indent in C++ extern blocks
o.cinoptions:append("(0") -- line up unclosed parentheses insides...
o.cinoptions:append("w1") -- ...but ignore whitespace after the open paren

-- Fix markdown indentation settings
vim.g.markdown_recommended_style = 0

o.background = 'dark'
c.colorscheme('tokyonight')

local icons = {
    diagnostics = {
        Error = " ",
        Warn = " ",
        Hint = " ",
        Info = " ",
    },
    git = {
        added = " ",
        modified = " ",
        removed = " ",
    },
    kinds = {
        Array = " ",
        Boolean = " ",
        Class = " ",
        Color = " ",
        Constant = " ",
        Constructor = " ",
        Copilot = " ",
        Enum = " ",
        EnumMember = " ",
        Event = " ",
        Field = " ",
        File = " ",
        Folder = " ",
        Function = " ",
        Interface = " ",
        Key = " ",
        Keyword = " ",
        Method = " ",
        Module = " ",
        Namespace = " ",
        Null = " ",
        Number = " ",
        Object = " ",
        Operator = " ",
        Package = " ",
        Property = " ",
        Reference = " ",
        Snippet = " ",
        String = " ",
        Struct = " ",
        Text = " ",
        TypeParameter = " ",
        Unit = " ",
        Value = " ",
        Variable = " ",
    },
}

local mopt = { noremap = true, silent = true }

local bufremove = require('mini.bufremove')

--- Keybindings
map('i', 'kj', '<ESC>', mopt)
map('n', 'vs', ':vsplit<cr>', mopt)
map('n', '<space><', ':bp<cr>', mopt)
map('n', '<space>>', ':bn<cr>', mopt)
-- list navigation (arrow keys aren't used otheriwse, make them useful)
map('n', '<left>', ':cprev<cr>zvzz', mopt)
map('n', '<right>', ':cnext<cr>zvzz', mopt)
map('n', '<up>', ':lprev<cr>zvzz', mopt)
map('n', '<down>', ':lnext<cr>zvzz', mopt)
-- select current line (sans indentation)
map('n', 'vv', '^vg_', mopt)
-- keep cursor in place when joining lines
map('n', 'J', 'mzJ`z', mopt)
-- split line (inverse of join)
map('n', 'S', 'i<cr><esc>^mwgk:silent! s/\v +$//<cr>:silent! noh<cr>`w', mopt)
-- center search matches after jumping
map('n', 'n', 'nzzzv', mopt)
map('n', 'N', 'Nzzzv', mopt)
-- import useful shortcuts from Emacs
map('i', '<c-l>', '<c-o>zz', mopt)
map('i', '<c-a>', '<c-o>^', mopt)
map('i', '<c-e>', '<c-o>$', mopt)
map('c', '<c-a>', '<home>', mopt)
map('c', '<c-e>', '<end>', mopt)
map('c', '<c-b>', '<left>', mopt)
map('c', '<c-f>', '<right>', mopt)
map('c', '<c-x>', '<c-f>', mopt)
-- open new line above/below current
map('i', '<m-o>', '<c-o>o', mopt)
map('i', '<m-O>', '<c-o>O', mopt)
-- make Y consistent with C and D
map('n', 'Y', 'y$', mopt)
-- move to last change
map('n', 'gI', '`.i', mopt)
-- select last inserted text
map('n', 'gV', '`[v`[', mopt)
-- insert current file's directory
map({'n','v','o'}, '<c-r><c-\\>', '<c-r>=expand("%:p:h", 1)<cr>', { silent=true })
-- invert line-wise up/down movement
map('n', 'j', 'gj', mopt)
map('n', 'k', 'gk', mopt)
map('n', 'gj', 'j', mopt)
map('n', 'gk', 'k', mopt)
-- easier window navigation
map('n', '<c-h>', '<c-w>h', mopt)
map('n', '<c-j>', '<c-w>j', mopt)
map('n', '<c-k>', '<c-w>k', mopt)
map('n', '<c-l>', '<c-w>l', mopt)
-- highlight version control conflict markers
-- TODO
-- maintain visual mode when indenting
map('v', '>', '>gv', mopt)
map('v', '<', '<gv', mopt)
-- close buffer
map('n', '<space>o', function() bufremove.delete(0, true) end, { silent = true })
-- toggle highlighting of searches
map('n', '<space>k', ':set invhlsearch<cr>', { silent = true })

require('lualine').setup({
  options = {
    theme = 'auto',
    icons_enabled = true,
    component_separators = '',
    section_separators = '',
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
        },
        lualine_x = {
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
  }
})

local pick = require('mini.pick')
pick.setup()

map('n', '<space><space>', function() pick.builtin.files({tool='git'}) end, mopt)
map('n', '<space>f', function() pick.builtin.files({tool='fd'}) end, mopt)
map('n', '<space>a', function() pick.builtin.grep() end, mopt)
map('n', '<space>b', function() pick.builtin.buffers() end, mopt)

require('nvim-treesitter.configs').setup({
  ensure_installed = {
    'bash',
    'c',
    'capnp',
    'cmake',
    'cpp',
    'diff',
    'dockerfile',
    'fish',
    'git_config',
    'git_rebase',
    'gitattributes',
    'gitcommit',
    'gitignore',
    'go',
    'gomod',
    'gosum',
    'gotmpl',
    'gowork',
    'hcl',
    'ini',
    'javascript',
    'jq',
    'json',
    'json5',
    'jsonc',
    'kconfig',
    'lua',
    'luadoc',
    'make',
    'markdown',
    'markdown_inline',
    'meson',
    'proto',
    'puppet',
    'python',
    'regex',
    'rst',
    'ruby',
    'rust',
    'ssh_config',
    'starlark',
    'terraform',
    'tmux',
    'toml',
    'typescript',
    'udev',
    'vim',
    'vimdoc',
    'xml',
    'yaml',
  },
  auto_install = true,
  highlight = {
    enable = true,
  },
  indent = {
    enable = false,
  },
  context_commentstring = {
    enable = true,
    enable_autocmd = true,
  },
})

local nls = require('null-ls')
nls.setup({
  root_dir = require('null-ls.utils').root_pattern(
    '.root',
    'Makefile',
    '.git',
    'Cargo.lock',
    'go.mod',
    'go.work'
  ),
  sources = {
    nls.builtins.formatting.fish_indent,
    nls.builtins.diagnostics.fish,
    nls.builtins.formatting.gofmt,
    nls.builtins.formatting.goimports,
    nls.builtins.formatting.stylua,
    nls.builtins.formatting.shfmt,
  },
})

require('mini.completion').setup()

local lsp_zero = require('lsp-zero')
local lsp_attach = function(client, bufnr)
  local opts = { buffer = bufnr }
  lsp_zero.buffer_autoformat()

  if client.name == 'gopls' then
      if not client.server_capabilities.semanticTokensProvider then
          local semantic = client.config.capabilities.textDocument.semanticTokens
          client.server_capabilities.semanticTokensProvider = {
              full = true,
              legend = {
                  tokenTypes = semantic.tokenTypes,
                  tokenModifiers = semantic.tokenModifiers,
              },
              range = true,
          }
      end
  end
end
lsp_zero.extend_lspconfig({
  sign_text = true,
  lsp_attach = lsp_attach,
})

local lspconfig = require('lspconfig')
lspconfig.gopls.setup({
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
        fieldalignment = true,
        nilness = true,
        unusedparams = true,
        unusedwrite = true,
        useany = true,
    },
    usePlaceholders = false,
    completeUnimported = true,
    staticcheck = true,
    directoryFilters = { '-.git', '-.vscode', '-.vscode-test', '-node_modules' },
    semanticTokens = true,
})
lspconfig.rust_analyzer.setup({})
