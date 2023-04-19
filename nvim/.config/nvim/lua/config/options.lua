-- this file is automatically loaded by plugins.config
local M = {}

function M.setup_vimopts()
    -- use ' for leader and \ for marks
    vim.g.mapleader = "'"

    local opt = vim.opt
    opt.autoindent = true
    opt.autoread = false
    opt.autowrite = true
    opt.backspace = "indent,eol,start"
    opt.belloff = "all"
    opt.breakindent = true
    opt.clipboard = "unnamedplus"
    opt.cmdheight = 2
    opt.complete = ".,w,b,u,t,d"
    opt.completeopt = "longest,menuone,noinsert"
    opt.confirm = true
    opt.cursorline = false
    opt.diffopt:append({ vertical = true })
    opt.expandtab = true
    opt.fillchars = "diff:?,vert:¦"
    opt.formatoptions = "qrn1j"
    opt.gdefault = true
    opt.hidden = true
    opt.history = 1000
    opt.hlsearch = true
    opt.incsearch = true
    opt.joinspaces = false
    opt.laststatus = 2
    opt.linespace = 0
    opt.list = true
    opt.listchars = "tab:  ,trail:-,extends:#,nbsp:."
    opt.modeline = true
    opt.mouse = "a"
    opt.number = false
    opt.pumblend = 10
    opt.pumheight = 10
    opt.scrolljump = 5
    opt.scrolloff = 3
    opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize" }
    opt.shiftround = true
    opt.shiftwidth = 4
    opt.shortmess:append({
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
    opt.showmatch = false
    opt.showmode = false
    opt.sidescroll = 1
    opt.sidescrolloff = 10
    opt.signcolumn = "yes"
    opt.smartcase = true
    opt.smartindent = true
    opt.softtabstop = 4
    opt.spell = false
    opt.spelllang = { "en" }
    opt.splitbelow = true
    opt.splitright = true
    opt.startofline = false
    opt.swapfile = false
    opt.synmaxcol = 200
    opt.tabstop = 4
    opt.termguicolors = true
    opt.textwidth = 140
    opt.timeoutlen = 300
    opt.title = true
    opt.undofile = true
    opt.undolevels = 10000
    opt.updatetime = 200
    opt.whichwrap = "b,s,h,l,<,>,[,]"
    opt.wildmenu = true
    opt.wildmode = "longest:full,full"
    opt.wrap = false

    opt.cinoptions:append("N-s") -- don't indent namespace
    opt.cinoptions:append("g0") -- don't indent C++ public/private
    opt.cinoptions:append(":-s") -- don't indent case labels
    opt.cinoptions:append("E-s") -- don't indent in C++ extern blocks
    opt.cinoptions:append("(0") -- line up unclosed parentheses insides...
    opt.cinoptions:append("w1") -- ...but ignore whitespace after the open paren

    -- Fix markdown indentation settings
    vim.g.markdown_recommended_style = 0
end

return M
