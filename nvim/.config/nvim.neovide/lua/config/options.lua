-- local c = vim.cmd
local g = vim.g
local o = vim.opt

g.mapleader = "\\"

o.autoindent = true
o.autoread = false
o.autowrite = true
o.backspace = "indent,eol,start"
o.belloff = "all"
o.breakindent = true
o.clipboard = "unnamedplus"
o.cmdheight = 2
o.complete = ".,w,b,u,t,d"
o.completeopt = "longest,menuone,noinsert"
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
o.relativenumber = false
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
o.cinoptions:append("g0") -- don't indent C++ public/private/protected
o.cinoptions:append(":-s") -- don't indent case labels
o.cinoptions:append("E-s") -- don't indent in C++ extern blocks
o.cinoptions:append("(0") -- line up unclosed parentheses insides...
o.cinoptions:append("w1") -- ...but ignore whitespace after the open paren

-- fix markdoown indentation style
g.markdown_recommended_style = 0

g.lazyvim_picker = "telescope"

if g.neovide then
	o.guifont = "Fira Code Retina:h12"

	g.neovide_remember_window_size = true
	g.neovide_cursor_animation_length = 0
	g.neovide_cursor_animate_in_insert_mode = false
	g.neovide_cursor_animate_command_line = false
	g.neovide_cursor_vfx_mode = ""
end
