local map = vim.keymap.set
local mopt = { noremap = true, silent = true }

--- Keybindings
map("i", "kj", "<esc>", mopt)
map("t", "kj", "<C-\\><C-n>", mopt)
map("n", "vs", ":vsplit<cr>", mopt)
map("n", "<space><", ":bprevious<cr>", mopt)
map("n", "<space>>", ":bnext<cr>", mopt)
-- list navigation (arrow keys aren't used otherwise, make them useful)
map("n", "<left>", ":cprevious<cr>", mopt)
map("n", "<right>", ":cnext<cr>", mopt)
map("n", "<up>", ":lprevious<cr>", mopt)
map("n", "<down>", ":lnext<cr>", mopt)
-- select current line (sans indentation)
map("n", "vv", "^vg_", mopt)
-- keep cursor in place when joining lines
map("n", "J", "mzJ`z", mopt)
-- split line (inverse of join)
map("n", "S", "i<cr><esc>^mwgk:silent! s/\v +$//<cr>:silent! noh<cr>`w", mopt)
-- center search matches after jumping
map("n", "n", "nzzzv", mopt)
map("n", "N", "Nzzzv", mopt)
-- import useful shortcuts from Emacs
map("i", "<c-l>", "<c-o>zz", mopt)
map("i", "<c-a>", "<c-o>^", mopt)
map("i", "<c-e>", "<c-o>$", mopt)
map("c", "<c-a>", "<home>", mopt)
map("c", "<c-e>", "<end>", mopt)
map("c", "<c-b>", "<left>", mopt)
map("c", "<c-f>", "<right>", mopt)
map("c", "<c-x>", "<c-f>", mopt)
-- open new line above/below current
map("i", "<m-o>", "<c-o>o", mopt)
map("i", "<m-O>", "<c-o>O", mopt)
-- make Y consistent with C and D
map("n", "Y", "y$", mopt)
-- move to last change
map("n", "gI", "`.i", mopt)
-- select last inserted text
map("n", "gV", "`[v`[", mopt)
-- insert current file's directory
map({ "n", "v", "o" }, "<c-r><c-\\>", '<c-r>=expand("%:p:h", 1)<cr>', { silent = true })
-- invert line-wise up/down movement
map("n", "j", "gj", mopt)
map("n", "k", "gk", mopt)
map("n", "gj", "j", mopt)
map("n", "gk", "k", mopt)
-- easier window navigation
map("n", "<c-h>", "<c-w>h", mopt)
map("n", "<c-j>", "<c-w>j", mopt)
map("n", "<c-k>", "<c-w>k", mopt)
map("n", "<c-l>", "<c-w>l", mopt)
-- highlight version control conflict markers
-- TODO
-- maintain visual mode when indenting
map("v", ">", ">gv", mopt)
map("v", "<", "<gv", mopt)
-- toggle highlighting of searches
map("n", "<space>k", ":set invhlsearch<cr>", { silent = true })
-- close buffer
map("n", "<space>o", function()
	require("snacks").bufdelete()
end, { silent = true })

-- LazyVim based keymaps
-- map("n", "<space>`", "<cmd>e #<cr>", mopt)
