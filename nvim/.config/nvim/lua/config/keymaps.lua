local function map(mode, lhs, rhs, opts)
    local keys = require("lazy.core.handler").handlers.keys
    ---@cast keys LazyKeysHandler
    if not keys.active[keys.parse({ lhs, mode = mode }).id] then
        opts = opts or {}
        opts.silent = opts.silent ~= false
        vim.keymap.set(mode, lhs, rhs, opts)
    end
end

map("i", "kj", "<esc>", { silent = true })
map("n", "<space>k", ":set invhlsearch<cr>", { silent = true })

map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

map("v", ">", ">gv")
map("v", "<", "<gv")

map("n", "<C-h>", "<C-w>h")
map("n", "<C-j>", "<C-w>j")
map("n", "<C-k>", "<C-w>k")
map("n", "<C-l>", "<C-w>l")

map("n", "<space><", ":bp<cr>")
map("n", "<space>>", ":bn<cr>")
map("n", "<space>vs", ":vsplit<cr>")

-- keep cursor in place when joining lines
map("n", "J", "mzJ`z")
-- split lines (inverse of join)
map("n", "S", "i<cr><esc>^mwgk:silent! s/\v +$//<cr>:silent! noh<cr>`w")
-- select current line (excluding indentation)
map("n", "vv", "^vg_")
-- center search matches after jumping
map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")
-- import Emacs-isms
map("i", "<C-l>", "<esc>zza")
map("i", "<C-a>", "<esc>I")
map("i", "<C-e>", "<esc>A")
map("c", "<C-a>", "<home>")
map("c", "<C-e>", "<end>")
map("c", "<C-d>", "<del>")
map("c", "<C-f>", "<right>")
map("c", "<C-b>", "<left>")
map("c", "<C-x>", "<C-f>")
map("i", "<M-o>", "<C-o>o")
map("i", "<M-O>", "<C-o>O")
-- make Y consistent with C and D
map("n", "Y", "y$")
-- move to last change
map("n", "gI", "`.i")
-- select last inserted text
map("n", "gV", "`[v`[")
-- list navigation (make the arrows useful)
map("n", "<left>", ":cprev<cr>zvzz")
map("n", "<right>", ":cnext<cr>zvzz")
map("n", "<up>", ":lprev<cr>zvzz")
map("n", "<down>", ":lnext<cr>zvzz")
