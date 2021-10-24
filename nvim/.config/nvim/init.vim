" vim: ff=unix fenc=utf-8
scriptencoding utf-8

" disable netrw
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1

" Plugins
call plug#begin(stdpath('cache') . '/plugged')
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/lsp_extensions.nvim'
Plug 'nvim-lua/completion-nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-telescope/telescope.nvim'

Plug 'itchyny/lightline.vim'
Plug 'haya14busa/is.vim'
Plug 'luochen1990/rainbow'
Plug 'lewis6991/gitsigns.nvim', {'branch': 'main'}

Plug 'jseba/vim-cpp-enhanced-highlight'
Plug 'rhysd/vim-clang-format'
Plug 'pboettch/vim-cmake-syntax'
Plug 'dag/vim-fish'
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'

Plug 'morhetz/gruvbox'
Plug 'nlknguyen/PaperColor-theme'
Plug 'sainnhe/sonokai'
call plug#end()

" Load Lua configuration
execute 'luafile ' . stdpath('config') . '/config.lua'

" General settings
syntax on
filetype plugin indent on

set autoindent
set autowrite
set background=dark
set backspace=indent,eol,start
set backup
set belloff=all
set breakindent
set cmdheight=2
set complete=.,w,b,u,t,d
set completeopt=longest,menuone,noinsert
set diffopt+=vertical
set expandtab
set fillchars=diff:?,vert:¦
set formatoptions=qrn1j
set gdefault
set hidden
set history=1000
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set linespace=0
set list
set listchars=tab:\ \ ,trail:-,extends:#,nbsp:.
set modeline
set mouse=a
set mousehide
set noautoread
set nojoinspaces
set noshowmatch
set noshowmode
set nospell
set nostartofline
set noswapfile
set nowrap
set nonumber
set scrolljump=5
set scrolloff=3
set shiftround
set shiftwidth=4
set shortmess+=filmnrxoOtT
set sidescroll=1
set sidescrolloff=10
set signcolumn=yes
set smartcase
set softtabstop=4
set splitbelow
set splitright
set synmaxcol=200
set tabstop=4
set textwidth=140
set title
set undofile
set updatetime=300
set viminfo^=%
set virtualedit=block,onemore
set whichwrap=b,s,h,l,<,>,[,]
set wildcharm=<c-z>
set wildmenu
set wildmode=list:longest,full
set wildignore+=.hg,.svn,.git
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.spl
set wildignore+=*.sw?
set wildignore+=*.DS_Store
set wildignore+=*.luac,*.pyc
set wildignore+=*.orig

if has('clipboard')
  if has ('unnamedplus')
    set clipboard=unnamed,unnamedplus
  else
    set clipboard=unnamed
  endif
endif

let &backupdir = stdpath('cache') . '/backup'
let &directory = stdpath('cache') . '/swap'
let &undodir = stdpath('cache') . '/undo'

if exists('*mkdir')
    if !isdirectory(&backupdir)
        call mkdir(&backupdir)
    endif
    if !isdirectory(&directory)
        call mkdir(&directory)
    endif
    if !isdirectory(&undodir)
        call mkdir(&undodir)
    endif
endif

" remove colors from $NINJA_STATUS output
let $NINJA_STATUS = '%u/%r/%f %o '

" clear search pattern (useful for reloads)
let @/ = ''

" automatically resize splits when vim is resized.
augroup resize_trigger
  au!
  au VimResized * exe 'normal! \<C-W>='
augroup END

" automatically return to last editing point
augroup resume_edit
  au!
  au BufReadPost *
            \ if line("'\'") > 0 && line("'\'") <= line('$') |
            \   exe "normal! g`\'" |
            \ endif
augroup END

" don't close window when deleting a buffer
function! <SID>BufCloseIt()
    let l:cbufnr = bufnr("%")
    let l:abufnr = bufnr("#")

    if buflisted(l:abufnr)
        buffer #
    else
        bnext
    endif

    if bufnr("%") == l:cbufnr
        new
    endif

    if buflisted(l:cbufnr)
        execute("bdelete! ".l:cbufnr)
    endif
endfunction
command! Bclose call <SID>BufCloseIt()

" disable paren matching in TeX, it's really slow
augroup tex_nomatchparen
  au!
  au FileType tex :NoMatchParen
augroup END

" automatic quickfix windows
augroup autoqf
  autocmd!
  autocmd QuickFixCmdPost [^l]* cwindow
  autocmd QuickFixCmdPost l* lwindow
augroup END

" Keybindings
inoremap kj <ESC>

nnoremap <silent> <Space>o :Bclose<CR>
nnoremap <silent> <Space>/ :set invhlsearch<CR>

" uppercase word
inoremap <C-u> <esc>mzgUiw`za

" keep cursor in place when joining lines
nnoremap J mzJ`z

" split line (inverse of join)
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:silent! noh<cr>`w

" select current line (excluding indentation)
nnoremap vv ^vg_

" quick access to common files
nnoremap <Space>ve :edit $MYVIMRC<CR>

" center search matches after jumping
nnoremap n nzzzv
nnoremap N Nzzzv

" import useful shortcuts from Emacs
inoremap <c-l> <esc>zza
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A
cnoremap <c-a> <home>
cnoremap <c-d> <del>
cnoremap <c-e> <end>
cnoremap <c-f> <right>
cnoremap <c-b> <left>
cnoremap <c-x> <c-f>

" open new line above/below
inoremap <m-o> <C-O>o
inoremap <m-O> <C-O>O

" make Y consistent with C and D
nnoremap Y y$

" current file directory
noremap! <silent> <c-r><c-\> <c-r>=expand('%:p:h', 1)<cr>

" invert line-wise up/down movement
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" easier window navigation
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l
noremap <c-h> <c-w>h

" list navigation (arrow keys aren't used otherwise, make them useful)
nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz

nnoremap <Space>< :bp<CR>
nnoremap <Space>> :bn<CR>
nnoremap <Space>vs :vsplit<CR>
nnoremap <Space>hs :split<CR>
nnoremap <Space>y "+y
nnoremap <Space>p "+p
nnoremap <Space>; *``cgn<ESC>
nnoremap <Space>, #``cgN<ESC>

noremap <Space>pp :setlocal paste!<CR>
noremap <Space>ss :setlocal spell!<CR>

" highlight version control conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'
noremap <Space>c /\v^[<\|=>]{7}( .*\|$)<CR>

if has('terminal') || has('nvim')
  tnoremap <C-j> <C-w>j
  tnoremap <C-k> <C-w>k
  tnoremap <C-l> <C-w>l
  tnoremap <C-h> <C-w>h
endif

vnoremap > >gv
vnoremap < <gv

" use ' for leader and \ for marks
nnoremap \ '
let mapleader = "'"

" Color scheme
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_italic = 1
let g:gruvbox_improved_warnings = 1
let g:PaperColor_Theme_Options = {
            \   'language': {
            \     'cpp': {
            \       'highlight_standard_library': 1,
            \     },
            \     'c': {
            \       'highlight_builtins': 1,
            \     },
            \   },
            \ }

if !has('gui_running')
  if !($TERM ==# 'linux' || $OLDTERM ==# 'putty-256color') && (has('termguicolors') && (has('nvim') || v:version >= 800 || has('patch1942')))
    if $TERM_PROGRAM !=# 'Apple_Terminal'
      set termguicolors
    endif
  endif
endif

colorscheme sonokai

" gitgutter
let g:gitgutter_map_keys = 0
if executable('rg')
    let g:gitgutter_grep = 'rg --color never'
endif

" lightline
let g:lightline = {
            \ 'colorscheme': 'PaperColor',
            \ 'active': {
            \   'left': [[ 'mode', 'paste' ],
            \            [ 'readonly', 'modified', 'filename' ]],
            \ },
            \ 'inactive': {
            \   'left':  [[ 'modified', 'filename' ]],
            \   'right': [[ ]],
            \ },
            \ 'separator': {
            \   'left': '',
            \   'right': '',
            \ },
            \ 'component': {
            \   'modified': '%M',
            \ },
            \ }

" C/C++
set cinoptions+=N-s    " don't indent namespaces
set cinoptions+=g0     " don't indent C++ public/private/protected
set cinoptions+=:-s    " don't indent case labels
set cinoptions+=E-s    " don't indent in C++ extern blocks
set cinoptions+=(0     " line up unclosed parentheses insides...
set cinoptions+=w1     " ...but ignore whitespace after the open paren
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1
let g:cpp_concepts_highlight = 2

" ClangFormat
let g:clang_format#code_style = 'LLVM'
let g:clang_format#detect_style = 1
let g:clang_format#auto_formatexpr = 1
augroup clangformatexpr
  au!
  au FileType c,cpp :set textwidth=0
  au BufWritePost *.cpp :ClangFormat
  au BufWritePost *.hpp :ClangFormat
augroup END

" Rainbow delimiters
let g:rainbow_active = 1
let g:rainbow_conf = {
            \ 'separately': {
            \   'cpp': {
            \     'parentheses': [
            \       'start=/(/ end=/)/ fold',
            \       'start=/\[/ end=/\]/ fold',
            \       'start=/{/ end=/}/ fold',
            \       'start=/\(\(\<operator\>\)\@<!<\)\&[a-zA-Z0-9_]\@<=<\ze[^<]/ end=/>/']
            \     }
            \   }
            \ }

" Telescope
nnoremap <Space>f <cmd>Telescope find_files<cr>
nnoremap <Space><Space> <cmd>Telescope git_files<cr>
nnoremap <Space>g <cmd>Telescope git_status
nnoremap <Space>l <cmd>Telescope git_commits<cr>
nnoremap <Space>b <cmd>Telescope buffers<cr>
nnoremap <Space>t <cmd>Telescope tags<cr>
nnoremap <Space>h <cmd>Telescope help_tags<cr>
nnoremap <space>\\ <cmd>Telescope commands<cr>
nnoremap <space>a <cmd>Telescope live_grep<cr>

