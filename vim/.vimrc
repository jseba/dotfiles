set nocompatible
scriptencoding utf-8
filetype plugin indent on

if has('win32')
  " use Unix vimfiles
  set runtimepath=~/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,~/.vim/after

  " skip loading mswin.vim
  let g:skip_loading_mswin = 1
endif

" Basic options
set autoindent
set autowrite
set backspace=indent,eol,start
set breakindent
set colorcolumn=+1
set diffopt+=vertical
set formatoptions+=j
set hidden
set history=1000
set laststatus=2
set lazyredraw
set linebreak
set list
set listchars=tab:>>,trail:-,extends:#,nbsp:.
set matchtime=3
set noautoread
set nojoinspaces
set nonumber
set norelativenumber
set noshowmode
set nostartofline
set ruler
set shiftround
set shortmess+=filmnrxoOtT
set showcmd
set splitbelow
set splitright
set title
set ttyfast
set viminfo^=%

" Spelling
" XXX: move me
set dictionary=/usr/share/dict/words
set spellfile=~/.vim/words.utf-8.add,~/.vim/local/words.utf-8.add
nnoremap zG 2zg

" iTerm is slow at rendering Unicode lines, so just use ASCII
set fillchars=diff:⣿,vert:│
set fillchars=diff:⣿,vert:\|

" No syntax highlighting if lines are too long
set synmaxcol=800

" Basic completion
set complete=.,w,b,u,t
set completeopt=longest,menuone

" Save when losing focus
" XXX: move me
au FocusLost * :silent! wall

" Leader
let mapleader=" "
let maplocalleader="\\"

" Trailing whitespace
" XXX: Move me
augroup trailing
    au!
    au InsertEnter * :set listchars-=trail:-
    au InsertLeave * :set listchars+=trail:-
augroup END

" Automatically return to last editing point
" XXX: move me
augroup edit_return
    au BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \   exe 'normal! g`"zvzz' |
                \ endif
augroup END

" Tabs, spaces, wrapping
set tabstop=4
set shiftwidth=2
set softtabstop=2
set expandtab
set wrap
set textwidth=140
set formatoptions=qrn1j

" Backups
set backup
set noswapfile
set undofile

set undodir=~/.vim/tmp/undo/
set backupdir=~/.vim/tmp/backup/
set directory=~/.vim/tmp/swap/

" Color scheme
set background=dark

" XXX: move me
if !has('gui_running')
    if $TERM_PROGRAM != "Apple_Terminal"
      set termguicolors
    endif

    if !has('nvim')
      set t_Co=256
      set t_so=[7m
      set t_se=[27m
      set t_ZH=[3m
      set t_ZR=[23m
      let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
      let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
      let &t_SI = "\<Esc>[5 q"
      let &t_SR = "\<Esc>[5 q"
      let &t_EI = "\<Esc>[2 q"
    endif
endif

syntax on
colorscheme badwolf

" highlight VC conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" base keymappings
inoremap kj <esc>

" uppercase word
inoremap <C-u> <esc>mzgUiw`za

" keep the cursor in place while joining lines
nnoremap J mzJ`z

" split line (inverse of join)
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w

" substitute shortcuts
nnoremap <c-s> :%s/
vnoremap <c-s> :s/

" select current line, excluding indentation
nnoremap vv ^vg_

" close buffer without closing window
nnoremap <leader>o :Bclose<cr>

" quick access to common files
nnoremap <leader>ve :edit $MYVIMRC<cr>
nnoremap <leader>vd :edit ~/.vim/words<cr>
nnoremap <leader>vg :edit ~/.gitconfig<cr>
nnoremap <leader>vz :edit ~/.zshrc<cr>
nnoremap <leader>vt :edit ~/.tmux.conf<cr>

" searching and movement
set ignorecase
set smartcase
set incsearch
set hlsearch
set gdefault

set scrolloff=5
set sidescroll=1
set sidescrolloff=10

set virtualedit=block,onemore

nnoremap / /\v
vnoremap / /\v

nnoremap <silent> <leader>k :set invhlsearch<cr>
nnoremap <silent> <leader>l :noh<cr>:call clearmatches()<cr>

runtime macros/matchit.vim

" center search matches after jumping
nnoremap n nzzzv
nnoremap N Nzzzv

" import useful shortcuts from Emacs
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A
cnoremap <c-a> <home>
cnoremap <c-e> <end>
cnoremap <c-b> <left>
cnoremap <c-f> <right>
cnoremap <c-x> <c-f>

" move to last change (like gi)
nnoremap gI `.i

" invert line-wise up/down movement
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" easier buffer navigation
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

" list navigation (arrow keys aren't used otherwise, make them useful)
nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz

" folding
set foldlevelstart=0

" recursively open whatever fold the cursor is in, even if partially open
nnoremap z0 zcz0

" grep
nnoremap <leader>a :grep<space>
nnoremap <leader>g g@
vnoremap <leader>g :<c-u>call grep#operator(visualmode())<cr>

" Highlight Interesting Words�
nnoremap <silent> <leader>1 <Plug>HiWord1
nnoremap <silent> <leader>2 <Plug>HiWord2
nnoremap <silent> <leader>3 <Plug>HiWord3
nnoremap <silent> <leader>4 <Plug>HiWord4
nnoremap <silent> <leader>5 <Plug>HiWord5
nnoremap <silent> <leader>6 <Plug>HiWord6
