set nocompatible
scriptencoding utf-8

if has('win32')
    " use Unix vimfiles
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after

    " skip loading mswin.vim
    let g:skip_loading_mswin = 1
endif

let s:plug_file = '$HOME/.vim/autoload/plug.vim'
if empty(glob(s:plug_file))
  silent execute '!curl -fLo ' . s:plug_file . ' --create-dirs '.
      \ 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source '$HOME/.vimrc'
endif

" Bundles
call plug#begin('~/.vim/plugged')

Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'romainl/vim-qf'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'morhetz/gruvbox'
Plug 'nlknguyen/papercolor-theme'

" Local plugins
if filereadable(expand("$HOME/.vim/local/plugs.vim"))
  so $HOME/.vim/local/plugs.vim
endif

call plug#end()

runtime macros/matchit.vim

" General settings
syntax on
filetype plugin indent on

set autoindent
set autowrite
set backspace=indent,eol,start
set backup
set backupdir=$HOME/.vim/tmp/backup
set belloff=all
set breakindent
set cmdheight=2
set colorcolumn=+1
set complete=.,w,b,u,t,d
set completeopt=longest,menuone
set diffopt+=vertical
set directory=$HOME/.vim/tmp/swap
set expandtab
set fillchars=diff:?,vert:¦
set formatoptions=qrn1j
set gdefault
set hidden
set history=1000
set hlsearch
set ignorecase
set incsearch
set lazyredraw
set linespace=0
set list
set listchars=tab:>>,trail:-,extends:#,nbsp:.
set modeline
set mouse=a
set mousehide
set noautoread
set nojoinspaces
set noshowmatch
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
set showmode
set sidescroll=1
set sidescrolloff=10
set smartcase
set softtabstop=4
set spell
set spellfile=$HOME/.vim/words.utf-8.add,$HOME/.vim/local/words.utf-8.add
set splitbelow
set splitright
set synmaxcol=800
set tabstop=4
set textwidth=140
set title
set undodir=$HOME/.vim/tmp/undo
set undofile
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
set wrap

if has('win32')
  set dictionary=
else
  set dictionary=/usr/share/dict/words
endif

if has('clipboard')
  if has ('unnamedplus')
    set clipboard=unnamed,unnamedplus
  else
    set clipboard=unnamed
  endif
endif

" clear search pattern (useful for reloads)
let @/ = ""

" highlight version control conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Highlight Interesting Words
hi def InterestingWord1 ctermbg=214 ctermfg=16 guibg=#ffa724 guifg=#000000
hi def InterestingWord2 ctermbg=154 ctermfg=16 guibg=#aeee00 guifg=#000000
hi def InterestingWord3 ctermbg=121 ctermfg=16 guibg=#8cffba guifg=#000000
hi def InterestingWord4 ctermbg=137 ctermfg=16 guibg=#b88853 guifg=#000000
hi def InterestingWord5 ctermbg=211 ctermfg=16 guibg=#ff9eb8 guifg=#000000
hi def InterestingWord6 ctermbg=195 ctermfg=16 guibg=#ff2c4b guifg=#000000

" automatically delete trailing whitespace
au BufWrite *.cpp :call helpers#delete_trailing_whitespace()
au BufWrite *.h :call helpers#delete_trailing_whitespace()

" automatically set the cursor to first line when editing a git commit message
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

" automatically resize splits when vim is resized.
au VimResized * exe "normal! \<C-W>="

" automatically return to last editing point
au BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

" don't close window when deleting a buffer
command! Bclose call helpers#bufcloseit()

" scratch buffer
let g:scratch_buffer_name = '__scratch__'

command! -nargs=0 Scratch call helpers#open_scratch_buffer(0)
command! -nargs=0 SplitScratch call helpers#open_scratch_buffer(1)

" disable paren matching in TeX, it's really slow
au FileType tex :NoMatchParen

" automatic quickfix windows
augroup autoqf
  autocmd!
  autocmd QuickFixCmdPost [^l]* cwindow
  autocmd QuickFixCmdPost l* lwindow
augroup END

" Keybindings
inoremap kj <ESC>

nnoremap <silent> <Space>o :Bclose<CR>
nnoremap <silent> <Space>k :set invhlsearch<CR>

" uppercase word
inoremap <C-u> <esc>mzgUiw`za

" keep cursor in place when joining lines
nnoremap J mzJ`z

" split line (inverse of join)
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w

" select current line (excluding indentation)
nnoremap vv ^vg_

" quick access to common files
nnoremap <Space>ve :edit $HOME/.vimrc<CR>
nnoremap <Space>vd :edit $HOME/.vim/words<CR>
nnoremap <Space>vg :edit $HOME/.gitconfig<CR>
nnoremap <Space>vz :edit $HOME/.zshrc<CR>
nnoremap <Space>vt :edit $HOME/.tmux.conf<CR>
nnoremap <Space>vs :Scratch<cr>

" stay in place on */#
nnoremap <silent> * :let sv=winsaveview()<cr>*:call winrestview(sv)<cr>
nnoremap <silent> # :let sv=winsaveview()<cr>#:call winrestview(sv)<cr>

" center search matches after jumping
nnoremap n nzzzv
nnoremap N Nzzzv

" import useful shortcuts from Emacs
inoremap <c-l> <esc>zza
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A
cnoremap <c-a> <home>
cnoremap <c-e> <end>
cnoremap <c-f> <right>
cnoremap <c-b> <left>
cnoremap <c-x> <c-f>

" move to last change
nnoremap gI `.i

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
nnoremap <right> :lnext<cr>zvzz

" grep
if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading
  set grepformat="%f:%l:%c:%m,%f:%l:%m"
endif

nnoremap <Space>a :grep<space>
nnoremap <Space>g :set opfunc=helpers#operator_grep<cr>g@
vnoremap <Space>g :<c-u> helpers#operator_grep(visualmode())<cr>

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
noremap <Space>fc /\v^[<\|=>]{7}( .*\|$)<CR>

if has('terminal') || has('nvim')
  tnoremap <C-j> <C-w>j
  tnoremap <C-k> <C-w>k
  tnoremap <C-l> <C-w>l
  tnoremap <C-h> <C-w>h
endif

vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

vnoremap > >gv
vnoremap < <gv

" Color scheme
set background=dark
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_italic = 1
let g:PaperColor_Theme_Options = {
            \ 'theme': {
            \   'default': {
            \     'transparent_background': 0,
            \   },
            \ 'language': {
            \   'cpp': {
            \     'highlight_standard_library': 1,
            \   },
            \   'c': {
            \     'highlight_builtins': 1,
            \   },
            \  },
            \ }
            \}

if !has('gui_running')
  if !($TERM == "linux" || $OLDTERM == "putty-256color") && (has('termguicolors') && (has('nvim') || v:version >= 800 || has('patch1942')))
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
else
  set guifont=Source\ Code\ Pro\ 9
  set guioptions-=T
  set guioptions-=e
  set guioptions-=m
  set guioptions-=r
  set guioptions-=l
  set guioptions-=C
  set guioptions-=L
  set guioptions+=c
endif

colorscheme PaperColor

" statusline setup
function! ModeForStatusline()
  let mode_status = {
        \ 'i': 'Insert',
        \ 'n': 'Normal',
        \ 'v': 'Visual',
        \ 'V': 'V-Line',
        \ "\<C-v>": 'V-Block',
        \ 'c': 'Command',
        \ 's': 'Select',
        \ 'S': 'S-Line',
        \ "\<C-s>": 'S-Block',
        \ 't': 'Terminal',
  \ }
  return get(mode_status, mode(), '')
endfunction

set noshowmode
set laststatus=2
set statusline=
set statusline+=\ %{ModeForStatusline()}
set statusline+=\ %#LineNr#
set statusline+=\ %f
set statusline+=\ %m
set statusline+=%=
set statusline+=\ %#TabLine#
set statusline+=\ %y
set statusline+=\ %{&fileformat}
set statusline+=\ %#StatusLine#
set statusline+=\ %P
set statusline+=\ %v
set statusline+=\:%l
set statusline+=\/%L
set statusline+=\ %{winnr()}
set statusline+=\ %#StatusLine#

" Tags
set tags=./tags;./TAGS
set tagcase=smart

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

" Read local machine settings
if filereadable(expand("~/.vim/local/vimrc.vim"))
  so ~/.lvimrc
endif

" Read project specific settings from current directory
if filereadable(".project.vim")
  so .project.vim
endif

