" vim: ff=unix fenc=utf-8
set nocompatible
scriptencoding utf-8

if has('win32')
    " use Unix vimfiles
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after

    " skip loading mswin.vim
    let g:skip_loading_mswin = 1
endif

" Plugins
call plug#begin('~/.vim/plugged')

Plug 'jseba/vim-cpp-enhanced-highlight'
Plug 'rhysd/vim-clang-format'
Plug 'pboettch/vim-cmake-syntax'
Plug 'airblade/vim-gitgutter'
Plug 'luochen1990/rainbow'
Plug 'tpope/vim-git'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-abolish'
Plug 'haya14busa/is.vim'
Plug 'morhetz/gruvbox'
Plug 'nlknguyen/PaperColor-theme'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'fatih/vim-go'

" Local plugins
if filereadable(expand('$HOME/.vim/local/plugs.vim'))
  so $HOME/.vim/local/plugs.vim
endif

call plug#end()

if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==#''
  runtime! macros/matchit.vim
endif

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
" set lazyredraw
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
set spellfile=$HOME/.vim/words.utf-8.add,$HOME/.vim/local/words.utf-8.add
set splitbelow
set splitright
set synmaxcol=200
set tabstop=4
set tags-=./tags tags-=./tags; tags^=./tags;
set textwidth=140
set title
set undodir=$HOME/.vim/tmp/undo
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

" remove colors from $NINJA_STATUS output
let $NINJA_STATUS = '%u/%r/%f %o '

" clear search pattern (useful for reloads)
let @/ = ''

" automatically delete trailing whitespace
augroup wsbutler
  au!
  au BufWrite *.c :call helpers#delete_trailing_whitespace()
  au BufWrite *.cpp :call helpers#delete_trailing_whitespace()
  au BufWrite *.h :call helpers#delete_trailing_whitespace()
  au BufWrite *.hpp :call helpers#delete_trailing_whitespace()
augroup END

" automatically set the cursor to first line when editing a git commit message
augroup gitcommit_sob
  au!
  au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])
augroup END

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
command! Bclose call helpers#bufcloseit()

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
nnoremap <silent> <Space>k :set invhlsearch<CR>

" uppercase word
inoremap <C-u> <esc>mzgUiw`za

" keep cursor in place when joining lines
nnoremap J mzJ`z

" split line (inverse of join)
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:silent! noh<cr>`w

" select current line (excluding indentation)
nnoremap vv ^vg_

" quick access to common files
nnoremap <Space>ve :edit $HOME/.vimrc<CR>
nnoremap <Space>vd :edit $HOME/.vim/words<CR>
nnoremap <Space>vg :edit $HOME/.gitconfig<CR>
nnoremap <Space>vz :edit $HOME/.zshrc<CR>
nnoremap <Space>vt :edit $HOME/.tmux.conf<CR>
nnoremap <Space>vl :edit $HOME/.vim/coc-settings.json<CR>

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

" move to last change
nnoremap gI `.i

" select last inserted text
nnoremap gV `[v`[

" current file directory
noremap! <silent> <c-r><c-\> <c-r>=expand('%:p:h', 1)<cr>

" invert line-wise up/down movement
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" run make
nnoremap <C-CR> :make<CR>
inoremap <C-CR> <C-o>:make<CR>

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

" Color scheme
set background=dark
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
    if !has('nvim')
      set t_Co=256
      set t_so=[7m
      set t_se=[27m
      set t_ZH=[3m
      set t_ZR=[23m
      let &t_ut = ''
      let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
      let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
      let &t_SI = "\<Esc>[5 q"
      let &t_SR = "\<Esc>[5 q"
      let &t_EI = "\<Esc>[2 q"
    endif
  endif
else
  if !has('gui_macvim')
    set guifont=Fira\ Code\ Retina\ 9
  else
    set guifont=Fira\ Code\ Retina:h12
  endif
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
set laststatus=2
set noshowmode
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

" gitgutter
let g:gitgutter_map_keys = 0
if executable('rg')
    let g:gitgutter_grep = 'rg --color never'
endif

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

" FZF
" Augment Rg command with fzf#vim#with_preview
command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
      \   <bang>0 ? fzf#vim#with_preview('up:60%')
      \           : fzf#vim#with_preview('right:50%:hidden', '?'),
      \   <bang>0)

" Prefer fd-find over standard find
if executable('fd')
  let $FZF_DEFAULT_COMMAND='fd --type f --hidden'
endif

let g:fzf_files_option = '--preview "cat {} 2>/dev/null | head -'.&lines.'"'
let g:fzf_layout = { 'down': '~20%' }
let g:fzf_colors = {
      \ 'fg':       ['fg', 'Normal'],
      \ 'bg':       ['bg', 'none'],
      \ 'hl':       ['fg', 'Comment'],
      \ 'fg+':      ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':      ['bg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'hl+':      ['fg', 'Statement'],
      \ 'info':     ['fg', 'PreProc'],
      \ 'prompt':   ['fg', 'Conditional'],
      \ 'pointer':  ['fg', 'Exception'],
      \ 'marker':   ['fg', 'Keyword'],
      \ 'spinner':  ['fg', 'Label'],
      \ 'header':   ['fg', 'Comment']
      \ }
nnoremap <Space>f :Files<cr>
nnoremap <Space><Space> :GFiles<cr>
nnoremap <Space>g :GFiles?<cr>
nnoremap <Space>l :Commits<cr>
nnoremap <Space>b :Buffer<cr>
nnoremap <Space>t :Tags<cr>
nnoremap <Space>h :Helptags<cr>
nnoremap <space>\\ :Commands<cr>
nnoremap <space>a :Rg<space>

" coc.nvim
augroup coc
  autocmd!

  autocmd CursorHold * silent call CocActionAsync('highlight')
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup END

xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

inoremap <silent><expr> <C-Space> coc#refresh()

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nnoremap <Space>k :call CocActionAsync('doHover')<CR>
if has('nvim-0.4.0') || has('patch-8.2.0750')
    nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
    nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
    inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<C-r>=coc#float#scroll(1)\<CR>" : "\<Right>"
    inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<C-r>=coc#float#scroll(0)\<CR>" : "\<Left>"
endif

nnoremap <silent><nowait> <leader>a :<C-u>CocList diagnostics<CR>
nnoremap <silent><nowait> <leader>e :<C-u>CocList extensions<CR>
nnoremap <silent><nowait> <leader>c :<C-u>CocList commands<CR>
nnoremap <silent><nowait> <leader>o :<C-u>CocList outline<CR>
nnoremap <silent><nowait> <leader>s :<C-u>CocList -I symbols<CR>
nnoremap <silent><nowait> <leader>p :<C-u>CocListResume<CR>

command! -nargs=0 Format :call CocAction('format')

" Read local machine settings
if filereadable(expand('~/.vim/local/vimrc.vim'))
  so ~/.lvimrc
endif

" Read project specific settings from current directory
if filereadable('.project.vim')
  so .project.vim
endif

