set nocompatible
scriptencoding utf-8

let s:plug_file = '~/.vim/autoload/plug.vim'
if empty(glob(s:plug_file))
  silent execute '!curl -fLo ' . s:plug_file . ' --create-dirs '.
      \ 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source '~/.vimrc'
endif

" Bundles
call plug#begin('~/.vim/plugged')

" VCS
Plug 'airblade/vim-gitgutter'

" Programming
Plug 'sheerun/vim-polyglot'
Plug 'nlknguyen/c-syntax.vim'
Plug 'rhysd/vim-clang-format'

" Editing
Plug 'tpope/vim-commentary'

" Interface
Plug 'junegunn/fzf' | Plug 'junegunn/fzf.vim'

" Colorschemes
Plug 'morhetz/gruvbox'

" Local plugins
if filereadable(expand("~/.local/vim/plugs.vim"))
  so ~/.local/vim/plugs.vim
endif

call plug#end()

" General settings
syntax on
filetype plugin indent on

set autoindent
set backspace=indent,eol,start
set backup
set backupdir=$HOME/.local/share/vim/backups
set breakindent
set cmdheight=2
set complete+=d
set expandtab
set formatoptions+=j
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
set nojoinspaces
set noshowmatch
set nospell
set nostartofline
set nowrap
set number
set scrolljump=5
set scrolloff=3
set shiftwidth=2
set shortmess+=filmnrxoOtT
set showmode
set smartcase
set softtabstop=2
set splitbelow
set splitright
set tabstop=2
set undodir=$HOME/.local/share/vim/undo
set undofile
set viminfo^=%
set virtualedit=onemore
set whichwrap=b,s,h,l,<,>,[,]
set wildmenu
set wildmode=list:longest,full

if has('clipboard')
  if has ('unnamedplus')
    set clipboard=unnamed,unnamedplus
  else
    set clipboard=unnamed
  endif
endif

" Automatically delete trailing whitespace
func! DeleteTrailingWS()
  exe "normal mz"
  $s/\s\+$//ge
  exe "normal `z"
endfunc
au BufWrite *.cpp :call DeleteTrailingWS()
au BufWrite *.h :call DeleteTrailingWS()

" Automatically set the cursor to first line
" when editing a git commit message
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

" Automatically resize splits when vim is resized.
au VimResized * exe "normal! \<C-W>="

" Automatically return to last editing point
au BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

" Don't close window when deleting a buffer
function! <SID>BufcloseCloseIt()
  let l:currentBufNum = bufnr("%")
  let l:alternateBufNum = bufnr("#")

  if buflisted(l:alternateBufNum)
    buffer #
  else
    bnext
  endif

  if bufnr("%") == l:currentBufNum
    new
  endif

  if buflisted(l:currentBufNum)
    execute("bdelete! ".l:currentBufNum)
  endif
endfunction
command! Bclose call <SID>BufcloseCloseIt()

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

inoremap (<CR> (<CR>)<ESC>O
inoremap {<CR> {<CR>}<ESC>O
inoremap [<CR> [<CR>]<ESC>O
inoremap (; (<CR>);<ESC>O
inoremap {; (<CR>);<ESC>O
inoremap [; (<CR>);<ESC>O
inoremap (<Space> ()<ESC>i
inoremap {<Space> ()<ESC>i
inoremap [<Space> ()<ESC>i

nnoremap <silent> <Space>o :Bclose<CR>
nnoremap <silent> <Space>k :set invhlsearch<CR>

nnoremap ; :
nnoremap <Space>< :bp<CR>
nnoremap <Space>> :bn<CR>
nnoremap <Space>vs :vsplit<CR>
nnoremap <Space>ss :split<CR>
nnoremap <Space>hh :resize 60<CR>
nnoremap <Space>y "+y
nnoremap <Space>p "+p
nnoremap <Space>ve :edit ~/.vimrc<CR>
nnoremap <Space>; *``cgn<ESC>
nnoremap <Space>, #``cgN<ESC>

nnoremap <M-n> :cnext<CR>
nnoremap <M-p> :cprevious<CR>

noremap <Space>pp :setlocal paste!<CR>
noremap <Space>ss :setlocal spell!<CR>
noremap <Space>fc /\v^[<\|=>]{7}( .*\|$)<CR>
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

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

if !has('gui_running')
  if !($TERM == "linux" || $OLDTERM == "putty-256color") && (has('termguicolors') && (has('nvim') || v:version >= 800 || has('patch1942')))
    set termguicolors
    if !has('nvim')
      set t_Co=256
      set t_so=[7m
      set t_se=[27m
      set t_ZH=[3m
      set t_ZR=[23m
      let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
      let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
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

colorscheme gruvbox

" allow for transparency
hi! Normal ctermbg=NONE guibg=NONE

" Statusline setup
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
" set statusline+=%#PmenuSel#
set statusline+=\ %{ModeForStatusline()}
set statusline+=\ %#LineNr#
set statusline+=\ %F
set statusline+=\ %m
set statusline+=%=
set statusline+=\ %y
set statusline+=\ %v
set statusline+=\:%l
set statusline+=\/%L
set statusline+=\ %{winnr()}

" Omnicomplete
set completeopt+=longest

" Tags
set tags=./tags;./TAGS
set tagcase=smart

" Polyglot/C++
set cinoptions=N-s
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1
"let g:cpp_experimental_template_highlight = 1

" Ag
let g:ag_working_path_mode = 'r'

" FZF
" Augment Ag command with fzf#vim#with_preview
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column  --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

let g:fzf_files_options = '--preview "cat {} 2>/dev/null | head -'.&lines.'"'
let g:fzf_layout = { 'down': '~15%' }
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'none'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

nnoremap <C-p> :Files<CR>
nnoremap <Space>p :GFiles<CR>
nnoremap <Space>gl :Commits<CR>
nnoremap <Space>gbl :BCommits<CR>
nnoremap <Space>gs :GFiles?<CR>
nnoremap <Space><Space> :Commands<CR>
nnoremap <Space>h :Helptags<CR>
nnoremap <Space>t :Tags<CR>
nnoremap <Space>b :Buffer<CR>
nnoremap <Space>a :Rg<Space>

" clang-format
let g:clang_format#code_style = 'llvm'
let g:clang_format#style_options = {
      \ 'AlignConsecutiveDeclarations': 'true',
      \ 'BreakBeforeBraces': 'Linux',
      \ }
augroup ClangFormat
  autocmd!
  autocmd FileType c,cpp nnoremap <buffer><Space><CR> :<C-u>ClangFormat<CR>
  autocmd FileType c,cpp vnoremap <buffer><Space><CR> :ClangFormat<CR>
augroup END

" Read local machine settings
if filereadable(expand("~/.lvimrc"))
  so ~/.lvimrc
endif

" Read project specific settings from cwd
if filereadable(".project.vim")
  so .project.vim
endif
