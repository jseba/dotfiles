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
Plug 'tpope/vim-fugitive'
Plug 'gregsexton/gitv'
"Plug 'mhinz/vim-signify'
Plug 'airblade/vim-gitgutter'

" Languages
Plug 'sheerun/vim-polyglot'
Plug 'neomake/neomake'
"Plug 'ajh17/VimCompletesMe'
"Plug 'lyuts/vim-rtags'
Plug 'Valloric/YouCompleteMe', { 'for': ['cpp', 'c', 'python', 'go', 'js', 'rs'] }
Plug 'rhysd/vim-clang-format'

" Editing
"Plug 'luochen1990/rainbow'
Plug 'scrooloose/nerdcommenter'
Plug 'will133/vim-dirdiff'

" Interface
Plug 'junegunn/fzf.vim'
Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
Plug 'christoomey/vim-tmux-navigator'
"Plug 'haya14busa/incsearch.vim' | Plug 'haya14busa/incsearch-fuzzy.vim' | Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'henrik/vim-indexed-search'
Plug 'tpope/vim-eunuch'

" Colorschemes
Plug 'morhetz/gruvbox'
Plug 'tomasr/molokai'
Plug 'nanotech/jellybeans.vim'
Plug 'nlknguyen/papercolor-theme'
Plug 'w0ng/vim-hybrid'
Plug 'chriskempson/base16-vim'
Plug 'arcticicestudio/nord-vim'

" Local plugins
if filereadable(expand("~/.local/vim/plugs.vim"))
  so ~/.local/vim/plugs.vim
endif

" Manually managed
Plug '~/.fzf'
Plug '~/.vim/gtags'

call plug#end()

" General settings
syntax on
filetype plugin indent on
set mouse=a
set mousehide
set shortmess+=filmnrxoOtT
set history=1000
set virtualedit=onemore
set hidden
set nospell
set showmode
set backspace=indent,eol,start
set linespace=0
set nostartofline
set number
set noshowmatch
set incsearch
set hlsearch
set ignorecase
set smartcase
set wildmenu
set modeline
set cmdheight=2
set wildmode=list:longest,full
set whichwrap=b,s,h,l,<,>,[,]
set scrolljump=5
set scrolloff=3
set list
set listchars=tab:>>,trail:-,extends:#,nbsp:.
set nowrap
set autoindent
set breakindent
set shiftwidth=2
set expandtab
set tabstop=2
set softtabstop=2
set nojoinspaces
set splitright
set splitbelow
set backup
set backupdir=$HOME/.local/share/vim/backups
set undofile
set undodir=$HOME/.local/share/vim/undo
set viminfo^=%
set formatoptions+=j
set lazyredraw

" Automatically set the cursor to first line
" when editing a git commit message
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

" Automatically delete trailing whitespace
func! DeleteTrailingWS()
  exe "normal mz"
  $s/\s\+$//ge
  exe "normal `z"
endfunc
au BufWrite *.cpp :call DeleteTrailingWS()
au BufWrite *.h :call DeleteTrailingWS()

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

" Automatically enter insert mode when focussing a terminal buffer
au BufWinEnter, WinEnter term://* startinsert

" disable paren matching in TeX, it's really slow
au FileType tex :NoMatchParen

" Keybindings
let mapleader=' '

inoremap kj <ESC>

nmap <silent> ,/ :set invhlsearch<CR>
nmap <silent> <Leader>o :Bclose<CR>

nnoremap ; :
nnoremap K <nop>
nnoremap Q <nop>
nnoremap <Leader>< :bp<CR>
nnoremap <Leader>> :bn<CR>
nnoremap <Leader>vs :vsplit<CR>
nnoremap <Leader>ss :split<CR>
nnoremap <Leader>hh :resize 60<CR>
nnoremap <Leader>y "+y
nnoremap <Leader>p "+p
nnoremap <Leader>ve :edit ~/.vimrc<CR>

map <Leader>pp :setlocal paste!<CR>
map <Leader>ss :setlocal spell!<CR>
map <leader>fc /\v^[<\|=>]{7}( .*\|$)<CR>
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h
map <C-g> <ESC>
map! <C-g> <ESC>

vmap <Tab> >gv
vmap <S-Tab> <gv

vnoremap > >gv
vnoremap < <gv

" Color scheme
set background=dark
let g:gruvbox_contrast_dark = 'hard'
let g:base16colorspace = 256
let g:base16_underline = 1
let g:base16_italic = 1
let g:base16_termtrans = 1
let g:nord_italic_comments = 10
let g:gruvbox_italicize_comments = 1
let g:onedark_terminal_italics = 1
if !has('gui_running')
  if !($TERM == "linux" || $OLDTERM == "putty-256color") && (has('termguicolors') && (has('nvim') || v:version >= 800 || has('patch1942')))
    call toggletheme#maptransparency("<F10>")
    call toggletheme#mapbg("<F11>")
    call toggletheme#map256("<F12>")
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

  " Read project specific settings from cwd
  if filereadable(expand("~/.base16_theme.vim"))
    try
      so ~/.base16_theme.vim
    catch /^Vim\%((\a\+\)\=:E185/
      colorscheme base16
    endtry
  endif

  "hi Normal ctermbg=NONE guibg=NONE
endif
if has('gui_running')
  set guifont=Source\ Code\ Pro\ 9
  set guioptions-=T
  set guioptions-=e
  set guioptions-=m
  set guioptions-=r
  set guioptions-=l
  set guioptions-=C
  set guioptions-=L
  set guioptions+=c
  colorscheme gruvbox
endif

" Airline setup
set noshowmode
set laststatus=2
let g:airline_theme = 'base16'
let g:airline_extensions =
            \ [
            \ 'branch',
            \ 'neomake',
            \ 'netrw',
            \ 'tabline',
            \ 'whitespace',
            \ 'ycm',
            \ ]
if !has('gui_running') && ($TERM == "linux" || $TERM == "putty-256color" || $OLDTERM == "putty-256color")
  " Disable powerline symbols when it seems unlikely we'll have them
  let g:airline_left_sep = ''
  let g:airline_left_alt_sep = ''
  let g:airline_right_sep = ''
  let g:airline_right_alt_sep = ''
  let g:airline_branch = ''
  let g:airline_readonly = ''
  let g:airline_linenr = ''
else
  let g:airline_left_sep = ''
  let g:airline_left_alt_sep = ''
  let g:airline_right_sep = ''
  let g:airline_right_alt_sep = ''
  let g:airline_branch = ''
  let g:airline_readonly = ''
  let g:airline_linenr = ''
endif

" Omnicomplete
set completeopt+=longest
hi Pmenu  guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
hi PmenuSbar  guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
hi PmenuThumb  guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE

" Tags
set tags=./tags;./TAGS

" Rainbow
let g:rainbow_active = 0

" Fugitive
nnoremap <silent> <Leader>gs :Gstatus<CR>
nnoremap <silent> <Leader>gd :Gdiff<CR>
nnoremap <silent> <Leader>gc :Gcommit<CR>
nnoremap <silent> <Leader>gb :Gblame<CR>
nnoremap <silent> <Leader>gl :Glog<CR>
nnoremap <silent> <Leader>gp :Git push<CR>
nnoremap <silent> <Leader>gr :Gread<CR>
nnoremap <silent> <Leader>gw :Gwrite<CR>
nnoremap <silent> <Leader>ge :Gedit<CR>
nnoremap <silent> <Leader>gi :Git add -p %<CR>
nnoremap <silent> <Leader>gg :SignifyToggle<CR>

" Polyglot
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
"let g:cpp_experimental_simple_template_highlight = 1
let g:polyglot_disabled = [ 'latex' ]

" AlternateFiles
let g:alternateNoDefaultAlternate = 1

" Ag
let g:ag_working_path_mode = 'r'

" Incsearch
if !has('nvim')
  "noremap <silent><expr> /  incsearch#go(<SID>incsearch_config())
  "noremap <silent><expr> ?  incsearch#go(<SID>incsearch_config({'command': '?'}))
  "noremap <silent><expr> g/ incsearch#go(<SID>incsearch_config({'is_stay': 1}))

  function! s:incsearch_config(...) abort
    return incsearch#util#deepextend(deepcopy({
                \ 'modules': [incsearch#config#easymotion#module({'overwin': 1})],
                \ 'keymap': {
                \   "\<C-l>": '<Over>(easymotion)'
                \ },
                \   'is_expr': 0
                \ }), get(a:, 1, {}))
  endfunction
  function! s:config_easyfuzzymotion(...) abort
    return extend(copy({
                \   'converters': [incsearch#config#fuzzyword#converter()],
                \   'modules': [incsearch#config#easymotion#module({'overwin': 1})],
                \   'keymap': {"\<C-l>": '<Over>(easymotion)'},
                \   'is_expr': 0,
                \   'is_stay': 1
                \ }), get(a:, 1, {}))
  endfunction
endif

" EasyMotion
let g:EasyMotion_startofline = 0
let g:EasyMotion_smartcase = 1
map <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)
nmap <Leader>s <Plug>(easymotion-overwin-f2)
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)
map <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)
nmap <Leader>h <Plug>(easymotion-linebackward)
nmap <Leader>j <Plug>(easymotion-j)
nmap <Leader>k <Plug>(easymotion-k)
nmap <Leader>l <Plug>(easymotion-lineforward)

" Rtags
let g:rtagsUseDefaultMappings = 0
let g:rtagsAutoLaunchRdm = 1
"noremap <Leader>ri :call rtags#SymbolInfo()<CR>
"noremap <Leader>rj :call rtags#JumpTo(g:SAME_WINDOW)<CR>
"noremap <Leader>rJ :call rtags#JumpTo(g:SAME_WINDOW, { '--declaration-only' : '' })<CR>
"noremap <Leader>rS :call rtags#JumpTo(g:H_SPLIT)<CR>
"noremap <Leader>rV :call rtags#JumpTo(g:V_SPLIT)<CR>
"noremap <Leader>rT :call rtags#JumpTo(g:NEW_TAB)<CR>
"noremap <Leader>rp :call rtags#JumpToParent()<CR>
"noremap <Leader>rf :call rtags#FindRefs()<CR>
"noremap <Leader>rn :call rtags#FindRefsByName(input("Pattern? ", "", "customlist,rtags#CompleteSymbols"))<CR>
"noremap <Leader>rs :call rtags#FindSymbols(input("Pattern? ", "", "customlist,rtags#CompleteSymbols"))<CR>
"noremap <Leader>rr :call rtags#ReindexFile()<CR>
"noremap <Leader>rl :call rtags#ProjectList()<CR>
"noremap <Leader>rw :call rtags#RenameSymbolUnderCursor()<CR>
"noremap <Leader>rv :call rtags#FindVirtuals()<CR>
"noremap <Leader>rb :call rtags#JumpBack()<CR>
"noremap <Leader>rC :call rtags#FindSuperClasses()<CR>
"noremap <Leader>rc :call rtags#FindSubClasses()<CR>
"noremap <Leader>rd :call rtags#Diagnostics()<CR>

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
nnoremap <Leader>p :GFiles<CR>
nnoremap <Leader>gl :Commits<CR>
nnoremap <Leader>gbl :BCommits<CR>
nnoremap <Leader>gs :GFiles?<CR>
nnoremap <Leader><Space> :Commands<CR>
nnoremap <Leader>h :Helptags<CR>
nnoremap <Leader>t :Tags<CR>
nnoremap <Leader>b :Buffer<CR>
nnoremap <Leader>a :Rg<Space>

" YouCompleteMe
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_add_preview_to_completeopt = 1
let g:ycm_confirm_extra_conf = 0
let g:ycm_disable_for_files_larger_than_kb = 2000
"inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

" VimCompletesMe
let g:vcm_default_maps = 0

" Neomake

" Neovim specific settings
if has('nvim')
  "set inccommand=nosplit
  tnoremap <C-g> <C-\><C-n>
  tnoremap <C-j> <C-\><C-n><C-w>j
  tnoremap <C-k> <C-\><C-n><C-w>k
  tnoremap <C-l> <C-\><C-n><C-w>l
  tnoremap <C-h> <C-\><C-n><C-w>h
endif

" clang-format
augroup clang-format
  autocmd FileType cpp nnoremap <C-m> :ClangFormat<CR>
  autocmd FileType cpp vnoremap <C-m> :ClangFormat<CR>
  autocmd FileType c nnoremap <C-m> :ClangFormat<CR>
  autocmd FileType c vnoremap <C-m> :ClangFormat<CR>
augroup END
let g:clang_format#code_style = 'llvm'

" Read local machine settings
if filereadable(expand("~/.lvimrc"))
  so ~/.lvimrc
endif

" Read project specific settings from cwd
if filereadable(".project.vim")
  so .project.vim
endif
