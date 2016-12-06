set nocompatible
scriptencoding utf-8

" Bundles
filetype off
runtime bundle/vim-pathogen/autoload/pathogen.vim
execute pathogen#infect()
execute pathogen#helptags()

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
set showmatch
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
set shiftwidth=4
set expandtab
set tabstop=4
set softtabstop=4
set nojoinspaces
set splitright
set splitbelow
set backup
set backupdir=$HOME/.nvim/backups
set undofile
set undodir=$HOME/.nvim/undo
set viminfo^=%

" Automatically set the cursor to first line
" when editing a git commit message
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

" Automatically close NerdTree when it's the last buffer open
au bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

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

" Keybindings
let mapleader=' '

inoremap kj <ESC>

nmap <silent> ,/ :set invhlsearch<CR>
nmap <silent> <C-q> :Bclose<CR>

nnoremap ; :
nnoremap K <nop>
nnoremap Q <nop>
nnoremap <Leader>< :bp<CR>
nnoremap <Leader>> :bn<CR>
nnoremap <Leader>vs :vsplit<CR>
nnoremap <Leader>ss :split<CR>
nnoremap <Leader>hh :resize 60<CR>

map <Leader>pp :setlocal paste!<CR>
map <Leader>ss :setlocal spell!<CR>
map <leader>fc /\v^[<\|=>]{7}( .*\|$)<CR>
map <C-S-J> <C-W><C-S-J>
map <C-S-K> <C-W><C-S-K>
map <C-S-L> <C-W><C-S-L>
map <C-S-H> <C-W><C-S-H>
map <C-G> <ESC>
map! <C-G> <ESC>

vmap <Tab> >gv
vmap <S-Tab> <gv

vnoremap > >gv
vnoremap < <gv

" Color scheme
set background=dark
colorscheme molokai
if $TERM != "linux"
    set t_Co=256
    set t_so=[7m
    set t_ZH=[3m
    set t_ZR=[23m
    let base16colorspace = 256
    let g:base16_termtrans = 1
    let g:base16_underline = 1
    let g:base16_italic = 1
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
endif

" Airline setup
set laststatus=2
if !has('gui_running') && ($TERM == "linux" || $TERM == "putty-256color" || $OLDTERM == "putty-256color")
    " Disable powerline symbols when it seems unlikely we'll have them
    let g:airline_powerline_fonts = 0
    let g:airline_left_sep = ''
    let g:airline_left_alt_sep = ''
    let g:airline_right_sep = ''
    let g:airline_right_alt_sep = ''
else
    if !exists('g:airline_symbols')
        let g:airline_symbols={}
    endif
    let g:airline_left_sep = ''
    let g:airline_left_alt_sep = ''
    let g:airline_right_sep = ''
    let g:airline_right_alt_sep = ''
    let g:airline_symbols.branch = ''
    let g:airline_symbols.readonly = ''
    let g:airline_symbols.linenr = ''
    let g:airline_powerline_fonts = 1
endif
let g:airline_theme = 'base16'
let g:airline_inactive_collapse = 0

" Omnicomplete
autocmd Filetype * 
    \if &omnifunc == "" |
        \setlocal omnifunc=syntaxcomplete#Complete |
    \endif
hi Pmenu  guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
hi PmenuSbar  guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
hi PmenuThumb  guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE

" Tags
set tags=./tags;./TAGS

" Rainbow
let g:rainbow_active=1

" NerdTree
map <C-e> <plug>NERDTreeTabsToggle<CR>
map <leader>e :NERDTreeFind<CR>
nmap <leader>nt :NERDTreeFind<CR>
let g:NERDTreeShowBookmarks = 1
let g:NERDTreeIgnore = ['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
let g:NERDTreeChDirMode = 0
let g:NERDTreeQuitOnOpen = 1
let g:NERDTreeShowHidden = 1
let g:NERDTreeKeepTreeInNewTab = 1
let g:NERDTreeDirArrowExpandable = '>'
let g:NERDTreeDirArrowCollapsible = 'v'
let g:nerdtree_tabs_open_on_gui_startup = 0

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

" AlternateFiles
let g:alternateNoDefaultAlternate = 1

" Ag
let g:ag_working_path_mode = 'r'

" EasyMotion
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
let g:EasyMotion_startofline = 0
let g:EasyMotion_smartcase = 1
noremap <silent><expr> /  incsearch#go(<SID>incsearch_config())
noremap <silent><expr> ?  incsearch#go(<SID>incsearch_config({'command': '?'}))
noremap <silent><expr> g/ incsearch#go(<SID>incsearch_config({'is_stay': 1}))
noremap <silent><expr> <Space>/ incsearch#go(<SID>config_easyfuzzymotion())
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
noremap <Leader>ri :call rtags#SymbolInfo()<CR>
noremap <Leader>rj :call rtags#JumpTo(g:SAME_WINDOW)<CR>
noremap <Leader>rJ :call rtags#JumpTo(g:SAME_WINDOW, { '--declaration-only' : '' })<CR>
noremap <Leader>rS :call rtags#JumpTo(g:H_SPLIT)<CR>
noremap <Leader>rV :call rtags#JumpTo(g:V_SPLIT)<CR>
noremap <Leader>rT :call rtags#JumpTo(g:NEW_TAB)<CR>
noremap <Leader>rp :call rtags#JumpToParent()<CR>
noremap <Leader>rf :call rtags#FindRefs()<CR>
noremap <Leader>rn :call rtags#FindRefsByName(input("Pattern? ", "", "customlist,rtags#CompleteSymbols"))<CR>
noremap <Leader>rs :call rtags#FindSymbols(input("Pattern? ", "", "customlist,rtags#CompleteSymbols"))<CR>
noremap <Leader>rr :call rtags#ReindexFile()<CR>
noremap <Leader>rl :call rtags#ProjectList()<CR>
noremap <Leader>rw :call rtags#RenameSymbolUnderCursor()<CR>
noremap <Leader>rv :call rtags#FindVirtuals()<CR>
noremap <Leader>rb :call rtags#JumpBack()<CR>
noremap <Leader>rC :call rtags#FindSuperClasses()<CR>
noremap <Leader>rc :call rtags#FindSubClasses()<CR>
noremap <Leader>rd :call rtags#Diagnostics()<CR>

" Deoplete
let g:deoplete#enable_at_startup = 1
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

" FZF
" Augment Ag command with fzf#vim#with_preview
au VimEnter * command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

let g:fzf_files_options = '--preview "(highlight -0 -ansi {} || cat {}) 2>/dev/null | head -'.&lines.'"'
let g:fzf_layout = { 'down': '~15%' }
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
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
nnoremap <Leader>a :Ag<Space>
inoremap <C-x><C-l> <Plug>(fzf-complete-line)


" Neomake
let g:neomake_cpp_enabled_makers = ['clangcheck']
"let g:neomake_cpp_clang_errorformat = '%f:%l:%c: %trror: %m,'

" Read project specific settings from cwd
if filereadable(".project.vim")
    so .project.vim
endif
