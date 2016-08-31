set nocompatible
scriptencoding utf-8

" Bundles
filetype off
runtime bundle/vim-pathogen/autoload/pathogen.vim
execute pathogen#infect()

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
set number
set showmatch
set incsearch
set hlsearch
set ignorecase
set smartcase
set wildmenu
set wildmode=list:longest,full
set whichwrap=b,s,h,l,<,>,[,]
set scrolljump=5
set scrolloff=3
set list
set listchars=tab:>>,trail:-,extends:#,nbsp:.
set nowrap
set autoindent
set shiftwidth=4
set expandtab
set tabstop=4
set softtabstop=4
set nojoinspaces
set splitright
set splitbelow
set cursorline
set undofile
set undodir=$HOME/.vim/undo

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
autocmd BufWrite *.cpp :call DeleteTrailingWS()
autocmd BufWrite *.h :call DeleteTrailingWS()

" Keybindings
let mapleader=','
inoremap jj <ESC>
nnoremap ; :
map <leader>fc /\v^[<\|=>]{7}( .*\|$)<CR>
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-L> <C-W>l
map <C-H> <C-W>h
map <C-S-J> <C-W><C-S-J>
map <C-S-K> <C-W><C-S-K>
map <C-S-L> <C-W><C-S-L>
map <C-S-H> <C-W><C-S-H>
nnoremap <Leader>< :bp<CR>
nnoremap <Leader>> :bn<CR>
nnoremap <Leader>vs :vsplit<CR>
nnoremap <Leader>ss :split<CR>
nnoremap <Leader>hh :resize 60<CR>
nmap <silent> <leader>/ :set invhlsearch<CR>
map <Leader>pp :setlocal paste!<CR>
map <Leader>ss :setlocal spell!<CR>

" Color scheme
set background=dark
if $TERM != "linux"
    set t_Co=256
    set t_so=[7m
    set t_ZH=[3m
    set t_ZR=[23m
    let base16colorspace=256
    let g:base16_termtrans=1
    let g:base16_underline=1
    let g:base16_italic=1
    colorscheme base16
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

nmap <Leader>( <Plug>ColorstepPrev
nmap <Leader>) <Plug>ColorstepNext

" Airline setup
set laststatus=2
if !has('gui_running') && $TERM == "linux"
    " Disable powerline symbols on Linux VT
    let g:airline_powerline_fonts=0
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
    let g:airline_powerline_fonts=1
endif
let g:airline_theme='base16'
let g:airline_inactive_collapse=0

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
let g:NERDTreeShowBookmarks=1
let g:NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
let g:NERDTreeChDirMode=0
let g:NERDTreeQuitOnOpen=1
let g:NERDTreeShowHidden=1
let g:NERDTreeKeepTreeInNewTab=1
let g:NERDTreeDirArrowExpandable='>'
let g:NERDTreeDirArrowCollapsible='v'
let g:nerdtree_tabs_open_on_gui_startup=0

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

" Deoplete
let g:deoplete#enable_at_startup=1

" Deoplete-clang
let g:deoplete#sources#clang#libclang_path="/usr/local/lib/libclang.so"
let g:deoplete#sources#clang#clang_header="/usr/local/lib/clang/3.9.0/include"
let g:deoplete#sources#clang#std={'c': 'c11', 'cpp': 'c++11' }
let g:deoplete#sources#clang#clang_complete_database="/ecn/cpp/build/debug"
inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : "<Tab>"

" Polyglot
let g:cpp_class_scope_highlight=1

" Unite
let g:unite_source_history_yank_enable=1
if executable("ag")
    "let g:unite_source_rec_async_command=['ag', '--follow', ' --nocolor', '--nogroup', '--hidden', '-g', '']
    let g:unite_source_grep_command='ag'
    let g:unite_source_grep_default_opts='-i --vimgrep --hidden --ignore ''.hg'' --ignore ''.git'' --ignore ''.svn'''
    let g:unite_source_grep_recursive_opts=''
endif
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#custom#profile('default', 'context', {
            \ 'direction': 'dynamicbottom'
            \ })
nnoremap <silent> <Leader>b :Unite -winheight=10 buffer<CR>
nnoremap <silent> <C-p> :Unite -buffer-name=files -start-insert file_rec/async<CR>
nnoremap <silent> <Leader>f :Unite -start-insert -auto-preview grep:.<CR>