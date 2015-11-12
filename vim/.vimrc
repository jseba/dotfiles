set nocompatible
scriptencoding utf-8

" Pathogen
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
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.
set nowrap
set autoindent
set shiftwidth=4
set expandtab
set tabstop=4
set softtabstop=4
set nojoinspaces
set splitright
set splitbelow

" Automatically set the cursor to first line
" when editing a git commit message
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

" Keybindings
let mapleader=','
inoremap jj <ESC>
nnoremap ; :
map <leader>fc /\v^[<\|=>]{7}( .*\|$)<CR>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <Leader>< :bp<CR>
nnoremap <Leader>> :bn<CR>
nnoremap <C-p> :CtrlPMixed<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>
nnoremap <Leader>f :CtrlP<CR>
nmap <silent> <leader>/ :set invhlsearch<CR>

" Powerline setup
set t_Co=256
set laststatus=2
python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

" Omnicomplete
autocmd Filetype * 
    \if &omnifunc == "" |
        \setlocal omnifunc=syntaxcomplete#Complete |
    \endif
hi Pmenu  guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
hi PmenuSbar  guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
hi PmenuThumb  guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE

" NerdTree
map <C-e> <plug>NERDTreeTabsToggle<CR>
map <leader>e :NERDTreeFind<CR>
nmap <leader>nt :NERDTreeFind<CR>
let NERDTreeShowBookmarks=1
let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
let NERDTreeChDirMode=0
let NERDTreeQuitOnOpen=1
let NERDTreeShowHidden=1
let NERDTreeKeepTreeInNewTab=1
let g:nerdtree_tabs_open_on_gui_startup=0

" Fugitive
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gb :Gblame<CR>
nnoremap <silent> <leader>gl :Glog<CR>
nnoremap <silent> <leader>gp :Git push<CR>
nnoremap <silent> <leader>gr :Gread<CR>
nnoremap <silent> <leader>gw :Gwrite<CR>
nnoremap <silent> <leader>ge :Gedit<CR>
nnoremap <silent> <leader>gi :Git add -p %<CR>
nnoremap <silent> <leader>gg :SignifyToggle<CR>

" YouCompleteMe
let g:acp_enableAtStartup=0
let g:ycm_collect_identifiers_from_tags_files=1
let g:ycm_confirm_extra_conf=0

" Polyglot
let g:cpp_class_scope_highlight=1
