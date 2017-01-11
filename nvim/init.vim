set nocompatible
scriptencoding utf-8

" Bundles
filetype off
runtime bundle/vim-pathogen/autoload/pathogen.vim
execute pathogen#infect('bundle/{}', '~/.local/nvim/{}')
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
set backupdir=$HOME/.local/share/nvim/backups
set undofile
set undodir=$HOME/.local/share/nvim/undo
set viminfo^=%
set inccommand=nosplit
set formatoptions+=j

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

" Automatically enter insert mode when focussing a terminal buffer
au BufWinEnter, WinEnter term://* startinsert

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
nnoremap <Leader>y "+y
nnoremap <Leader>p "+p
nnoremap <Leader>ve :edit $HOME/.config/nvim/init.vim<CR>

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

tnoremap <C-g> <C-\><C-n>
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
tnoremap <C-h> <C-\><C-n><C-w>h

" Color scheme
set background=dark
colorscheme gruvbox
if !has('gui_running') && !($TERM == "linux" || $TERM == "putty-256color" || $OLDTERM == "putty-256color")
    set t_Co=256
    set t_so=[7m
    set t_ZH=[3m
    set t_ZR=[23m
    let g:base16_termtrans = 1
    let g:base16_underline = 1
    let g:base16_italic = 1
    let g:base16_italic = 1
    let g:base16colorspace = 1
    let g:gruvbox_contrast_dark = 'hard'
    let g:gruvbox_italicize_comments = 1
    set termguicolors
    hi Normal guibg=none
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

" Lightline setup
set laststatus=2
if !has('gui_running') && ($TERM == "linux" || $TERM == "putty-256color" || $OLDTERM == "putty-256color")
    " Disable powerline symbols when it seems unlikely we'll have them
    let g:statusline_left_sep = ''
    let g:statusline_left_alt_sep = ''
    let g:statusline_right_sep = ''
    let g:statusline_right_alt_sep = ''
    let g:statusline_branch = ''
    let g:statusline_readonly = ''
    let g:statusline_linenr = ''
else
    let g:statusline_left_sep = ''
    let g:statusline_left_alt_sep = ''
    let g:statusline_right_sep = ''
    let g:statusline_right_alt_sep = ''
    let g:statusline_branch = ''
    let g:statusline_readonly = ''
    let g:statusline_linenr = ''
endif
let g:lightline = {
            \ 'colorscheme': 'jellybeans'
            \ }

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
nnoremap <Leader>a :Ag<Space>
inoremap <C-x><C-l> <plug>(fzf-complete-line)

" YouCompleteMe
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_add_preview_to_completeopt = 1
let g:ycm_confirm_extra_conf = 0


" Neomake
let g:neomake_cpp_enabled_makers = ['clangcheck']

" Read local machine settings
if filereadable($HOME . "/.local/nvim/init.vim")
    so $HOME/.local/nvim/init.vim
endif

" Read project specific settings from cwd
if filereadable(".project.vim")
    so .project.vim
endif
