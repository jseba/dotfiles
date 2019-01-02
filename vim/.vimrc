set nocompatible
scriptencoding utf-8
filetype plugin indent on

" Basic options
set autoindent
set hidden
set ruler
set backspace=indent,eol,start
set noshowmode
set showcmd
set hidden
set ttyfast
set nonumber
set laststatus=2
set history=1000
set splitbelow
set splitright
set listchars=tab:>>,trail:-,extends:#,nbsp:.
set list
set colorcolumn=+1
set diffopt+=vertical
set linebreak
set norelativenumber
set noautoread
set autowrite
set shiftround
set title
set matchtime=3
set lazyredraw

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

" Wildmenu completion
" XXX: move me
set wildmenu
set wildmode=list:longest

" Ignore uninteresting files/folders
" XXX: move me
set wildignore+=.hg,.git,.svn
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.spl
set wildignore+=*.sw?
set wildignore+=*.DS_Store
set wildignore+=*.luac
set wildignore+=*.pyc
set wildignore+=*.orig

" TODO: add more things
" set wildignore+=

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

set undodir=~/.vim/tmp/undo//
set backupdir=~/.vim/tmp/backup//
set directory=~/.vim/tmp/swap//

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

" Highlight VC conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Abbreviations
" XXX: moveme
silent! digr -. 8230 " U+2026 HORIZONTAL ELLIPSIS
silent! digr !, 8816 " U+2270 NEITHER LESS-THAN NOR EQUAL TO
silent! digr !. 8817 " U+2271 NEITHER GREATER-THAN NOR EQUAL TO
silent! digr xs 8339 " U+2093 SUBSCRIPT X
silent! digr >< 8652 " U+21cc EQUILIBRIUM
silent! digr o+ 8853 " U+2295 CIRCLED PLUS

" Base keymappings
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

" quick access to common files
nnoremap <leader>ve :edit $MYVIMRC<cr>
nnoremap <leader>vd :edit ~/.vim/words<cr>
nnoremap <leader>vg :edit ~/.gitconfig<cr>
nnoremap <leader>vz :edit ~/.zshrc<cr>
nnoremap <leader>vt :edit ~/.tmux.conf<cr>

" Status Line
" XXX: move me

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

" TODO: dim status line of inactive windows
" see Emacs configuration
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

" Searching and Movement
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
map <tab> %
silent! unmap [%
silent! unmap ]%

" don't move on */#
nnoremap <silent> * :let stay_view = winsaveview()<cr>*:call winrestview(stay_view)<cr>
nnoremap <silent> # :let stay_view = winsaveview()<cr>#:call winrestview(stay_view)<cr>

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

" Visual Mode */#
" XXX: move me
function! s:VSetSearch()
    let l:temp = @@
    norm! gvy
    let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
    let @@ = temp
endfunction

vnoremap * :<c-u>call <SID>VSetSearch()<cr>//<cr><c-o>
vnoremap # :<c-u>call <SID>VSetSearch()<cr>??<cr><c-o>

" List navigation (arrow keys aren't used otherwise, make them useful)
nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz

" Folding
set foldlevelstart=0

" recursively open whatever fold the cursor is in, even if partially open
nnoremap z0 zcz0

" Focus current line
" XXX: move me
function! FocusLine()
    let l:oldscrolloff = &scrolloff
    set scrolloff=0
    exe "keepjumps normal! mzzMzvzt25\<c-y>`z:Pulse\<cr>"
    let &scrolloff = l:oldscrolloff
endfunction
nnoremap <c-z> :call FocusLine()<cr>

" Filetypes
" XXX: move me

" TODO: set these better
set cinoptions+=N-s    " don't indent namespaces
set cinoptions+=g0     " don't indent C++ public/private/protected
set cinoptions+=:-s    " don't indent case labels
set cinoptions+=E-s    " don't indent in C++ extern blocks
set cinoptions+=(0     " line up unclosed parentheses insides...
set cinoptions+=w1     " ...but ignore whitespace after the open paren

augroup ft_c
    au!
    au FileType c setlocal foldmethod=marker foldmarker={,}
    au FileType c setlocal ts=2 sts=2 sw=2 expandtab
augroup END

augroup ft_cpp
    au!
    au FileType cpp setlocal foldmethod=marker foldmarker={,}
    au FileType cpp setlocal ts=2 sts=2 sw=2 expandtab
augroup END

function! DiffFoldLevel()
    let l:line=getline(v:lnum)
    if l:line =~# '^\(diff\|Index\)' " file
        return '>1'
    elseif l:line =~# '^\(@@\|\d\)'  " hunk
        return '>2'
    elseif l:line =~# '^\*\*\* \d\+,\d\+ \*\*\*\*$' " context: file1
        return '>2'
    elseif l:line =~# '^--- \d\+,\d\+ ----$'        " context: file2
        return '>2'
    else
        return '='
    endif
endfunction

augroup ft_diff
    au!
    autocmd FileType diff setlocal foldmethod=expr
    autocmd Filetype diff setlocal foldexpr=DiffFoldLevel()
augroup END

augroup ft_dtrace
    au!
    autocmd BufNewFile,BufRead *.d set filetype=dtrace
augroup END

augroup ft_vim
    au!
    autocmd FileType vim setlocal foldmethod=marker keywordprg=:help
    autocmd FileType help setlocal textwidth=78
    autocmd BufEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END

" Grep
" XXX: move me
if executable('rg')
    let g:grepprg='rg --vimgrep --no-heading'
    let g:grepformat='%f:%l:%c:%m,%f:%l:%m'
endif
nnoremap <leader>a :grep<space>

" Location List Toggles
" XXX: move me
function! ErrorsToggle()
    if exists("w:is_error_window")
        unlet w:is_error_window
        exec "q"
    else
        exec "Errors"
        lopen
        let w:is_error_window=1
    endif
endfunction
command! ErrorsToggle call ErrorsToggle()

function! LocationToggle()
    if exists("w:is_location_window")
        unlet w:is_location_window
        exec "q"
    else
        lopen
        let w:is_location_window=1
    endif
endfunction
command! LocationToggle call LocationToggle()

function! QuickFixToggle(forced)
    if exists("g:quickfix_win") && a:forced == 0
        cclose
        unlet g:quickfix_win
    else
        copen 10
        let g:quickfix_win = bufnr("$")
    endif
endfunction
command! -bang -nargs=? QuickFixToggle call QuickFixToggle(<bang>0)

" Grep operator
" XXX: move me
function! s:GrepMotion(type) abort
    let l:reg_save = @@

    if a:type ==# 'v'
        normal! `<v`>y
    elseif a:type ==# 'char'
        normal! `[v`]y
    else
        return
    endif

    silent execute "grep! -R " . shellescape(@@) . " ."
    copen

    let @@ = l:reg_save
endfunction

nnoremap <leader>g :set operatorfunc=<SID>GrepOperator<cr>g@
vnoremap <leader>g :<c-u>call <SID>GrepOperator(visualmode())<cr>

" Pulse line
" XXX: move me
function! s:Pulse()
    redir => old_hi
        silent execute 'hi CursorLine'
    redir END
    let old_hi = split(old_hi, '\n')[0]
    let old_hi = substitute(old_hi, 'xxx', '')

    let steps = 8
    let width = 1
    let start = width
    let end = steps * width
    let color = 233

    for i in range(start,end,width)
        execute "hi CursorLine ctermbg=" . (color+i)
        redraw
        sleep 6m
    endfor
    for i in range(end,start,-1*width)
        execute "hi CursorLine ctermbg=" . (color+i)
        redraw
        sleep 6m
    endfor

    execute 'hi ' . old_hi
endfunction
command! -nargs=0 Pulse call :Pulse()

" Highlight Interesting Words
function! HiInterestingWord(n)
    normal! mz
    normal! "zyiw
    let l:mid = 86750 + a:n
    silent! call matchdelete(mid)
    let l:pat = '\V\<' . escape(@z,'\') . '\>'
    call matchadd("InterestingWord" . a:n, pat, 1, mid)
    normal! `z
endfunction

hi def InterestingWord1 ctermbg=214 ctermfg=16 guibg=#ffa724 guifg=#000000
hi def InterestingWord2 ctermbg=154 ctermfg=16 guibg=#aeee00 guifg=#000000
hi def InterestingWord3 ctermbg=121 ctermfg=16 guibg=#8cffba guifg=#000000
hi def InterestingWord4 ctermbg=137 ctermfg=16 guibg=#b88853 guifg=#000000
hi def InterestingWord5 ctermbg=211 ctermfg=16 guibg=#ff9eb8 guifg=#000000
hi def InterestingWord6 ctermbg=195 ctermfg=16 guibg=#ff2c4b guifg=#000000

nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>

" Scratch buffer
let g:scratch_buffer_name = '__scratch__'

function! s:ScratchBufferOpen(new_win)
    let split_win = a:new_win

    " if current buffer is modified, default to opening the scratch buffer in
    " a new window
    if !split_win && &modified
        let split_win = 1
    endif

    " check for the scratch buffer's existence
    let scratch_bufnum = bufnr(g:scratch_buffer_name)
    if scratch_bufnum == -1
        if split_win
            exe "new " . g:scratch_buffer_name
        else
            exe "edit " . g:scratch_buffer_name
        endif
    else
        " check if an open window contains the scratch buffer
        let scratch_winnum = bufwinnr(scratch_bufnum)
        if scratch_winnum != -1
            if winnr() != scratch_winnum
                exe scratch_winnum . "wincmd w"
            endif
        else
            if split_win
                exe "split +buffer" . scratch_bufnum
            else
                exe "buffer " . scratch_bufnum
            endif
        endif
    endif
endfunction

function! s:ScratchMarkBuffer()
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal nowswapfile
    setlocal buflisted
endfunction

augroup scratch_buffer
    autocmd BufNewFile __Scratch__ call s:ScratchMarkBuffer()
augroup END

command! -nargs=0 Scratch  call :ScratchBufferOpen(0)
command! -nargs=0 SplitScratch call :ScratchBufferOpen(1)
