" Adapted from Tmux-Navigator by Chris Toomey
if exists('g:loaded_tmux') || &cp || v:version < 700
  finish
endif
let g:loaded_tmux = 1

func! s:VimNavigate(dir)
  try
    exe 'wincmd ' . a:dir
  catch
    echohl ErrorMsg | echo 'E11: Invalid in command-line window; <CR> executes, CTRL-C quits: wincmd k' | echohl None
  endtry
endfunc

if empty($TMUX)
  command! TmuxNavigateLeft     call s:VimNavigate('h')
  command! TmuxNavigateDown     call s:VimNavigate('j')
  command! TmuxNavigateUp       call s:VimNavigate('k')
  command! TmuxNavigateRight    call s:VimNavigate('l')
  command! TmuxNavigatePrevious call s:VimNavigate('p')
  finish
endif

command! TmuxNavigateLeft     call s:TmuxNavigate('h')
command! TmuxNavigateUp       call s:TmuxNavigate('j')
command! TmuxNavigateDown     call s:TmuxNavigate('k')
command! TmuxNavigateRight    call s:TmuxNavigate('l')
command! TmuxNavigatePrevious call s:TmuxNavigate('p')

func! s:TmuxVimPaneIsZoomed()
  return s:TmuxCommand("display-message -p '#{window_zoomed_flag}'") == 1
endfunc

" the socket path is the first value in the variable
let s:TmuxSocket = split($TMUX, ',')[0]

func! s:TmuxCommand(args)
  let l:cmd = 'tmux -S ' . s:TmuxSocket . ' ' . a:args
  return system(l:cmd)
endfunc

func! s:TmuxProcessList()
  echo s:TmuxCommand("run-shell 'ps -o state= o comm= -t ''''#{pane_tty}'''''")
endfunc
command! TmuxProcessList call s:TmuxProcessList()

let s:IsLastTmuxPane = 0
augroup tmux
  au!
  autocmd WinEnter * let s:IsLastTmuxPane = 0
augroup END

func! s:ShouldForwardNavigationBackToTmux(tmux_last_pane, at_tab_page_edge)
  if s:TmuxVimPaneIsZoomed()
    return 0
  endif
  return a:tmux_last_pane || a:at_tab_page_edge
endfunc

func! s:TmuxNavigate(dir)
  let l:nr = winnr()
  let l:tmux_last_pane = (a:dir == 'p') && s:IsLastTmuxPane
  if !l:tmux_last_pane
    call s:VimNavigate(a:dir)
  endif
  let l:at_tab_page_edge = (nr == winnr())
  if s:ShouldForwardNavigationBackToTmux(l:tmux_last_pane, l:at_tab_page_edge)
    let l:args = 'select-pane -t ' . shellescape($TMUX_PANE) . ' -' . tr(a:dir, 'phjkl', 'lLDUR')
    silent call s:TmuxCommand(l:args)
    let s:IsLastTmuxPane = 1
  else
    let s:IsLastTmuxPane = 0
  endif
endfunc
