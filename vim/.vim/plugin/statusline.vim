" Statusline

if exists('g:loaded_statusline')
  finish
endif

function ModeForStatusline()
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

let g:loaded_statusline = 1
