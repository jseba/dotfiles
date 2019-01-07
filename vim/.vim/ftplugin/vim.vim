" keep help windows at the bottom
if &ft == 'help'
  wincmd J
endif

setlocal shiftwidth=2
setlocal softtabstop=-1
setlocal foldmethod=marker
setlocal keywordprg=:help
setlocal textwidth=78

if !exists('b:undo_ftplugin') | let b:undo_ftplugin = '' | endif
let b:undo_ftplugin .= '|setlocal foldmethod< keywordprg< textwidth< shiftwidth< softtabstop<'
