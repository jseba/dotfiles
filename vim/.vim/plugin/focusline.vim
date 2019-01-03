" Focus Current Line

if exists('g:loaded_focusline')
  finish
endif

command! -nargs=0 Pulse     call pulse#pulse()
command! -nargs=0 FocusLine call pulse#focusline()

let g:loaded_focusline = 1
