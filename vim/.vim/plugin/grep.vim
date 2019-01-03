" Grep

if exists('g:loaded_grep')
  finish
endif

if executable('rg')
    set grepprg=rg\ --vimgrep\ --no-heading
    set grepformat='%f:%l:%c:%m,%f:%l:%m'
endif

set operatorfunc=grep#operator

let g:loaded_grep = 1
