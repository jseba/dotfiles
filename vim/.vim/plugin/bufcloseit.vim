if exists('g:bufcloseit_loaded')
    finish
endif

command! Bclose call buf#bufcloseit()

let g:bufcloseit_loaded = 1
