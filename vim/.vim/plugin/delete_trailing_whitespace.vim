" Automatically delete trailing whitespace
if exists('g:deletetrailingwhitespace_loaded')
    finish
endif

function DeleteTrailingWS()
    if !binary && filetype != 'diff'
        normal mz
        normal Hmy
        $s/\s\+$//ge
        normal 'yz<CR>
        normal `z
    endif
endfunction

let g:deletetrailingwhitespace_loaded = 1
