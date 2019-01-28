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

setlocal foldmethod=expr
setlocal foldexpr=DiffFoldLevel()

if !exists('b:undo_ftplugin') | let b:undo_ftplugin = '' | endif
let b:undo_ftplugin .= 'setl foldmethod< foldexpr<'
