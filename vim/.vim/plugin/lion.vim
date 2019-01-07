" lion.vim - A Vim plugin for text alignment operators
" Maintainer:   Tom McDonald <http://github.com/tommcdo>
" Version:      1.0
"
" Modified to use autoloads by Josh Seba <https://github.com/jseba>

function! s:assign_map(map, func)
    if a:map ==# ''
        return
    endif
    execute 'nmap <silent> ' . a:map . ' <Plug>Lion' . a:func
    execute 'vmap <silent> ' . a:map . ' <Plug>VLion' . a:func
endfunction

function! s:command(func, dir, ...)
    let s:count = v:count
    if a:0
        return ":\<C-U>call " . a:func . "(visualmode(), 1)\<CR>"
    else
        return ":\<C-U>set opfunc=" . a:func . "\<CR>g@"
    endif
endfunction

nnoremap <silent> <Plug>LionRepeat .
nnoremap <silent> <expr> <Plug>LionRight  <SID>command("lion#alignRight", 0)
vnoremap <silent> <expr> <Plug>VLionRight <SID>command("lion#alignRight", 1)
nnoremap <silent> <expr> <Plug>LionLeft   <SID>command("lion#alignLeft", 0)
vnoremap <silent> <expr> <Plug>VLionLeft  <SID>command("lion#alignLeft", 1)

if get(g:, 'lion_create_maps', 1)
    call <SID>assign_map(get(g:, 'lion_map_right', 'gl'), 'Right')
    call <SID>assign_map(get(g:, 'lion_map_left',  'gL'), 'Left')
endif
