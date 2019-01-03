" Highlight Interesting Words

if exists('g:loaded_hiword')
  finish
endif

function s:HiInterestingWord(n)
    normal! mz
    normal! "zyiw
    let l:mid = 86750 + a:n
    silent! call matchdelete(mid)
    let l:pat = '\V\<' . escape(@z,'\') . '\>'
    call matchadd("InterestingWord" . a:n, pat, 1, mid)
    normal! `z
endfunction

" TODO: make sure these highlight colors are useable
hi def InterestingWord1 ctermbg=214 ctermfg=16 guibg=#ffa724 guifg=#000000
hi def InterestingWord2 ctermbg=154 ctermfg=16 guibg=#aeee00 guifg=#000000
hi def InterestingWord3 ctermbg=121 ctermfg=16 guibg=#8cffba guifg=#000000
hi def InterestingWord4 ctermbg=137 ctermfg=16 guibg=#b88853 guifg=#000000
hi def InterestingWord5 ctermbg=211 ctermfg=16 guibg=#ff9eb8 guifg=#000000
hi def InterestingWord6 ctermbg=195 ctermfg=16 guibg=#ff2c4b guifg=#000000

nnoremap <Plug>HiWord1 :call <SID>HiInterestingWord(1)<cr>
nnoremap <Plug>HiWord2 :call <SID>HiInterestingWord(2)<cr>
nnoremap <Plug>HiWord3 :call <SID>HiInterestingWord(3)<cr>
nnoremap <Plug>HiWord4 :call <SID>HiInterestingWord(4)<cr>
nnoremap <Plug>HiWord5 :call <SID>HiInterestingWord(5)<cr>
nnoremap <Plug>HiWord6 :call <SID>HiInterestingWord(6)<cr>

let g:loaded_hiword = 1
