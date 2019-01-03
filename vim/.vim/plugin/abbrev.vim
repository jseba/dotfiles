" Abbreviations

if exists('g:loaded_abbrev')
  finish
endif

" TODO: add more
silent! digr -. 8230 " U+2026 HORIZONTAL ELLIPSIS
silent! digr !, 8816 " U+2270 NEITHER LESS-THAN NOR EQUAL TO
silent! digr !. 8817 " U+2271 NEITHER GREATER-THAN NOR EQUAL TO
silent! digr xs 8339 " U+2093 SUBSCRIPT X
silent! digr >< 8652 " U+21cc EQUILIBRIUM
silent! digr o+ 8853 " U+2295 CIRCLED PLUS

let g:loaded_abbrev = 1
