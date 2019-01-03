function! s:GrepOperator(type) abort
  let l:reg_save = @@

  if a:type ==# 'v'
      normal! `<v`>y
  elseif a:type ==# 'char'
      normal! `[v`]y
  else
      return
  endif

  silent execute "grep! " . shellescape(@@) . " ."
  copen

  let @@ = l:reg_save
endfunction

function! grep#operator(type) abort
  call s:GrepOperator(a:type)
endfunction
