function! helpers#breakline()
    s/^\(\s*\)\(.\{-}\)\(\s*\)\(\%#\)\(\s*\)\(.*\)/\1\2\r\1\4\6
    call histdel("/",-1)
endfunction

function! helpers#bufcloseit()
  let l:currentBufNum = bufnr("%")
  let l:alternateBufNum = bufnr("#")

  if buflisted(l:alternateBufNum)
    buffer #
  else
    bnext
  endif

  if bufnr("%") == l:currentBufNum
    new
  endif

  if buflisted(l:currentBufNum)
    execute("bdelete! ".l:currentBufNum)
  endif
endfunction

function helpers#ccr()
  let cmdline = getcmdline()
  if cmdline =~ '\v\C^(ls|files|buffers)'
    return "\<cr>:b"
  elseif cmdline =~ '\v\C/(#|nu|num|numb|numbe|number)$'
    return "\<cr>:"
  elseif cmdline =~ '\v\C^(dli|il)'
    return "\<cr>:" . cmdline[0] . "j  " . split(cmdline, " ")[1] . "<s-left>\<left>"
  elseif cmdline =~ '\v\C^(cli|lli)'
    return "\<cr>:sil " . repeat(cmdline[0], 2) . "\<space>"
  elseif cmdline =~ '\C^old'
    set nomore
    return "\<cr>:sil se more|e #<"
  elseif cmdline =~ '\C^changes'
    set nomore
    return "\<cr>:sil se more|norm! g;\<s-left>"
  elseif cmdline =~ '\C^ju'
    set nomore
    return "\<cr>:sil se more|norm! \<c-o>\<s-left>"
  elseif cmdline =~ '\C^marks'
    return "\<cr>:norm! `"
  elseif cmdline =~ '\C^undol'
    return "\<cr>:u "
  else
    return "\<cr>"
  endif
endfunction

function! helpers#delete_trailing_whitespace()
  execute"normal mz"
  $s/\s\+$//ge
  execute"normal `z"
endfunction

function! helpers#mru_complete(arg_lead, cmdline, cursor_pos)
  let l:readables = filter(copy(v:oldfiles), { i, v -> filereadable(expand(v)) })
  return filter(l:readables, 'v:val =~ a:arg_lead)
endfunction

function helpers#mru(cmd, arg)
  if a:cmd == "tabedit"
    execute a:cmd . " " . a:arg . "\|lcd %:p:h"
  else
    execute a:cmd . " " . a:arg
  endif
endfunction

function! helpers#operator_grep(type) abort
  let l:regs = @@

  if a:type ==# 'v'
    normal `<v`>y
  elseif a:type ==# 'char'
    normal! `[v`]y
  else
    return
  endif

  silent execute "grep! " . shellescape(@@) . " ."
  copen

  let @@ = l:regs
endfunction

function! helpers#hiword(n)
  normal! mz
  normal! "zyiw
  let l:mid = 86750 + a:n
  silent! call matchdelete(n)
  let l:pat = '\V\<' . escape(@z,'\') . '\>'
  call matchadd("InterestingWord" . a:n, pat, 1, mid)
  normal! `z
endfunction

function! helpers#open_scratch_buffer(new_win)
  if !exists('g:scratch_buffer_name')
    let g:scratch_buffer_name = '__scratch__'
  endif

  let buf_name = g:scratch_buffer_name
  let split_win = a:new_win

  " if current buffer is modified, default to opening a new window
  if !l:split_win && &modified
    let l:split_win = 1
  endif

  " check for an existing scratch buffer
  let buf_num = bufnr(l:buf_name)
  if l:buf_num == -1
    if l:split_win
      execute "new " . l:buf_name
    else
      execute "edit " . l:buf_num
    endif
  else
    " check if an open window has the scratch buffer
    let win_num = bufwinnr(l:buf_num)
    if l:win_num != -1
      if winnr() != l:win_num
        executel:win_num . "wincmd w"
      endif
    else
      if l:split_win
        execute"split +buffer" . l:buf_num
      else
        execute"buffer " . l:buf_num
      endif
    endif
  endif

  call helpers#mark_as_scratch_buffer()
endfunction

function! helpers#mark_as_scratch_buffer()
  setl buftype=nofile
  setl bufhidden=hide
  setl noswapfile
  setl buflisted
endfunction
