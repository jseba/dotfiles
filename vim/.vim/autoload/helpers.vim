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

function! helpers#delete_trailing_whitespace()
  exe "normal mz"
  $s/\s\+$//ge
  exe "normal `z"
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
        exe l:win_num . "wincmd w"
      endif
    else
      if l:split_win
        exe "split +buffer" . l:buf_num
      else
        exe "buffer " . l:buf_num
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
