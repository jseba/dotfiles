function! s:CCR()
  let cmdline = getcmdline()

  if cmdline =~ '\v\C^(ls|files|buffers)'
