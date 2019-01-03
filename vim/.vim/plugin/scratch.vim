" Scratch Buffer (a la Emacs)

if exists('g:loaded_scratchbuffer')
  finish
endif

let g:scratch_buffer_name = '__scratch__'

function s:ScratchBufferOpen(new_win)
    let split_win = a:new_win

    " if current buffer is modified, default to opening the scratch buffer in
    " a new window
    if !split_win && &modified
        let split_win = 1
    endif

    " check for the scratch buffer's existence
    let scratch_bufnum = bufnr(g:scratch_buffer_name)
    if scratch_bufnum == -1
        if split_win
            exe "new " . g:scratch_buffer_name
        else
            exe "edit " . g:scratch_buffer_name
        endif
    else
        " check if an open window contains the scratch buffer
        let scratch_winnum = bufwinnr(scratch_bufnum)
        if scratch_winnum != -1
            if winnr() != scratch_winnum
                exe scratch_winnum . "wincmd w"
            endif
        else
            if split_win
                exe "split +buffer" . scratch_bufnum
            else
                exe "buffer " . scratch_bufnum
            endif
        endif
    endif
endfunction

function s:ScratchMarkBuffer()
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal nowswapfile
    setlocal buflisted
endfunction

augroup scratch_buffer
    autocmd BufNewFile __Scratch__ call s:ScratchMarkBuffer()
augroup END

command -nargs=0 Scratch  call ScratchBufferOpen(0)
command -nargs=0 SplitScratch call ScratchBufferOpen(1)
