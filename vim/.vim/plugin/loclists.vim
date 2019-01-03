" Location Lists

if exists('g:loaded_loclists')
  finish
endif

function s:ErrorsToggle()
    if exists("w:is_error_window")
        unlet w:is_error_window
        exec "q"
    else
        exec "Errors"
        lopen
        let w:is_error_window=1
    endif
endfunction

function s:LocationToggle()
    if exists("w:is_location_window")
        unlet w:is_location_window
        exec "q"
    else
        lopen
        let w:is_location_window=1
    endif
endfunction

function s:QuickFixToggle(forced)
    if exists("g:quickfix_win") && a:forced == 0
        cclose
        unlet g:quickfix_win
    else
        copen 10
        let g:quickfix_win = bufnr("$")
    endif
endfunction

command ErrorsToggle call <SID>ErrorsToggle()
command LocationToggle call <SID>LocationToggle()
command -bang -nargs=? QuickFixToggle call <SID>QuickFixToggle(<bang>0)

let g:loaded_loclists = 1
