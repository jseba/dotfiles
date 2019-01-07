" remove inapplicable defaults from 'indentkeys'
setlocal indentkeys-=0#,0{,0},0),:
if !exists('b:undo_indent')
    let b:undo_indent = 'setlocal indentkeys<'
endif

setlocal shiftwidth=2
setlocal softtabstop=2
let b:undo_indent .= '|setlocal shiftwidth< softtabstop<'
