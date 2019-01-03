setlocal foldmethod=marker foldmarker={,}
setlocal ts=4 sts=4 sw=4 expandtab

set cinoptions+=:-s    " don't indent case labels
set cinoptions+=(0     " line up unclosed parentheses insides...
set cinoptions+=w1     " ...but ignore whitespace after the open paren

if !exists('b:undo_ftplugin') | let b:undo_ftplugin='' | endif
let b:undo_ftplugin .= 'setl foldmethod< foldmarker< cinoptions< ts< sts< sw<'
