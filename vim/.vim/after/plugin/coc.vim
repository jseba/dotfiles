" CoC

inoremap <silent><expr> <C-space> coc#refresh()

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use <tab> for trigger completion and navigate to next complete item
inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_backspace() ? "\<TAB>" :
      \ coc#refresh()

inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"

function! s:check_backspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col-1] =~ '\s'
endfunction

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Remap for rename current word
nmap <space>qr <Plug>(coc-rename)

" Remap for format selected region
xmap <space>\\  <Plug>(coc-format-selected)
nmap <space>\\  <Plug>(coc-format-selected)

augroup coc
  autocmd!

  " Close preview window when completion is finished.
  autocmd CompleteDone * if pumvisible() == 0 | pclose | endif

  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json,cpp,c setl formatexpr=CocAction('formatSelected')

  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <space>qA  <Plug>(coc-codeaction-selected)
nmap <space>qA  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <space>qa  <Plug>(coc-codeaction)

" Fix autofix problem of current line
nmap <space>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
" set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
nnoremap <silent> <space>qd :<C-u>CocList diagnostics<cr>
nnoremap <silent> <space>qe :<C-u>CocList extensions<cr>
nnoremap <silent> <space>qc :<C-u>CocList commands<cr>
nnoremap <silent> <space>qo :<C-u>CocList outline<cr>
nnoremap <silent> <space>qs :<C-u>CocList -I symbols<cr>
nnoremap <silent> <space>qj :<C-u>CocNext<CR>
nnoremap <silent> <space>qk :<C-u>CocPrev<CR>
nnoremap <silent> <space>qp :<C-u>CocListResume<CR>
