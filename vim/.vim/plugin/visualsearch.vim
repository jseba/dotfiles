" Visual Search

if exists('g:loaded_visualsearch')
  finish
endif

command! VSearchForward call vsearch#forward()
command! VSearchBackward call vsearch#backward()
