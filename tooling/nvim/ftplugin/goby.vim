" Goby filetype plugin
if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal commentstring=#\ %s
setlocal expandtab
setlocal shiftwidth=2
setlocal tabstop=2
setlocal softtabstop=2

let b:undo_ftplugin = 'setlocal commentstring< expandtab< shiftwidth< tabstop< softtabstop<'
