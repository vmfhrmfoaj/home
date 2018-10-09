autocmd VimEnter * silent exec "! echo -ne '\e[1 q'"
autocmd VimLeave * silent exec "! echo -ne '\e[6 q'"
autocmd InsertEnter * silent exec "! echo -ne '\e[6 q'"
autocmd InsertLeave * silent exec "! echo -ne '\e[1 q'"
