autocmd VimEnter * silent exec "! echo -ne '\e[1 q'"
autocmd VimLeave * silent exec "! echo -ne '\e[4 q'"
autocmd InsertEnter * silent exec "! echo -ne '\e[4 q'"
autocmd InsertLeave * silent exec "! echo -ne '\e[1 q'"

execute "set <M-n>=\en"
execute "set <M-p>=\ep"
map <M-n> }
map <M-p> {
