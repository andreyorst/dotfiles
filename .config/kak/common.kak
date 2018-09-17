set-option global tabstop 4
set-option global indentwidth 4

colorscheme base16-guvbox-dark-soft

add-highlighter global/ number-lines -relative -hlcursor
add-highlighter global/ show-matching
add-highlighter global/ show-whitespaces -tab "▏" -lf " " -nbsp "⋅" -spc " "
add-highlighter global/ wrap -word -indent -marker ↪
set-option global scrolloff 3,3
set-option global grepcmd 'rg -L --with-filename --column'

set-option global ui_options ncurses_assistant=none

map global normal '' :comment-line<ret>

hook global InsertCompletionShow .* %{map   window insert <tab> <c-n>; map   window insert <s-tab> <c-p>}
hook global InsertCompletionHide .* %{unmap window insert <tab> <c-n>; unmap window insert <s-tab> <c-p>}

set-face global delimiters rgb:af3a03,default

add-highlighter global/ regex (\+|-|\*|=|\\|\?|%|\|-|!|\||->|\.|,|<|>|::|\^) 0:operator
add-highlighter global/ regex (\(|\)|\[|\]|\{|\}|\;|') 0:delimiters

