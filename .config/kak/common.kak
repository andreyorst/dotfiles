set-option global tabstop 4
set-option global indentwidth 4

colorscheme base16-guvbox-dark-soft

add-highlighter global/ number-lines -relative -hlcursor
add-highlighter global/ show-matching
add-highlighter global/ show-whitespaces -tab "▏" -lf " " -nbsp "⋅" -spc " "
add-highlighter global/ wrap -word -indent
set-option global scrolloff 3,3
set-option global grepcmd 'rg -L --with-filename --column'

set-option global ui_options ncurses_assistant=none

map global normal '' :comment-line<ret>

hook global InsertCompletionShow .* %{map   window insert <tab> <c-n>; map   window insert <s-tab> <c-p>}
hook global InsertCompletionHide .* %{unmap window insert <tab> <c-n>; unmap window insert <s-tab> <c-p>}

hook global WinSetOption filetype=(c|cpp) %{
    add-highlighter buffer/ regex \w+(\h+)?(?=\() 0:function
    add-highlighter buffer/ regex (?<=\.)\w+(?!\()(?!>)(?!") 0:Child
    add-highlighter buffer/ regex (?<=->)\w+(?!\()(?!>)(?!") 0:Child
    add-highlighter buffer/ regex \b(v|u|vu)\w+(8|16|32|64)(_t)?\b 0:type
    add-highlighter buffer/ regex \b(v|u|vu)?(_|__)?(s|u)(8|16|32|64)(_t)?\b 0:type
    add-highlighter buffer/ regex \b(v|u|vu)?(_|__)?(int|short|char|long)(_t)?\b 0:type
    add-highlighter buffer/ regex \b\w+_t\b 0:type
}
add-highlighter global/ regex (\+|-|\*|=|\\|\?|%|\|-|!|\||->|\.|,|<|>|::) 0:operator
add-highlighter global/ regex (\(|\)|\[|\]|\{|\}|\;|') 0:Delimiters

