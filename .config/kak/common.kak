set-option global tabstop 4
set-option global indentwidth 4

colorscheme base16-guvbox-dark-soft

add-highlighter global number_lines -relative -hlcursor
add-highlighter global show_matching
add-highlighter global show_whitespaces -tab "▏" -lf " " -nbsp "⋅" -spc " "
add-highlighter global wrap -word -indent
set-option global scrolloff 3,3
set-option global grepcmd 'rg -L --with-filename --column'

set-option global ui_options ncurses_assistant=none

map global normal '' :comment-line<ret>

hook global InsertCompletionShow .* %{map   window insert <tab> <c-n>; map   window insert <s-tab> <c-p>}
hook global InsertCompletionHide .* %{unmap window insert <tab> <c-n>; unmap window insert <s-tab> <c-p>}

hook global WinSetOption filetype=(c|cpp) %{
    add-highlighter global regex \w+(\h+)?(?=\() 0:function
    add-highlighter global regex (?<=\.)\w+(?!\()(?!>)(?!") 0:Child
    add-highlighter global regex (?<=->)\w+(?!\()(?!>)(?!") 0:Child
}
add-highlighter global regex (\+|\*|=|\\|\?|\%|\|-|!|\||->|\.|,) 0:operator
add-highlighter global regex (\(|\)|\[|\]|\{|\}|\;|') 0:Delimiters

set-option global modelinefmt '%val{bufname} {blue}{rgb:3c3836,blue} %opt{filetype} {rgb:3c3836,blue}{default,default} {{context_info}}{{mode_info}} {blue}{rgb:3c3836,blue} %val{cursor_line}:%val{cursor_char_column} {rgb:3c3836,blue}{default,default} %val{client}@[%val{session}] '

