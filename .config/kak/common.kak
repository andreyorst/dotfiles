set global tabstop 4
set global indentwidth 4

colorscheme base16-guvbox-dark-soft

add-highlighter global number_lines -relative -hlcursor
add-highlighter global show_matching
add-highlighter global show_whitespaces -tab "▏" -lf " " -nbsp "⋅" -spc " "

map global normal '' :comment-line<ret>

hook global InsertCompletionShow .* %{map   window insert <tab> <c-n>; map   window insert <s-tab> <c-p>}
hook global InsertCompletionHide .* %{unmap window insert <tab> <c-n>; unmap window insert <s-tab> <c-p>}

add-highlighter global regex \w+(\h+)?(?=\() 0:function
add-highlighter global regex (?<=\.)\w+(?!=\() 0:Child
add-highlighter global regex (?<=->)\w+(?!=\() 0:Child
add-highlighter global regex (\+|\*|=|\\|\?|\%|\|-|!|\|) 0:operator
add-highlighter global regex (\(|\)|\[|\]|\{|\}|\;|') 0:Delimiters

