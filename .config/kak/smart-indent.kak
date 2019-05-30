define-command -override smart-indent %[ evaluate-commands -draft -save-regs 'a' %[ try %[
    try %[
        # this will take care of cursor position relative to opened curly brace.
        # we need to be behind it in order for algorithm to work right, because
        # the cursor can't be "between" chars like in other editors.
        execute-keys '<a-h><a-l>s\{\z<ret>h'
    ] catch %[
        # if no curly brace found in the line we try to find closing one
        try %[ execute-keys '<a-h><a-l>s\}\z<ret>' ]
    ]
    # now we can store all remaining braces upt to the beginning of the buffer
    # not sure how good is this solution, since we can't really identify where
    # minimum scope ends.
    execute-keys -draft 'GkGhs\{|\}<ret>"ay'
    evaluate-commands %sh{
        indentation=0
        eval "set -- $kak_reg_a"
        while [ $# -gt 0 ]; do
            [ "$1" = "{" ] && indentation=$((indentation+=1))
            [ "$1" = "}" ] && indentation=$((indentation-=1))
            [ ${indentation} -lt 0 ] && indentation=0
            shift
        done
        # deindenting current line
        printf "%s\n" "execute-keys -draft 999<lt>"
        # hack: indentation is always zero on first char of first line
        [ ${kak_cursor_line} -eq 1 ] && [ ${kak_cursor_column} -eq 1 ] && exit
        # setting correct indentation
        [ ${indentation} -gt 0 ] && printf "%s\n" "execute-keys -draft ${indentation}>"
    }
]]]

map global normal <tab> ': smart-indent<ret>'
