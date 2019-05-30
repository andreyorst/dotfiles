define-command smart-indent %[ evaluate-commands -draft -save-regs 'a' %[ try %[
    try %[
        # this will take care of cursor position relative to opened curly brace.
        # we need to be behind it in order for algorithm to work right, because
        # the cursor can't be "between" chars like in other editors.
        # If there's a opening curly brace in the line we need to stand before it
        # and start search for all curly braces to the beginning of the file.
        execute-keys -draft '<a-h><a-l>s\{\z<ret>hGkGhs\{|\}<ret>"ay'
    ] catch %[
        # if there's no opening curly brace, we check for closing one and search.
        try %[ execute-keys -draft '<a-h><a-l>s\}\z<ret>GkGhs\{|\}<ret>"ay' ] catch %[ fail ]
    ] catch %[
        # everything before failed, so we search for curly braces as is.
        execute-keys -draft '<a-h>hGkGhs\{|\}<ret>"ay'
    ]
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
