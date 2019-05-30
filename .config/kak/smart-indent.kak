define-command -override smart-indent %[ evaluate-commands -save-regs 'a' %[ try %[
    evaluate-commands -draft %<
        # we need to test if current line has either opening, closing,
        # or no curly brace at all.
        try %<
            # if current line has opening curly brace we need to put cursor behind it
            execute-keys '<a-x>s[{(\[]\K<ret>2h'
        > catch %<
            # if current line has closing curly brace we need to stand on it.
            execute-keys '<a-x>s[})\]]\K<ret>'
        > catch %<
            # everything failed but we need to continue
            nop
        >
        # now we can search for all remaining curly braces from current point
        # to the top of the buffer, since we can't determine minimum scope
        execute-keys 'Gks[{}()\[\]]<ret>"ay'
    >
    evaluate-commands %sh{
        indentation=0
        eval "set -- $kak_reg_a"
        # calculating indentation
        while [ $# -gt 0 ]; do
            case $1 in
                '{'|'['|'(') indentation=$((indentation+=1)) ;;
                '}'|']'|')') indentation=$((indentation-=1)) ;;
            esac
            [ ${indentation} -lt 0 ] && indentation=0
            shift
        done
        # deindent current line
        printf "%s\n" "execute-keys -draft 999<lt>"
        # hack: first line always zero indented
        [ ${kak_cursor_line} -eq 1 ] && exit
        # if indentation isn't zero we set proper indentation level
        [ ${indentation} -gt 0 ] && printf "%s\n" "execute-keys -draft ${indentation}>"
    }
]]]

map global normal <tab> ': smart-indent<ret>'
