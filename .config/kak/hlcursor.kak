declare-option str highlight_line_face default,rgb:3C3836+d
declare-option bool highlight_current_line true

define-command -hidden highlight-current-line -docstring "Highlight current line" %{
    try %{ remove-highlighter %opt{line_high_path} }

    declare-option str line_high_path %sh{
        printf "%s\n" "window/line_${kak_cursor_line}_${kak_opt_highlight_line_face}"
    }
    try %{ add-highlighter window/ line %val{cursor_line} %opt{highlight_line_face} }
}

hook global NormalKey .+ highlight-current-line
hook global NormalIdle .* highlight-current-line
hook global InsertKey .+ highlight-current-line

define-command cursorline -docstring "Toggle Highlighting for current line" %{
    eval %sh{
        if [ "$kak_opt_highlight_current_line" = true ] ; then
            printf "%s\n" 'set-option global highlight_current_line false'
            printf "%s\n" 'try %(remove-highlighter %opt{line_high_path})'
        else
            printf "%s\n" 'set-option global highlight_current_line true'
            printf "%s\n" 'highlight-current-line'
        fi
    }
}

