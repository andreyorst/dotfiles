declare-option str cursorline_face default,rgb:3C3836+d
declare-option bool cursorline true

hook global -group cursorline RawKey .* update-cursorline

define-command -hidden update-cursorline -docstring "Highlight current line" %{
    try %{ remove-highlighter window/cursorline }
    try %{ add-highlighter window/cursorline line %val{cursor_line} %opt{cursorline_face} }
}

define-command cursorline -docstring "Toggle Highlighting for current line" %{
    evaluate-commands %sh{
        if [ "$kak_opt_cursorline" = true ] ; then
            printf "%s\n" "set-option global cursorline false"
            printf "%s\n" "try %(remove-highlighter window/cursorline)"
            printf "%s\n" "remove-hooks global cursorline"
        else
            printf "%s\n" "set-option global cursorline true"
            printf "%s\n" "update-cursorline"
            printf "%s\n" "hook global -group cursorline RawKey .* update-cursorline"
        fi
    }
}

