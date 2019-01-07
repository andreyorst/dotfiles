set-face global cursorline default,rgb:3a3633
declare-option bool cursorline true

hook global -group cursorline RawKey .* update-cursorline

define-command -hidden update-cursorline -docstring "Highlight current line" %{
    try %{ remove-highlighter window/cursorline }
    try %{ add-highlighter window/cursorline line %val{cursor_line} cursorline }
}

define-command cursorline-toggle -docstring "Toggle highlighting of current line" %{
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

