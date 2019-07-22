# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ common.kak       │
# ╞═════════════╩══════════════════╡
# │ Common settings for Kakoune    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

# Common options
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾
set-option global scrolloff 4,4

## Grep
## ‾‾‾‾
evaluate-commands %sh{
    [ ! -z "$(command -v rg)" ] && printf "%s\n" "set-option global grepcmd 'rg -L --hidden --with-filename --column'"
}

hook global BufSetOption filetype=grep %{
    remove-highlighter buffer/wrap
}

## Tabstop and indentwidth
## ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
set-option global tabstop 4
set-option global indentwidth 4

## Align with spaces
## ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
hook global WinCreate .* %{
    set-option window aligntab false
}

## Use main client as jumpclient
## ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
set-option global jumpclient client0

# Highlighters
# ‾‾‾‾‾‾‾‾‾‾‾‾
set-face global delimiter rgb:af3a03,default

hook global WinCreate .* %{ try %{
    add-highlighter buffer/numbers          number-lines -relative -hlcursor -separator ' '
    add-highlighter buffer/matching         show-matching
    add-highlighter buffer/wrap             wrap -word -indent -marker '↪'
    add-highlighter buffer/show-whitespaces show-whitespaces -lf ' ' -spc ' ' -nbsp '⋅'
}}

# Hooks
# ‾‾‾‾‾

## Editorconfig
## ‾‾‾‾‾‾‾‾‾‾‾‾
hook global BufOpenFile .* editorconfig-load
hook global BufNewFile  .* editorconfig-load

# Aliases
# ‾‾‾‾‾‾‾
alias global h doc

# Scratch buffer
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾

## Delete the `*scratch*' buffer as soon as another is created, but only if it's empty
## ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
hook global BufCreate '^\*scratch\*$' %{
    execute-keys -buffer *scratch* '%d'
    hook -once -always global BufCreate '^(?!\*scratch\*).*$' %{ try %{
        # throw if the buffer has something other than newlines in the beginning of lines
        execute-keys -buffer *scratch* '%s\A\n\z<ret>'
        delete-buffer *scratch*
    }}
}
