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
set-option global scrolloff 4,4

## Grep
evaluate-commands %sh{
    [ ! -z "$(command -v rg)" ] && printf "%s\n" "set-option global grepcmd 'rg -L --with-filename --column'"
}

## Tabstop and indentwidth
set-option global tabstop 4
set-option global indentwidth 4

## Use main client as jumpclient
set-option global jumpclient client0

# Highlighters
set-face global delimiters rgb:aa3a03,default

hook global WinCreate .* %{
    add-highlighter window/numbers          number-lines -relative -hlcursor -separator ' '
    add-highlighter window/matching         show-matching
    add-highlighter window/wrap             wrap -word -indent -marker '↪'
    add-highlighter window/show-whitespaces show-whitespaces -lf ' ' -spc ' ' -nbsp '⋅'
    add-highlighter window/operators        regex (\+|-|\*|&|=|\\|\?|%|\|-|!|\||->|\.|,|<|>|:|\^|/|~) 0:operator
    add-highlighter window/delimiters       regex (\(|\)|\[|\]|\{|\}|\;|') 0:delimiters
}

# Hooks
## Editorconfig
hook global BufOpenFile .* editorconfig-load
hook global BufNewFile  .* editorconfig-load

# Aliases
alias global h doc

