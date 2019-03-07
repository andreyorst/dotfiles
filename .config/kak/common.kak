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

# Grep
evaluate-commands %sh{
    [ ! -z "$(command -v rg)" ] && printf "%s\n" "set-option global grepcmd 'rg -L --with-filename --column'"
}

# Tabstop and indentwidth
set-option global tabstop 4
set-option global indentwidth 4

# Use main client as jumpclient
set-option global jumpclient client0

# Highlighters
hook global WinCreate .* %{
    add-highlighter window/numbers          number-lines -relative -hlcursor -separator ' '
    add-highlighter window/matching         show-matching
    add-highlighter window/wrap             wrap -word -indent -marker '↪'
    add-highlighter window/show-whitespaces show-whitespaces -lf ' ' -spc ' ' -nbsp '⋅'
    add-highlighter window/operators        regex (\+|-|\*|&|=|\\|\?|%|\|-|!|\||->|\.|,|<|>|:|\^|/|~) 0:operator
    add-highlighter window/delimiters       regex (\(|\)|\[|\]|\{|\}|\;|') 0:delimiters
}

# Maps
map -docstring "comment/uncomment selection (<c-/>)" global normal ''     ': comment-line<ret>'
map -docstring "add currsor/jump on current word"    global normal '<c-d>' ': select-or-add-cursor<ret>'
map -docstring "convert leading spaces to tabs"      global user   't'     ': leading-spaces-to-tabs<ret>'
map -docstring "convert leading tabs to spaces"      global user   'T'     ': leading-tabs-to-spaces<ret>'

## Goto
map -docstring "file non-recursive"             global goto '<a-f>' '<esc>gf'
map -docstring "file recursive"                 global goto 'f'     '<esc>: smart-select word; search-file %val{selection}<ret>'
map -docstring "next buffer"                    global goto 'b'     '<esc>: buffer-next<ret>'
map -docstring "previous buffer"                global goto 'B'     '<esc>: buffer-previous<ret>'
map -docstring "search tag in current file"     global goto '['     '<esc>: smart-select word; symbol<ret>'
map -docstring "search tag in global tags file" global goto ']'     '<esc>: smart-select word; ctags-search<ret>'

## System clipboard
map -docstring "copy to system clipboard"                   global user 'y' '<a-|>xsel -b -i<ret>'
map -docstring "cut to system clipboard"                    global user 'd' '|xsel -b -i<ret>'
map -docstring "cut to system clipboard, enter insert mode" global user 'c' '|xsel -b -i<ret>i'
map -docstring "paste from system clipboard before cursor"  global user 'P' '!xsel --output --clipboard<ret>'
map -docstring "paste from system clipboard after cursor"   global user 'p' '<a-!>xsel --output --clipboard<ret>'
map -docstring "replace selection with system clipboard"    global user 'R' '|xsel --output --clipboard<ret>'

# Completion
hook global InsertCompletionShow .* %{ try %{
    execute-keys -draft 'h<a-K>\h<ret>'
    map window insert <tab> <c-n>
    map window insert <s-tab> <c-p>
}}

hook global InsertCompletionHide .* %{
    unmap window insert <tab> <c-n>
    unmap window insert <s-tab> <c-p>
}

# Editorconfig
hook global BufOpenFile .* editorconfig-load
hook global BufNewFile  .* editorconfig-load

# Aliases
alias global h doc

