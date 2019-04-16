# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ mappings.kak     │
# ╞═════════════╩══════════════════╡
# │ Mappings for Kakoune modes     │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

# Normal mode mappings
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
map -docstring "comment/uncomment selection (<c-/>)" global normal ''     ': comment-line<ret>'
map -docstring "add currsor/jump on current word"    global normal '<c-d>' ': select-or-add-cursor<ret>'

# Avoid escape key
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
map -docstring "avoid escape key"                    global normal '<c-g>' '<esc>;<space>'
map -docstring "avoid escape key"                    global prompt '<c-g>' '<esc>;<space>'
map -docstring "avoid escape key"                    global insert '<c-g>' '<esc>;<space>'
map -docstring "avoid escape key"                    global user   '<c-g>' '<esc>;<space>'

# User Mappings
# ‾‾‾‾‾‾‾‾‾‾‾‾‾
map -docstring "convert leading spaces to tabs"      global user   't'     ': leading-spaces-to-tabs<ret>'
map -docstring "convert leading tabs to spaces"      global user   '<a-t>' ': leading-tabs-to-spaces<ret>'

## System clipboard mappings
## ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
map -docstring "copy to system clipboard"                   global user 'y' '<a-|>xsel -b -i<ret>;:<space>echo -markup %{{Information}yanked selection to system clipboard}<ret>'
map -docstring "cut to system clipboard"                    global user 'd' '|xsel -b -i<ret>'
map -docstring "cut to system clipboard, enter insert mode" global user 'c' '|xsel -b -i<ret>i'
map -docstring "paste from system clipboard before cursor"  global user 'P' '!xsel --output --clipboard<ret>'
map -docstring "paste from system clipboard after cursor"   global user 'p' '<a-!>xsel --output --clipboard<ret>'
map -docstring "replace selection with system clipboard"    global user 'R' '|xsel --output --clipboard<ret>'

# Insert mode mappings
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
## Completion
## ‾‾‾‾‾‾‾‾‾‾
hook global InsertCompletionShow .* %{ try %{
    execute-keys -draft 'h<a-K>\h<ret>'
    map window insert <tab> <c-n>
    map window insert <s-tab> <c-p>
}}

hook global InsertCompletionHide .* %{
    unmap window insert <tab> <c-n>
    unmap window insert <s-tab> <c-p>
}

# Goto mode mappings
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
map -docstring "file non-recursive"             global goto '<a-f>' '<esc>gf'
map -docstring "file recursive"                 global goto 'f'     '<esc>: smart-select WORD; search-file %val{selection}<ret>'
map -docstring "next buffer"                    global goto 'b'     '<esc>: buffer-next<ret>'
map -docstring "previous buffer"                global goto 'B'     '<esc>: buffer-previous<ret>'
map -docstring "search tag in current file"     global goto '['     '<esc>: smart-select word; symbol<ret>'
map -docstring "search tag in global tags file" global goto ']'     '<esc>: smart-select word; ctags-search<ret>'

