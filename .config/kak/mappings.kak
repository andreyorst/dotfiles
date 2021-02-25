# Normal mode mappings
map -docstring "comment/uncomment selection (<c-/>)"                              global normal ''     ': comment-line<ret>'
map -docstring "add currsor/jump on current word"                                 global normal '<c-d>' ': select-or-add-cursor<ret>'
map -docstring "behave as <a-x> if line not selected, select line down otherwise" global normal '<a-x>' ': alt-x J<ret>'
map -docstring "behave as <a-x> if line not selected, select line up otherwise"   global normal '<a-X>' ': alt-x K<ret>'

# Avoid escape key
map -docstring "avoid escape key" global normal '<c-g>' ';<space>'
map -docstring "avoid escape key" global prompt '<c-g>' '<esc>'
map -docstring "avoid escape key" global insert '<c-g>' '<esc>'

# System clipboard mappings
if %[ -n "$(command -v xsel)" ] %{
    map -docstring "copy to system clipboard"                   global user 'y' '<a-|>xsel -b -i<ret>:<space>echo -markup %{{Information}yanked selection to system clipboard}<ret>'
    map -docstring "cut to system clipboard"                    global user 'd' '|xsel -b -i<ret>'
    map -docstring "cut to system clipboard, enter insert mode" global user 'c' '|xsel -b -i<ret>i'
    map -docstring "paste from system clipboard before cursor"  global user 'P' '!xsel --output --clipboard<ret>'
    map -docstring "paste from system clipboard after cursor"   global user 'p' '<a-!>xsel --output --clipboard<ret>'
    map -docstring "replace selection with system clipboard"    global user 'R' '|xsel --output --clipboard<ret>'
} else %{
    echo -debug "'xsel' is not installed"
}

# Completion
hook global InsertCompletionShow .* %{ try %{
    execute-keys -draft 'h<a-K>\h<ret>'
    map window insert <tab> <c-n>
    map window insert <s-tab> <c-p>
    map window insert <c-g> <c-o>
}}

hook global InsertCompletionHide .* %{
    unmap window insert <tab> <c-n>
    unmap window insert <s-tab> <c-p>
    unmap window insert <c-g> <c-o>
}

# Goto mode mappings
map -docstring "file (non-recursive)"           global goto '<a-f>' '<esc>gf'
map -docstring "file (recursive)"               global goto 'f'     '<esc>: smart-select-file; search-file %val{selection}<ret>'
map -docstring "next buffer"                    global goto 'b'     '<esc>: buffer-next<ret>'
map -docstring "previous buffer"                global goto 'B'     '<esc>: buffer-previous<ret>'
map -docstring "search tag in current file"     global goto '['     '<esc><c-s>: smart-select w; symbol<ret>'
map -docstring "search tag in global tags file" global goto ']'     '<esc><c-s>: smart-select w; ctags-search<ret>'

# Custom text objects
map global object w 'c\s,\s<ret>' -docstring "select between whitespace"
