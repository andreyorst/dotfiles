# Common options
set-option global scrolloff 4,4
set-option global ui_options ncurses_set_title=false

# Grep
if %[ -n "$(command -v rg)" ] %{
    set-option global grepcmd 'rg -L --hidden --with-filename --column'
}

hook global BufSetOption filetype=grep %{
    remove-highlighter buffer/wrap
}

# Tabstop and indentwidth
set-option global tabstop 4
set-option global indentwidth 4

# Align with spaces
hook global WinCreate .* %{
    set-option window aligntab false
}

# Use main client as jumpclient
set-option global jumpclient client0

# Highlighters
set-face global delimiter rgb:af3a03,default

hook global WinCreate .* %{ try %{
    add-highlighter buffer/numbers          number-lines -relative -hlcursor -separator ' '
    add-highlighter buffer/matching         show-matching
    add-highlighter buffer/wrap             wrap -word -indent -marker '↪'
    add-highlighter buffer/show-whitespaces show-whitespaces -lf ' ' -spc ' ' -nbsp '⋅'
}}

# Editorconfig
hook global BufOpenFile .* editorconfig-load
hook global BufNewFile  .* editorconfig-load

# Aliases
alias global h doc

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

# Delete the `*scratch*' buffer as soon as another is created, but only if it's empty
hook global BufCreate '^\*scratch\*$' %{
    execute-keys -buffer *scratch* '%d'
    hook -once -always global BufCreate '^(?!\*scratch\*)$' %{ try %{
        # throw if the buffer has something other than newlines in the beginning of lines
        execute-keys -buffer *scratch* '%s\A\n\z<ret>'
        delete-buffer *scratch*
    }}
}

# C/Cpp
hook global WinSetOption filetype=(c|cpp) %{
    set-option buffer formatcmd 'clang-format'
}

# Rust
hook global WinSetOption filetype=rust %{
    set-option buffer formatcmd 'rustfmt'
}

# Makefile
hook global BufCreate .*\.mk$ %{
    set-option buffer filetype makefile
}

# Kakscript
hook global WinSetOption filetype=kak %{ hook global NormalIdle .* %{
    evaluate-commands -save-regs 'a' %{ try %{
        execute-keys -draft <a-i>w"ay
        evaluate-commands %sh{ (
            color="${kak_reg_a}"
            inverted_color=$(echo "${color}" | perl -pe 'tr/0123456789abcdefABCDEF/fedcba9876543210543210/')
            printf "%s\n" "evaluate-commands -client $kak_client %{ try %{
                               echo -markup %{{rgb:${inverted_color},rgb:${color}+b}   #${color}   }
                           }}" | kak -p $kak_session
        ) >/dev/null 2>&1 </dev/null & }
    }}
}}

# Assemply
hook global WinSetOption filetype=gas %{
    set-option window comment_line '#'
}

hook global ModuleLoaded gas %{ try %{
    # a c-like line comment highlighter for compatibility reasons
    add-highlighter shared/gas/c_line_comment region // (?<!\\\\)(?=\n) fill comment
}}

hook global WinSetOption filetype=(lisp|clojure|scheme|racket|fennel) %{
    set-option window indentwidth 2
    set-option window tabstop 2
}
