# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ languages.kak    │
# ╞═════════════╩══════════════════╡
# │ Custom language settings for   │
# │ different filetypes that       │
# │ change how Kakoune works.      │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

# Custom faces
set-face global child rgb:fb4934,default+b
set-face global delimiters rgb:af3a03,default
set-face global namespace rgb:b57614,default

# Never.
hook global WinCreate .* %{
    set-option window aligntab false
}

# Highlight operators and delimiters
hook -group ope-delim global WinCreate .* %{
    add-highlighter buffer/operators  regex (\+|-|\*|&|=|\\|\?|%|\|-|!|\||->|\.|,|<|>|::|\^|/) 0:operator
    add-highlighter buffer/delimiters regex (\(|\)|\[|\]|\{|\}|\;|') 0:delimiters
    add-highlighter buffer/namespace  regex [a-zA-Z](\w+)?(\h+)?(?=::) 0:namespace
}

# C/Cpp/Rust syntax fixes
hook global WinSetOption filetype=(c|cpp|rust|java) %{
    add-highlighter buffer/functions      regex \w+(\h+)?(?=\() 0:function
    add-highlighter buffer/child          regex ((?<=\.)|(?<=->))[a-zA-Z](\w+)?\b(?![>"\(]) 0:child
    add-highlighter buffer/child_function regex ((?<=\.)|(?<=->))[a-zA-Z](\w+)?(\h+)?(?=\() 0:function
}

# C/Cpp
hook global WinSetOption filetype=(c|cpp) %{
    set-option window formatcmd 'clang-format'
    smarttab
    # Custom C/Cpp types highlighing
    add-highlighter buffer/c_types      regex \b(v|u|vu)\w+(8|16|32|64)(_t)?\b 0:type
    add-highlighter buffer/c_types2     regex \b(v|u|vu)?(_|__)?(s|u)(8|16|32|64)(_t)?\b 0:type
    add-highlighter buffer/c_types3     regex \b(v|u|vu)(_|__)?(int|short|char|long)(_t)?\b 0:type
    add-highlighter buffer/c_user_types regex \b(\w+_t|lambda)\b 0:type
    add-highlighter window/return       regex return 0:child
}

# Rust
hook global WinSetOption filetype=rust %{
    set window formatcmd 'rustfmt'
    expandtab
}

# Markdown
hook global WinSetOption filetype=markdown %{
    remove-highlighter buffer/operators
    remove-highlighter buffer/delimiters
    expandtab
}

# Kakoune
hook global WinSetOption filetype=kak %{
    expandtab
}

# Makefile
hook global BufCreate .*\.mk$ %{
    set-option buffer filetype makefile
}

hook global WinSetOption filetype=makefile %{
    noexpandtab
}

# Lisp, Scheme
hook global WinSetOption filetype=(lisp|scheme) %{
    expandtab
}
