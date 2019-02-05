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
set-face global delimiters rgb:aa3a03,default
set-face global namespace rgb:be8019,default

# Never.
hook global WinCreate .* %{
    set-option window aligntab false
}

# Highlight operators and delimiters
hook -once global WinCreate .* %{
    add-highlighter global/operators  regex (\+|-|\*|&|=|\\|\?|%|\|-|!|\||->|\.|,|<|>|::|\^|/) 0:operator
    add-highlighter global/delimiters regex (\(|\)|\[|\]|\{|\}|\;|') 0:delimiters
}

# C/Cpp/Rust syntax fixes
hook global WinSetOption filetype=(c|cpp|rust|java) %{
    add-highlighter "buffer/%val{hook_param_capture_1}_functions"      regex \w+(\h+)?(?=\() 0:function
    add-highlighter "buffer/%val{hook_param_capture_1}_child"          regex ((?<!\.\.)(?<=\.)|(?<=->))[a-zA-Z](\w+)?\b(?![>"\(]) 0:child
    add-highlighter "buffer/%val{hook_param_capture_1}_child_function" regex ((?<!\.\.)(?<=\.)|(?<=->))[a-zA-Z](\w+)?(\h+)?(?=\() 0:function
    hook -always -once window WinSetOption filetype=.* "
        remove-highlighter buffer/%val{hook_param_capture_1}_functions
        remove-highlighter buffer/%val{hook_param_capture_1}_child
        remove-highlighter buffer/%val{hook_param_capture_1}_child_function
    "
}

# C/Cpp
hook global WinSetOption filetype=(c|cpp) %{
    set-option window formatcmd 'clang-format'
    # Custom C/Cpp types highlighing
    add-highlighter "buffer/%val{hook_param_capture_1}_types"      regex \b(v|u|vu)\w+(8|16|32|64)(_t)?\b 0:type
    add-highlighter "buffer/%val{hook_param_capture_1}_types2"     regex \b(v|u|vu)?(_|__)?(s|u)(8|16|32|64)(_t)?\b 0:type
    add-highlighter "buffer/%val{hook_param_capture_1}_types3"     regex \b(v|u|vu)(_|__)?(int|short|char|long)(_t)?\b 0:type
    add-highlighter "buffer/%val{hook_param_capture_1}_user_types" regex \b(\w+_t|lambda)\b 0:type
    add-highlighter "window/%val{hook_param_capture_1}_return"     regex \breturn\b 0:child
    add-highlighter "buffer/%val{hook_param_capture_1}_namespace"  regex [a-zA-Z](\w+)?(\h+)?(?=::) 0:namespace
    hook -always -once window WinSetOption filetype=.* "
        remove-highlighter buffer/%val{hook_param_capture_1}_types
        remove-highlighter buffer/%val{hook_param_capture_1}_types2
        remove-highlighter buffer/%val{hook_param_capture_1}_types3
        remove-highlighter buffer/%val{hook_param_capture_1}_user_types
        remove-highlighter window/%val{hook_param_capture_1}_return
        remove-highlighter buffer/%val{hook_param_capture_1}_namespace
    "
}

# Rust
hook global WinSetOption filetype=rust %{
    set window formatcmd 'rustfmt'
}

# Markdown
hook global WinSetOption filetype=markdown %{
    remove-highlighter buffer/operators
    remove-highlighter buffer/delimiters
}

# Makefile
hook global BufCreate .*\.mk$ %{
    set-option buffer filetype makefile
}
