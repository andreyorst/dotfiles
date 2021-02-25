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

hook global WinSetOption filetype=(lisp|clojure|scheme|racket) %{
    set-option window indentwidth 2
    set-option window tabstop 2
}
