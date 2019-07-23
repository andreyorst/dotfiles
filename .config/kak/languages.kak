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

# C/Cpp
# ‾‾‾‾‾
hook global WinSetOption filetype=(c|cpp) %{
    set-option buffer formatcmd 'clang-format'
    set-option buffer matching_pairs '{' '}' '[' ']' '(' ')'
}

hook global ModuleLoaded c-family %{ try %{ evaluate-commands %sh{
    join() { sep=$2; eval set -- $1; IFS="$sep"; echo "$*"; }

    # taken from rc/filetype/c-family.kak
    c_keywords='asm break case continue default do else for goto if return
                sizeof switch while offsetof alignas alignof'

    # Highlight functions ignoring C specific keywords
    printf "%s\n" "add-highlighter shared/c/code/my_functions regex (\w*?)\b($(join '${c_keywords}' '|'))?(\h+)?(?=\() 1:function"

    # taken from rc/filetype/c-family.kak
    cpp_keywords='alignas alignof and and_eq asm bitand bitor break case catch
                  compl const_cast continue decltype delete do dynamic_cast
                  else export for goto if new not not_eq operator or or_eq
                  reinterpret_cast return sizeof static_assert static_cast switch
                  throw try typedef typeid using while xor xor_eq'

    # Highlight functions ignoring C++ specific keywords
    printf "%s\n" "add-highlighter shared/cpp/code/functions regex (\w*?)\b($(join '${cpp_keywords}' '|'))?(\h+)?(?=\() 1:function"
    # Namespace highlighting
    printf "%s\n" "add-highlighter shared/cpp/code/namespace  regex [a-zA-Z](\w+)?(\h+)?(?=::) 0:module"
    # Types and common highlightings. Same for C and C++
    for filetype in c cpp; do
        printf "%s\n" "add-highlighter shared/$filetype/code/my_field   regex ((?<!\.\.)(?<=\.)|(?<=->))[a-zA-Z](\w+)?\b(?![>\"\(]) 0:meta
                       add-highlighter shared/$filetype/code/my_method  regex ((?<!\.\.)(?<=\.)|(?<=->))[a-zA-Z](\w+)?(\h+)?(?=\() 0:function
                       add-highlighter shared/$filetype/code/my_return  regex \breturn\b 0:meta
                       add-highlighter shared/$filetype/code/my_types_1 regex \b(v|u|vu)\w+(8|16|32|64)(_t)?\b 0:type
                       add-highlighter shared/$filetype/code/my_types_2 regex \b(v|u|vu)?(_|__)?(s|u)(8|16|32|64)(_t)?\b 0:type
                       add-highlighter shared/$filetype/code/my_types_3 regex \b(v|u|vu)(_|__)?(int|short|char|long)(_t)?\b 0:type
                       add-highlighter shared/$filetype/code/my_types_4 regex \b\w+_t\b 0:type
                       add-highlighter shared/$filetype/code/function_type regex \w+\s+(?:\w+::)?\w+\(\s*(?:(\w+)\s+\w+(?:,\s+)?)+\s*\) 1:type
                       add-highlighter shared/$filetype/code/my_types_5 regex \((\w+)\h*\*\)\h*\w+ 1:type"
    done
}}}

# Rust
# ‾‾‾‾
hook global WinSetOption filetype=rust %{
    set-option buffer formatcmd 'rustfmt'
    set-option buffer matching_pairs '{' '}' '[' ']' '(' ')'
    # I use my own highlighting defined in 'rust_syntax.kak'
    remove-highlighter shared/rust
    remove-highlighter window/rust
}

# Makefile
# ‾‾‾‾‾‾‾‾
hook global BufCreate .*\.mk$ %{
    set-option buffer filetype makefile
}

# Kakscript
# ‾‾‾‾‾‾‾‾‾
hook global WinSetOption filetype=kak %{ hook global NormalIdle .* %{
    evaluate-commands -save-regs 'a' %{ try %{
        execute-keys -draft <a-i>w"ay
        evaluate-commands %sh{ (
            color="${kak_reg_a}"
            inverted_color=$(echo "${color}" | perl -pe 'tr/0123456789abcdefABCDEF/fedcba9876543210543210/')
            printf "%s\n" "evaluate-commands -client $kak_client %{ try %{ echo -markup %{{rgb:${inverted_color},rgb:${color}+b}   #${color}   } } }" | kak -p $kak_session
        ) >/dev/null 2>&1 </dev/null & }
    }}
}}

# Assemply
# ‾‾‾‾‾‾‾‾
hook global WinSetOption filetype=gas %{
    set-option window comment_line '#'
    # a c-like line comment highlighter for compatibility reasons
}

hook global ModuleLoaded gas %{ try %{
    add-highlighter shared/gas/c_line_comment region // (?<!\\\\)(?=\n) fill comment
}}

hook -once global WinSetOption filetype=markdown %{
    define-command markdown-require-highlighters %{ evaluate-commands -save-regs 'a' %{ try %{
        execute-keys -draft 'gtGbGls```\h*\K[^\s]+<ret>"ay'
        nop %sh{ (
            eval "set -- $kak_reg_a"
            while [ $# -gt 0 ]; do
                case $1 in
                    c|cpp|c++|objc) module="c-family" ;;
                    kak)            module="kakrc"    ;;
                    *)              module="$1"       ;;
                esac
                [ -n "$module" ] && printf "%s\n" "evaluate-commands -client $kak_client %{ require-module $module }"
                module=
                shift
            done | kak -p $kak_session
        ) > /dev/null 2>&1 < /dev/null & }
    }}}

    hook buffer NormalIdle .* markdown-require-highlighters
    hook buffer InsertIdle .* markdown-require-highlighters
}
