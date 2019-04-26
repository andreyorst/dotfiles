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
try %{ require-module c-family }

hook global WinSetOption filetype=(c|cpp) %{
    set-option window formatcmd 'clang-format'

    try %{
        remove-highlighter window/operators
        add-highlighter    buffer/operators regex (\+|-|\*|&|=|\\|\?|%|\|-|!|\||->|\.|,|<|>|:|\^|/|~|\[|\]) 0:operator
    }
    try %{
        remove-highlighter window/delimiters
        add-highlighter    buffer/delimiters regex (\(|\)||\{|\}|\;|'|`) 0:delimiter
    }
    try %{ evaluate-commands %sh{
        # Types and common highlightings. Same for C and C++
        for filetype in c cpp; do
            printf "%s\n" "add-highlighter shared/$filetype/code/field   regex ((?<!\.\.)(?<=\.)|(?<=->))[a-zA-Z](\w+)?\b(?![>\"\(]) 0:meta
                           add-highlighter shared/$filetype/code/method  regex ((?<!\.\.)(?<=\.)|(?<=->))[a-zA-Z](\w+)?(\h+)?(?=\() 0:function
                           add-highlighter shared/$filetype/code/return  regex \breturn\b 0:meta
                           add-highlighter shared/$filetype/code/types_1 regex \b(v|u|vu)\w+(8|16|32|64)(_t)?\b 0:type
                           add-highlighter shared/$filetype/code/types_2 regex \b(v|u|vu)?(_|__)?(s|u)(8|16|32|64)(_t)?\b 0:type
                           add-highlighter shared/$filetype/code/types_3 regex \b(v|u|vu)(_|__)?(int|short|char|long)(_t)?\b 0:type
                           add-highlighter shared/$filetype/code/types_4 regex \b\w+_t\b 0:type"
        done

        join() { sep=$2; eval set -- $1; IFS="$sep"; echo "$*"; }

        # taken from rc/filetype/c-family.kak
        c_keywords='asm break case continue default do else for goto if return
                    sizeof switch while offsetof alignas alignof'

        # Highlight functions ignoring C specific keywords
        printf "%s\n" "add-highlighter shared/c/code/functions regex (\w*?)\b($(join '${c_keywords}' '|'))?(\h+)?(?=\() 1:function"

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
    }}
}

# Rust
# ‾‾‾‾
try %{ require-module rust }

hook global WinSetOption filetype=rust %{
    set window formatcmd 'rustfmt'

    evaluate-commands %sh{
        # Common highlightings for Rust
        printf "%s\n" "add-highlighter shared/rust/code/field     regex ((?<!\.\.)(?<=\.))[a-zA-Z](\w+)?\b(?![>\"\(]) 0:meta
                       add-highlighter shared/rust/code/method    regex ((?<!\.\.)(?<=\.))[a-zA-Z](\w+)?(\h+)?(?=\() 0:function
                       add-highlighter shared/rust/code/return    regex \breturn\b 0:meta
                       add-highlighter shared/rust/code/usertype  regex \b[A-Z]\w*\b 0:type
                       add-highlighter shared/rust/code/namespace regex [a-zA-Z](\w+)?(\h+)?(?=::) 0:module"

        # Taken from rc/filetype/rust.kak
        rust_keywords="let as fn return match if else loop for in while
                       break continue move box where impl dyn pub unsafe"

        join() { sep=$2; eval set -- $1; IFS="$sep"; echo "$*"; }

        # Highlight functions ignoring Rust specific keywords
        printf "%s\n" "add-highlighter shared/rust/code/functions regex (\w*?)\b($(join '${rust_keywords}' '|'))?\h*(?=\() 1:function"
    }
}

# Makefile
# ‾‾‾‾‾‾‾‾
hook global BufCreate .*\.mk$ %{
    set-option buffer filetype makefile
}

# Kakscript
# ‾‾‾‾‾‾‾‾‾
hook global WinSetOption filetype=kak %{
    hook global NormalIdle .* %{
        evaluate-commands -save-regs 'a' %{ try %{
            execute-keys -draft <a-i>w"ay
            evaluate-commands %sh{
                color=$(echo "$kak_reg_a" | perl -pe "s/'//g")
                inverted_color=$(echo "${color}" | perl -pe 'tr/0123456789abcdefABCDEF/fedcba9876543210543210/')
                printf "%s\n" "echo -markup %{{rgb:${inverted_color},rgb:${color}+b}  #${color}  }"
            }
        }}
    }
}

# Assemply
# ‾‾‾‾‾‾‾‾
hook global WinSetOption filetype=gas %{
    set-option window comment_line '#'
}
