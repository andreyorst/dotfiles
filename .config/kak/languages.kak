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

# Highlight operators and delimiters
hook -group ope_delim global WinCreate .* %{
    add-highlighter buffer/operators regex (\+|-|\*|=|\\|\?|%|\|-|!|\||->|\.|,|<|>|::|\^|/) 0:operator
    add-highlighter buffer/delimiters regex (\(|\)|\[|\]|\{|\}|\;|') 0:delimiters
}

# C/Cpp/Rust syntax fixes
hook global WinSetOption filetype=(c|cpp|rust|java) %{
    add-highlighter buffer/functions      regex \w+(\h+)?(?=\() 0:function
    add-highlighter buffer/child          regex ((?<=\.)|(?<=->))[a-zA-Z](\w+)?\b(?![>"\(]) 0:child
    add-highlighter buffer/child_function regex ((?<=\.)|(?<=->))[a-zA-Z](\w+)?(\h+)?(?=\() 0:function
}

# C/Cpp
hook global WinSetOption filetype=(c|cpp) %{
    # Custom C/Cpp types highlighing
    add-highlighter window/c_types      regex \b(v|u|vu)\w+(8|16|32|64)(_t)?\b 0:type
    add-highlighter window/c_types2     regex \b(v|u|vu)?(_|__)?(s|u)(8|16|32|64)(_t)?\b 0:type
    add-highlighter window/c_types3     regex \b(v|u|vu)(_|__)?(int|short|char|long)(_t)?\b 0:type
    add-highlighter window/c_user_types regex \b(\w+_t|lambda)\b 0:type
    set-option window formatcmd 'clang-format'
    noexpandtab
}

hook global WinSetOption filetype=c %{
    set-option global clang_options '-Wall --std=c99 -I./include'
    evaluate-commands  %sh{ (
        while [ $(pwd) != $HOME ]; do
            if [ -f "./testkit.settings" ]; then
                eval echo "set-option -add global clang_options %{ -I$(pwd)/include}"
                eval echo "set-option -add global clang_options %{ -I$(pwd)/testpacks/SPW_TESTS/spw_lib_src}"
                eval echo "set-option -add global clang_options %{ -I$(pwd)/testpacks/SK_VG11/pci_master_slave_cross_test}"
                eval echo "set-option -add global clang_options %{ -I$(pwd)/testpacks/CAN/can_lib_src}"
                eval echo "set-option -add global clang_options %{ -I$(pwd)/testpacks/MKIO/mkio_lib_src}"
                eval echo "set-option -add global clang_options %{ -I$(pwd)/platforms/$(cat ./testkit.settings | grep '?=' |  sed -E 's/.*= //')/include}"
                break
            elif [ -f "./startf.S" ]; then
                eval echo "set-option -add global clang_options %{ -I$(pwd)/include}"
                eval echo "set-option -add global clang_options %{ -I$(pwd)/include/cp2}"
                eval echo "set-option -add global clang_options %{ -I$(pwd)/include/hdrtest}"
                eval echo "set-option -add global clang_options %{ -I$(pwd)/../../include}"
                echo "set-option global tabstop 8"
                echo "set-option global indentwidth 8"
                break
            fi
            cd ..
        done
    ) }
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

