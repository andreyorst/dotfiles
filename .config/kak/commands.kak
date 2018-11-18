# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ commands.kak     │
# ╞═════════════╩══════════════════╡
# │ Custom commands for Kakoune    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

define-command -override -docstring "call recursive-search for selection, if selection is only one character selects WORD under cursor" \
smart-gf %{ execute-keys -with-hooks %sh{
    if [ "$(expr $(echo $kak_selection | wc -m) - 1)" = "1" ]; then
        echo "<a-i><a-w>:<space>recursive-search<space>%reg{dot}<ret>"
    else
        echo ":<space>recursive-search<space>%reg{dot}<ret>"
    fi
}}

define-command -override -hidden -params 1 recursive-search %{ evaluate-commands %sh{
    for buffer in $kak_buflist; do
        buffer="${buffer%\'}"; buffer="${buffer#\'}"
        if [ -z "${buffer##*$1}" ]; then
            echo "buffer $buffer"
            exit
        fi
    done
    if [ -e "'$1'" ]; then
        echo "edit -existing '$1'"
        exit
    fi
    for path in $kak_opt_path; do
        path="${path%\'}"; path="${path#\'}"
        case $path in
            "./") path=${kak_buffile%/*};;
            "%/") path=$(pwd);;
        esac
        if [ -z "${1##*/*}" ]; then
            test=$(eval echo "'$path/$1'")
            [ -e "$test" ] && file=$test
        else
            file=$(find -L $path -xdev -type f -name $(eval echo $1) | head -n 1)
        fi
        if [ ! -z "$file" ]; then
            echo "edit -existing '$file'"
            exit
        fi
    done
    echo "echo -markup '{Error}unable to find file ''$1'''"
}}

define-command -override -docstring "select a word under cursor, or add cursor on next occurrence of current selection" \
select-or-add-cursor %{ execute-keys -save-regs '' %sh{
    if [ $(expr $(echo $kak_selection | wc -m) - 1) -eq 1 ]; then
        echo "<a-i>w*"
    else
        echo "*<s-n>"
    fi
}}

define-command -override leading-spaces-to-tabs %{
    execute-keys -draft %{%s^\h+<ret><a-@>}
}

define-command -override leading-tabs-to-spaces %{
    execute-keys -draft %{%s^\h+<ret>@}
}

define-command -override noexpandtab %{
    remove-hooks global noexpandtab
    hook -group noexpandtab global NormalKey <gt> %{ try %{
        execute-keys -draft "<a-x>s^\h+<ret><a-@>"
    }}
    set-option global aligntab true
    remove-hooks global expandtab
    remove-hooks global smarttab
}

define-command -override expandtab %{
    remove-hooks global expandtab
    hook -group expandtab global InsertChar '\t' %{ execute-keys -draft h@ }
    hook -group expandtab global InsertKey <backspace> %{ try %{
        execute-keys -draft <a-h><a-k> "^\h+.\z" <ret>I<space><esc><lt>
    }}
    set-option global aligntab false
    remove-hooks global noexpandtab
    remove-hooks global smarttab
}

define-command -override smarttab %{
    remove-hooks global smarttab
    hook -group smarttab global InsertKey <tab> %{ try %{
        execute-keys -draft <a-h><a-k> "^\h*.\z" <ret>
    } catch %{
        execute-keys -draft h@
    }}
    hook -group smarttab global NormalKey <gt> %{ try %{
        execute-keys -draft "<a-x>s^\h+<ret><a-@>"
    }}
    set-option global aligntab false
    remove-hooks global expandtab
    remove-hooks global noexpandtab
}
