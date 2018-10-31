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
    declare-option -hidden int cline %val{cursor_line}
    declare-option -hidden int ccol %val{cursor_column}
    execute-keys %{ %s^\h+<ret><a-@>}
    execute-keys %opt{cline}g %opt{ccol}lh
}

define-command -override leading-tabs-to-spaces %{
    declare-option -hidden int cline %val{cursor_line}
    declare-option -hidden int ccol %val{cursor_column}
    execute-keys %{ %s^\h+<ret>@}
    execute-keys %opt{cline}g %opt{ccol}lh
}

define-command -override noexpandtab %{
    hook -group noexpandtab global NormalKey <gt> %{
        execute-keys -draft "xs^\h+<ret><a-@>"
    }
    remove-hooks global expandtab
}

define-command -override expandtab %{
    hook -group expandtab global InsertChar \t %{
        execute-keys -draft h@
    }
    hook -group expandtab global InsertDelete ' ' %{ try %{
        execute-keys -draft 'h<a-h><a-k>\A\h+\z<ret>i<space><esc><lt>'
    }}
    remove-hooks global noexpandtab
}

declare-option str statusline_pos_percent

define-command -override update-statusline-pos-percent %{ evaluate-commands %sh{
    echo "set-option window statusline_pos_percent $(($kak_cursor_line * 100 / $kak_buf_line_count))%"
}}

declare-option str statusline_git_branch

define-command -override update-statusline-git-branch %{ set-option global statusline_git_branch %sh{
    branch=$(cd "${kak_buffile%/*}" 2>/dev/null && git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if [ ! -z $branch ]; then
        echo " $branch  "
    else
        echo ""
    fi
}}

