# ╭─────────────╥───────────────────╮
# │ Author:     ║ File:             │
# │ Andrey Orst ║ common.kak        │
# ╞═════════════╩═══════════════════╡
# │ Custom commands for Kakoune     │
# ╞═════════════════════════════════╡
# │ Rest of .dotfiles:              │
# │ GitHub.com/andreyorst/dotfiles  │
# ╰─────────────────────────────────╯
 
define-command -override -docstring "find file recursively searching for it under path" \
find -params 1 -shell-candidates %{ find . \( -path '*/.svn*' -o -path '*/.git*' \) -prune -o -type f -print } %{ evaluate-commands %sh{
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

define-command -override -docstring "run command if Kakoune was launched in termux" in-termux -params 2 %{
    evaluate-commands %sh{
        case $PATH in
        *termux*)
            echo "$1"
            ;;
        *)
            echo "$2"
            ;;
        esac
    }
}
