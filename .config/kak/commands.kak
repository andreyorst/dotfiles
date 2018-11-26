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

define-command -override -docstring "find <fuzzystring>: fuzzy match through filenames" find -params 1 -shell-script-candidates %{ find -type f } %{ edit %arg{1} }

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

define-command -override symbol -params 1 -shell-script-candidates %{
    tags="${TMPDIR:-/tmp}/tags-${kak_buffile##*/}"; tags="${tags%.*}"
    ctags -f $tags $kak_buffile
    readtags -t $tags -l | cut -f 1 | awk '!x[$0]++' | grep -v -e "__anon.*"
    } %{ evaluate-commands %sh{
    tags="${TMPDIR:-/tmp}/tags-${kak_buffile##*/}"; tags="${tags%.*}"
    readtags -t $tags $1 | awk -F '\t|\n' '
        /^!TAGROOT\t/ { tagroot=$2 }
        /[^\t]+\t[^\t]+\t\/\^.*\$?\// {
            re=$0;
            sub(".*\t/\\^", "", re); sub("\\$?/$", "", re); gsub("(\\{|\\}|\\\\E).*$", "", re);
            keys=re; gsub(/</, "<lt>", keys); gsub(/\t/, "<c-v><c-i>", keys);
            out = out " %{" $2 " {MenuInfo}" re "} %{evaluate-commands %{ try %{ edit %{" tagroot $2 "}; execute-keys %{/\\Q" keys "<ret>vc} } catch %{ echo %{unable to find tag} } } }"
        }
        /[^\t]+\t[^\t]+\t[0-9]+/ { out = out " %{" $2 ":" $3 "} %{evaluate-commands %{ edit %{" tagroot $2 "} %{" $3 "}}}" }
        END { print ( length(out) == 0 ? "echo -markup %{{Error}no such tag " ENVIRON["tagname"] "}" : "menu -markup -auto-single " out ) }'
    rm $tags
}}
