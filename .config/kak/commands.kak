# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ commands.kak     │
# ╞═════════════╩══════════════════╡
# │ Custom commands for Kakoune    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

define-command -override -hidden \
-docstring "smart-select: select WORD if current selection is only one character" \
smart-select -params ..1 %{ execute-keys -save-regs '' %sh{
    if [ "$1" = "WORD" ]; then
        keys="<a-w>"
    elif [ "$1" = "word" ]; then
        keys="w"
    fi
    if [ $(expr $(printf "%s\n" $kak_selection | wc -m) - 1) -eq 1 ]; then
        printf "%s\n" "<a-i>${keys:-<a-w>}"
    fi
}}

define-command -override -docstring \
"find <filename>: search for file recusively under path option: %opt{path}" \
find -params 1 %{ evaluate-commands %sh{
    file=$1
    eval "set -- $kak_buflist"
    while [ $# -gt 0 ]; do            # Check if buffer with this
        if [ "$file" = "$1" ]; then   # file already exists. Basically
            printf "%s\n" "buffer $1" # emulating what edit command does
            exit
        fi
        shift
    done
    if [ -e "$file" ]; then                     # Test if file exists under
        printf "%s\n" "edit -existing %{$file}" # servers' working directory
        exit                                    # this is last resort until
    fi                                          # we start recursive searchimg

    # if everthing  above fails - search for file under path
    eval "set -- $kak_opt_path"
    while [ $# -gt 0 ]; do
        case $1 in                        # Since we want to check fewer places
            ./) path=${kak_buffile%/*} ;; # I've swapped ./ and %/ because
            %/) path=$PWD ;;              # %/ usually has smaller scope. So
            *)  path=$1 ;;                # this trick is a speedi-up hack.
        esac
        if [ -z "${file##*/*}" ]; then # test if filename contains path
            if [ -e "$path/$file" ]; then
                printf "%s\n" "edit -existing %{$path/$file}"
                exit
            fi
        else # build list of candidates or automatically select if only one found
            for candidate in $(find -L $path -mount -type f -name "$file"); do
                if [ -n "$candidate" ]; then
                    candidates="$candidates %{$candidate} %{evaluate-commands %{edit -existing %{$candidate}}}"
                fi
            done
            if [ -n "$candidates" ]; then
                printf "%s\n" "menu -auto-single $candidates"
                exit
            fi
        fi
        shift
    done
    printf "%s\n" "echo -markup %{{Error}unable to find file '$file'}"
}}

define-command -override -docstring \
"select a word under cursor, or add cursor on next occurrence of current selection" \
select-or-add-cursor %{ execute-keys -save-regs '' %sh{
    if [ $(expr $(printf "%s\n" $kak_selection | wc -m) - 1) -eq 1 ]; then
        printf "%s\n" "<a-i>w*"
    else
        printf "%s\n" "*<s-n>"
    fi
}}

define-command -override -docstring "Convert all leading spaces to tabs" \
leading-spaces-to-tabs %{
    execute-keys -draft %{%s^\h+<ret><a-@>}
}

define-command -override -docstring "Convert all leading tabs to spaces" \
leading-tabs-to-spaces %{
    execute-keys -draft %{%s^\h+<ret>@}
}

define-command -override -docstring "symbol [<symbol>]: jump to symbol definition in current file.
If no symbol given, current selection is used as a symbol name" \
-shell-script-candidates %{
    tags="${TMPDIR:-/tmp}/tags-${kak_buffile##*/}"; tags="${tags%.*}"
    ctags -f "$tags" "$kak_buffile"
    cut -f 1 "$tags" | grep -v '^!' | awk '!x[$0]++'
} symbol -params ..1 %{ evaluate-commands %sh{
    tagname=${1:-${kak_selection}}
    tags="${TMPDIR:-/tmp}/tags-${kak_buffile##*/}"; tags="${tags%.*}"
    if [ ! -s "$tags" ]; then
        ctags -f "$tags" "$kak_buffile"
    fi
    menu="${TMPDIR:-/tmp}/ctags-menu"
    open='{'; close='}'
    readtags -t "$tags" "$tagname" |
    while read tag; do
        name=$(printf "%s\n" "$tag" | cut -f 2 | sed "s:':'':g")
        menuinfo=$(printf "%s\n" "$tag" | sed "s:.*/\^\(\s\+\)\?::;s:\(\\\$\)\?/$::;s:':'':g;s:$open:\\\\$open:g")
        keys=$(printf "%s\n" "$tag" | sed "s:.*/\^::;s:\(\\\$\)\?/$::;s:':'''''''''''''''':g;s:<:<lt>:g;s:\\t:<c-v><c-i>:g")
        file=$(printf "%s\n" "$name" | sed "s:'':'''''''''''''''':g")
        select=$(printf "%s\n" "$tagname" | sed "s:':'''''''''''''''':g;s:<:<lt>:g;s:\\t:<c-v><c-i>:g")
        command="evaluate-commands '' try '''' edit ''''''''$tagroot/$file''''''''; execute-keys ''''''''/\Q$keys<ret>vcs\Q$select<ret>'''''''' '''' catch '''' echo -markup ''''''''{Error}unable to find tag'''''''' '''' ''"
        if [ -n "$file" ] && [ -n "$keys" ]; then
            printf "%s " "'$name {MenuInfo}$menuinfo' '$command'" >> $menu
        fi
    done
    if [ -s "$menu" ]; then
        printf "%s\n" "menu -auto-single -markup $(cat $menu)"
        rm $menu
    else
        printf "%s\n" "echo -markup %{{Error}tag '${1:-$kak_selection}' not found}"
    fi
    rm $tags
}}

