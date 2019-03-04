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
"search-file <filename>: search for file recusively under path option: %opt{path}" \
search-file -params 1 %{ evaluate-commands %sh{
    file=$(printf "%s\n" $1 | sed -E "s:^~/:$HOME/:")
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
            IFS='
'
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
    tags="${TMPDIR:-/tmp}/tags-tmp"
    ctags -f "$tags" "$kak_buffile"
    cut -f 1 "$tags" | grep -v '^!' | awk '!x[$0]++'
} symbol -params ..1 %{ evaluate-commands %sh{
    export tagname="${1:-$kak_selection}"
    tags="${TMPDIR:-/tmp}/tags-tmp"
    if [ ! -s "$tags" ]; then
        ctags -f "$tags" "$kak_buffile"
    fi
    readtags -t "$tags" "$tagname" | awk -F '\t|\n' '
        /[^\t]+\t[^\t]+\t\/\^.*\$?\// {
            opener = "{"; closer = "}"
            line = $0; sub(".*\t/\\^", "", line); sub("\\$?/$", "", line);
            menu_info = line; gsub("!", "!!", menu_info); gsub(/^[\t+ ]+/, "", menu_info); gsub(opener, "\\"opener, menu_info); gsub(/\t/, " ", menu_info);
            keys = line; gsub(/</, "<lt>", keys); gsub(/\t/, "<c-v><c-i>", keys); gsub("!", "!!", keys); gsub("&", "&&", keys); gsub("?", "??", keys); gsub("\\|", "||", keys); gsub("\\\\/", "/", keys);
            menu_item = $2; gsub("!", "!!", menu_item);
            edit_path = $2; gsub("&", "&&", edit_path); gsub("?", "??", edit_path); gsub("\\|", "||", edit_path);
            select = $1; gsub(/</, "<lt>", select); gsub(/\t/, "<c-v><c-i>", select); gsub("!", "!!", select); gsub("&", "&&", select); gsub("?", "??", select); gsub("\\|", "||", select);
            out = out "%!" menu_item ": {MenuInfo}" menu_info "! %!evaluate-commands %? try %& edit -existing %|" edit_path "|; execute-keys %|/\\Q" keys "<ret>vc| & catch %& echo -markup %|{Error}unable to find tag| &; try %& execute-keys %|s\\Q" select "<ret>| & ? !"
        }
        /[^\t]+\t[^\t]+\t[0-9]+/ {
            opener = "{"; closer = "}"
            menu_item = $2; gsub("!", "!!", menu_item);
            select = $1; gsub(/</, "<lt>", select); gsub(/\t/, "<c-v><c-i>", select); gsub("!", "!!", select); gsub("&", "&&", select); gsub("?", "??", select); gsub("\\|", "||", select);
            menu_info = $3; gsub("!", "!!", menu_info); gsub(opener, "\\"opener, menu_info);
            edit_path = $2; gsub("!", "!!", edit_path); gsub("?", "??", edit_path); gsub("&", "&&", edit_path); gsub("\\|", "||", edit_path);
            line_number = $3;
            out = out "%!" menu_item ": {MenuInfo}" menu_info "! %!evaluate-commands %? try %& edit -existing %|" edit_path "|; execute-keys %|" line_number "gx| & catch %& echo -markup %|{Error}unable to find tag| &; try %& execute-keys %|s\\Q" select "<ret>| & ? !"
        }
        END { print ( length(out) == 0 ? "echo -markup %{{Error}no such tag " ENVIRON["tagname"] "}" : "menu -markup -auto-single " out ) }'
}}

define-command -override -docstring "wrap all occurrences of `tos()', `ton()', `tonf()', and `tob()' functions with `// clang-format off/on' comments, execute formatting of a buffer with clang format and remove thosecomments." \
format-c %{
    execute-keys -draft '%s(tos|ton(f)?|tob)(\h+)?\(<ret><a-h>O//<space>clang-format<space>off<esc>jo//<space>clang-format<space>on<esc><space>;:<space> format<ret>%stos|ton|tob<ret><a-h>kxdjxd<space>;'
}

define-command -override -docstring "evaluate-buffer: evaluate current buffer contents as kakscrupt" \
evaluate-buffer %{
    execute-keys -draft '%:<space><c-r>.<ret>'
}
