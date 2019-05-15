# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ commands.kak     │
# ╞═════════════╩══════════════════╡
# │ Custom commands for Kakoune    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

define-command -hidden \
-docstring "smart-select: select WORD if current selection is only one character" \
smart-select -params 1 %{ evaluate-commands %sh{
    case $1 in
        WORD) keys="<a-w>" ;;
        word) keys="w" ;;
        *)    printf "%s\n" "fail %{wrong word type '$1'}"; exit ;;
    esac
    if [ $(printf "%s\n" ${kak_selection} | wc -m) -eq 2 ]; then
        printf "%s\n" "execute-keys -save-regs '' <a-i>${keys}"
    fi
}}

define-command -docstring \
"search-file <filename>: search for file recusively under path option: %opt{path}" \
search-file -params 1 %{ evaluate-commands %sh{
    if [ -n "$(command -v fd)" ]; then                          # create find command template
        find='fd -L --type f "${file}" "${path}"'               # if `fd' is installed it will
    else                                                        # be used because it is faster
        find='find -L "${path}" -mount -type f -name "${file}"' # if not, we fallback to find.
    fi

    file=$(printf "%s\n" $1 | sed -E "s:^~/:$HOME/:") # we want full path

    eval "set -- ${kak_buflist}"
    while [ $# -gt 0 ]; do            # Check if buffer with this
        if [ "${file}" = "$1" ]; then # file already exists. Basically
            printf "%s\n" "buffer $1" # emulating what edit command does
            exit
        fi
        shift
    done

    if [ -e "${file}" ]; then                     # Test if file exists under
        printf "%s\n" "edit -existing %{${file}}" # servers' working directory
        exit                                      # this is last resort until
    fi                                            # we start recursive searchimg

    # if everthing  above fails - search for file under `path'
    eval "set -- ${kak_opt_path}"
    while [ $# -gt 0 ]; do                # Since we want to check fewer places,
        case $1 in                        # I've swapped ./ and %/ because
            ./) path=${kak_buffile%/*} ;; # %/ usually has smaller scope. So
            %/) path=${PWD}            ;; # this trick is a speedi-up hack.
            *)  path=$1                ;; # This means that `path' option should
        esac                              # first contain `./' and then `%/'

        if [ -z "${file##*/*}" ] && [ -e "${path}/${file}" ]; then
            printf "%s\n" "edit -existing %{${path}/${file}}"
            exit
        else
            # build list of candidates or automatically select if only one found
            # this doesn't support files with newlines in them unfortunately
            IFS='
'
            for candidate in $(eval "${find}"); do
                [ -n "${candidate}" ] && candidates="${candidates} %{${candidate}} %{evaluate-commands %{edit -existing %{${candidate}}}}"
            done

            # we want to get out as early as possible
            # so if any candidate found in current cycle
            # we prompt it in menu and exit
            if [ -n "${candidates}" ]; then
                printf "%s\n" "menu -auto-single ${candidates}"
                exit
            fi
        fi

        shift
    done

    printf "%s\n" "echo -markup %{{Error}unable to find file '${file}'}"
}}

define-command -docstring \
"select a word under cursor, or add cursor on next occurrence of current selection" \
select-or-add-cursor %{ execute-keys -save-regs '' %sh{
    if [ $(printf "%s\n" ${kak_selection} | wc -m) -eq 2 ]; then
        printf "%s\n" "<a-i>w*"
    else
        printf "%s\n" "*<s-n>"
    fi
}}

define-command -docstring "Convert all leading spaces to tabs" \
leading-spaces-to-tabs %{
    execute-keys -draft %{%s^\h+<ret><a-@>}
}

define-command -docstring "Convert all leading tabs to spaces" \
leading-tabs-to-spaces %{
    execute-keys -draft %{%s^\h+<ret>@}
}

define-command -docstring "symbol [<symbol>]: jump to symbol definition in current file.
If no symbol given, current selection is used as a symbol name" \
-shell-script-candidates %{
    tags="${TMPDIR:-/tmp}/tags-tmp"
    ctags -f "${tags}" "${kak_buffile}"
    cut -f 1 "${tags}" | grep -v '^!' | awk '!x[$0]++'
} symbol -params ..1 %{ evaluate-commands %sh{
    export tagname="${1:-${kak_selection}}"
    tags="${TMPDIR:-/tmp}/tags-tmp"

    if [ ! -s "${tags}" ]; then
        ctags -f "${tags}" "${kak_buffile}"
    fi

    if [ -n "$(command -v readtags)" ]; then
        tags_cmd='readtags -t "${tags}" "${tagname}"'
    else
        tags_cmd='grep "^\b${tagname}\b.*\$/" "${tags}" -o'
    fi

    eval "${tags_cmd}" | awk -F '\t|\n' '
        /[^\t]+\t[^\t]+\t\/\^.*\$?\// {
            opener = "\\{"; closer = "\\}"
            line = $0; sub(".*\t/\\^", "", line); sub("\\$?/$", "", line);
            menu_info = line; gsub("#", "##", menu_info); gsub(/^[\t+ ]+/, "", menu_info); gsub(opener, "\\"opener, menu_info); gsub(/\t/, " ", menu_info);
            keys = line; gsub(/</, "<lt>", keys); gsub(/\t/, "<c-v><c-i>", keys); gsub("#", "##", keys); gsub("&", "&&", keys); gsub("!", "!!", keys); gsub("\\|", "||", keys); gsub("\\\\/", "/", keys);
            menu_item = $2; gsub("#", "##", menu_item);
            edit_path = $2; gsub("&", "&&", edit_path); gsub("!", "!!", edit_path); gsub("\\|", "||", edit_path);
            select = $1; gsub(/</, "<lt>", select); gsub(/\t/, "<c-v><c-i>", select); gsub("#", "##", select); gsub("&", "&&", select); gsub("!", "!!", select); gsub("\\|", "||", select);
            out = out "%#" menu_item ": {MenuInfo}" menu_info "# %#evaluate-commands %! try %& edit -existing %|" edit_path "|; execute-keys %|/\\Q" keys "<ret>vc| & catch %& echo -markup %|{Error}unable to find tag| &; try %& execute-keys %|s\\Q" select "<ret>| & ! #"
        }
        /[^\t]+\t[^\t]+\t[0-9]+/ {
            opener = "\\{"; closer = "\\}"
            menu_item = $2; gsub("#", "##", menu_item);
            select = $1; gsub(/</, "<lt>", select); gsub(/\t/, "<c-v><c-i>", select); gsub("#", "##", select); gsub("&", "&&", select); gsub("!", "!!", select); gsub("\\|", "||", select);
            menu_info = $3; gsub("#", "##", menu_info); gsub(opener, "\\"opener, menu_info);
            edit_path = $2; gsub("#", "##", edit_path); gsub("!", "!!", edit_path); gsub("&", "&&", edit_path); gsub("\\|", "||", edit_path);
            line_number = $3;
            out = out "%#" menu_item ": {MenuInfo}" menu_info "# %#evaluate-commands %! try %& edit -existing %|" edit_path "|; execute-keys %|" line_number "gx| & catch %& echo -markup %|{Error}unable to find tag| &; try %& execute-keys %|s\\Q" select "<ret>| & ! #"
        }
        END { print ( length(out) == 0 ? "echo -markup %{{Error}no such tag " ENVIRON["tagname"] "}" : "menu -markup -auto-single " out ) }'
}}

define-command -hidden format-c -docstring \
"wrap all occurrences of `tos()', `ton()', `tonf()', and `tob()' functions with `// clang-format off/on' comments, execute formatting of a buffer with clang format and remove those comments." \
%{ try %{
    execute-keys -draft '%s(to[nbs](f)?)(\h+)?\(<ret><a-h>O//<space>clang-format<space>off<esc>jo//<space>clang-format<space>on<esc>%|clang-format<ret>%s(to[nbs](f)?)(\h+)?\(<ret><a-h>kxdjxd'
} catch %{
    execute-keys -draft '%|clang-format<ret>'
}}

define-command -docstring "evaluate-buffer: evaluate current buffer contents as kakscrupt" \
evaluate-buffer %{
    execute-keys -draft '%:<space><c-r>.<ret>'
}

define-command -docstring "evaluate-selection: evaluate current sellection contents as kakscrupt" \
evaluate-selection %{
    execute-keys -itersel -draft ':<space><c-r>.<ret>'
}

define-command -docstring "split tmux vertically" \
vsplit -params .. -command-completion %{
    tmux-terminal-horizontal kak -c %val{session} -e "%arg{@}"
}

define-command -docstring "split tmux horizontally" \
split -params .. -command-completion %{
    tmux-terminal-vertical kak -c %val{session} -e "%arg{@}"
}

define-command -docstring "create new tmux window" \
tabnew -params .. -command-completion %{
    tmux-terminal-window kak -c %val{session} -e "%arg{@}"
}

define-command -docstring "print current working directory" \
pwd %{ evaluate-commands %sh{
    printf "%s\n" "echo -markup %{{Information}${PWD}}"
}}

define-command -docstring "Ask before repeating last command (with `.' (dot)) for next search match" \
query-repeat %{ try %{
    execute-keys n
    prompt "confirm? [yn]: " -on-change %{ execute-keys %sh{
        case ${kak_text} in
            y) printf "%s\n" "<esc>.: query-repeat<ret>";;
            n) printf "%s\n" "<esc>: query-repeat<ret>" ;;
            *) ;;
        esac
    }} nop
} catch %{
    fail "no search pattern"
}}

map global normal <c-n> ': query-repeat<ret>'

define-command fd -params 1 -shell-script-candidates %{fd --hidden --type f} %{edit -existing %arg{1}}

define-command if -params 2..4 %{ evaluate-commands %sh{
    condition="[ $1 ]"
    else_op="$3"
    if [ -n "$else_op" ] && [ "$else_op" != "else" ]; then
        printf "%s\n" "fail %{if: unknow operator '$else_op'}"
        exit
    fi
    if [ $# -eq 3 ]; then
        printf "%s\n" "fail %{if: wrong argument count}"
        exit
    fi
    if eval $condition; then
        printf "%s\n" "evaluate-commands %arg{2}"
    elif [ $# -eq 4 ]; then
        printf "%s\n" "evaluate-commands %arg{4}"
    fi
}}
