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
-docstring "smart-select <w|a-w>: select <word> if current selection is only one character." \
smart-select -params 1 %{
    try %{
        execute-keys "<a-k>..<ret>"
    } catch %{
        execute-keys "<a-i><%arg{1}>"
    } catch nop
}

define-command -override -hidden \
-docstring "smart-select-file: tries to select file path in current line automatically." \
smart-select-file %{
    try %{
        execute-keys "<a-k>..<ret>"
    } catch %{
        # guess we have nonblank character under cursor
        execute-keys "<a-k>\w<ret>"
        execute-keys "<a-i><a-w>"
    } catch %{
        # try selecting inside first occurrence of <...> string
        execute-keys "<a-x>s<<ret>)<space>"
        execute-keys "<a-i>a"
    } catch %{
        # try selecting inside first occurrence of "..."
        execute-keys '<a-x>s"<ret>)<space>'
        execute-keys "<a-i>Q"
    } catch %{
        # try selecting inside first  occurrence of '...'
        execute-keys "<a-x>s'<ret>)<space>"
        execute-keys "<a-i>q"
    } catch %{
        # try select from cursor to the end of the line
        execute-keys "<a-l><a-k>\w<ret>"
    } catch %{
        # try select from beginning to the end of the line
        execute-keys "Gi<a-l><a-k>\w<ret>"
    } catch %{
        fail "no file can be selected"
    }
    try %{
        execute-keys "s/?\w[\S]+(?!/)<ret>)<space>"
    } catch %{
        fail "failed to select file"
    }
}

define-command -hidden \
-docstring "alt-x <J|K>: wrapper around alt x" \
alt-x -params 1 %{
    try %{
        execute-keys "<a-k>.\n<ret>"
        execute-keys "%arg{1}<a-x>"
    } catch %{
        execute-keys "<a-x>"
    }
}

define-command -docstring \
"search-file <filename>: search for file recusively under path option: %opt{path}" \
search-file -params 1 %{ evaluate-commands %sh{
    if [ -n "$(command -v fd)" ]; then                          # create find command template
        find='fd -L --type f "${file}" "${path}"'               # if `fd' is installed it will
    else                                                        # be used because it is faster
        find='find -L "${path}" -mount -type f -name "${file}"' # if not, we fallback to find.
    fi

    file=$(printf "%s\n" $1 | sed -E "s:^~/:$HOME/:") # we want full path

    eval "set -- ${kak_quoted_buflist}"
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
    eval "set -- ${kak_quoted_opt_path}"
    while [ $# -gt 0 ]; do                # Since we want to check fewer places,
        case $1 in                        # I've swapped ./ and %/ because
            (./) path=${kak_buffile%/*} ;; # %/ usually has smaller scope. So
            (%/) path=${PWD}            ;; # this trick is a speedi-up hack.
            (*)  path=$1                ;; # This means that `path' option should
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

define-command -hidden -docstring \
"select a word under cursor, or add cursor on next occurrence of current selection" \
select-or-add-cursor %{
    try %{
        execute-keys "<a-k>\A.\z<ret>"
        execute-keys -save-regs '' "_<a-i>w*"
    } catch %{
        execute-keys -save-regs '' "_*<s-n>"
    } catch nop
}

define-command -docstring "Convert all leading spaces to tabs" \
leading-spaces-to-tabs %{
    execute-keys -draft %{%s^\h+<ret><a-@>}
}

define-command -docstring "Convert all leading tabs to spaces" \
leading-tabs-to-spaces %{
    execute-keys -draft %{%s^\h+<ret>@}
}

define-command -override -docstring "symbol [<symbol>]: jump to symbol definition in current file.
If no symbol given, current selection is used as a symbol name" \
-shell-script-candidates %{
    tags="${TMPDIR:-/tmp}/tags-tmp"
    ctags -f "${tags}" "${kak_buffile}"
    cut -f 1 "${tags}" | grep -v '^!' | awk '!x[$0]++'
} symbol -params ..1 %[ evaluate-commands %sh[
    tags="${TMPDIR:-/tmp}/tags-tmp"
    tagname="${1:-${kak_selection}}"

    if [ ! -s "${tags}" ]; then
        ctags -f "${tags}" "${kak_buffile}"
    fi

    if [ -n "$(command -v readtags)" ]; then
        tags_cmd='readtags -t "${tags}" "${tagname}"'
    else
        tags_cmd='grep "^\b${tagname}\b.*\$/" "${tags}" -o'
    fi

    eval "${tags_cmd}" | tagname="$tagname" awk -F '\t|\n' '
        /[^\t]+\t[^\t]+\t\/\^.*\$?\// {
            line = $0; sub(".*\t/\\^", "", line); sub("\\$?/.*$", "", line);
            menu_info = line; gsub("!", "!!", menu_info); gsub(/^[\t ]+/, "", menu_info); gsub("{", "\\{", menu_info); gsub(/\t/, " ", menu_info);
            keys = line; gsub(/</, "<lt>", keys); gsub(/\t/, "<c-v><c-i>", keys); gsub("!", "!!", keys); gsub("&", "&&", keys); gsub("#", "##", keys); gsub("\\|", "||", keys); gsub("\\\\/", "/", keys);
            menu_item = ENVIRON["tagname"]; gsub("!", "!!", menu_item);
            edit_path = $2; gsub("&", "&&", edit_path); gsub("#", "##", edit_path); gsub("\\|", "||", edit_path);
            select = $1; gsub(/</, "<lt>", select); gsub(/\t/, "<c-v><c-i>", select); gsub("!", "!!", select); gsub("&", "&&", select); gsub("#", "##", select); gsub("\\|", "||", select);
            out = out "%!" menu_item ": {MenuInfo}" menu_info "! %!evaluate-commands %# try %& edit -existing %|" edit_path "|; execute-keys %|/\\Q" keys "<ret>vc| & catch %& echo -markup %|{Error}unable to find tag| &; try %& execute-keys %|s\\Q" select "<ret>| & # !"
        }
        /[^\t]+\t[^\t]+\t[0-9]+/ {
            menu_item = $2; gsub("!", "!!", menu_item);
            select = $1; gsub(/</, "<lt>", select); gsub(/\t/, "<c-v><c-i>", select); gsub("!", "!!", select); gsub("&", "&&", select); gsub("#", "##", select); gsub("\\|", "||", select);
            menu_info = $3; gsub("!", "!!", menu_info); gsub("{", "\\{", menu_info);
            edit_path = $2; gsub("!", "!!", edit_path); gsub("#", "##", edit_path); gsub("&", "&&", edit_path); gsub("\\|", "||", edit_path);
            line_number = $3;
            out = out "%!" menu_item ": {MenuInfo}" menu_info "! %!evaluate-commands %# try %& edit -existing %|" edit_path "|; execute-keys %|" line_number "gx| & catch %& echo -markup %|{Error}unable to find tag| &; try %& execute-keys %|s\\Q" select "<ret>| & # !"
        }
        END { print ( length(out) == 0 ? "echo -markup %{{Error}no such tag " ENVIRON["tagname"] "}" : "menu -markup -auto-single " out ) }'
]]

alias global @ symbol

define-command -docstring "evaluate-buffer: evaluate current buffer contents as kakscrupt" \
evaluate-buffer %{
    execute-keys -draft '%:<space><c-r>.<ret>'
}

define-command -docstring "evaluate-selection: evaluate current sellection contents as kakscrupt" \
evaluate-selection %{
    execute-keys -itersel -draft ':<space><c-r>.<ret>'
}

hook global ModuleLoaded tmux %{
    define-command -docstring "vsplit [<commands>]: split tmux vertically" \
    vsplit -params .. -command-completion %{
        tmux-terminal-horizontal kak -c %val{session} -e "%arg{@}"
    }

    define-command -docstring "split [<commands>]: split tmux horizontally" \
    split -params .. -command-completion %{
        tmux-terminal-vertical kak -c %val{session} -e "%arg{@}"
    }

    define-command -docstring "tabnew [<commands>]: create new tmux window" \
    tabnew -params .. -command-completion %{
        tmux-terminal-window kak -c %val{session} -e "%arg{@}"
    }
}

define-command -docstring "print current working directory" \
pwd %{ echo %sh{ printf "%s\n" "${PWD}" }}

define-command -docstring "Ask before repeating last command (with `.' (dot)) for next search match" \
query-repeat %{ try %{
    execute-keys n
    prompt "confirm? [yn]: " -on-change %{ execute-keys %sh{
        case ${kak_text} in
            (y) printf "%s\n" "<esc>.: query-repeat<ret>";;
            (n) printf "%s\n" "<esc>: query-repeat<ret>" ;;
            (*) ;;
        esac
    }} nop
} catch %{
    fail "no search pattern"
}}

map global normal <c-n> ': query-repeat<ret>'

define-command -override -docstring "file <path> [<line>]: Fuzzy search and open file. If <line> argument is specified jump to the <line> after opening" \
file -shell-script-candidates %{
    [ -n "$(command -v fd)" ] && fd . -L --hidden --no-ignore --type f || find . -follow -type f
} -params 1..2 %{ evaluate-commands %sh{
    file=$(printf "%s\n" "$1" | sed "s/&/&&/g")
    printf "%s\n" "edit -existing -- %&${file}&"
    [ $# -gt 1 ] && printf "%s\n" "execute-keys '${2}g'"
}}

alias global $ file

require-module kak
add-highlighter shared/kakrc/code/if_else regex \b(if|else)\b 0:keyword

define-command -docstring "if <condition> <expression> [else [if <condition>] <expression>]: if statement that accepts shell-valid condition string" \
if -params 2.. %{ evaluate-commands %sh{
    while [ true ]; do
        condition="[ $1 ]"
        if [ -n "$3" ] && [ "$3" != "else" ]; then
            printf "%s\n" "fail %{if: unknown operator '$3'}"
        elif [ $# -eq 3 ]; then
            printf "%s\n" "fail %{if: wrong argument count}"
        elif eval $condition; then
            [ -n "${2##*&*}" ] && arg="$2" || arg="$(printf '%s' "$2" | sed 's/&/&&/g')"
            printf "%s\n" "evaluate-commands %& $arg &"
        elif [ $# -eq 4 ]; then
            [ -n "${4##*&*}" ] && arg="$4" || arg="$(printf '%s' "$4" | sed 's/&/&&/g')"
            printf "%s\n" "evaluate-commands %& $arg &"
        elif [ $# -gt 4 ]; then
            if [ "$4" = "if" ]; then
                shift 4
                continue
            else
                printf "%s\n" "fail %{if: wrong argument count}"
            fi
        fi
        exit
    done
}}

define-command -docstring "flygrep: run grep on every key" \
flygrep %{
    edit -scratch *grep*
    prompt "flygrep: " -on-change %{
        flygrep-call-grep %val{text}
    } nop
}

define-command -hidden flygrep-call-grep -params 1 %{ evaluate-commands %sh{
    [ -z "${1##*&*}" ] && text=$(printf "%s\n" "$1" | sed "s/&/&&/g") || text="$1"
    [ -z "${1##*@*}" ] && text=$(printf "%s\n" "$text" | sed "s/@/@@/g") || text="$text"
    if [ ${#1} -gt 2 ]; then
        printf "%s\n" "info"
        printf "%s\n" "evaluate-commands %&grep %@$text@&"
    else
        printf "%s\n" "info -title flygrep %{$((3-${#1})) more chars}"
    fi
}}

define-command -hidden clang-find-and-parse-compile-flags %{
    set-option -add window clang_options %sh{ (
        while [ "$PWD" != "$HOME" ]; do
            if [ -e "$PWD/compile_flags.txt" ]; then
                printf "%s\n" "$(cat "$PWD/compile_flags.txt" | tr '\n' ' ')"
                exit
            fi
            cd ..
        done
    ) }
}

add-highlighter shared/kakrc/code/map-sequence regex \b(un)?map-sequence\b 0:keyword
define-command -docstring "map-sequence <sequence> <command>: map <sequence> of keys to <command> in insert mode." \
map-sequence -params 2 %{ evaluate-commands %sh{
    keys=$(printf "%s" "$1" | sed "s/\([&|]\)/\1\1/g")
    cmd=$(printf "%s" "$2" | sed "s/\([@&|]\)/\1\1/g")
    printf "%s\n" "hook global -group $1-seq InsertChar ${1##${1%%?}} %|
        try %&
            execute-keys -draft h<a-B> <a-k>$keys<ret> s$keys\z<ret> d
            evaluate-commands %@$cmd@
        &
    |"
}}

map-sequence jj %{exec <esc>}

define-command -docstring "unmap-sequence <scope> <sequence>: unmap <sequence> of keys in insert mode." \
unmap-sequence -params 1 %{
    remove-hooks global "%arg{2}-seq"
}
