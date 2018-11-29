# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ commands.kak     │
# ╞═════════════╩══════════════════╡
# │ Custom commands for Kakoune    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

define-command -override -docstring "find <name>: fuzzy match through filenames" \
find -params 1 -shell-script-candidates %{ find -L -type f } %{ edit %arg{1} }

define-command -override -docstring \
"file-search-rec <filename>: search for file recusively under path option: %opt{path}" \
file-search-rec -params 1 %{ evaluate-commands %sh{
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
            [ -e "$path/$file" ] && result="$path/$file"
        else # get first match with find
            result=$(find -L $path -mount -type f -name "$file" | head -n 1)
        fi
        if [ ! -z "$result" ]; then
            printf "%s\n" "edit -existing %{$result}"
            exit
        fi
        shift
    done
    printf "%s\n" "echo -markup %{{Error}unable to find file '$file'}"
}}

define-command -override -docstring \
"select a word under cursor, or add cursor on next occurrence of current selection" \
select-or-add-cursor %{ execute-keys -save-regs '' %sh{
    if [ $(expr $(echo $kak_selection | wc -m) - 1) -eq 1 ]; then
        echo "<a-i>w*"
    else
        echo "*<s-n>"
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

define-command -override -docstring "jump to symbol definition in current file" \
-shell-script-candidates %{
    tags="${TMPDIR:-/tmp}/tags-${kak_buffile##*/}"; tags="${tags%.*}"
    ctags -f $tags $kak_buffile
    readtags -t $tags -l | cut -f 1 | awk '!x[$0]++' | grep -v -e "__anon.*"
} symbol -params 1 %{ evaluate-commands %sh{
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

