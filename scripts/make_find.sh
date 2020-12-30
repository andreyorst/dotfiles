bake_find_command() {
    if [ $# -eq 0 ]; then
        printf "%s\n" "Warning, zero arguments passed. Generating default find command." >&2
        set -- "-default"
    fi
    while [ $# -gt 0 ]; do
        case $1 in
            (-exclude-file=*)  exclude_files="'${1#-exclude-file=}' $exclude_files" ;;
            (-F|-exclude-file) shift; exclude_files="'$1' $exclude_files" ;;
            (-exclude-dir=*)  exclude_files="'${1#-exclude-dir=}' $exclude_files" ;;
            (-D|-exclude-dir) shift; exclude_dirs="'$1' $exclude_dirs" ;;
            (-default)
                exclude_files="'*.o' '*.bin' '*.obj' $exclude_files"
                exclude_dirs="'.git' '.svn' $exclude_dirs" ;;
            (-clean)
                unset -f find
                unset exclude_files
                unset exclude_dirs
                return 0 ;;
            (-help|-h)
                printf "%s"
                "usage: bake_find_command [-F <filename>]* [-D <dirname>]*

  -F -exclude-file: exclude this file from search.  Globs supported.
  -D -exclude-dir:  exclude this dir from search.  Globs supported.

     -default:      exclude default set of files and directories.
                    Default files: '*.o' '*.bin' '*.obj'
                    Default directories: '.git' '.svn'

     -clean:        remove all ignored patterns and exit.

  -h -help:         print this message and exit." >&2
                unset exclude_files
                unset exclude_dirs
                return 0 ;;
            (*) printf "unknown switch\n" >&2
                unset exclude_files
                unset exclude_dirs
                return 1 ;;
        esac
        shift
    done
    eval "set -- $exclude_files"
    while [ $# -gt 0 ]; do
        exclude="$exclude -name '$1' -o"
        shift
    done
    eval "set -- $exclude_dirs"
    while [ $# -gt 0 ]; do
        exclude="$exclude -path '*/$1' -o"
        shift
    done
    cmd="command find . \( ${exclude% -o} \) -prune -o"

    eval "find() { $cmd \$@ -print; }"

    unset exclude_files
    unset exclude_dirs
    unset exclude
    unset cmd
}
