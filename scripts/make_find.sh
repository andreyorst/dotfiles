bake_find_command() {
    if [ $# -eq 0 ]; then
        printf "%s\n" "Warning, zero arguments passed. Generating default find command." >&2
        set -- "-default"
    fi
    while [ $# -gt 0 ]; do
        case $1 in
            (-F) shift; exclude_files="'$1' $exclude_files" ;;
            (-exclude-file=*) exclude_files="'${1#-exclude-file=}' $exclude_files" ;;
            (-D) shift; exclude_dirs="'$1' $exclude_dirs" ;;
            (-exclude-dir=*) exclude_files="'${1#-exclude-dir=}' $exclude_files" ;;
            (-default)
                exclude_files="'*.o' '*.bin' '*.obj' $exclude_files"
                exclude_dirs="'.git' '.svn' $exclude_dirs" ;;
            (-help|-h)
                printf "%s\n" "usage: bake_find_command [-F <filename>] ... [-D <dirname>] ...\n" >&2
                printf "%s\n" "  -F -exclude-file=: exclude this file from search." >&2
                printf "%s\n" "  -D -exclude-dir=: exclude this dir from search." >&2
                printf "%s\n" "     -default: exclude default set of files and dirs." >&2
                printf "%s\n" "  -h -help: print this message" >&2 ;;
            (*) printf "unknown switch\n" >&2
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
