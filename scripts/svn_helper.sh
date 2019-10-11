# Smarter svn command. It will use more robust versions
# instead of builtin ones or fallback to default if there's
# no handler for particular svn command.
svn() {
    cmd="$1"
    shift
    case "$cmd" in
        (diff)  svn_diff "$@"           ;;
        (log)   svn_log "$@"            ;;
        (st)    svn_st "$@"             ;;
        (blame) svn_blame "$@"          ;;
        (clean) svn_clean               ;;
        (*)     command svn "$cmd" "$@" ;;
    esac
}

## calls svn diff with colors piped to less if needed.
svn_diff() {
    if [ -n "$(command -v colordiff)" ]; then
        command svn diff -x -w "$@" | colordiff | less -RF
    else
        command svn diff -x -w "$@" | less -F
    fi
}

## calls svn log piped to less when log
## takes more screen space than currently available.
svn_log() {
    command svn log "$@" | less -F
}

## calls svn st, but dismiss all untracked files
svn_st() {
    command svn st "$@" | grep -v '^?'
}

## calls svn blame, and puts it ouptup to less if needed
svn_blame() {
    command svn blame "$@" | less -F
}

## calls svn st, and greps all untracked files;
## strips first column with `?[\t ]+', and prints null-terminated results with awk;
## passes to `xargs -0', and deletes with `rm -rf'
## WARNING: doesn't work with busybox awk
svn_clean() {
    command svn st | grep '^?' | awk '{ sub(/^.[\t ]+/, "", $0); printf("%s%c", $0, 0); }' | xargs -0 rm -rf
}
