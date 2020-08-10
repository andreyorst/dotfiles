# Create new C project from template
cnew()
{(
    cleanup () {
        rm -rf "$1"
        printf "%s\n" "Error creating a project '$1'" >&2
        printf "%s\n" "$2" >&2
    }

    if [ $# -lt 1 ]; then
        printf "%s\n" "At leas 1 argument required" >&2
        return 1
    fi

    res=0
    vcs="git"
    readme=
    empty=
    while [ $# -gt 0 ]; do
        case "$1" in
            (-add-flag)
                shift
                flags=$([ -z "$flags" ] && printf "%s" "$1" || printf "%s\n%s" "$flags" "$1") ;;
            (-vcs) shift; vcs="$1"  ;;
            (-readme) readme="true" ;;
            (-empty) empty="true"   ;;
            (-help)
                printf "%s\n" "usage: cnew [-add-flag <compile flag>] [-git] [-readme] [-help] project_name\n" >&2
                printf "%s\n" "  -add-flag <flag>:  add compile flag to project compile_flags.txt file" >&2
                printf "%s\n" "  -vcs [git | none]: create git repository in project" >&2
                printf "%s\n" "  -readme:           create empty readme file for a project" >&2
                return 0 ;;
            (*) project_name="$1" ;;
        esac
        shift
    done


    if ! mkdir "$project_name" >/dev/null 2>&1; then
        res=$?
        printf "%s\n" "Error creating a project '$project_name': Could not create directory ($res)" >&2
        return $res
    fi

    if [ -z "$empty" ]; then
        if ! cp -r ~/.dotfiles/c_project_template/. "$project_name/" >/dev/null 2>&1; then
            res=$?
            cleanup "$project_name" "Could not initialize project with files ($res)"
            return $res
        fi
    fi

    if [ -n "$flags" ] && [ -z "$empty" ]; then
        if ! printf "%s\n" "$flags" >> "$project_name/compile_flags.txt"; then
            res=$?
            cleanup "$project_name" "Could not specify additional flags ($res)"
            return $res
        fi
    fi

    if [ "$vcs" = "git" ]; then
        if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
            (
                cd "$project_name" || cleanup_and_exit "$project_name" "Could not initialize git repository ($res)"
                git init >/dev/null 2>&1
                git add . >/dev/null 2>&1
                git commit --allow-empty-message -m '' >/dev/null 2>&1
                res=$?
            )
        fi
    elif [ ! "$vcs" = "none" ]; then
        cleanup "$project_name" "VCS '$vcs' is not supported by the script (1)"
        return 1
    fi

    if [ -n "$readme" ] && [ -z "$empty" ]; then
        if ! printf "%s\n" "# $project_name" > "$project_name/README.md"; then
            res=$?
            cleanup "$project_name" "Could not create README.md ($res)"
            return $res
        fi
    fi

    if [ $res -eq 0 ]; then
        printf "%s\n" "Created project: $project_name"
    fi
)}
