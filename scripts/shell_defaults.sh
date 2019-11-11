# Aliases and Defaults
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

if [ -n "$(command -v dash)" ] && [ -n "$(command -v kak)" ]; then
    export KAKOUNE_POSIX_SHELL=$(command -v dash)
fi

if [ -n "$(command -v exa)" ]; then
    unset LS_COLORS
    unset LSCOLORS
    alias ls="exa"
    alias la="exa -la"
    alias l="exa -l"
    alias ll="exa -l"
fi

if [ -n "$(command -v kak)" ]; then
    export EDITOR="kak"
elif [ -n "$(command -v emacs)" ]; then
    export EDITOR="emacs"
else
    export EDITOR="vi"
fi

alias e="$EDITOR"

alias gti="git"
alias g="git"

# Edit things in apropriate environment
alias zshconf="$EDITOR ~/.zshrc"
alias tmuxconf="$EDITOR ~/.tmux.conf"
alias kakconf="kak ~/.config/kak/kakrc"
alias emacsconf="emacs ~/.emacs.d/config.org"

alias less="less --tabs 4"

# ultimate alias to create and attach to tmux sessions
alias tmux="tmux new-session -d -s \>_ 2>/dev/null; tmux new-session -t \>_ \; set-option destroy-unattached"

# to use sudo with aliases
alias sudo="sudo "

# Functions
# ‾‾‾‾‾‾‾‾‾

emacs() { emacsclient -a 'emacs' -n "$@" 2>/dev/null || command emacs; }

# create dir and cd into it
mkcd() { mkdir -p $1 && cd $1; }

# convert webm to mp4
webmp4() {
    if [ -z "$(command -v ffmpeg)" ]; then
        printf "%s\n" "ffmpeg is not installed" >&2
        return 1
    fi
    while [ $# -ne 0 ]; do
        ffmpeg -i "$1" "${1%.*}.mp4"
        shift
    done
}

# gifify wrapper for fast gif creating
gif() {
    if [ -n "$(command -v gifify)" ]; then
        gifify $1 -o ${1#.*}.gif --colors 256 --compress 0 --fps 30
    else
        printf "%s\n" "gifify is not installed" >&2
        return 1
    fi
}

# Create new C project from template
cnew() {
    if [ $# -lt 1 ]; then
        printf "%s\n" "At leas 1 argument required" >&2
        return 1
    fi

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

    mkdir "$project_name" >/dev/null 2>&1
    res=$?
    if [ $res -ne 0 ]; then
        printf "%s\n" "Error creating a project '$project_name': Could not create directory($res)" >&2
        return $res
    fi

    if [ -z "$empty" ]; then
        cp -r ~/.dotfiles/.c_project_template/. "$project_name/" >/dev/null 2>&1
        res=$?
        if [ $res -ne 0 ]; then
            rm -rf "$project_name"
            printf "%s\n" "Error creating a project '$project_name'" >&2
            printf "%s\n" "Could not initialize project with files ($res)" >&2
            return $res
        fi
    fi

    if [ -n "$flags" ] && [ -z "$empty" ]; then
        printf "%s\n" "$flags" >> "$project_name/compile_flags.txt"
        res=$?
        if [ $res -ne 0 ]; then
            rm -rf "$project_name"
            printf "%s\n" "Error creating a project '$project_name'" >&2
            printf "%s\n" "Could not specify additional flags ($res)" >&2
            return $res
        fi
    fi

    if [ "$vcs" = "git" ]; then
        if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
            git init "$project_name" >/dev/null 2>&1
            res=$?
        fi
        if [ $res -ne 0 ]; then
            rm -rf "$project_name"
            printf "%s\n" "Error creating a project '$project_name'" >&2
            printf "%s\n" "Could not initialize git repository ($res)" >&2
            return $res
        fi
    elif [ "$vcs" = "none" ]; then
        res=0
    else
        rm -rf "$project_name"
        printf "%s\n" "Error creating a project '$project_name'" >&2
        printf "%s\n" "VCS '$vcs' is not supported by the script (1)" >&2
        return 1
    fi

    if [ -n "$readme" ] && [ -z "$empty" ]; then
        printf "%s\n" "# $project_name" > "$project_name/README.md"
        res=$?
        if [ $res -ne 0 ]; then
            rm -rf "$project_name"
            printf "%s\n" "Error creating a project '$project_name'" >&2
            printf "%s\n" "Could not create README.md ($res)" >&2
            return $res
        fi
    fi

    if [ $res -eq 0 ]; then
        printf "%s\n" "Created project: $project_name"
    fi
}

# Package manager wrappers
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
for pm in yay pacman rpm-ostree dnf yum apt; do
    if [ -n "$(command -v $pm)" ]; then
        alias pm="pm_$(echo $pm | sed "s/[^a-zA-Z_]/_/g")"
        break
    fi
done

pm_yay() {
    cmd="$1"
    shift
    case "$cmd" in
        (install) yay -Sy "$@"                                                                              ;;
        (update)  yay -Syyu --noconfirm --devel --nodiffmenu --noeditmenu --noupgrademenu --removemake "$@" ;;
        (remove)  yay -Rsc "$@"                                                                             ;;
        (search)  yay -Ss "$@"                                                                              ;;
        (*)       yay "$cmd" "$@"                                                                           ;;
    esac
}

pm_packman() {
    cmd="$1"
    shift
    case "$cmd" in
        (install) sudo pacman -Sy "$@"               ;;
        (update)  sudo pacman -Syyu --noconfirm "$@" ;;
        (remove)  sudo pacman -Rsc "$@"              ;;
        (search)  pacman -Ss "$@"                    ;;
        (*)       pacman "$cmd" "$@"                 ;;
    esac
}

pm_dnf() {
    cmd="$1"
    shift
    case "$cmd" in
        (install) sudo dnf install "$@" ;;
        (update)  sudo dnf upgrade "$@" ;;
        (remove)  sudo dnf remove "$@"  ;;
        (search)  dnf search "$@"       ;;
        (*)       dnf "$cmd" "$@"       ;;
    esac
}

pm_yum() {
    cmd="$1"
    shift
    case "$cmd" in
        (install) sudo yum install "$@" ;;
        (update)  sudo yum update "$@"  ;;
        (remove)  sudo yum remove "$@"  ;;
        (search)  yum search "$@"       ;;
        (*)       yum "$cmd" "$@"       ;;
    esac
}

pm_rpm_ostree() {
    cmd="$1"
    shift
    case "$cmd" in
        (install) rpm-ostree install "$@" ;;
        (update)  rpm-ostree upgrade "$@" ;;
        (remove)  rpm-ostree remove "$@"  ;;
        (*)       rpm-ostree "$cmd" "$@"  ;;
    esac
}

if [ -n "${PATH##*termux*}" ]; then
    pm_apt() {
        cmd="$1"
        shift
        case "$cmd" in
            (install) sudo apt install "$@" ;;
            (update)  sudo apt update "$@"  ;;
            (remove)  sudo apt remove "$@"  ;;
            (search)  apt search "$@"       ;;
            (*)       apt "$cmd" "$@"       ;;
        esac
    }
else
    pm_apt() {
        cmd="$1"
        shift
        case "$cmd" in
            (install) apt install "$@" ;;
            (update)  apt update && apt upgrade "$@"  ;;
            (remove)  apt remove "$@" ;;
            (search)  apt search "$@" ;;
            (*)       apt "$cmd" "$@" ;;
        esac
    }
fi

getpasswd() {(
    file="$HOME/.passwords.gpg"
    while [ $# -gt 0 ]; do
        case $1 in
            (-file)    shift; file="$1" ;;
            (-file=*)  file="${1#-file=}";;
            (-copy|-c) copy="true" ;;
            (-help|-h)
                printf "%s\n" "usage: getpasswd [-file <filename>] [-copy] [-help] [keyname] ...\n" >&2
                printf "%s\n" "  -c -copy: copy password to clipboard. If specified only one key is used." >&2
                printf "%s\n" "     -file filename: look for passwords in specifiled file." >&2
                printf "%s\n" "  -h -help: print this message" >&2
                return 0 ;;
            (*) args="'$1' $args" ;;
        esac
        shift
    done
    while [ -z "$file" ] || [ ! -e "$file" ]; do
        [ -n "$file" ] && printf "No such file '%s'\n" "$file"
        printf "Please specify a file: "
        read -r file
    done
    eval "set -- $args"
    if [ $# -lt 1 ]; then
        printf "Enter service name(s) (^D to finish): " >&2
        set -- $(</dev/stdin)
        printf "\n"
    fi
    if [ "$copy" = "true" ] || [ $# -eq 1 ]; then
        name="$1"
        result=$(gpg --decrypt "$file" 2>/dev/null | grep -Po -- "(?<=^- $name :: ).*")
        amount=$(printf "%s\n" "$result" | wc -l)
        if [ $amount -gt 1 ]; then
            printf "Multiple passwords found for '%s'. Select which to copy [%s]: " "$name" "$(seq -s ', ' 1 $amount)" >&2
            read -r choice
            if [ $choice -lt 1 ] || [ $choice -gt $amount ]; then
                printf "Bad choice '%s'.\n" "$choice" >&2
                return 1
            fi
            printf "%s\n" $result | head -n $choice | tail -n +$choice | tr -d '\n' | xsel -b -i
        fi
        printf "%s\n" "Password for '$1' copied to clipboard" >&2
    else
        for name in $@; do
            names="^- ${name} :: \|${names}"
        done
        names="${names%\\|}"
        gpg --decrypt "$file" 2>/dev/null | grep -- "${names}"
    fi
)}
