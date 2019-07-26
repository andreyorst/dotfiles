if [ -n "$(command -v exa)" ]; then
    unset LS_COLORS
    unset LSCOLORS
    alias ls="exa"
    alias la="exa -la"
    alias l="exa -l"
fi

if [ -n "$(command -v kak)" ]; then
    export EDITOR="kak"
elif [ -n "$(command -v emacs)" ]; then
    export EDITOR="emacs"
else
    export EDITOR="vi"
fi

mkcd() { mkdir -p $1 && cd $1; }

webmp4() {
    while [ $# -ne 0 ]; do
        ffmpeg -i "$1" "${1%.*}.mp4"
        shift
    done
}

alias zshconf="$EDITOR ~/.zshrc"
alias tmuxconf="$EDITOR ~/.tmux.conf"
alias kakconf="kak ~/.config/kak/kakrc"
alias emacsconf="emacs ~/.emacs.d/config.org"
alias less="less --tabs 4"
alias tmux="tmux new-session -d -s \>_ 2>/dev/null; tmux new-session -t \>_ \; set-option destroy-unattached"
alias gti="git"
alias sudo="sudo "

alias g="git"
alias e="$EDITOR"

for pm in yay pacman dnf yum apt; do
    if [ -n "$(command -v $pm)" ]; then
        alias pm="pm_$pm"
        break
    fi
done

pm_yay() {
    cmd="$1"
    shift
    case "$cmd" in
        (install) yay -S "$@"     ;;
        (update)  yay -Syyu "$@"  ;;
        (remove)  yay -Rsc "$@"   ;;
        (search)  yay -Ss "$@"    ;;
        (*)       yay "$cmd" "$@" ;;
    esac
}

pm_packman() {
    cmd="$1"
    shift
    case "$cmd" in
        (install) sudo pacman -S "$@"     ;;
        (update)  sudo pacman -Syyu "$@"  ;;
        (remove)  sudo pacman -Rsc "$@"   ;;
        (search)  pacman -Ss "$@"         ;;
        (*)       sudo pacman "$cmd" "$@" ;;
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
        (*)       sudo dnf "$cmd" "$@"  ;;
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
        (*)       sudo yum "$cmd" "$@"  ;;
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
            (*)       sudo apt "$cmd" "$@"  ;;
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

if [ -n "$(command -v dash)" ]; then
    export KAKOUNE_POSIX_SHELL=$(command -v dash)
fi

