export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="classyTouch/classyTouch"

ENABLE_CORRECTION="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh

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

mkcd() { mkdir "$1" && cd "$1" }

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

for pm in yay packman dnf yum apt; do
    if [ -n "$(command -v $pm)" ]; then
        alias pm="$pm"
        break
    fi
done

if [ -n "$(command -v dash)" ]; then
    export KAKOUNE_POSIX_SHELL=$(command -v dash)
fi

if [ -z "$TMUX" ]; then
    tmux
fi
