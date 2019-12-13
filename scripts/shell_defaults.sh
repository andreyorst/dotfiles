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

# smarter `emacs' launcher that will open file in existing emacs if one exists
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

sep() {
    for i in $(seq 1 $(tput cols)); do
        sep="${sep}="
    done
    echo $sep
    unset sep
}
