# Aliases and Defaults
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

unset LS_COLORS
unset LSCOLORS

# Hack to make sudo work with aliases
alias sudo="sudo "

alias gti="git"
alias g="git"

[ -n "$(command -v tmux)" ] && alias tmux="tmux new-session -d -s \>_ 2>/dev/null; tmux new-session -t \>_ \; set-option destroy-unattached"

# Edit things in apropriate environment
alias tmuxconf="$EDITOR ~/.tmux.conf"
alias emacsconf="emacs ~/.emacs.d/README.org"

alias less="less --tabs 4 -R"

if [ -n "$(command -v exa)" ]; then
    alias ls="exa"
    alias la="exa -la"
    alias l="exa -l"
    alias ll="exa -l"
else
    alias ll="ls -l"
    alias l="ls -l"
    alias la="ls -la"
fi

if [ -n "$(command -v emacs)" ]; then
    export EDITOR="emacs"
elif [ -n "$(command -v vim)" ]; then
    export EDITOR="vim"
else
    export EDITOR="vi"
fi

HISTCONTROL=ignoredups:erasedups
HISTIGNORE='ls:ll:cd:pwd:bg:fg:history'
HISTSIZE=100000
HISTFILESIZE=10000000
shopt -s histappend

# smarter `emacs' launcher that will open file in existing emacs if one exists
function emacs() { [ $# -gt 0 ] && emacsclient -a emacs -n "$@" || command emacs; }

# create dir and cd into it
function mkcd() { mkdir -p "$1" && cd "$1"; }

# convert webm to mp4
function webmp4() {
    if [ -z "$(command -v ffmpeg)" ]; then
        printf "%s\n" "ffmpeg is not installed" >&2
        return 1
    fi
    while [ $# -ne 0 ]; do
        ffmpeg -i "$1" "${1%.*}.mp4"
        shift
    done
}

function scour() {
    if [ -z "$(command -v scour)"  ]; then
        printf "%s\n" "scour is not installed" >&2
        return 1
    fi
    while [ $# -ne 0 ]; do
        command scour -i "$1" -o tmp.svg
        mv tmp.svg "$1"
        shift
    done
}

# gifify wrapper for fast gif creating
function gif() {
    if [ -z "$(command -v gifify)" ]; then
        printf "%s\n" "gifify is not installed" >&2
        return 1
    fi
    gifify $1 -o ${1#.*}.gif --colors 256 --compress 0 --fps 30
}

function sep() {
    for i in $(seq 1 $(tput cols)); do
        sep="${sep}="
    done
    echo $sep
    unset sep
}
