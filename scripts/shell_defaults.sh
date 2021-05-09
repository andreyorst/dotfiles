#!/usr/bin/bash
# Aliases and Defaults

unset LS_COLORS
unset LSCOLORS

# Hack to make sudo work with aliases
alias sudo="sudo "

alias gti="git"
alias g="git"

alias ssh="TERM=xterm ssh"

alias copy="xsel -bi"

alias lispwrap="rlwrap -r -q'\"' -b \"(){}[],^%#@\\\";:'\""

[ -n "$(command -v tmux)" ] && alias tmux="tmux new-session -d -s \>_ 2>/dev/null; tmux new-session -t \>_ \; set-option destroy-unattached"

# Edit things in apropriate environment
alias tmuxconf="\$EDITOR ~/.tmux.conf"
alias kakconf="kak ~/.dotfiles/.config/kak/kakrc"
alias emacsconf="emacs ~/.dotfiles/.config/emacs/README.org"

if [ -n "$(command -v dash)" ]; then
    KAKOUNE_POSIX_SHELL="$(command -v dash)"
    export KAKOUNE_POSIX_SHELL
fi

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
    export EDITOR="command emacs -nw"
elif [ -n "$(command -v kak)" ]; then
    export EDITOR="kak"
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

emacsupdate() {
    emacs --batch --load ~/.dotfiles/.config/emacs/init.el --eval '(progn (straight-pull-all) (straight-rebuild-all))';
}

# create dir and cd into it
mkcd() { mkdir -p "$1" && cd "$1" || return $?; }

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

# Reverse mv.
# Handy when you need to do `mv file file.tmp' and you don't want to
# rewrite command to revert this change, so you can use `remv file file.tmp'
remv() {
    mv "$2" "$1"
}

scour() {
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

# ffmpeg and gifsicle wrapper for fast gif creating
gif() {
    if [ -z "$(command -v ffmpeg)" ]; then
        printf "%s\n" "ffmpeg is not installed" >&2
        return 1
    fi
    if [ -z "$(command -v gifsicle)" ]; then
        printf "%s\n" "gifsicle is not installed" >&2
        return 1
    fi
    palette=$(mktemp "/tmp/palette-XXXXXXXXX.png")
    filters="fps=30,scale=-1:-1:flags=lanczos"
    file="$1"
    shift
    echo "generating palette"
    ffmpeg -v warning -i "$file" -vf "$filters,palettegen" -y "$palette"
    echo "generating gif"
    ffmpeg -v warning -i "$file" -i "$palette" -lavfi "$filters [x]; [x][1:v] paletteuse" -f gif - | gifsicle "$@" > "${file%.*}.gif"
    rm -f "$palette"
}

sep() {
    (
        sep=""
        for _ in $(seq 1 "$(tput cols)"); do
            sep="${sep}${1:-=}"
        done
        [ -n "$2" ] && file="${1:-=} $2 ${1:-=}"
        echo "${file}${sep}" | cut -c 1-"$(tput cols)"
    )
}


sepcat() {
    (
        while [ $# -ne 0 ]; do
            case $1 in
                (-s=*|--sep=*) separator="${1#*=}" ;;
                (--sep) shift ; separator="$1" ;;
                (--help) printf "%s\n" "sepcat -- cat with separator between files" >&2
                         printf "%s\n" "sepcat [-s=<separator>|--sep=<separator>|--sep <separator>] file1 file2 ... fileN" ;;
                (*) files="$files $1" ;;
            esac
            shift
        done
        for file in $files; do
            sep "$separator" "$file"
            cat "$file"
        done
    )
}
