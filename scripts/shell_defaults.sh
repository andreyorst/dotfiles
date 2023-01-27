#!/usr/bin/env bash
# Aliases and Defaults

unset LS_COLORS
unset LSCOLORS

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
