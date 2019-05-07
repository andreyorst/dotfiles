export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="classyTouch/classyTouch"

ENABLE_CORRECTION="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh

if [ -n "$(command -v exa)" ]; then
    unset LS_COLORS
    unset LSCOLORS
    alias ls=exa
fi

if [ -n "$(command -v kak)" ]; then
    export EDITOR="kak"
elif [ -n "$(command -v emacs)" ]; then
    export EDITOR="emacs"
else
    export EDITOR="vi"
fi

alias vimdiff="nvim -u ~/.dotfiles/.vimdiffrc -d"
alias zshconf="$EDITOR ~/.zshrc"
alias tmuxconf="$EDITOR ~/.tmux.conf"
alias vimconf="nvim ~/.config/nvim/init.vim"
alias kakconf="kak ~/.config/kak/kakrc"
alias emacsconf="emacs ~/.emacs.d/config.org"
alias stopwatch='while true; do echo -ne "\r$(date +%-M:%S:%N)"; done'
alias mc="command mc -x"
alias less="less --tabs 4"
alias tmux="tmux new-session -d -s \>_ 2>/dev/null; tmux new-session -t \>_ \; set-option destroy-unattached"
alias gti="git"

if [ -n "$(command -v dash)" ]; then
    export KAKOUNE_POSIX_SHELL=$(command -v dash)
fi

if [ -z "$TMUX" ]; then
    tmux
fi
