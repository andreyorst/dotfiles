export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="classyTouch/classyTouch"

ENABLE_CORRECTION="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh

alias vimdiff="nvim -d"
alias zshconf="kak ~/.zshrc"
alias tmuxconf="kak ~/.tmux.conf"
alias vimconf="nvim ~/.config/nvim/init.vim"
alias kakconf="kak ~/.config/kak/kakrc"
alias emacsconf="emacs ~/.emacs.d/init.el"
alias stopwatch='while true; do echo -ne "\r$(date +%-M:%S:%N)"; done'
alias mc="command mc -x"
alias less="less --tabs 4"

export EDITOR="kak"

if [ -z "$TMUX" ]; then
    alias tmux="tmux new-session -d -s \>_ >/dev/null 2>&1; tmux new-session -t \>_ \; set-option destroy-unattached"
    tmux
fi
