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
alias tmux="tmux new-session -d -s \>_ 2>/dev/null; tmux new-session -t \>_ \; set-option destroy-unattached"

export EDITOR="kak"

if [ -z "$TMUX" ]; then
    tmux
fi
