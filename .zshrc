export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="classyTouch/classyTouch"

# CASE_SENSITIVE="true"
# HYPHEN_INSENSITIVE="true"
# DISABLE_AUTO_UPDATE="true"
# export UPDATE_ZSH_DAYS=13
# DISABLE_LS_COLORS="true"
# DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
# HIST_STAMPS="mm/dd/yyyy"

# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
  git
)

source $ZSH/oh-my-zsh.sh

alias vimdiff="nvim -d"
alias zshconf="kak ~/.zshrc"
alias tmuxconf="kak ~/.tmux.conf"
alias vimconf="nvim ~/.config/nvim/init.vim"
alias kakconf="kak ~/.config/kak/kakrc"
alias emacsconf="emacs ~/.emacs.d/init.el"
alias stopwatch='while true; do echo -ne "\r$(date +%-M:%S:%N)"; done'
alias mc="command mc -x"
# alias tmux="command tmux new-session -A -s \>_"
alias tmux="tmux new-session -d -s \>_; tmux new-session -t \>_ \; set-option destroy-unattached"
alias less="less --tabs 4"

export EDITOR="kak"

if [ -z "$TMUX" ]; then
    tmux
fi
