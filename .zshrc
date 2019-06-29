export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="classyTouch/classyTouch"

ENABLE_CORRECTION="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh
source $HOME/.dotfiles/.shell_defaults.sh

if [ -z "$TMUX" ]; then
    tmux
fi
