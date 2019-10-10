export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="classyTouch/classyTouch"

ENABLE_CORRECTION="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh
source $HOME/.dotfiles/scripts/shell_defaults.sh
source $HOME/.dotfiles/scripts/svn_helper.sh
