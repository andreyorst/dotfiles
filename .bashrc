# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

. $HOME/.dotfiles/.shell_defaults.sh

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# classyTouch Prompt
if [ -n "$SSH_CONNECTION" ]; then
    export PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]] [\[\e[m\]ssh\[\e[0;31m\]]\n\$(if [[ \$? == 0 ]]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"
else
    export PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]]\n\$(if [[ \$? == 0 ]]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"
fi

source $HOME/.dotfiles/.shell_defaults.sh
