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
    export PS1="\[$(tput setaf 1)\]┌─╼[\[$(tput setaf 7)\]\w\[$(tput setaf 1)\]] [\[$(tput setaf 7)\]ssh\[$(tput setaf 1)\]]\n\$(if [[ \$? == 0 ]]; then echo \"\[$(tput setaf 1)\]└────╼\"; else echo \"\[$(tput setaf 1)\]└╼\"; fi) \[$(tput setaf 7)\]"
else
    export PS1="\[$(tput setaf 1)\]┌─╼[\[$(tput setaf 7)\]\w\[$(tput setaf 1)\]]\n\$(if [[ \$? == 0 ]]; then echo \"\[$(tput setaf 1)\]└────╼\"; else echo \"\[$(tput setaf 1)\]└╼\"; fi) \[$(tput setaf 7)\]"
fi
