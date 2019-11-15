# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Scripts and better defaults for shell
for i in $HOME/.dotfiles/scripts/*.sh; do
    . "$i"
done

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.dotfiles/scripts:$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# classyTouch Prompt
if [ -n "$SSH_CONNECTION" ]; then
    export PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]] [\[\e[m\]ssh\[\e[0;31m\]]\n\$(if [[ \$? == 0 ]]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"
else
    export PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]]\n\$(if [[ \$? == 0 ]]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"
fi

# Avoid duplicates
HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
