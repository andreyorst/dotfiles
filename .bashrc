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
PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]] \$SSH_PS1\$TOOLBOX_PS1\$GIT_PS1
\$(if [ \$? -eq 0 ]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"

# Avoid duplicates
HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

git_ps1() {
    if git rev-parse --is-inside-work-tree 1>/dev/null 2>&1; then
        if ! git diff-files --no-ext-diff --quiet || ! git diff-index --no-ext-diff --quiet --cached HEAD; then
            GIT_PS1="[$(tput setaf 7)git$(tput setaf 1):$(tput setaf 7)$(git branch 2>/dev/null | grep '^*' | colrm 1 2)*$(tput setaf 1)] "
        else
            GIT_PS1="[$(tput setaf 7)git$(tput setaf 1):$(tput setaf 7)$(git branch 2>/dev/null | grep '^*' | colrm 1 2)$(tput setaf 1)] "
        fi
    else
        GIT_PS1=
    fi
}

ssh_ps1() {
    if [ -n "$SSH_CONNECTION" ]; then
        SSH_PS1="[$(tput setaf 7)ssh$(tput setaf 1)] "
    else
        SSH_PS1=
    fi
}

screen_ps1() {
    case "$TERM" in
        screen*) SCREEN_PS1="[$(tput setaf 7)screen$(tput setaf 1)] " ;;
        *) SCREEN_PS1= ;;
    esac
}

toolbox_ps1() {
    if [ -e /run/.toolboxenv ]; then
        TOOLBOX_PS1="[$(tput setaf 7)toolbox$(tput setaf 1)] "
    else
        TOOLBOX_PS1=
    fi
}

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r; git_ps1; ssh_ps1; screen_ps1; toolbox_ps1"
