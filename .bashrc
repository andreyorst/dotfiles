# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Scripts and better defaults for shell
for i in $HOME/.dotfiles/scripts/*.sh; do
    . "$i"
done

# place to install npm packages
NPM_PACKAGES="${HOME}/.npm-packages"
# Preserve MANPATH if you already defined it somewhere in your config.
# Otherwise, fall back to `manpath` so we can inherit from `/etc/manpath`.
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.dotfiles/scripts:$HOME/.local/bin:$HOME/bin:$NPM_PACKAGES/bin:$PATH"
fi
export PATH

unset LS_COLORS

# classyTouch Prompt
PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]] \$SSH_PS1\$CONTAINER_PS1\$GIT_PS1
\$(if [ \$? -eq 0 ]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"

# Avoid duplicates
HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

git_ps1() {
    if git rev-parse --is-inside-work-tree 1>/dev/null 2>&1; then
        branch=$(git symbolic-ref --short HEAD 2>/dev/null)
        [ $? -ne 0 ] && branch=$(git rev-parse --short HEAD)
        if [ -n "$(git status --porcelain 2>/dev/null)" ]; then
            GIT_PS1="[$(tput sgr0)git$(tput setaf 1):$(tput sgr0)${branch:-unknown}*$(tput setaf 1)] "
        else
            GIT_PS1="[$(tput sgr0)git$(tput setaf 1):$(tput sgr0)${branch:-unknown}$(tput setaf 1)] "
        fi
    else
        GIT_PS1=
    fi
}

ssh_ps1() {
    if [ -n "$SSH_CONNECTION" ]; then
        SSH_PS1="[$(tput sgr0)ssh$(tput setaf 1)] "
    else
        SSH_PS1=
    fi
}

screen_ps1() {
    case "$TERM" in
        screen*) SCREEN_PS1="[$(tput sgr0)screen$(tput setaf 1)] " ;;
        *) SCREEN_PS1= ;;
    esac
}

container_ps1() {
    if [ -e /run/.toolboxenv ]; then
        CONTAINER_PS1="[$(tput sgr0)toolbox$(tput setaf 1)] "
    elif [ -e /run/.containerenv  ]; then
        CONTAINER_PS1="[$(tput sgr0)podman$(tput setaf 1)] "
    elif [ -e /.dockerenv ]; then
        CONTAINER_PS1="[$(tput sgr0)docker$(tput setaf 1)] "
    else
        CONTAINER_PS1=
    fi
}

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r; git_ps1; ssh_ps1; screen_ps1; container_ps1"

if [ -z "$TMUX" ]; then
    tmux
fi
