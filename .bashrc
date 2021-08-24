# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    source /etc/bashrc
fi

# Scripts and better defaults for the shell
for i in "$HOME"/.dotfiles/scripts/*.sh; do
    # shellcheck disable=SC1090
    source "$i"
done

# User specific environment
PATH="$HOME/.dotfiles/scripts:$PATH"
PATH="$HOME/.local/bin:$PATH"

export PATH

# Lua related stuff
[ -n "$(command -v luarocks)" ] && eval "$(luarocks path)"

for path in "$HOME"/.local/share/lua/*; do
    LUA_PATH="$path/?.lua;$LUA_PATH"
done

export LUA_PATH

# History
export HISTFILESIZE=
export HISTSIZE=-1
export HISTTIMEFORMAT="[%F %T] "
export HISTIGNORE='ls:ll:cd:pwd:bg:fg:history'

# Prompt
PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]] \$TIME_PS1\$SSH_PS1\$CONTAINER_PS1\$GIT_PS1
\$(if [ \$? -eq 0 ]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"

if [ -n "$VTERM" ]; then
    vterm_printf(){
        if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
            # Tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "${TERM%%-*}" = "screen" ]; then
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$1"
        else
            printf "\e]%s\e\\" "$1"
        fi
    }

    vterm_prompt_end(){
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }
    PS1=$PS1'\[$(vterm_prompt_end)\]'
fi

PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -n; time_ps1; git_ps1; ssh_ps1; container_ps1"

function time_ps1() {
    TIME_PS1="[$(tput sgr0)$(date +'%a %H:%M')$(tput setaf 1)] "
}

## NO_GIT_PS1 can be bound in order to skip parsing if repository is
## too big and it takes too long for prompt to appear
function git_ps1() {
    if test -z "$NO_GIT_PS1" && git rev-parse --is-inside-work-tree 1>/dev/null 2>&1; then
        if ! branch=$(git symbolic-ref --short HEAD 2>/dev/null); then
            if ! branch=$(git name-rev HEAD --name-only --no-undefined --tags 2>/dev/null); then
                branch=$(git rev-parse --short HEAD)
            fi
        fi
        if [ -n "$(git status --porcelain 2>/dev/null)" ]; then
            GIT_PS1="[$(tput sgr0)git$(tput setaf 1):$(tput sgr0)${branch:-unknown}*$(tput setaf 1)] "
        else
            GIT_PS1="[$(tput sgr0)git$(tput setaf 1):$(tput sgr0)${branch:-unknown}$(tput setaf 1)] "
        fi
    else
        GIT_PS1=
    fi
}

function ssh_ps1() {
    if [ -n "$SSH_CONNECTION" ]; then
        SSH_PS1="[$(tput sgr0)ssh$(tput setaf 1)] "
    else
        SSH_PS1=
    fi
}

function container_ps1() {
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

# Start TMUX session automatically
if [ -n "$(command -v tmux)" ] && [ -z "$TMUX" ] && [ -z "$VTERM" ]; then
    tmux
fi
