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

# Rebar3
[ -d "$HOME/.cache/rebar3" ] && PATH="$HOME/.cache/rebar3/bin:$PATH"

# Lua related stuff
[ -n "$(command -v luarocks)" ] && eval "$(luarocks path)"

export PATH

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
PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]] \$TIME_PC\$SSH_PC\$CONTAINER_PC\$GIT_PC
\$(if [ \$? -eq 0 ]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"

function vterm_printf {
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

function vterm_prompt_end {
    vterm_printf "51;A$(pwd)"
}

if [ -n "$VTERM" ]; then
    PS1=$PS1'\[$(vterm_prompt_end)\]'
fi

PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -n; time_pc; git_pc; ssh_pc; container_pc"

function time_pc {
    TIME_PC="[[m$(date +'%a %H:%M')[31m] "
}

## NO_GIT_PC can be bound in order to skip parsing if repository is
## too big and it takes too long for prompt to appear
function git_pc {
    if test -z "$NO_GIT_PC" && git rev-parse --is-inside-work-tree 1>/dev/null 2>&1; then
        if ! branch=$(git symbolic-ref --short HEAD 2>/dev/null); then
            if ! branch=$(git name-rev HEAD --name-only --no-undefined --tags 2>/dev/null); then
                branch=$(git rev-parse --short HEAD)
            fi
        fi
        if [ -n "$(git status --porcelain 2>/dev/null)" ]; then
            GIT_PC="[[mgit[31m:[m${branch:-unknown}*[31m] "
        else
            GIT_PC="[[mgit[31m:[m${branch:-unknown}[31m] "
        fi
    else
        GIT_PC=
    fi
}

function ssh_pc {
    if [ -n "$SSH_CONNECTION" ]; then
        SSH_PC="[[mssh[31m] "
    else
        SSH_PC=
    fi
}

function container_pc {
    if [ -e /run/.toolboxenv ]; then
        CONTAINER_PC="[[mtoolbox[31m] "
    elif [ -e /run/.containerenv  ]; then
        CONTAINER_PC="[[mpodman[31m] "
    elif [ -e /.dockerenv ]; then
        CONTAINER_PC="[[mdocker[31m] "
    else
        CONTAINER_PC=
    fi
}

export XCURSOR_SIZE=24
export LSP_USE_PLISTS=true
