# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    source /etc/bashrc
fi

# Some defaults
unset LS_COLORS
unset LSCOLORS

if [ -n "$(command -v emacs)" ]; then
    export EDITOR="command emacs -nw"
elif [ -n "$(command -v kak)" ]; then
    export EDITOR="kak"
elif [ -n "$(command -v vim)" ]; then
    export EDITOR="vim"
else
    export EDITOR="vi"
fi

HISTCONTROL=ignoredups:erasedups
HISTIGNORE='ls:ll:cd:pwd:bg:fg:history'
HISTSIZE=100000
HISTFILESIZE=10000000
shopt -s histappend

# User specific environment
PATH="$HOME/.dotfiles/scripts:$PATH"
PATH="$HOME/.local/bin:$PATH"

## Graal
[ -d "$HOME/.graalvm" ] && PATH="$HOME/.graalvm/bin/:$PATH"

## Rebar3
[ -d "$HOME/.cache/rebar3" ] && PATH="$HOME/.cache/rebar3/bin:$PATH"

## Cask

[ -d "$HOME/.cask" ] && PATH="$HOME/.cask/bin:$PATH"

## Lua related stuff
[ -n "$(command -v luarocks)" ] && eval "$(luarocks path)"
for path in "$HOME"/.local/share/lua/*; do LUA_PATH="$path/?.lua;$LUA_PATH"; done
export LUA_PATH

## Finalize PATH building
export PATH

# History
export HISTFILESIZE=
export HISTSIZE=-1
export HISTTIMEFORMAT="[%F %T] "
export HISTIGNORE='ls:ll:cd:pwd:bg:fg:history'


# Prompt (assuming 256 colors)
PS1="\$TIME_PC[36m\w[m\$GIT_PC\$CONTAINER_PC\$SSH_PC
\[\e[27m\]\$(if [ ! \$? -eq 0 ]; then echo [31m; fi)\$[m "

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
    TIME_PC="[38;5;243m$(date +'%a %H:%M') [m"
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
            GIT_PC="[38;5;243m on [35m${branch:-unknown}[m"
        else
            GIT_PC="[38;5;243m on ${branch:-unknown}[m"
        fi
    else
        GIT_PC=
    fi
}

function ssh_pc {
    if [ -n "$SSH_CONNECTION" ]; then
        SSH_PC=" [38;5;243mvia ssh[m"
    else
        SSH_PC=
    fi
}

function container_pc {
    if [ -e /run/.toolboxenv ]; then
        CONTAINER_PC=" [38;5;243min toolbox[m"
    elif [ -e /run/.containerenv  ]; then
        CONTAINER_PC=" [38;5;243min podman[m"
    elif [ -e /.dockerenv ]; then
        CONTAINER_PC=" [38;5;243min docker[m"
    else
        CONTAINER_PC=
    fi
}

export XCURSOR_SIZE=24
export LSP_USE_PLISTS=true

[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion
