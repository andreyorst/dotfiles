export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="classyTouch/classyTouch"

# CASE_SENSITIVE="true"
# HYPHEN_INSENSITIVE="true"
# DISABLE_AUTO_UPDATE="true"
# export UPDATE_ZSH_DAYS=13
# DISABLE_LS_COLORS="true"
# DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
# HIST_STAMPS="mm/dd/yyyy"

# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
  git
)

source $ZSH/oh-my-zsh.sh

alias vimdiff="nvim -d"
alias zshconf="nvim ~/.zshrc"
alias vimconf="nvim ~/.config/nvim/init.vim"
alias neofetch="clear && echo && neofetch --ascii_distro mac --disable DE WM Theme Icons Shell GPU Resolution  --color_blocks --underline_char '─' --cpu_cores --os_arch"
alias stopwatch='while true; do echo -ne "\r$(date +%-M:%S:%N)"; done'

export EDITOR="nvim -u NORC"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"
