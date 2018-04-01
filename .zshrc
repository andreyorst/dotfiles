export ZSH=/home/aorst/.oh-my-zsh

ZSH_THEME="classyTouch"

# CASE_SENSITIVE="true"
# HYPHEN_INSENSITIVE="true"
# DISABLE_AUTO_UPDATE="true"
# export UPDATE_ZSH_DAYS=13
# DISABLE_LS_COLORS="true"
# DISABLE_AUTO_TITLE="true"
# ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
# HIST_STAMPS="mm/dd/yyyy"

# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
  git
)

source $ZSH/oh-my-zsh.sh

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

alias vimdiff="nvim -d"
alias zshconf="nvim ~/.zshrc"
alias vimconf="nvim ~/.config/nvim/init.vim"
alias neofetch="clear && echo && neofetch --ascii_distro mac --disable DE WM Theme Icons Shell GPU Resolution  --color_blocks --underline_char '─' --cpu_cores --os_arch"
alias stopwatch='while true; do echo -ne "$(date | sed -E "s/.*12 //" | sed -E "s/ MS.*//"):$(date +%-N)\r"; done'

# Blur konsole
if [[ $(ps --no-header -p $PPID -o comm) =~ '^terminal - |konsole$' ]]; then
        for wid in $(xdotool search --pid $PPID); do
            xprop -f _KDE_NET_WM_BLUR_BEHIND_REGION 32c -set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -id $wid; done
fi
