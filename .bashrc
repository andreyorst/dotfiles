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
    export PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]] \$GIT_BRANCH_AND_STATUS[\[\e[m\]ssh\[\e[0;31m\]]\n\$(if [[ \$? == 0 ]]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"
else
    export PS1="\[\e[0;31m\]┌─╼[\[\e[m\]\w\[\e[0;31m\]] \$GIT_BRANCH_AND_STATUS\n\$(if [[ \$? == 0 ]]; then echo \"\[\e[0;31m\]└────╼\"; else echo \"\[\e[0;31m\]└╼\"; fi) \[\e[m\]"
fi

# Avoid duplicates
HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

git_ps1_generate() {
    if git rev-parse --is-inside-work-tree 1>/dev/null 2>&1; then
        branch_name=$(git branch 2>/dev/null | grep '^*' | colrm 1 2)
        [ $(git status --porcelain=v1 | wc -l) -gt 0 ] && changes="*" || chages=""
        export GIT_BRANCH_AND_STATUS="[$branch_name$changes] "
    else
        export GIT_BRANCH_AND_STATUS=
    fi
    branch_name=
    changes=
}

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r; git_ps1_generate"
