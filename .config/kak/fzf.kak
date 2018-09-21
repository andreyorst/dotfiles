declare-user-mode fzf

map global normal <c-p> ':enter-user-mode fzf<ret>'

map global fzf -docstring "open file" f '<esc>:fzf-edit<ret>'
map global fzf -docstring "open buffer" b '<esc>:fzf-buffer<ret>'

define-command -docstring 'Invoke fzf to open a file' -params 0 fzf-edit %{
    evaluate-commands %sh{
        if [ -z "${kak_client_env_TMUX}" ]; then
            printf 'fail "client was not started under tmux"\n'
        else
            file="$(find . \( -path '*/.svn*' -o -path '*/.git*' \) -prune -o -print | TMUX="${kak_client_env_TMUX}" fzf-tmux -d 15)"
            if [ -n "$file" ]; then
                printf 'edit "%s"\n' "$file"
            fi
        fi
    }
}

define-command -docstring 'Invoke fzf to switch buffers' -params 0 fzf-buffer %{
    evaluate-commands %sh{
        if [ -z "${kak_client_env_TMUX}" ]; then
            printf 'fail "client was not started under tmux"\n'
        else
            buffers=
            for buffer in $kak_buflist; do
                buffer="${buffer%\'}"; buffer="${buffer#\'}"
                buffers=$buffers"$buffer\n"
            done
                buffer="$(echo -n $buffers | TMUX="${kak_client_env_TMUX}" fzf-tmux -d 15)"
            if [ -n "$buffer" ]; then
                buffer="${buffer%\'}"; buffer="${buffer#\'}"
                echo "buffer $buffer"
            fi
        fi
    }
}

