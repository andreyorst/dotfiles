# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ tmux.kak         │
# ╞═════════════╩══════════════════╡
# │ Tmux scripts that adds some    │
# │ new commands to handle splits  │
# │ and handle window names.       │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

# tmux tricks
define-command -override -docstring "rename tmux window to current buffer filename" \
rename-tmux %{ nop %sh{ [ -n "$kak_client_env_TMUX" ] && tmux rename-window "kak: ${kak_bufname##*/}" } }

hook -group tmux global FocusIn .* %{rename-tmux}
hook -group tmux global WinDisplay .* %{rename-tmux}
hook -group tmux global KakEnd .* %{ %sh{ [ -n "$TMUX" ] && tmux rename-window 'zsh' } }

evaluate-commands %sh{
    if  [ -n "$TMUX" ]; then
        printf "%s\n" "define-command vsplit -params .. -command-completion %{ tmux-terminal-horizontal kak -c %val{session} -e \"%arg{@}\" }"
        printf "%s\n" "define-command split -params .. -command-completion %{ tmux-terminal-vertical kak -c %val{session} -e \"%arg{@}\" }"
        printf "%s\n" "define-command tabnew -params .. -command-completion %{ tmux-terminal-window kak -c %val{session} -e \"%arg{@}\" }"
    fi
}

