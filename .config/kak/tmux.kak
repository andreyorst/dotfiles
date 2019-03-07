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

define-command -override -docstring "rename tmux window to current buffer filename" \
rename-tmux %{ nop %sh{ [ -n "$kak_client_env_TMUX" ] && tmux rename-window "kak: ${kak_bufname##*/}" } }

hook -group tmux global FocusIn .* %{rename-tmux}
hook -group tmux global WinDisplay .* %{rename-tmux}
hook -group tmux global KakEnd .* %{ %sh{ [ -n "$TMUX" ] && tmux rename-window 'zsh' } }

define-command -override -docstring "split tmux vertically" \
vsplit -params .. -command-completion %{
    tmux-terminal-horizontal kak -c %val{session} -e "%arg{@}"
}

define-command -override  -docstring "split tmux horizontally" \
split -params .. -command-completion %{
    tmux-terminal-vertical kak -c %val{session} -e "%arg{@}"
}

define-command -override -docstring "create new tmux window" \
tabnew -params .. -command-completion %{
    tmux-terminal-window kak -c %val{session} -e "%arg{@}"
}

