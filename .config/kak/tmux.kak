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

