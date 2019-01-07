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
    if  [ -n "$kak_client_env_TMUX" ]; then
        printf "%s\n" "alias global vsplit   tmux-new-horizontal"
        printf "%s\n" "alias global vspl     tmux-new-horizontal"
        printf "%s\n" "alias global vert     tmux-new-horizontal"
        printf "%s\n" "alias global vertical tmux-new-horizontal"
        printf "%s\n" "alias global split    tmux-new-vertical"
        printf "%s\n" "alias global spl      tmux-new-vertical"
        printf "%s\n" "alias global tabnew   tmux-new-window"
        printf "%s\n" "alias global tabn     tmux-new-window"
    fi
}

