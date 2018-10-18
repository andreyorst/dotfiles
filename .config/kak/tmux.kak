# ╭─────────────╥───────────────────╮
# │ Author:     ║ File:             │
# │ Andrey Orst ║ tmux.kak          │
# ╞═════════════╩═══════════════════╡
# │ Tmux scripts that change the    │
# │ look of tmux inside kakoune     │
# ╞═════════════════════════════════╡
# │ Rest of .dotfiles:              │
# │ GitHub.com/andreyorst/dotfiles  │
# ╰─────────────────────────────────╯
 
# tmux tricks
    define-command tmux-allow-rename -params 1 %{
        nop %sh{
            if [ $1 = "true" ]; then
                tmux setw -g window-status-format         '#[fg=colour8] #{window_panes} #(echo "#W")'
                tmux setw -g window-status-current-format '#[fg=colour7] #{window_panes} #(echo "#W")'
            else
                tmux setw -g window-status-format         '#[fg=colour8] #{window_panes} #(echo "#{pane_current_command}")'
                tmux setw -g window-status-current-format '#[fg=colour7] #{window_panes} #(echo "#{pane_current_command}")'
            fi
        }
    }
    define-command -override -docstring "rename tmux window to current buffer filename" \
    rename-tmux  %{ tmux-allow-rename 'true'; nop %sh{ [ -z "$TMUX" ] && exit; tmux rename-window "${kak_bufname##*/}" }}

    hook -group tmux global FocusIn .* %{rename-tmux}
    hook -group tmux global WinDisplay .* %{rename-tmux}
    hook -group tmux global KakEnd .* %{tmux-allow-rename 'false'}

    evaluate-commands %sh{
        if  [ -z "$TMUX" ]; then
            echo "nop"
            exit
        fi
        # Aliases
        echo "alias global vsplit   tmux-new-horizontal"
        echo "alias global vspl     tmux-new-horizontal"
        echo "alias global vert     tmux-new-horizontal"
        echo "alias global vertical tmux-new-horizontal"
        echo "alias global split    tmux-new-vertical"
        echo "alias global spl      tmux-new-vertical"
        echo "alias global tabnew   tmux-new-window"
        echo "alias global tabn     tmux-new-window"
        # Maps
        echo "unmap global goto t"
        echo "map -docstring 'next tab' global goto t '<esc>: nop %sh{tmux next-window}<ret>'"
        echo "unmap global goto T"
        echo "map -docstring 'previous tab' global goto T '<esc>: nop %sh{tmux previous-window}<ret>'"
    }

