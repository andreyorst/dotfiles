# tmux tricks
    define-command init-tmux -hidden %{ nop %sh{
        command tmux set-option -g status-position top
        command tmux set-option -g status-style "bg=#3c3836"
        command tmux set -g pane-border-style "fg=#3b3735"
        command tmux set -ga pane-border-style "bg=#3c3836"
        command tmux set -g pane-active-border-style "fg=#665c54"
        command tmux set -ga pane-active-border-style "bg=#3c3836"
        command tmux set -g status-left "#[fg=#3c3836]#[bg=#83a598] tabs #[fg=#83a598]#[bg=#3c3836]"
        command tmux set -g status-right "#[fg=#83a598]#[bg=#3c3836]#[fg=#3c3836]#[bg=#83a598] #{session_name} "
        command tmux setw -g window-status-format ' #[fg=colour7]#[bg=#3c3836]#{window_panes} #(echo "#W")'
        command tmux setw -g window-status-current-format '#[fg=#3c3836]#[bg=#83a598]#[fg=#3c3836]#[bg=#83a598] #{window_panes} #(echo "#W") #[fg=#83a598]#[bg=#3c3836]'
    }}
    define-command restore-tmux %{nop %sh{
        command tmux set -g status-left ""
        command tmux set-option -g status-style ""
        command tmux set -g pane-border-style "fg=#3b3735"
        command tmux set -ga pane-border-style "bg=default"
        command tmux set -g pane-active-border-style "fg=#665c54"
        command tmux set -ga pane-active-border-style "bg=default"
        command tmux setw -g window-status-format '#[fg=colour8] #(echo "#W") '
        command tmux setw -g window-status-current-format '#[fg=colour7] #(echo "#W") '
        command tmux set -g status-right "#{session_name} "
        command tmux rename-window zsh
        command tmux set-option -g status-position bottom
    }}
    define-command rename-tmux -hidden %{nop %sh{ command tmux rename-window "${kak_bufname##*/}" }}

    hook -group tmux global WinDisplay .* %{rename-tmux}
    hook -group tmux global KakBegin .* %{init-tmux}
    hook -group tmux global KakEnd .* %{restore-tmux}

# Aliases
    alias global vert tmux-new-vertical
    alias global vertical tmux-new-vertical
    alias global win tmux-new-window
    alias global window tmux-new-window
