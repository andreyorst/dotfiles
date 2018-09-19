# Tmux tricks
    hook -group Tmux global NormalIdle .* %{nop %sh{
        command tmux rename-window ${kak_bufname##*/}
    }}
    hook -group Tmux global KakBegin .* %{nop %sh{
        command tmux set-option -g status-position top
        command tmux set-option -g status-style "bg=#3c3836"
        command tmux set -g status-left "#[fg=#3c3836]#[bg=#83a598] tabs #[fg=#83a598]#[bg=#3c3836]"
        command tmux set -g status-right "#[fg=#83a598]#[bg=#3c3836]#[fg=#3c3836]#[bg=#83a598] #{session_name} "
        command tmux setw -g window-status-format ' #[fg=colour7]#[bg=#3c3836]#(echo "#W")'
        command tmux setw -g window-status-current-format '#[fg=#3c3836]#[bg=#83a598]#[fg=#3c3836]#[bg=#83a598] #(echo "#W") #[fg=#83a598]#[bg=#3c3836]'
    }}
    hook -group Tmux global KakEnd .* %{nop %sh{
        command tmux set -g status-left ""
        command tmux set-option -g status-style ""
        command tmux setw -g window-status-format '#[fg=colour8] #(echo "#W") '
        command tmux setw -g window-status-current-format '#[fg=colour7] #(echo "#W") '
        command tmux set -g status-right "#{session_name} "
        command tmux rename-window zsh
        command tmux set-option -g status-position bottom
    }}
