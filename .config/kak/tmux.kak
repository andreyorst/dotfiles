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
	define-command -override -docstring "recolor tmux statusline to look like it is part of kakoune, and move it to the top." \
	init-tmux  %{ nop %sh{
		[ -z "$TMUX" ] && exit
		tmux set-option -g status-position top
		tmux set-option -g status-style "bg=#3c3836"
		tmux set -g pane-border-style "fg=#3b3735"
		tmux set -ga pane-border-style "bg=#3c3836"
		tmux set -g pane-active-border-style "fg=#665c54,bold"
		tmux set -ga pane-active-border-style "bg=#3c3836"
		tmux set -g status-left "#[fg=#3c3836,bold]#[bg=#83a598] tabs #[fg=#83a598]#[bg=#3c3836]"
		tmux set -g status-right "#[fg=#83a598]#[bg=#3c3836]#[fg=#3c3836,bold]#[bg=#83a598] #{session_name} "
		tmux setw -g window-status-format ' #[fg=colour7]#[bg=#3c3836]#{window_panes} #(echo "#W")'
		tmux setw -g window-status-current-format '#[fg=#3c3836]#[bg=#83a598]#[fg=#3c3836,bold]#[bg=#83a598] #{window_panes} #(echo "#W") #[fg=#83a598]#[bg=#3c3836]'
		tmux setw -g window-style "bg=#32302f fg=#fbf1c7"
	}}

	define-command -override -docstring "restore tmux statusline look, and move it to the bottom." \
	restore-tmux  %{ nop %sh{
		[ -z "$TMUX" ] && exit
		tmux set -g status-left ""
		tmux set-option -g status-style ""
		tmux set -g pane-border-style "fg=#3b3735"
		tmux set -ga pane-border-style "bg=default"
		tmux set -g pane-active-border-style "fg=#665c54"
		tmux set -ga pane-active-border-style "bg=default"
		tmux setw -g window-status-format '#[fg=colour8] #(echo "#{pane_current_command}")'
		tmux setw -g window-status-current-format '#[fg=colour7] #(echo "#{pane_current_command}")'
		tmux set -g status-right "#{session_name} "
		tmux rename-window zsh
		tmux set-option -g status-position bottom
		tmux setw -g window-style "bg=default fg=default"
	}}

	define-command -override -docstring "rename tmux window to current buffer filename" \
	rename-tmux  %{ nop %sh{ [ -z "$TMUX" ] && exit; tmux rename-window "${kak_bufname##*/}" }}

	define-command -override -docstring "create new horizontal tmux split without forking kakoune" \
	tmux-new-terminal %{ nop %sh{
			tmux split-window
	}}

	hook -group tmux global FocusIn .* %{rename-tmux}
	hook -group tmux global WinDisplay .* %{rename-tmux}
	hook -group tmux global KakBegin .* %{init-tmux}
	hook -group tmux global KakEnd .* %{restore-tmux}

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
		echo "alias global term     tmux-new-terminal"
		# Maps
		echo "unmap global goto t"
		echo "map -docstring 'next tab' global goto t '<esc>: nop %sh{tmux next-window}<ret>'"
		echo "unmap global goto T"
		echo "map -docstring 'previous tab' global goto T '<esc>: nop %sh{tmux previous-window}<ret>'"
	}

