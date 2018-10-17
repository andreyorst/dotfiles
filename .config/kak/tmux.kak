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
	define-command -override -docstring "rename tmux window to current buffer filename" \
	rename-tmux  %{ nop %sh{ [ -z "$TMUX" ] && exit; tmux rename-window "${kak_bufname##*/}" }}

	hook -group tmux global FocusIn .* %{rename-tmux}
	hook -group tmux global WinDisplay .* %{rename-tmux}

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

