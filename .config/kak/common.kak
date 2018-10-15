# ╭─────────────╥───────────────────╮
# │ Author:     ║ File:             │
# │ Andrey Orst ║ common.kak        │
# ╞═════════════╩═══════════════════╡
# │ Common settings for Kakoune     │
# ╞═════════════════════════════════╡
# │ Rest of .dotfiles:              │
# │ GitHub.com/andreyorst/dotfiles  │
# ╰─────────────────────────────────╯
 
# Common options
	set-option global scrolloff 3,3
	evaluate-commands %sh{
		[ ! -z $(command -v rg) ] && echo "set-option global grepcmd 'rg -L --with-filename --column'"
	}
	set-option global tabstop 4
	set-option global indentwidth 4

# UI
	colorscheme base16-guvbox-dark-soft
	set-option global ui_options ncurses_status_on_top=no ncurses_assistant=dilbert
	set-option global modelinefmt '{rgb:83a598}{rgb:3c3836,rgb:83a598+b} %val{bufname}{{context_info}} {default,rgb:3c3836} {{mode_info}} {rgb:83a598+b}%val{cursor_line}{default}:{rgb:83a598+b}%val{cursor_char_column} {rgb:83a598}{rgb:3c3836,rgb:83a598+b} %opt{filetype} {rgb:3c3836,rgb:83a598}{rgb:83a598} {rgb:83a598,default+b}%val{client}{default} at {magenta,default+b}[%val{session}] '

# Highlighters
	hook global KakBegin .* %{
		add-highlighter global/numbers number-lines -relative -hlcursor
		add-highlighter global/matching show-matching
		add-highlighter global/whitespace show-whitespaces -tab "▏" -lf " " -nbsp "⋅" -spc " "
		add-highlighter global/wrap wrap -word -indent -marker ↪
	}

# Maps and hooks
	map global normal ''     ': comment-line<ret>'                     -docstring "<c-/> to comment/uncomment selection"
	map global normal '<c-r>' 'U'                                       -docstring "vim-like redo"
	map global goto   '<a-f>' '<esc><a-i><a-w>gf'                       -docstring "file non-recursive"
	map global goto   'f'     '<esc><a-i><a-w>: smart-f %reg{dot}<ret>' -docstring "file recursive"
	map global normal '*'     '<a-i>w*'                                 -docstring "search word under cursor"
	map global normal '<a-*>' '<a-i><a-w>*'                             -docstring "search WORD under cursor"
	map global goto   'b'     '<esc>:bn<ret>'                           -docstring "next buffer"
	map global goto   'B'     '<esc>:bp<ret>'                           -docstring "previous buffer"
	map global normal '<c-d>' ': select-or-add-cursor<ret>'             -docstring "add currsor on current word, and jump to the next match"

	hook global InsertCompletionShow .* %{ map   window insert <tab> <c-n>; map   window insert <s-tab> <c-p> }
	hook global InsertCompletionHide .* %{ unmap window insert <tab> <c-n>; unmap window insert <s-tab> <c-p> }

	hook global BufOpenFile .* %{ editorconfig-load }
	hook global BufNewFile .* %{ editorconfig-load }
