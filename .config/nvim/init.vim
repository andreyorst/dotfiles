" ╭─────────────╥──────────────────╮
" │ Author:     ║ File:            │
" │ Andrey Orst ║ init.vim         │
" ╞═════════════╩══════════════════╡
" │ Rest of .dotfiles:             │
" │ GitHub.com/andreyorst/dotfiles │
" ╰────────────────────────────────╯

set nocompatible
filetype off

let s:config_path = $HOME.'/.config/nvim/'
let s:config_files = [
	\'functions.vim',
	\'plugins.vim',
	\'common_settings.vim',
	\'mappings.vim',
	\'plugin_configs.vim',
\]

for file in s:config_files
	if filereadable(s:config_path.file)
		exec "so ".s:config_path.file
	endif
endfor

