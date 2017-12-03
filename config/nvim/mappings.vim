" Map Settings
	" Open file under cursor
		nnoremap gf <C-w>gf

	" Toggle terminal on/off
		nnoremap <A-t> :call Term_toggle(12)<CR>
		inoremap <A-t> <Esc>:call Term_toggle(12)<CR>
		tnoremap <A-t> <C-\><C-n>:call Term_toggle(12)<CR>

	" Toggle netrw on/off
		" noremap <A-s> :call Netrw_toggle()<CR>
		" noremap <A-s> <Esc>:call Netrw_toggle()<CR>

	" Terminal go back to normal mode
		:tnoremap <Esc> <C-\><C-n>
		:tnoremap :q! <C-\><C-n>:q!<CR>

	" visual mode from insert mode
		inoremap <C-v> <Esc>l<C-v>

	" Tagbar
		noremap <A-b> :TagbarToggle<CR>

	" NERDTree
		noremap <A-n> <Esc>:NERDTreeToggle<CR>
