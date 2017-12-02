" Map Settings

	" Toggle terminal on/off
		nnoremap <A-t> :call Term_toggle(12)<cr>
		inoremap <A-t> <Esc>:call Term_toggle(12)<cr>
		tnoremap <A-t> <C-\><C-n>:call Term_toggle(12)<cr>

	" Terminal go back to normal mode
		:tnoremap <Esc> <C-\><C-n>
		:tnoremap :q! <C-\><C-n>:q!<CR>

	" visual mode from insert mode
		inoremap <C-v> <Esc>l<C-v>

	" Tagbar
		noremap <A-b> :TagbarToggle<CR>
	
	" NERDTree
		noremap <A-n> :NERDTreeToggle<CR>
