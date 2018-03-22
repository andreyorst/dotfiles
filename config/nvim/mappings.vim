" Map Settings:
	" Highlights word under cursor by placing it in @/ register
		nnoremap <silent> * :set hlsearch<Cr>:exe "let @/='\\<".expand("<cword>")."\\>'"<Cr>

	" Rename 'symbol'
		nnoremap <F2> :noh<Cr> :%s/\<<C-r><C-w>\>//g<left><left>

	" Open file under cursor
		nnoremap gf <C-w>gf

	" Toggle terminal on/off (neovim)
		nnoremap <A-t> :call Term_toggle(12)<CR>
		inoremap <A-t> <Esc>:call Term_toggle(12)<CR>
		tnoremap <A-t> <C-\><C-n>:call Term_toggle(12)<CR>

	" Terminal go back to normal mode
		tnoremap <Esc> <C-\><C-n>
		tnoremap :q! <C-\><C-n>:q!<CR>

	" Tagbar
		noremap <A-b> <Esc>:TagbarToggle<CR>
		tnoremap <A-b> <C-\><C-n>:TagbarToggle<CR>

	" NERDTree
		noremap <A-n> <Esc>:NERDTreeToggle<CR>
		tnoremap <A-n> <C-\><C-n>:NERDTreeToggle<CR>

" Common fixes
	nnoremap gb :bn<Cr>
	nnoremap gB :bp<Cr>

	nmap <F1> <nop>
	imap <F1> <nop>

	" visual mode from insert mode
	inoremap <C-v> <Esc>l<C-v>

