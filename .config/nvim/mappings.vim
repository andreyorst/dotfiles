" Map Settings:
	" Highlights word under cursor by placing it in @/ register
		nnoremap <silent> * :set hlsearch<Cr>:exe "let @/='\\<".expand("<cword>")."\\>'"<Cr>


	" Rename word under cursor in whole document
		nnoremap <silent><F2> :call RenameCWord()<Cr>
		inoremap <silent><F2> <Esc>:call RenameCWord()<Cr>

	" Open file under cursor
		nnoremap gf <C-w>gf

	" Toggle terminal on/off (neovim)
		nnoremap <A-t> :call TermToggle(12)<CR>
		inoremap <A-t> <Esc>:call TermToggle(12)<CR>
		tnoremap <A-t> <C-\><C-n>:call TermToggle(12)<CR>

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

" My own snippets "engine"
	" emulate jump on theese markers
		inoremap <silent><c-j> <Esc>:set nohlsearch<Cr>/\v\$\{[0-9]+:.*\}<Cr>msdf:f}:set hlsearch<Cr>:noh<Cr>i<Del><Esc>me`sv`e<c-g>
		nnoremap <silent><c-j> <Esc>:set nohlsearch<Cr>/\v\$\{[0-9]+:.*\}<Cr>msdf:f}:set hlsearch<Cr>:noh<Cr>i<Del><Esc>me`sv`e<c-g>
		snoremap <silent><c-j> <Esc>:set nohlsearch<Cr>/\v\$\{[0-9]+:.*\}<Cr>msdf:f}:set hlsearch<Cr>:noh<Cr>i<Del><Esc>me`sv`e<c-g>
		cnoremap <silent><c-j> <Esc>:set nohlsearch<Cr>/\v\$\{[0-9]+:.*\}<Cr>msdf:f}:set hlsearch<Cr>:noh<Cr>i<Del><Esc>me`sv`e<c-g>
		" can be used only in self expanded snippets
		inoremap <silent><c-h> <Esc>:set nohlsearch<Cr>/\v\$\{[0-9]+:.*\}<Cr>msdf:f}:set hlsearch<Cr>:noh<Cr>i<Del><Esc>:noh<Cr>:%s/\<<C-r><C-w>\>/g<left><left>
		nnoremap <silent><c-h> <Esc>:set nohlsearch<Cr>/\v\$\{[0-9]+:.*\}<Cr>msdf:f}:set hlsearch<Cr>:noh<Cr>i<Del><Esc>:noh<Cr>:%s/\<<C-r><C-w>\>/g<left><left>
		snoremap <silent><c-h> <Esc>:set nohlsearch<Cr>/\v\$\{[0-9]+:.*\}<Cr>msdf:f}:set hlsearch<Cr>:noh<Cr>i<Del><Esc>:noh<Cr>:%s/\<<C-r><C-w>\>/g<left><left>
