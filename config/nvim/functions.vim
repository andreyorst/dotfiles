" Functions
	" Highligts all occurrences under cursor
	 	" autocmd CursorMoved * silent! exe printf("match Search /\\<%s\\>/", expand('<cword>'))

	" Delete all trailing spaces on file open
		autocmd BufWritePre *.* :call RemoveTrailingSpaces()

	" Delete trailing spaces (frantsev)
		function! RemoveTrailingSpaces()
			normal! mzHmy
			execute '%s:\s\+$::ge'
			normal! 'yzt`z
		endfunction

" Terminal Function
	let g:term_buf = 0
	let g:term_win = 0

	function! Term_toggle(height)
		if win_gotoid(g:term_win)
			hide
		else
			botright new
			exec "resize " . a:height
			try
				exec "buffer " . g:term_buf
			catch
				call termopen($SHELL, {"detach": 0})
				let g:term_buf = bufnr("")
				set nonu
				set signcolumn=no
			endtry
			startinsert!
			let g:term_win = win_getid()
		endif
	endfunction

" Toggle Netrw
	let g:netrw_win = 0

	function! Netrw_toggle()
		if win_gotoid(g:netrw_win)
			:q
			let g:netrw_win = 0
		else
			:Lexplore 30
			let g:netrw_win = win_getid()
		endif
	endfunction

""  Encoding
	" <F7> EOL format (dos <CR><NL>,unix <NL>,mac <CR>)
		set  wcm=<Tab>
		menu EOL.unix :set fileformat=unix<CR>
		menu EOL.dos  :set fileformat=dos<CR>
		menu EOL.mac  :set fileformat=mac<CR>
		map  <F7> :emenu EOL.<Tab>

	" <F8> Change encoding
		set  wcm=<Tab>
		menu Enc.cp1251  :e! ++enc=cp1251<CR>
		menu Enc.koi8-r  :e! ++enc=koi8-r<CR>
		menu Enc.cp866   :e! ++enc=ibm866<CR>
		menu Enc.utf-8   :e! ++enc=utf-8<CR>
		menu Enc.ucs-2le :e! ++enc=ucs-2le<CR>
		menu Enc.koi8-u  :e! ++enc=koi8-u<CR>
		map  <F8> :emenu Enc.<Tab>

	" <F20> Convert file encoding
		set  wcm=<Tab>
		menu FEnc.cp1251  :set fenc=cp1251<CR>
		menu FEnc.koi8-r  :set fenc=koi8-r<CR>
		menu FEnc.cp866   :set fenc=ibm866<CR>
		menu FEnc.utf-8   :set fenc=utf-8<CR>
		menu FEnc.ucs-2le :set fenc=ucs-2le<CR>
		menu FEnc.koi8-u  :set fenc=koi8-u<CR>
		map  <F20> :emenu FEnc.<Tab>
