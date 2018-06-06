" Functions
	" Delete all trailing spaces on file open
		function! RemoveTrailingSpaces()
			let l:win_view = winsaveview()
			let l:save_slash = getreg('/')
			execute 'keepjumps%s:\s\+$::ge'
			call histdel("/", -1)
			call winrestview(l:win_view)
			call setreg('/', l:save_slash)
		endfunction

		autocmd BufWritePre *.* :call RemoveTrailingSpaces()

	" Terminal Function
		let s:term_buf = 0
		let s:term_win = 0

		function! TermToggle(height)
			if win_gotoid(s:term_win)
				hide
			else
				new terminal
				exec "resize " . a:height
				try
					exec "buffer " . s:term_buf
					exec "bd terminal"
				catch
					call termopen($SHELL, {"detach": 0})
					let s:term_buf = bufnr("")
					set nonumber
					set norelativenumber
					set signcolumn=no
					set nocursorline
				endtry
				startinsert!
				let s:term_win = win_getid()
			endif
		endfunction

	" Rename word under cursor in whole file
		function! RenameCWord()
			let a:cursor_pos = getpos(".")
			let l:word = expand("<cword>")
			let l:rename = input('Rename "'.l:word.'" to: ')
			if l:rename != ''
				execute "%s/\\<".l:word."\\>/".l:rename."/g"
			endif
			call cursor(a:cursor_pos[1], a:cursor_pos[2])
		endfunction

	" Visual Selection Macro
		function! ExecuteMacroOverVisualRange()
			echo "@".getcmdline()
			execute ":'<,'>normal @".nr2char(getchar())
		endfunction
