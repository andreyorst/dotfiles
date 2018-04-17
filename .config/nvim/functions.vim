" Functions
	let g:placeholder_contents = []
	let g:placeholders = 0
	let g:jumped = 0
	let g:active = 0
	let g:snip_start_line = 0
	let g:snip_end_line = 0
"	int
"for (int ${2:i} = 0; i < ${1:10}; i${3:++}) {
"	${0:/* expression */}
"}

	function! ParseAndInitPlaceholders()
		let g:placeholder_contents = []
		let g:active = 1
		let g:jumped = 0
		let a:cursor_pos = getpos(".")
		let a:regex = '\v\$(\{)?[0-9]+(:)?'
		let g:placeholders = Count(a:regex)
		call Parse(g:placeholders)
		call cursor(a:cursor_pos[1], a:cursor_pos[2])
	endfunction

	function! Count(word)
		redir => cnt
		silent exe '%s/' . a:word . '//gn'
		redir END
		let res = strpart(cnt, 0, stridx(cnt, " "))
		let res = substitute(res, '\v%^\_s+|\_s+%$', '', 'g')
		return res
	endfunction

	function! Parse(amount)
		let i = 1
		let current = i
		while i <= a:amount
			if i == a:amount
				let current = 0
			endif
			exec '/\v\$(\{)?' . current . '(:)?'
			normal n
			if i == 1
				let g:snip_start_line = line(".")
				let g:snip_end_line = line(".")
			endif
			if line(".") < g:snip_start_line
				let g:snip_start_line = line(".")
			endif
			if line(".") > g:snip_end_line
				let g:snip_end_line = line(".")
			endif
			if match(expand("<cWORD>"), '\v.*\$[0-9]+>') == 0
				"short placeholder
				exe "normal d//e\<Cr>"
				let g:placeholders -= 1
			else
				" long placeholder
				call add(g:placeholder_contents, matchstr(getline('.'), '\v(\$\{'. current . ':)@<=.{-}(\})@='))
				exe "normal df:f}i\<Del>\<Esc>"
			endif
			let i += 1
			let current = i
		endwhile
	endfunction

	function! Jump()
		if line(".") < g:snip_start_line || line(".") > g:snip_end_line
			"let g:active = 0
			echo "not in snippet"
		else
			if g:active != 0
				let current_placeholder = escape(g:placeholder_contents[g:jumped], '/\*')
				if current_placeholder =~#"w"
					echo vaiv
					return
					call cursor(g:snip_start_line, 1)
					call search('\<' . current_placeholder . '\>', '', g:snip_end_line)
					normal ms
					call search('\<' . current_placeholder . '\>', 'ce', g:snip_end_line)
					normal me
				else
					call cursor(g:snip_start_line, 1)
					call search(current_placeholder, '', g:snip_end_line)
					normal ms
					call search(current_placeholder, 'ce', g:snip_end_line)
					normal me
				endif
				call feedkeys("`sv`e\<c-g>")
				let g:jumped += 1
				if g:jumped == g:placeholders
					let g:active = 0
					let g:jumped = 0
				endif
			else
				echo "no jumps left"
			endif
		endif
	endfunction

	inoremap <silent><expr><Tab> g:active ? "<Esc>:call Jump()<Cr>" : "\<Tab>"
	vnoremap <silent><expr><Tab> g:active ? "<Esc>:call Jump()<Cr>" : "\<Tab>"
	snoremap <silent><expr><Tab> g:active ? "<Esc>:call Jump()<Cr>" : "\<Tab>"
	nnoremap <silent><expr><Tab> g:active ? "<Esc>:call Jump()<Cr>" : "\<Tab>"

	" WARNING:
	" Function Prototype to highlight every struct/typedef type for C
	" Need more time to optimize it. Dont use it for now
	" Struggles on huge files.
	" TODO:
	" find a way to get list of every regular expression match without moving
	" cursor or and adding jumps. Find a way not to source it each time, but add
	" only new ones. Possibly should store last amount of items in list
	" somewhere.
		function! HighlightTypes()
			let a:cursor_pos = getpos(".")
			let types = []
			let type_regexes = [
						\ '\v((struct)\s+)@<=\w+',
						\ '\v((typedef).*)@<=\w+(;)@=',
						\ '\v((typedef).*\(\*)@<=\w+(\)\(.*\);)@=',
						\ '\v(typedef struct\_.*)@!(\}\s+)@<=\w+'
						\ ]
			for regexp in type_regexes
				let find_type = 'g/' . regexp . "/call add(types, matchstr(getline('.'),'" . regexp . "'))"
				exec find_type
			endfor
			call cursor(a:cursor_pos[1], a:cursor_pos[2])
			noh
			for s in types
				let syntax = 'syntax match Type "' . s . '"'
				exec syntax
			endfor
		endfunction

		"autocmd FileType c,cpp,h,hpp autocmd InsertLeave * :silent! call HighlightTypes()
		"autocmd FileType c,cpp,h,hpp autocmd BufEnter *  :silent! call HighlightTypes()

	" Delete all trailing spaces on file open
		function! RemoveTrailingSpaces()
			normal! mzHmy
			execute '%s:\s\+$::ge'
			normal! 'yzt`z
		endfunction

		autocmd BufWritePre *.* :call RemoveTrailingSpaces()

	" Terminal Function
		let g:term_buf = 0
		let g:term_win = 0

		function! TermToggle(height)
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
					set nonumber
					set norelativenumber
					set signcolumn=no
				endtry
				startinsert!
				let g:term_win = win_getid()
			endif
		endfunction

	" Encoding
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

	" Visual Selection Macro
		xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

		function! ExecuteMacroOverVisualRange()
			echo "@".getcmdline()
			execute ":'<,'>normal @".nr2char(getchar())
		endfunction
