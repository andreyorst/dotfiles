" Functions
	let g:ph_contents = []
	let g:ph_types = []
	let g:ph_amount = 0
	let g:jumped_ph = 0
	let g:active = 0
	let g:snip_start = 0
	let g:snip_end = 0
	let g:snip_search_path = $HOME . '/.vim/snippets/'

	function! IsExpandable()
		let snip = expand("<cWORD>")
		let a:path = g:snip_search_path . &ft . '/' . snip
		if filereadable(a:path)
			return 1
		else
			return 0
		endif
	endfunction

	function! IsExpandableInsert()
		let l:col = col('.') - 1
		let l:res = matchstr(getline('.'), '\v\w+%' . l:col . 'c.')
		if filereadable($HOME.'/.vim/snippets/c/' . l:res)
			return 1
		else
			return 0
		endif
	endfunction

	function! IsJumpable()
		if IsInside()
			if g:active == 1
				return 1
			endif
		endif
		let g:active = 0
		return 0
	endfunction

	function! IsInside()
		if g:active == 1
			if line(".") >= g:snip_start && line(".") <= g:snip_end
				return 1
			else
				return 0
			endif
		endif
		let g:active = 0
		return 0
	endfunction

	function! ExpandOrJump()
		let l:col = max([col('.')-1, 1])
		let l:char = matchstr(getline('.'), '\%' . l:col . 'c.')
		if IsExpandable()
			return ExpandSnippet()
		else
			if IsInside()
				return Jump()
			else
				return "\<Tab>"
	endfunction

	function! ExpandSnippet()
		let snip = expand("<cword>")
		let a:path = g:snip_search_path . &ft . '/' . snip
		if IsExpandable()
			normal diw
			let g:lines = 0
			for i in readfile(a:path)
				let g:lines +=1
			endfor
			silent exec ':read' . a:path
			silent exec "normal! i\<Bs>"
			silent exec 'normal V' . g:lines . 'j='
			silent call ParseAndInitPlaceholders()
			call Jump()
		else
			echo '[ERROR] No "' . snip . '" snippet in ' . g:snip_search_path . &ft . '/'
		endif
	endfunction

	function! ParseAndInitPlaceholders()
		let a:cursor_pos = getpos(".")
		let g:ph_contents = []
		let g:ph_types = []
		let g:active = 1
		let g:jumped_ph = 0
		let g:ph_amount = CountPlaceholders('\v\$(\{)?[0-9]+(:)?')
		call Parse(g:ph_amount)
		call cursor(a:cursor_pos[1], a:cursor_pos[2])
	endfunction

	function! CountPlaceholders(pattern)
		redir => cnt
		silent exe '%s/' . a:pattern . '//gn'
		redir END
		let res = strpart(cnt, 0, stridx(cnt, " "))
		let res = substitute(res, '\v%^\_s+|\_s+%$', '', 'g')
		return res
	endfunction

	function! Parse(amount)
		let i = 1
		let current = i
		let g:snip_start = line(".")
		let g:snip_end = line(".")
		while i <= a:amount
			call cursor(g:snip_start, 1)
			if i == a:amount
				let current = 0
			endif
			call search('\v\$(\{)?' . current . '(:)?', 'c')
			if line(".") > g:snip_end
				let g:snip_end = line(".")
			endif
			if match(expand("<cWORD>"), '\v.*\$[0-9]+>') == 0
				"short placeholder currently not handled well
				call add(g:ph_types, '0')
				call search('\v(.*)@<=\$' . current . '>', 'c')
				call search('\v(.*)@<=\$' . current . '>', 'sce')
				exec "normal! v`'c\<esc>"
				exec "normal! v0\<esc>"
				call add(g:ph_contents, getline("'<")[getpos("'<")[2]-1:getpos("'>")[2]])
			elseif match(expand("<cWORD>"), '\v.*\$\{[0-9]+:.{-}:\}') == 0
				" mirrored ph
				call add(g:ph_types, '2')
				call add(g:ph_contents, matchstr(
							\ getline('.'), '\v(\$\{'. current . ':)@<=.{-}(:\})@=')
							\ )
				exe "normal df:f}i\<Del>\<Bs>\<Esc>"
			else
				" long placeholder
				call add(g:ph_types, '1')
				call add(g:ph_contents, matchstr(
							\ getline('.'), '\v(\$\{'. current . ':)@<=.{-}(\})@=')
							\ )
				exe "normal df:f}i\<Del>\<Esc>"
			endif
			let i += 1
			let current = i
		endwhile
	endfunction
	" ${1:iter}
	" ${2:i:}

	function! Jump()
		if IsInside()
			let current_ph = escape(g:ph_contents[g:jumped_ph], '/\*')
			if match(g:ph_types[g:jumped_ph], '1') == 0
				call NormalPlaceholder(current_ph)
			else
				call EmptyPlaceholder(current_ph)
			endif
			let g:jumped_ph += 1
			if g:jumped_ph == g:ph_amount
				let g:active = 0
				let g:jumped_ph = 0
			endif
		else
			echo "[WARN]: Can't jump outside of snippet's body"
		endif
	endfunction
	function! JumpSkipAll()
		if IsInside()
			let g:active = 0
			let current_ph = escape(g:ph_contents[-1], '/\*')
			if match(g:ph_types[-1], '1') == 0
				call NormalPlaceholder(current_ph)
			else
				call EmptyPlaceholder(current_ph)
			endif
			let g:jumped_ph = 0
		else
			echo "[WARN]: Can't jump outside of snippet's body"
		endif
	endfunction

	function! NormalPlaceholder(placeholder)
		let ph = a:placeholder
		if ph !~ "\\W"
			let ph = '\<' . ph . '\>'
		endif
		call cursor(g:snip_start, 1)
		call search(ph, 'c', g:snip_end)
		normal ms
		call search(ph, 'ce', g:snip_end)
		normal me
		call feedkeys("`sv`e\<c-g>")
	endfunction

	function! EmptyPlaceholder(placeholder)
		call cursor(g:snip_start, 1)
		call search(a:placeholder, 'ce', g:snip_end)
		exec "normal! a"
		exec "normal! \<right>"
	endfunction

	function! MirrorPlaceholder(placeholder)
		let ph = a:placeholder
		if ph !~ "\\W"
			let ph = '\<' . ph . '\>'
		endif
		call cursor(g:snip_start, 1)
		call search(ph, 'c', g:snip_end)
		normal ms
		call search(ph, 'ce', g:snip_end)
		normal me
		execute "normal! " . g:snip_start . "," . g:snip_end . "s/" . ph . "/" . input('placeholder content: ') . "/g"
	endfunction

	inoremap <silent><expr><Tab> pumvisible() ? "\<c-n>" : IsExpandableInsert() ? "<Esc>:call ExpandSnippet()<Cr>" : IsJumpable() ? "<esc>:call Jump()<Cr>" : "\<Tab>"
	inoremap <silent><expr><Cr> pumvisible() ? IsExpandableInsert() ? "<Esc>:call ExpandSnippet()<Cr>" : IsJumpable() ? "<esc>:call Jump()<Cr>" : "\<Cr>"

	snoremap <silent><expr><Tab> IsExpandable() ? "<Esc>:call ExpandSnippet()<Cr>" : g:active ? "<Esc>:call Jump()<Cr>" : "\<Tab>"
	snoremap <silent><expr><S-Tab> IsJumpable() ? "<Esc>:call JumpSkipAll()<Cr>" : "\<Tab>"

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
