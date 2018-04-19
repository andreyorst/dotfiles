" Functions
	let g:ph_contents = []
	let g:ph_types = []
	let g:ph_amount = 0
	let g:jumped_ph = 0
	let g:active = 0
	let g:snip_start = 0
	let g:snip_end = 0
	let g:snippet_line_count = 0
	let g:currently_edited_file = ''
	let g:snip_search_path = $HOME . '/.vim/snippets/'

	function! IsExpandable()
		let l:filetype = &ft
		let l:snip = expand("<cWORD>")
		if filereadable(g:snip_search_path . l:filetype . '/' . l:snip)
			return 1
		elseif filereadable(g:snip_search_path . 'all/' . l:snip)
			return 1
		else
			return 0
		endif
	endfunction

	function! IsActive()
		if g:active == 1
			return 1
		else
			return 0
		endif
	endfunction

	function! IsExpandableInsert()
		let l:col = col('.') - 1
		let l:snip = matchstr(getline('.'), '\v\w+%' . l:col . 'c.')
		let l:filetype = &ft
		if filereadable($HOME.'/.vim/snippets/' . l:filetype . '/' . l:snip)
			return 1
		elseif filereadable(g:snip_search_path . 'all/' . l:snip)
			return 1
		else
			return 0
		endif
	endfunction

	function! IsJumpable()
		if IsInside()
			if IsActive()
				return 1
			endif
		endif
		let g:active = 0
		return 0
	endfunction

	function! IsInside()
		if IsActive()
			if g:currently_edited_file == @%
				if line(".") >= g:snip_start && line(".") <= g:snip_end
					return 1
				else
					return 0
				endif
			else
				let g:active = 0
			endif
		endif
		let g:active = 0
		return 0
	endfunction

	function! ExpandOrJump()
		if IsExpandable()
			return ExpandSnippet()
		elseif IsInside()
			return Jump()
		else
	endfunction

	function! ExpandSnippet()
		let l:snip = expand("<cword>")
		if IsExpandable()
			let l:filetype = GetFileType(l:snip)
			let a:path = g:snip_search_path . l:filetype . '/' . l:snip
			normal diw
			let g:snippet_line_count = 0
			for i in readfile(a:path)
				let g:snippet_line_count +=1
			endfor
			silent exec ':read' . a:path
			silent exec "normal! i\<Bs>"
			silent exec 'normal V' . g:snippet_line_count . 'j='
			silent call ParseAndInitPlaceholders()
			call Jump()
		else
			echo '[ERROR] No "' . l:snip . '" snippet in ' . g:snip_search_path . &ft . '/'
		endif
	endfunction

	function! GetFileType(snip)
		let l:filetype = &ft
		if filereadable(g:snip_search_path . l:filetype . '/' . a:snip)
			return l:filetype
		elseif filereadable(g:snip_search_path . 'all/' . a:snip)
			return 'all'
		else
			echo "[ERROR] can't find snippet"
			return -1
		endif
	endfunction

	function! ParseAndInitPlaceholders()
		let a:cursor_pos = getpos(".")
		let g:ph_contents = []
		let g:ph_types = []
		let g:active = 1
		let g:jumped_ph = 0
		let g:snippet_end = 0
		let g:currently_edited_file = @%
		let g:ph_amount = CountPlaceholders('\v\$(\{)?[0-9]+(:)?')
		call Parse(g:ph_amount)
		call cursor(a:cursor_pos[1], a:cursor_pos[2])
	endfunction

	function! CountPlaceholders(pattern)
		redir => cnt
		silent exe '%s/' . a:pattern . '//gn'
		redir END
		let l:count = strpart(cnt, 0, stridx(cnt, " "))
		let l:count = substitute(l:count, '\v%^\_s+|\_s+%$', '', 'g')
		return l:count
	endfunction

	function! Parse(amount)
		let l:i = 1
		let l:current = l:i
		let g:snip_start = line(".")
		let g:snip_end = g:snip_start + g:snippet_line_count - 1
		let l:type = 0
		while l:i <= a:amount
			call cursor(g:snip_start, 1)
			if l:i == a:amount
				let l:current = 0
			endif
			call search('\v\$(\{)?' . l:current . '(:)?', 'c')
			let l:type = GetPhType()
			call InitPlaceholder(l:current, l:type)
			let l:i += 1
			let l:current = l:i
		endwhile
	endfunction

	function! GetPhType()
			if match(expand("<cWORD>"), '\v.*\$[0-9]+>') == 0
				return 0
			elseif match(expand("<cWORD>"), '\v.*\$\{[0-9]+:.{-}:\}') == 0
				return 2
			else
				return 1
			endif
	endfunction

	function! InitPlaceholder(current, type)
		call add(g:ph_types, a:type)
		if a:type == '0'
			"short placeholder currently not handled well
			call search('\v(.*)@<=\$' . a:current . '>', 'c')
			call search('\v(.*)@<=\$' . a:current . '>', 'sce')
			exec "normal! v`'c\<esc>"
			"exec "normal! v0\<esc>"
			"call add(g:ph_contents, getline("'<")[getpos("'<")[2]-1:getpos("'>")[2]])
			g:ph_amount -= 1
		elseif a:type == '1'
			" long placeholder
			call add(g:ph_contents, matchstr(
						\ getline('.'), '\v(\$\{'. a:current . ':)@<=.{-}(\})@=')
						\ )
			exe "normal df:f}i\<Del>\<Esc>"
		elseif a:type == '2'
			call add(g:ph_contents, matchstr(
						\ getline('.'), '\v(\$\{'. a:current . ':)@<=.{-}(:\})@=')
						\ )
			exe "normal df:f}i\<Del>\<Bs>\<Esc>"
		endif
	endfunction

	function! Jump()
		if IsInside()
			let l:current_ph = escape(g:ph_contents[g:jumped_ph], '/\*')
			let l:current_jump = g:jumped_ph
			let g:jumped_ph += 1
			if g:jumped_ph == g:ph_amount
				let g:active = 0
				let g:jumped_ph = 0
			endif
			if match(g:ph_types[l:current_jump], '0') == 0
				call EmptyPlaceholder(l:current_ph)
			elseif match(g:ph_types[l:current_jump], '1') == 0
				call NormalPlaceholder(l:current_ph)
			elseif match(g:ph_types[l:current_jump], '2') == 0
				call MirrorPlaceholder(l:current_ph)
			endif
		else
			echo "[WARN]: Can't jump outside of snippet's body"
		endif
	endfunction

	function! JumpSkipAll()
		if IsInside()
			let g:active = 0
			let l:current_ph = escape(g:ph_contents[-1], '/\*')
			if match(g:ph_types[-1], '0') == 0
				call EmptyPlaceholder(l:current_ph)
			elseif match(g:ph_types[-1], '1') == 0
				call NormalPlaceholder(l:current_ph)
			elseif match(g:ph_types[-1], '2') == 0
				call MirrorPlaceholder(l:current_ph)
			endif
			let g:jumped_ph = 0
			let g:active = 0
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
		if ph =~ "\\W"
			echo '[ERROR] Placeholder "'.ph.'"'."can't be mirrored"
		else
			call cursor(g:snip_start, 1)
			call search('\<' .ph .'\>', 'c', g:snip_end)
			let a:cursor_pos = getpos(".")
			redraw
			let l:rename = input('Replace placeholder "'.ph.'" with: ')
			if l:rename != ''
				execute g:snip_start . "," . g:snip_end . "s/\\<" . ph ."\\>/" . l:rename . "/g"
			endif
			call cursor(a:cursor_pos[1], a:cursor_pos[2])
			call Jump()
		endif
	endfunction

	nnoremap <silent><expr><F9> IsExpandable() ? ":call ExpandSnippet()<Cr>" : ":\<Esc>"
	inoremap <silent><expr><Tab> pumvisible() ? "\<c-n>" : IsExpandableInsert() ? "<Esc>:call ExpandSnippet()<Cr>" : IsJumpable() ? "<esc>:call Jump()<Cr>" : "\<Tab>"
	inoremap <silent><expr><S-Tab> pumvisible() ? "\<c-p>" : IsJumpable() ? "<esc>:call JumpSkipAll()<Cr>" : "\<S-Tab>"
	inoremap <silent><expr><Cr> pumvisible() ? IsExpandableInsert() ? "<Esc>:call ExpandSnippet()<Cr>" : IsJumpable() ? "<esc>:call Jump()<Cr>" : "\<Cr>" : "\<Cr>"
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

	function! RenameCWord()
		let a:cursor_pos = getpos(".")
		let l:word = expand("<cword>")
		let l:rename = input('Rename "'.l:word.'" to: ')
		if l:rename != ''
			execute "%s/\\<".l:word."\\>/".l:rename."/g"
		endif
		call cursor(a:cursor_pos[1], a:cursor_pos[2])
	endfunction

	nnoremap <silent><F2> :call RenameCWord()<Cr>
	inoremap <silent><F2> <Esc>:call RenameCWord()<Cr>

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
