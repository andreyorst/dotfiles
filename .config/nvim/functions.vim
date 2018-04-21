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
		let l:mode = mode()
		let l:snip = ''
		if l:mode == 'i'
			let l:col = col('.') - 1
			let l:snip = matchstr(getline('.'), '\v\w+%' . l:col . 'c.')
		else
			let l:snip = expand("<cword>")
		endif
		if GetFileType(l:snip) != -1
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
			let g:snippet_line_count = 0
			for i in readfile(a:path)
				let g:snippet_line_count +=1
			endfor
			if g:snippet_line_count != 0
				normal diw
				silent exec ':read' . a:path
				silent exec "normal! i\<Bs>"
				if g:snippet_line_count != 1
					let l:indent_lines = g:snippet_line_count - 1
					silent exec 'normal V' . l:indent_lines . 'j='
				else
					normal ==
				endif
				silent call ParseAndInitPlaceholders()
			else
				echo '[ERROR] Snippet body is empty'
			endif
		else
			echo '[ERROR] No "' . l:snip . '" snippet in ' . g:snip_search_path . &ft . '/'
		endif
	endfunction

	"Checks if snippet availible via current filetype, if not searches in all
	"snippets. If snippet still not found returns -1
	function! GetFileType(snip)
		let l:filetype = FiletypeWrapper()
		if filereadable(g:snip_search_path . l:filetype . '/' . a:snip)
			return l:filetype
		elseif filereadable(g:snip_search_path . 'all/' . a:snip)
			return 'all'
		else
			echo "[ERROR] Can't".' find "'.a:snip.'" snippet in '.g:snip_search_path.l:filetype.'/'.a:snip
			return -1
		endif
	endfunction

	function! FiletypeWrapper()
		let l:ft = &ft
		if l:ft == ''
			return 'all'
		elseif l:ft == 'tex' || l:ft == 'plaintex'
			 return 'tex'
		elseif l:ft == 'sh' || l:ft == 'bash' || l:ft == 'zsh'
			 return 'bash'
		endif
		return l:ft
	endfunction

	function! ParseAndInitPlaceholders()
		let a:cursor_pos = getpos(".")
		let g:ph_contents = []
		let g:ph_types = []
		let g:active = 1
		let g:jumped_ph = 0
		let g:snippet_end = 0
		let g:currently_edited_file = @%
		let g:ph_amount = CountPlaceholders('\v\$(\{)?[0-9]+(:|!|\|)?')
		if g:ph_amount != 0
			call Parse(g:ph_amount)
			call cursor(a:cursor_pos[1], a:cursor_pos[2])
			call Jump()
		else
			let g:active = 0
			call cursor(a:cursor_pos[1], a:cursor_pos[2])
		endif
	endfunction

	function! CountPlaceholders(pattern)
		redir => l:cnt
		silent! exe '%s/' . a:pattern . '//gn'
		redir END
		let l:count = strpart(l:cnt, 0, stridx(l:cnt, " "))
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

	let g:snip_edit_buf = 0
	let g:snip_edit_win = 0

	function! EditSnippet()
		let l:filetype = FiletypeWrapper()
		let l:path = g:snip_search_path . l:filetype
		if !isdirectory(l:path)
			call mkdir(l:path, "p")
		endif
		let l:trigger = input('Select a trigger: ')
		if l:trigger != ''
			if win_gotoid(g:snip_edit_win)
				execute "edit " . l:path . '/' . l:trigger
			else
				vertical new
				try
					exec "buffer " . g:snip_edit_buf
				catch
					execute "edit " . l:path . '/' . l:trigger
					let g:term_buf = bufnr("")
				endtry
				let g:snip_edit_win = win_getid()
			endif
		endif
	endfunction

	command! EditSnippet call EditSnippet()

	function! GetPhType()
			if match(expand("<cWORD>"),'\v.*\$\{[0-9]+:') == 0
				return 1
			elseif match(expand("<cWORD>"), '\v.*\$\{[0-9]+\|') == 0
				return 2
			elseif match(expand("<cWORD>"),'\v.*\$\{[0-9]+!') == 0
				return 3
			endif
	endfunction

	function! InitPlaceholder(current, type)
		call add(g:ph_types, a:type)
		if a:type == 1
			call InitNormalPh(a:current)
		elseif a:type == 2
			call InitMirrorPh(a:current)
		elseif a:type == 3
			call InitShellPh(a:current)
		endif
	endfunction

	function! InitNormalPh(current)
		let l:placeholder = '\v(\$\{'. a:current . ':)@<=.{-}(\})@='
		call add(g:ph_contents, matchstr(getline('.'), l:placeholder))
		exe "normal df:f}i\<Del>\<Esc>"
	endfunction

	function! InitMirrorPh(current)
		let l:placeholder = '\v(\$\{'. a:current . '\|)@<=.{-}(\})@='
		call add(g:ph_contents, matchstr(getline('.'), l:placeholder))
		exe "normal df|f}i\<Del>\<Esc>"
	endfunction

	function! InitShellPh(current)
		let l:placeholder = '\v(\$\{'. a:current . '!)@<=.{-}(\})@='
		let l:command = matchstr(getline('.'), l:placeholder)
		let l:result = system(l:command)
		let l:result = substitute(l:result, '\n\+$', '', '')
		let @s = l:result
		exe "normal df}"
		normal "sp
		call add(g:ph_contents, l:result)
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
			if match(g:ph_types[l:current_jump], '1') == 0
				call NormalPlaceholder(l:current_ph)
			elseif match(g:ph_types[l:current_jump], '2') == 0
				call MirrorPlaceholder(l:current_ph)
			elseif match(g:ph_types[l:current_jump], '3') == 0
				call ShellPlaceholder(l:current_ph)
			endif
		else
			echo "[WARN]: Can't jump outside of snippet's body"
		endif
	endfunction

	function! JumpToLast()
		if IsInside()
			let g:active = 0
			let l:current_ph = escape(g:ph_contents[-1], '/\*')
			if match(g:ph_types[-1], '0') == 0
				call EmptyPlaceholder(l:current_ph)
			elseif match(g:ph_types[-1], '1') == 0
				call NormalPlaceholder(l:current_ph)
			elseif match(g:ph_types[-1], '2') == 0
				call MirrorPlaceholder(l:current_ph)
			elseif match(g:ph_types[-1], '3') == 0
				call ShellPlaceholder(l:current_ph)
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
			redir => l:cnt
			silent! exe g:snip_start.','.g:snip_end.'s/\<' . a:placeholder . '\>//gn'
			redir END
			noh
			let l:count = strpart(l:cnt, 0, stridx(l:cnt, " "))
			let l:count = substitute(l:count, '\v%^\_s+|\_s+%$', '', 'g')
			let l:i = 0
			let l:matchpositions = []
			while l:i < l:count
				call search('\<' .ph .'\>', '', g:snip_end)
				let l:line = line('.')
				let l:start = col('.')
				call search('\<' .ph .'\>', 'ce', g:snip_end)
				let l:length = col('.') - l:start + 1
				call add(l:matchpositions, matchaddpos('Visual', [[l:line, l:start, l:length]]))
				let l:i += 1
			endwhile
			call cursor(g:snip_start, 1)
			call search('\<' .ph .'\>', 'c', g:snip_end)
			let a:cursor_pos = getpos(".")
			redraw
			let l:rename = input('Replace placeholder "'.ph.'" with: ')
			if l:rename != ''
				execute g:snip_start . "," . g:snip_end . "s/\\<" . ph ."\\>/" . l:rename . "/g"
				noh
			endif
			let l:i = 0
			while l:i < l:count
				call matchdelete(l:matchpositions[l:i])
				let l:i += 1
			endwhile
			redraw
			call cursor(a:cursor_pos[1], a:cursor_pos[2])
			call Jump()
		endif
	endfunction

	function! FlashSnippet(snippet_defenition, line_count)
		" Expands snippet that defined by user, for example by iabbr
		" iabbr tag <esc>:call FlashSnippet('Tag N${1:1}: ${0:Name}', 1)<Cr>
			normal diw
			exec "normal a " . a:snippet_defenition
			let g:snippet_line_count = a:line_count
			if g:snippet_line_count != 1
				let l:indent_lines = g:snippet_line_count - 1
				silent exec 'normal V' . l:indent_lines . 'j='
			else
				normal ==
			endif
			silent call ParseAndInitPlaceholders()
			call Jump()
	endfunction

	function! ShellPlaceholder(placeholder)
		call NormalPlaceholder(a:placeholder)
	endfunction

	nnoremap <silent><expr><F9> IsExpandable() ? ":call ExpandSnippet()<Cr>" : ":\<Esc>"
	inoremap <silent><expr><Tab> pumvisible() ? "\<c-n>" : IsExpandable() ? "<Esc>:call ExpandSnippet()<Cr>" : IsJumpable() ? "<esc>:call Jump()<Cr>" : "\<Tab>"
	inoremap <silent><expr><S-Tab> pumvisible() ? "\<c-p>" : IsJumpable() ? "<esc>:call JumpToLast()<Cr>" : "\<S-Tab>"
	inoremap <silent><expr><Cr> pumvisible() ? IsExpandable() ? "<Esc>:call ExpandSnippet()<Cr>" : IsJumpable() ? "<esc>:call Jump()<Cr>" : delimitMate#WithinEmptyPair() ? "\<C-R>=delimitMate#ExpandReturn()\<CR>" : "\<Cr>" : delimitMate#WithinEmptyPair() ? "\<C-R>=delimitMate#ExpandReturn()\<CR>" : "\<Cr>"
	snoremap <silent><expr><Tab> IsExpandable() ? "<Esc>:call ExpandSnippet()<Cr>" : g:active ? "<Esc>:call Jump()<Cr>" : "\<Tab>"
	snoremap <silent><expr><S-Tab> IsJumpable() ? "<Esc>:call JumpToLast()<Cr>" : "\<Tab>"

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
