
" ╭─────────────╥──────────────────╮
" │ Author:     ║ File:            │
" │ Andrey Orst ║ functions.vim    │
" ╞═════════════╩══════════════════╡
" │ Rest of .dotfiles:             │
" │ GitHub.com/andreyorst/dotfiles │
" ╰────────────────────────────────╯

" Delete all trailing spaces
	function! RemoveTrailingSpaces()
		let l:win_view = winsaveview()
		let l:save_slash = getreg('/')
		execute 'keepjumps%s:\s\+$::ge'
		call histdel("/", -1)
		call winrestview(l:win_view)
		call setreg('/', l:save_slash)
	endfunction

" Terminal Function
	let s:term_buf = 0
	let s:term_win = 0

	function! TermToggle(height)
		if win_gotoid(s:term_win)
			hide
		else
			new terminal
			exec "resize ".a:height
			try
				exec "buffer ".s:term_buf
				exec "bd terminal"
			catch
				call termopen($SHELL, {"detach": 0})
				let s:term_buf = bufnr("")
				setlocal nonumber
				setlocal norelativenumber
				setlocal signcolumn=no
				setlocal nocursorline
			endtry
			startinsert!
			let s:term_win = win_getid()
		endif
	endfunction

" Rename word under cursor in whole file
	function! RenameCWord(cword)
		let l:cursor_pos = getpos(".")
		let l:word = expand("<".a:cword.">")
		let l:rename = input('Rename: ', l:word)
		if l:rename != ''
			if a:cword == "cword"
				execute "%s/\\<".l:word."\\>/".l:rename."/g"
			elseif a:cword == "cWORD"
				let l:word = escape(l:word, '/')
				execute "%s/".l:word."/".l:rename."/g"
			endif
		endif
		call cursor(l:cursor_pos[1], l:cursor_pos[2])
	endfunction

" Visual Selection Macro
	function! ExecuteMacroOverVisualRange()
		echo "@".getcmdline()
		execute ":'<,'>normal @".nr2char(getchar())
	endfunction

" Search upwards for file, and return its path
	function! FindProjectRootByFile(filename)
		let l:path = getcwd()
		while l:path != ''
			if filereadable(l:path.'/'.a:filename)
				return l:path
			else
				let l:path = substitute(l:path, '\v(.*)\/.*', '\1', 'g')
			endif
		endwhile
		return -1
	endfunction

" Check if we in termux
	function! IsTermux()
		return match($PATH, "termux") >= 0
	endfunction

" Add syntax blocks
	function! DefineSyntaxRegion(filetype, start, end, Highlight) abort
		let l:ft = toupper(a:filetype)
		if exists('b:current_syntax')
			let l:current_syntax = b:current_syntax
			unlet b:current_syntax
		endif
		try
			execute 'syntax include @'.l:ft.' syntax/'.a:filetype.'.vim'
		catch
		endtry
		try
			execute 'syntax include @'.l:ft.' after/syntax/'.a:filetype.'.vim'
		catch
		endtry
		if exists('l:current_syntax')
			let b:current_syntax = l:current_syntax
		else
			unlet b:current_syntax
		endif
		execute 'syntax region Region'.l:ft.'
					\ matchgroup='.a:Highlight.'
					\ keepend
					\ start="'.a:start.'" end="'.a:end.'"
					\ contains=@'.l:ft
	endfunction

	" Highlights word under cursor, and all occurences
	function! HlUnderCursor()
		let l:ignore = ["Statement", "Type", "PreProc", "Keyword", "StorageClass", "Repeat", "Label", "Conditional"]
		let l:syntaxgroup = GetHlGroupName()
		if !exists("s:HlUnderCursor")
			if index(l:ignore, l:syntaxgroup) == -1
				silent! exe printf('match Visual /\V\<%s\>/', escape(expand('<cword>'), '/\'))
			else
				exe 'match Visual /\V\<\>/'
			endif
		endif
	endfunction

	function! GetHlGroupName()
		return synIDattr(synIDtrans(synID(line("."), stridx(getline("."), expand('<cword>')) + 1, 1)), "name")
	endfunction

	function! ToggleCursorWordHl()
		if !exists("s:HlUnderCursor")
			let s:HlUnderCursor = 1
			exe 'match Visual /\V\<\>/'
		else
			unlet s:HlUnderCursor
		endif
	endfunction
