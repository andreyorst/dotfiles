
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
                setlocal nonu nornu scl=no nocul
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
		while l:path != $HOME
			let l:res = findfile(a:filename, l:path.'/**')
			if l:res != ''
				let l:res = substitute(l:res, '\v(.*)\/.*', '\1', &gd ? 'gg' : 'g')
				if match(l:res, l:path) == 0
					return fnameescape(l:res)
				else
					return fnameescape(l:path.'/'.l:res)
				endif
			else
				let l:path = substitute(l:path, '\v(.*)\/.*', '\1', &gd ? 'gg' : 'g')
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

