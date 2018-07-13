
" ╭─────────────╥──────────────────╮
" │ Author:     ║ File:            │
" │ Andrey Orst ║ languages.vim    │
" ╞═════════════╩══════════════════╡
" │ Rest of .dotfiles:             │
" │ GitHub.com/andreyorst/dotfiles │
" ╰────────────────────────────────╯

let s:clang_format_options = {
	\ "BasedOnStyle": "webkit",
	\ "IndentWidth": 4,
	\ "PointerAlignment": "Left",
	\ "AlignAfterOpenBracket": "DontAlign",
	\ "AlignConsecutiveAssignments": "true",
	\ "AlignConsecutiveDeclarations": "true",
	\ "AlignTrailingComments": "true"
\}

function! s:ParseClangOptions(options)
	let l:tmp = substitute(string(a:options), "'", "", &gdefault ? 'gg' : 'g')
	return "'".l:tmp."'"
endfunction

function! ClangFormat(options) range
	if a:options == ''
		let l:options = s:ParseClangOptions(s:clang_format_options)
	elseif a:options ==? "LLVM" ||
				\ a:options ==? "Google" ||
				\ a:options ==? "Chromium" ||
				\ a:options ==? "Mozilla" ||
				\ a:options ==? "WebKit"
		let l:options = a:options
	else
		let l:options = s:ParseClangOptions(a:options)
	endif
	exec a:firstline.",".a:lastline."!clang-format -style=".l:options
endfunction

augroup Cpp
	autocmd!
	" Highlightings for C/C++ types and struct/class members.
	autocmd FileType c,cpp,h,hpp
		\ syntax match Type "\v<\w+_t>"                                  |
		\ syntax match Type "\v<__signed>"                               |
		\ syntax match Type "\v<(v|u|vu)\w+(8|16|32|64)>"                |
		\ syntax match Type "\v<(v|u|vu)?(_|__)?(int|short|char)>"       |
		\ syntax match Type "\v<(v)?(_|__)?(s|u)(8|16|32|64)>"           |
		\ syntax match Child "\v(-\>|\.)@<=(\s+)?\w+"                    |
		\ syntax match Function "\v(-\>|\.)@<=(\s+)?\w+(\s+)?(\(.*\))@=" |
	" Create custom command to format code with clang-format
	autocmd FileType c,cpp,h,hpp
		\ exec "command! -range=% -nargs=? ClangFormat <line1>,<line2>call ClangFormat('<args>')"
augroup end
