" AirLine config
	set laststatus=2

	" Bottom row
		if !exists('g:airline_symbols')
			let g:airline_symbols = {}
		endif
		let g:airline_symbols.linenr = '≣'

	" Tabs
		let g:airline#extensions#tabline#enabled = 1
		let g:airline#extensions#tabline#left_sep = ''
		let g:airline#extensions#tabline#left_alt_sep = ''
	" Theme
		let g:airline_powerline_fonts = 1

" Deoplete
	set completeopt-=preview
	let g:deoplete#enable_at_startup = 1

	inoremap <silent><expr> <TAB>
		\ pumvisible() ? "\<C-n>" :
		\ <SID>check_back_space() ? "\<TAB>" :
		\ deoplete#mappings#manual_complete()
	function! s:check_back_space() abort
		let col = col('.') - 1
		return !col || getline('.')[col - 1]  =~ '\s'
	endfunction

" Deoplete Clang
	let g:deoplete#sources#clang#libclang_path='/usr/lib/libclang.so'
	let g:deoplete#sources#clang#clang_header='/lib/clang/'

" ALE
	let g:airline#extensions#ale#enabled = 1
	let g:ale_lint_delay = 350

	highlight ALEErrorSign guibg=#282a2e guifg=#cc6666
	let g:ale_sign_error = '⬥ '
	let g:ale_sign_warning = '⬥ '

	let g:ale_linters = {
		\ 'c': ['gcc', 'clang'],
		\ 'cpp': ['clang', 'gcc'],
		\ 'rust': ['rls']
	\}

	" C/C++
		let g:ale_cpp_clang_options = '-Wall --std=c++11'
		let g:ale_c_clang_options = '-Wall --std=c11'
		let g:ale_c_gcc_options = '-Wall --std=c11'

		" Weird function that generates path to librares for current
		" platform if testkit.settings exists
		" WARNING: you 99.9% don't need this function,
		" delete it if you somehow got this file
		function! GenPlatformIncludes()
			if filereadable("./testkit.settings")
				let g:includepath = system('echo
							\ -I $(pwd)/include/
							\ -I $(pwd)/testpacks/SPW_TESTS/spw_lib_src/
							\ -I $(pwd)/platforms/$(sed -E "s/.*?= //" ./testkit.settings)/include/'
							\)
				let g:ale_c_clang_options.= g:includepath
				let g:ale_c_gcc_options.= g:includepath
			endif
			if filereadable("./main.c")
				let g:includepath = system('echo
							\ -I $(pwd)/include
							\ -I $(pwd)/include/cp2
							\ -I $(pwd)/include/hdrtest
							\ -I $(pwd)../../include
							\)
				let g:ale_c_clang_options.= g:includepath
				let g:ale_c_gcc_options.= g:includepath
			endif
		endfunction

		call GenPlatformIncludes()

" DelimitMate
	let delimitMate_expand_cr = 1
	let delimitMate_expand_space = 0
	let delimitMate_nesting_quotes = ['`']

" NERDTree
	autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Tagbar
	let g:tagbar_sort = 0
	let g:tagbar_compact = 1
	autocmd FileType c,cpp nested :TagbarToggle

" LanguageClient-neovim
	autocmd FileType rust nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>

	let g:LanguageClient_serverCommands = {
		\ 'rust': ['rustup', 'run', 'nightly', 'rls'],
	\ }

