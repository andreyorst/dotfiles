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
		" let g:airline_theme='tomorrow'

" Base 16
	if filereadable(expand("~/.vimrc_background"))
		let base16colorspace=256
		source ~/.vimrc_background
	endif

" Deoplete
	set completeopt-=preview
	let g:deoplete#enable_at_startup = 1

	inoremap <silent><expr> <TAB>
		\ pumvisible() ? "\<C-n>" :
		\ <SID>check_back_space() ? "\<TAB>" :
		\ deoplete#mappings#manual_complete()
	function! s:check_back_space() abort "{{{
		let col = col('.') - 1
		return !col || getline('.')[col - 1]  =~ '\s'
	endfunction"}}}

" Deoplete Clang
	let g:deoplete#sources#clang#libclang_path='/usr/lib/libclang.so'
	let g:deoplete#sources#clang#clang_header='/lib/clang/'

" ALE
	let g:airline#extensions#ale#enabled = 1
	let g:ale_lint_delay = 350
	highlight ALEErrorSign guibg=#252224 guifg=#cc6666
	let g:ale_sign_error = '⬥ '
	let g:ale_sign_warning = '⬥ '
	let g:ale_linters = {
		\   'c': ['gcc', 'clang'],
		\   'cpp': ['clang', 'gcc']
	\}

	" Weird function that generates path to librares for current
	" platform if testkit.settings exists
	" WARNING: you 99.9% don't need this function,
	" delete it if you somehow got this file
	function! GenPlatformIncludes()
		if filereadable("./testkit.settings")
			set shell=/bin/sh
			silent! !echo
				\ let g:ale_c_clang_options=
				\ \'-Wall
				\ -I $(pwd)/include/
				\ -I $(pwd)/testpacks/SPW_TESTS/spw_lib_src/
				\ -I $(pwd)/platforms/$(grep ?=.* ./testkit.settings | awk -F '?= ' '{print $NF}')/include/\'
				\ > ./.ale_local_include_paths
			silent! !echo
				\ let g:ale_c_gcc_options=
				\ \'-Wall
				\ -I $(pwd)/include/
				\ -I $(pwd)/testpacks/SPW_TESTS/spw_lib_src/
				\ -I $(pwd)/platforms/$(grep ?=.* ./testkit.settings | awk -F '?= ' '{print $NF}')/include/\'
				\ >> ./.ale_local_include_paths
			set shell=$SHELL
				if filereadable("./.ale_local_include_paths")
					so ./.ale_local_include_paths
				endif
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
	" let g:tagbar_autofocus = 1
	let g:tagbar_sort = 0
	let g:tagbar_compact = 1
	autocmd FileType c,cpp nested :TagbarToggle
