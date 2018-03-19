" Airline
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

" ALE
	let g:airline#extensions#ale#enabled = 1
	let g:ale_lint_delay = 350

	highlight ALEErrorSign guibg=#282a2e guifg=#cc6666
	let g:ale_sign_error = '⬥ '
	let g:ale_sign_warning = '⬥ '

	let g:ale_linters = {
		\ 'c': ['clang', 'gcc'],
		\ 'cpp': ['clang'],
		\ 'rust': ['rls']
	\}

	" C/C++
		let g:ale_cpp_clang_options = '-Wall --std=c++11 '
		let g:ale_c_clang_options = '-Wall --std=c99 '
		let g:ale_c_gcc_options = '-Wall --std=c99 '

		let g:includepath = ''

		if filereadable("./testkit.settings")
			let g:includepath = system('echo -n
						\ -I $(pwd)/include
						\ -I $(pwd)/testpacks/SPW_TESTS/spw_lib_src
						\ -I $(pwd)/testpacks/CAN/can_lib_src
						\ -I $(pwd)/platforms/$(cat ./testkit.settings | grep "?=" |  sed -E "s/.*= //")/include
						\')
		elseif filereadable("./main.c")
			let g:includepath = system('echo -n
						\ -I $(pwd)/include
						\ -I $(pwd)/include/cp2
						\ -I $(pwd)/include/hdrtest
						\ -I $(pwd)../../include
						\')
		endif

		let g:ale_c_clang_options.= g:includepath
		let g:ale_c_gcc_options.= g:includepath

" DelimitMate
	let delimitMate_expand_cr = 1
	let delimitMate_expand_space = 0
	let delimitMate_nesting_quotes = ['`']

" Deoplete
	" set completeopt-=preview
	" let g:deoplete#enable_at_startup = 1
	" inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" Deoplete Clang
	" let g:deoplete#sources#clang#libclang_path='/usr/lib/libclang.so'
	" let g:deoplete#sources#clang#clang_header='/lib/clang/'

	" if filereadable("./testkit.settings") || filereadable("./main.c")
	" 	redir! > ./.clang
	" 	silent! echon 'flags = ' g:includepath
	" 	redir END
	" endif

" LanguageClient-neovim
	autocmd FileType rust nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>

	let g:LanguageClient_serverCommands = {
		\ 'rust': ['rustup', 'run', 'nightly', 'rls'],
	\ }

" NERDTree
	autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Tagbar
	let g:tagbar_sort = 0
	let g:tagbar_compact = 1
	autocmd FileType c,cpp nested :TagbarToggle

" NCM
	set shortmess+=c
	let g:cm_matcher = {'module': 'cm_matchers.abbrev_matcher', 'case': 'smartcase'}
	let g:cm_refresh_length = 2
	inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
	inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"


	let g:cm_sources_override = {
    \ 'cm-tags': {'enable':0}
    \ }

" NCM Clang
	if filereadable("./testkit.settings") || filereadable("./main.c")
		redir! > ./.clang_complte
		silent! echon 'flags = ' g:includepath
		redir END
	endif

" Indent Guides
	let g:indentguides_spacechar = '▏'
	let g:indentguides_tabchar = '▏'

" Ultisnips
	let g:UltiSnipsEditSplit="vertical"
	let g:UltiSnipsExpandTrigger        = "<Plug>(ultisnips_expand)"
	let g:UltiSnipsJumpForwardTrigger   = "<c-j>"
	let g:UltiSnipsJumpBackwardTrigger  = "<c-k>"
	let g:UltiSnipsRemoveSelectModeMappings = 0
	" optional
	inoremap <silent> <c-u> <c-r>=cm#sources#ultisnips#trigger_or_popup("\<Plug>(ultisnips_expand)")<cr>
	let g:UltiSnipsSnippetsDir = "~/.vim/snippets/UltiSnips"
