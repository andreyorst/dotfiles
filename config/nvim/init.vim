set nocompatible
filetype off

" Plugins
	set rtp+=~/.vim/bundle/Vundle.vim
	call vundle#begin()
	Plugin 'VundleVim/Vundle.vim'

	" Style
		Plugin 'chriskempson/base16-vim'
		Plugin 'vim-airline/vim-airline'
		Plugin 'vim-airline/vim-airline-themes'

	" Tools
		Plugin 'octol/vim-cpp-enhanced-highlight'
		Plugin 'justinmk/vim-syntax-extra'
		Plugin 'Raimondi/delimitMate'
		Plugin 'Shougo/deoplete.nvim'
		Plugin 'zchee/deoplete-clang'
		Plugin 'w0rp/ale'

	call vundle#end()

" Common Settings
	set encoding=utf-8

	set mouse=a

	set number

	syntax on

	set path+=**
	set wildmenu

	set foldmethod=syntax
	set foldlevelstart=20
	hi Folded ctermfg=black
	hi Folded ctermbg=white

	set foldmethod=syntax
	set foldlevelstart=20
	hi Folded ctermbg=black

	set cursorline
	set wrap
	set linebreak

	set listchars=tab:▏\ ,eol:\ ,extends:,precedes:,space:\ ,trail:⋅
	set list
	set termguicolors

	set splitright
	set splitbelow
	set signcolumn=yes

" Tabs
	set noexpandtab
	set tabstop=4
	set shiftwidth=4
	set autoindent
	set smartindent

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

" Functions
	" Highligts all occurrences under cursor
	 	" autocmd CursorMoved * silent! exe printf("match Search /\\<%s\\>/", expand('<cword>'))

	" Delete all trailing spaces on file open
		autocmd BufEnter *.* :call RemoveTrailingSpaces()

	" Delete trailing spaces (frantsev)
		function! RemoveTrailingSpaces()
			normal! mzHmy
			execute '%s:\s\+$::ge'
			normal! 'yzt`z
		endfunction


" Map Settings

	" Toggle terminal on/off
		nnoremap <A-t> :call Term_toggle(12)<cr>
		inoremap <A-t> <Esc>:call Term_toggle(12)<cr>
		tnoremap <A-t> <C-\><C-n>:call Term_toggle(12)<cr>

	" Terminal go back to normal mode
		:tnoremap <Esc> <C-\><C-n>

	" visual mode from insert mode
		inoremap <C-v> <Esc>l<C-v>


" Terminal Function
	let g:term_buf = 0
	let g:term_win = 0

	function! Term_toggle(height)
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
			endtry
			startinsert!
			let g:term_win = win_getid()
		endif
	endfunction

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

" DelimitMate
	let delimitMate_expand_cr = 1
	let delimitMate_expand_space = 0
	let delimitMate_nesting_quotes = ['`']

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

" Snippets
nnoremap class<Tab> :-1read $HOME/.vim/snippets/class<CR>/_Class_Name_<CR>:%s///g<left><left>

