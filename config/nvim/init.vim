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
		" Plugin 'octol/vim-cpp-enhanced-highlight'
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

	set foldmethod=indent
	set foldlevelstart=20
	hi Folded ctermfg=black
	hi Folded ctermbg=white

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
		:tnoremap :q! <C-\><C-n>:q!<CR>

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
				set nonu
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

" ALE
	let g:ale_sign_error = '⬥ '
	let g:ale_sign_warning = '⬥ '
	let g:ale_linters = {
		\   'c': ['gcc', 'clang'],
		\   'cpp': ['clang', 'gcc']
	\}
	let g:ale_c_clang_options='-Wall -I ~/s183/kmdtrunk/include/ -I ~/s183/kmdtrunk/platforms/sw_hub_3/include/'
	let g:ale_c_gcc_options='-Wall -I ~/s183/kmdtrunk/include/ -I ~/s183/kmdtrunk/platforms/sw_hub_3/include/'

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
	" Creates empty class template with public constructor and
	" virtual destructor, and empty private section
	" usage: in insert mode type class and press / twice e.g class//
	" It will expand class in that line and search for template _Class_Name_
	" in the class, and promt you a :%s///g command where you can type a class name
	" so it could be set automatically
	iabbr class/ <Esc>:-1read $HOME/.vim/snippets/class<CR><Esc>/_Class_Name_<CR>:noh<CR>:%s//g<left><left>

	" Generates getter and setter for C++ private class items.
	" Input  format: type name; e.g. unsigned int* name;
	" Output format: type obtainName() {return name;}
	"                void establishName(type Name) {name = Name;}
	" For example: unsigned char* name; will produce:
	" unsigned char* obtainName() {return name;}
	" void establishName(unsigned char* Name) {name = Name;}
	" just above private: keyword
	" WARNING: must be used below private: keyword
	nnoremap ,gen<Tab> <Esc>0:set nohlsearch<CR>/;<CR>y^:silent!?private<CR>:-1read $HOME/.vim/snippets/obtain<CR>0Pa()<Esc>bbyw~hiobtain<Esc>/;<CR>P:noh<CR>>>j0>>/)<CR>bPnbb~hiestablish<Esc>nPnb~/ =<CR>P/;<CR>Pnb~?obtain<CR>y^j/(<CR>p^:set hlsearch<CR>:noh<CR>

	" bunch of for(;;) {} snippets.
	" for/ acts like a class snippet and lets you to define iterator name
	iabbr for/ for(_Iterator_ = 0; _Iterator_ <; _Iterator_++) {}<Esc>/_Iterator_<CR>:noh<CR>:%s//g<left><left>
	" fori and forj generates a simple for cycle and puts cursor to position where amount is being set
	iabbr fori for(i = 0; i <; i++) {}<Esc>8hi
	iabbr forj for(j = 0; j <; j++) {}<Esc>8hi

	" Simple main() snip
	nnoremap ,main<Tab> <Esc>:-1read $HOME/.vim/snippets/main<CR>2ji
