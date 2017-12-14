" Common Settings
	set encoding=utf-8

	set mouse=a

	set number

	syntax on

	set path+=**
	set wildmenu
	set termguicolors

	set updatetime=350

	set signcolumn=yes

" Folds
	set foldmethod=indent
	set foldlevelstart=20
	hi Folded ctermfg=black
	hi Folded ctermbg=white

	set wrap
	set linebreak

" Tabs, trailing spaces
	set listchars=tab:▏\ ,eol:\ ,extends:,precedes:,space:\ ,trail:⋅
	set list

" Splits
	set noequalalways
	set splitright
	set splitbelow

" Tabs
	set noexpandtab
	set tabstop=4
	set shiftwidth=4
	set autoindent
	set smartindent

" Netrw
	let g:netrw_banner=0
	let g:netrw_liststyle = 3
