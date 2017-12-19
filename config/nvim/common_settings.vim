" Common Settings
	set encoding=utf-8
	set mouse=a
	set number
	syntax on
	set path+=**
	set wildmenu
	set termguicolors
	colorscheme base16-tomorrow-night
	set updatetime=350
	set signcolumn=yes
	set wrap
	set linebreak

" Folds
	set foldmethod=indent
	set foldlevelstart=20
	hi Folded ctermfg=black
	hi Folded ctermbg=white

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
	let g:netrw_banner = 0
	let g:netrw_liststyle = 3

" Highlights
	highlight EndOfBuffer guifg=#1D1F21
	highlight ALEErrorSign guibg=#282a2e guifg=#cc6666
