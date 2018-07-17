
" ╭─────────────╥──────────────────╮
" │ Author:     ║ File:            │
" │ Andrey Orst ║ common.vim       │
" ╞═════════════╩══════════════════╡
" │ Rest of .dotfiles:             │
" │ GitHub.com/andreyorst/dotfiles │
" ╰────────────────────────────────╯

" Common Settings
	filetype plugin indent on
	set encoding=utf-8
	set mouse=a
	set number
	set relativenumber
	set cursorline
	syntax on
	set path+=**
	set wildmenu
	set termguicolors
	colorscheme base16-gruvbox-dark-soft
	set updatetime=350
	set signcolumn=yes
	set wrap
	set linebreak
	set scrolloff=5
	set shortmess+=c
	set lazyredraw
	set hidden
	set inccommand=nosplit

" Folds
	set foldmethod=indent
	set foldlevelstart=20 " Disables automatic closing of all folds on fileopen
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
	set smartindent

" Keymap fixes
	set keymap=russian-jcukenwin
	set iminsert=0
	set imsearch=0

" Spell
	set spelllang=ru_yo,en_us

" Justify text
	runtime macros/justify.vim

" Highlights
	" Common highlights
		highlight EndOfBuffer guifg=#32302f guibg=NONE
		highlight ErrorSign guibg=#3c3836 guifg=#fb4934
		highlight WarningSign guibg=#3c3836 guifg=#fabd2f
		highlight InfoSign guibg=#3c3836 guifg=#8ec07c
		highlight Search guifg=#282a2e
		highlight IncSearch guifg=#282a2e
		highlight Ignore guifg=#969896
		highlight Child guifg=#fb4934 guibg=NONE cterm=bold gui=bold

	" vim-sneak
		highlight Sneak guifg=black guibg=orange

" Autocmds
	augroup Trailing
		autocmd!
		autocmd BufWritePre *.* :call RemoveTrailingSpaces()
	augroup end

	" Enter insert mode when terminal shows up
	augroup Term
		autocmd!
		autocmd TermOpen * startinsert
	augroup end

