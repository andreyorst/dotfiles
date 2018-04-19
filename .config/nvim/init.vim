set nocompatible
filetype off

" Plugins
	set rtp+=~/.vim/bundle/Vundle.vim
	call vundle#begin()
	Plugin 'VundleVim/Vundle.vim'

	" Look
		Plugin 'chriskempson/base16-vim'
		Plugin 'vim-airline/vim-airline'
		Plugin 'vim-airline/vim-airline-themes'

	" Tools
		"Plugin 'autozimu/LanguageClient-neovim'
		"Plugin 'craigemery/vim-autotag'
		"Plugin 'ctrlpvim/ctrlp.vim'
		"Plugin 'majutsushi/tagbar'
		"Plugin 'Raimondi/delimitMate'
		"Plugin 'scrooloose/nerdtree'
		Plugin 'Shougo/deoplete.nvim'
		"Plugin 'sirver/UltiSnips'
		"Plugin 'honza/vim-snippets'
		"Plugin 'w0rp/ale'

	" Rust
		"Plugin 'rust-lang/rust.vim'

	" C/C++
		Plugin 'octol/vim-cpp-enhanced-highlight'
		"Plugin 'zchee/deoplete-clang'

	" Syntax Highlighting
		Plugin 'justinmk/vim-syntax-extra'

	call vundle#end()

filetype plugin indent on

" Settings
	source ~/.config/nvim/common_settings.vim
	source ~/.config/nvim/plugin_configs.vim
	source ~/.config/nvim/mappings.vim
	source ~/.config/nvim/functions.vim
	source ~/.config/nvim/snippets.vim

