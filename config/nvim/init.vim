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
		Plugin 'majutsushi/tagbar'
		Plugin 'craigemery/vim-autotag'
		Plugin 'scrooloose/nerdtree'
		Plugin 'Raimondi/delimitMate'
		Plugin 'sirver/UltiSnips'
		Plugin 'roxma/nvim-completion-manager'
		Plugin 'roxma/ncm-clang'
		Plugin 'autozimu/LanguageClient-neovim'
		Plugin 'w0rp/ale'

	" Rust
		Plugin 'rust-lang/rust.vim'

	" C/C++
		Plugin 'zchee/deoplete-clang'
		Plugin 'octol/vim-cpp-enhanced-highlight'

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

