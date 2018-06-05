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
		Plugin 'andreyorst/SimpleSnippets.vim'
		Plugin 'andreyorst/SimpleSnippets-snippets'
		Plugin 'autozimu/LanguageClient-neovim'
		Plugin 'Chiel92/vim-autoformat'
		Plugin 'craigemery/vim-autotag'
		Plugin 'ctrlpvim/ctrlp.vim'
		Plugin 'junegunn/goyo.vim'
		Plugin 'justinmk/vim-sneak'
		Plugin 'majutsushi/tagbar'
		Plugin 'Raimondi/delimitMate'
		Plugin 'scrooloose/nerdtree'
		Plugin 'Shougo/deoplete.nvim'
		Plugin 'tpope/vim-surround'
		Plugin 'w0rp/ale'

	" Rust
		Plugin 'rust-lang/rust.vim'

	" C/C++
		Plugin 'octol/vim-cpp-enhanced-highlight'
		Plugin 'zchee/deoplete-clang'

	" Syntax Highlighting
		Plugin 'justinmk/vim-syntax-extra'

	call vundle#end()

filetype plugin indent on

" Settings
	source ~/.config/nvim/common_settings.vim
	source ~/.config/nvim/plugin_configs.vim
	source ~/.config/nvim/mappings.vim
	source ~/.config/nvim/functions.vim

