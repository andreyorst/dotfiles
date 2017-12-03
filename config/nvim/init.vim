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
		Plugin 'taglist.vim'
		Plugin 'majutsushi/tagbar'
		Plugin 'scrooloose/nerdtree'
		Plugin 'octol/vim-cpp-enhanced-highlight'
		Plugin 'justinmk/vim-syntax-extra'
		Plugin 'Raimondi/delimitMate'
		Plugin 'Shougo/deoplete.nvim'
		Plugin 'zchee/deoplete-clang'
		Plugin 'w0rp/ale'

	call vundle#end()

" Settings
	source ~/.config/nvim/common_settings.vim
	source ~/.config/nvim/plugin_configs.vim
	source ~/.config/nvim/mappings.vim
	source ~/.config/nvim/functions.vim
	source ~/.config/nvim/snippets.vim

