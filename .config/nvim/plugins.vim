" Plugins
	let github = 'https://github.com/'
	let gitlag = 'https://gitlab.com/'

	set rtp+=~/.vim/bundle/Vundle.vim
	call vundle#begin()
	Plugin 'VundleVim/Vundle.vim'

	" Look
		Plugin github.'chriskempson/base16-vim'
		Plugin github.'vim-airline/vim-airline'
		Plugin github.'vim-airline/vim-airline-themes'

	" Tools
		Plugin github.'andreyorst/SimpleSnippets.vim'
		Plugin github.'andreyorst/SimpleSnippets-snippets'
		Plugin github.'craigemery/vim-autotag'
		Plugin github.'justinmk/vim-sneak'
		Plugin github.'Raimondi/delimitMate'
		Plugin github.'Shougo/deoplete.nvim'
		Plugin github.'tpope/vim-surround'

		if IsTermux()
			Plugin github.'w0rp/ale'
			Plugin github.'zchee/deoplete-clang'
		else
			Plugin github.'autozimu/LanguageClient-neovim'
			Plugin github.'Shougo/denite.nvim'
			Plugin github.'scrooloose/nerdtree'
			Plugin github.'majutsushi/tagbar'
			Plugin github.'junegunn/goyo.vim'
		endif

	" Rust
		Plugin github.'rust-lang/rust.vim'

	" C/C++
		Plugin github.'octol/vim-cpp-enhanced-highlight'

	" Syntax Highlighting
		Plugin github.'justinmk/vim-syntax-extra'

	call vundle#end()

filetype plugin indent on

