# ╭─────────────╥───────────────────╮
# │ Author:     ║ File:             │
# │ Andrey Orst ║ plugins.kak       │
# ╞═════════════╩═══════════════════╡
# │ plugins for Kakoune with        │
# │ settings                        │
# ╞═════════════════════════════════╡
# │ Rest of .dotfiles:              │
# │ GitHub.com/andreyorst/dotfiles  │
# ╰─────────────────────────────────╯

# Plugin manager
plug "andreyorst/plug.kak"

# Extended text objects and selections
plug "delapouite/kakoune-text-objects"
plug "occivink/kakoune-vertical-selection"

# Wrapper for GDB
plug "occivink/kakoune-gdb"

# fzf integration
plug "andreyorst/fzf.kak" "branch: fuzzy_buffer_search"
	evaluate-commands %sh{
		[ -z "${kak_opt_plug_loaded_plugins##*fzf.kak*}" ] || exit
		echo "map -docstring 'fzf mode' global normal '<c-p>' ': fzf-mode<ret>'"
		echo 'in-termux "set-option global fzf_tmp /data/data/com.termux/files/usr/tmp/" "nop"'
		echo "set-option global fzf_file_command \"find . \( -path '*/.svn*' -o -path '*/.git*' \) -prune -o -type f -print\""
	}


# automatic pair insertion and surroundig
plug "alexherbo2/auto-pairs.kak"
evaluate-commands %sh{
	[ -z "${kak_opt_plug_loaded_plugins##*auto-pairs.kak*}" ] || exit
	echo "hook global WinCreate .* %{ auto-pairs-enable }"
	echo "map global normal <a-s> ': auto-pairs-surround<ret>'"
}

plug 'andreyorst/SimpleClangFormat.vim'
plug 'andreyorst/SimpleSnippets-snippets'
plug 'andreyorst/SimpleSnippets.vim'
plug 'andreyorst/SimpleWorkspaces.vim'
plug 'justinmk/vim-sneak'
plug 'raimondi/delimitMate'
plug 'shougo/deoplete.nvim'
plug 'tomtom/tcomment_vim'
plug 'tpope/vim-surround'
plug 'wellle/targets.vim'
plug 'w0rp/ale'
plug 'zchee/deoplete-clang'
plug 'autozimu/LanguageClient-neovim'
plug 'majutsushi/tagbar'
plug 'scrooloose/nerdtree'
plug 'shougo/denite.nvim'
plug 'rust-lang/rust.vim'
plug 'justinmk/vim-syntax-extra'
plug 'bfrg/vim-cpp-modern'

