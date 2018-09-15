
" ╭─────────────╥──────────────────╮
" │ Author:     ║ File:            │
" │ Andrey Orst ║ languages.vim    │
" ╞═════════════╩══════════════════╡
" │ Rest of .dotfiles:             │
" │ GitHub.com/andreyorst/dotfiles │
" ╰────────────────────────────────╯

augroup Cpp
	autocmd!
	" Highlightings for C/C++ types and struct/class members.
	autocmd FileType c,cpp,h,hpp
		\ syntax match Type "\v<\w+_t>"                                  |
		\ syntax match Type "\v<__signed>"                               |
		\ syntax match Type "\v<(v|u|vu)\w+(8|16|32|64)>"                |
		\ syntax match Type "\v<(v|u|vu)?(_|__)?(int|short|char)>"       |
		\ syntax match Type "\v<(v)?(_|__)?(s|u)(8|16|32|64)>"           |
		\ syntax match Child "\v(-\>|\.)@<=(\s+)?\w+"                    |
		\ syntax match Function "\v(-\>|\.)@<=(\s+)?\w+(\s+)?(\(.*\))@=" |
augroup end

augroup Markdown
	autocmd!
	autocmd FileType markdown
		\ setlocal spell expandtab colorcolumn=80 tw=80                       |
		\ call DefineSyntaxRegion(   'c',    '```c', '```', 'SpecialComment') |
		\ call DefineSyntaxRegion( 'cpp',  '```cpp', '```', 'SpecialComment') |
		\ call DefineSyntaxRegion( 'vim',  '```vim', '```', 'SpecialComment') |
		\ call DefineSyntaxRegion(  'sh',   '```sh', '```', 'SpecialComment') |
		\ call DefineSyntaxRegion('rust', '```rust', '```', 'SpecialComment') |
		\ autocmd! Trailing                                                   |
augroup end

augroup Vimdoc
	autocmd!
	autocmd FileType help
		\ setlocal spell     |
		\ setlocal expandtab |
augroup end

augroup Rust
	autocmd!
	autocmd FileType rust
		\ set listchars+=space:⋅                           |
		\ highlight WhiteSpaceBol guifg=#665c54 guibg=NONE |
		\ highlight WhiteSpaceMol guifg=#32302f guibg=NONE |
		\ match WhiteSpaceMol / /                          |
		\ 2match WhiteSpaceBol /^ \+/                      |
augroup end
