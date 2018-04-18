" Some Notes About Snippets:
"   Snippets are located at `$HOME/.vim/snippets/fyletype/trigger`. So for
"   example `for` snippet for `c` fyletype will be defined in
"   `$HOME/.vim/snippets/c/for`
"   Currently only one type of placeholder is supported, basic jumps are
"   avalible via mappings, and engine has lot of limitations rightnow. Better
"   avoiding placeholders with same text inside one snippet, and avoid using
"   empty placeholders for now.
"          a) $1        - empty placeholder (not supported)
"          b) ${1:text} - placeholder containing standard text entry
"   Snippet Example:
"   Simple `for` snippet. Place this at the beginning of snippet file.
"   for (int ${2:i} = 0; i < ${1:count}; i++) {
"       ${0:code}
"   }
"   It will be expanded and jumped firstly to count marker, then to iterator
"   palceholder, and lastly to code placeholder.

	" WARN: This is total mess. Don't try this at home. Requires ninja skills.
	"
	" Generates getter and setter for C++ private class items.
	" Input  Format: type name;
	" Output Format: type obtainName() {return name;}
	"                void establishName(type Name) {name = Name;}
	"
	" For example: 'unsigned char* letter;' will produce:
	" unsigned char* obtainLetter() {return letter;}
	" void establishLetter(unsigned char* Letter) {letter = Letter;}
	"
	" just above 'private:' keyword
	" You can change function names in this string only
	" Search for hiobtain and hiestablish and change
	" to higet hiset or to any hi* pattern
	" WARNING: must be used below 'private' keyword in a class scope
		autocmd FileType cpp,h,hpp nnoremap <F3> <Esc>0:set nohlsearch<CR>/;<CR>y^?private<CR>:-1read $HOME/.vim/snippets/getSet.cpp<CR>0Pa()<Esc>bbyw~hiobtain<Esc>/;<CR>P:noh<CR>==j0==/)<CR>bPnbb~hiestablish<Esc>nPnb~/ =<CR>P/;<CR>Pnb~?obtain<CR>y^j/(<CR>p^:set hlsearch<CR>:noh<CR>

