" Some Notes About Snippets:
"   Snippets are located at `$HOME/.vim/snippets/fyletype/trigger`. So for
"   example `for` snippet for `c` fyletype will be defined in
"   `$HOME/.vim/snippets/c/for`
"   Currently only one type of placeholder is supported, basic jumps are
"   avalible via mappings, and engine has lot of limitations rightnow. Better
"   avoiding placeholders with same text inside one snippet, and avoid using
"   empty placeholders for now.
"
"       a) $1         - empty placeholder, Not supported for now
"       b) ${1:text}  - placeholder containing standard text entry
"       c) ${1:text:} - "mirrored placeholder. See note below
"
"   Mirrored placeholders works not exactly the same like in other plugins,
"   such as UltiSnips. It is a placeholder, wich cotained text will be replaced
"   ower all current snippet. You will be promted in command line to replace
"   your placeholder text like so:
"
"       Replace placeholder "Name" with: |
"
"   Where '|' is your cursor. Pressing <Esc> will return you to insert mode
"   and leaving text unchanged and pressing <Cr> will act the same. If text is
"   changed it will be applied to every match of placeholders content inside
"   snippet body.
"
"   Snippet Example:
"   Simple `for` snippet. Place this at the beginning of snippet file.
"
"       for (int ${2:i} = 0; i < ${1:count}; i++) {
"           ${0:code}
"       }
"
"   It will be expanded and jumped firstly to count marker, then to iterator
"   palceholder, and lastly to code placeholder.

" Getter and setter generation
"
" TODO: Make a function of it
" WARN: This is total mess. Don't try this at home. Requires ninja skills.
"
" Generates getter and setter for C++ private class items.
"
"     Input  Format: type name;
"     Output Format: type obtainName() {return name;}
"                    void establishName(type Name) {name = Name;}
"
" For example: 'unsigned char* letter;' will produce:
" unsigned char* obtainLetter() {return letter;}
" void establishLetter(unsigned char* Letter) {letter = Letter;}
" just above 'private:' keyword.
" You can change function names in this string only
" Search for hiobtain and hiestablish and change
" to higet hiset or to any hi* pattern
" WARNING: must be used *below* 'private' keyword in a class scope
		autocmd FileType cpp,h,hpp nnoremap <F3> <Esc>0:set nohlsearch<CR>/;<CR>y^?private<CR>:-1read $HOME/.vim/snippets/getSet.cpp<CR>0Pa()<Esc>bbyw~hiobtain<Esc>/;<CR>P:noh<CR>==j0==/)<CR>bPnbb~hiestablish<Esc>nPnb~/ =<CR>P/;<CR>Pnb~?obtain<CR>y^j/(<CR>p^:set hlsearch<CR>:noh<CR>

