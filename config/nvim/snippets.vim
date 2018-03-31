" Some Notes About Snippets:
"	There are 4 categories for now:
"	1. Normal mode snippets:
"	   Snippet begins with comma and ends with Tab keypress.
"	   Basically the simplest one out there to use, all you need is to
"	   type ,snippetName and hit Tab to expand it. Cursor will be placed
"	   accordingly to snippet context, and mode will be changed to insert
"
"	2. Insert mode snippets:
"	   Snippets that are just abbreviations, yet a snippet with some cursor
"	   positioning.
"
"	3. Insert mode interactive snippets:
"	   These snippets are abbreviations too, but they end with a '/' symbol.
"	   The complicated part to understand is that these snippets must be ended
"	   with double press of '/' button: first press is part of abbreviation and
"	   the second one is part of ':%s///g' command that is executed at the end.
"	   This command will be used to replace all template names in the snippet to
"	   desired one.
"
"	4. *NEW* snippets with placeholders *NEW*
"	   I bet you never seen such stupid thing in your life. This snippets
"	   contain special markers, that can be jumped at. Markers are listed
"	   below:
"          a) ${1: }    - empty placeholder
"          b) ${1:text} - placeholder containing standard text entry
"
"      When jump preformed with <c-j> (usable from any mode, be careful) the
"      mapping will automatically delete all unnecessary symbols, and will
"      leave you with placeholder text selected in visual mode. You can press
"      `c` to modify it, or jump to next placeholder if you satisfied wit
"      standard placeholder's text. Examples can be found on line 80 or just
"      search for \<" for,\> pattern
"
"	Snippet files are located in ~/.vim/snippets/ and named *.vim

" C Snippets

	" Creates empty class template with public constructor and
	" virtual destructor, and empty private section
	"
	" USAGE: in insert mode type class and press / twice e.g class//
	"
	" It will expand class in that line and search for template _Class_Name_
	" in the class, and promt you a :%s///g command where you can type a class name
	" so it could be set automatically. Cursor is positioned automatically in
	" repace command.
		autocmd FileType cpp,h,hpp iabbr class/ <Esc>:-1read $HOME/.vim/snippets/class.cpp<CR>msf{v%=`s<c-h>

	" Simple empty class and struct snippets
		autocmd FileType cpp,h,hpp iabbr class, class {};<Left><Left><Cr>  <Esc>ddk0f{<left>i
		autocmd FileType c,cpp,h,hpp iabbr struct, struct {};<Left><Left><Cr><Esc>ddk0f{<left>i


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
		autocmd FileType cpp,h,hpp nnoremap <F3> <Esc>0:set nohlsearch<CR>/;<CR>y^?private<CR>:-1read $HOME/.vim/snippets/getSet.cpp<CR>0Pa()<Esc>bbyw~hiobtain<Esc>/;<CR>P:noh<CR>>>j0>>/)<CR>bPnbb~hiestablish<Esc>nPnb~/ =<CR>P/;<CR>Pnb~?obtain<CR>y^j/(<CR>p^:set hlsearch<CR>:noh<CR>

	" Bunch of for(;;) {} snippets.
		" fori and forj generates a simple for cycle and puts cursor to position where amount is being set
			autocmd FileType c,cpp iabbr fori for(int i = 0; i <; i++) {}<left><Cr><Esc>ddk0f<a
			autocmd FileType c,cpp iabbr forj for(int j = 0; j <; j++) {}<left><Cr><Esc>ddk0f<a
		" for, contain placeholders
			autocmd FileType c iabbr tos, tos("${1: }"); ton(${2: }); tos("${3:\n\r}");${0: }<Esc>^
		" for/ acts like a class snippet and lets you to define iterator name
			autocmd FileType c,cpp iabbr for/ <Esc>:-1read ~/.vim/snippets/for.c<Cr>V2j=2k<c-h>

	" Simple main() snip
		autocmd FileType c,cpp iabbr main, <Esc>:-1read $HOME/.vim/snippets/main.c<CR>jA


