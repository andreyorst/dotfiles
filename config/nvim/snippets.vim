" Some notes about snippets:
" There are 3 categories for now:
	" 1. Normal mode snippets.
	" Snippet begins with comma and ends with Tab keypress.
	" Basically the simplest one out there to use, all you need is to
	" type ,snippetName and hit Tab to expand it. Cursor will be placed
	" accordingly to snippet context, and mode will be changed to insert

	" 2. Insert mode snippets.
	" Snippets that are just abbreviations, yet a snippet with some cursor
	" positioning.

	" 3. Insert mode interactive snippets.
	" These snippets are abbreviations too, but they end with a '/' symbol.
	" The complicated part to understand is that these snippets must be ended
	" with double press of '/' button: first press is part of abbreviation and
	" the second one is part of ':%s///g' command that is executed at the end.
	" This command will be used to replace all template names in the snippet to
	" desired one.

" Snippet files are located in ~/.vim/snippets/

" Snippets

	" Creates empty class template with public constructor and
	" virtual destructor, and empty private section
	" usage: in insert mode type class and press / twice e.g class//
	" It will expand class in that line and search for template _Class_Name_
	" in the class, and promt you a :%s///g command where you can type a class name
	" so it could be set automatically
	iabbr class/ <Esc>:-1read $HOME/.vim/snippets/class.cpp<CR>2j<S-v>j=/_Class_Name_<CR>:noh<CR>:%s//g<left><left>

	" Simple empty class and struct snippets
	iabbr class class {};<Left><Left><Cr>  <Esc>ddk0f{<left>i
	iabbr struct struct {};<Left><Left><Cr><Esc>ddk0f{<left>i

	" This is total mess. Don't try this at home. Requires ninja skills.
	" Generates getter and setter for C++ private class items.
	" Input  format: type name;
	" Output format: type obtainName() {return name;}
	"                void establishName(type Name) {name = Name;}
	" For example: 'unsigned char* letter;' will produce:
	" unsigned char* obtainLetter() {return letter;}
	" void establishLetter(unsigned char* Letter) {letter = Letter;}
	" just above 'private:' keyword
	" You can change function names in this string only
	" Search for hiobtain and hiestablish and change
	" to higet hiset or to any hi* pattern
	" WARNING: must be used below 'private' keyword in a class scope
	nnoremap ,gen<Tab> <Esc>0:set nohlsearch<CR>/;<CR>y^?private<CR>:-1read $HOME/.vim/snippets/getSet.cpp<CR>0Pa()<Esc>bbyw~hiobtain<Esc>/;<CR>P:noh<CR>>>j0>>/)<CR>bPnbb~hiestablish<Esc>nPnb~/ =<CR>P/;<CR>Pnb~?obtain<CR>y^j/(<CR>p^:set hlsearch<CR>:noh<CR>

	" bunch of for(;;) {} snippets.
	" for/ acts like a class snippet and lets you to define iterator name
	iabbr for/ for(_Iterator_ = 0; _Iterator_ <; _Iterator_++) {}<Esc>/_Iterator_<CR>:noh<CR>:%s//g<left><left>
	" fori and forj generates a simple for cycle and puts cursor to position where amount is being set
	iabbr fori for(int i = 0; i <; i++) {}<left><Cr><Esc>ddk0f<a
	iabbr forj for(int j = 0; j <; j++) {}<left><Cr><Esc>ddk0f<a

	" Simple main() snip
	nnoremap ,main<Tab> <Esc>:-1read $HOME/.vim/snippets/main.c<CR>$v%=%2ji<Tab>

