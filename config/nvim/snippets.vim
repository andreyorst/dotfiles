" Snippets
	" Creates empty class template with public constructor and
	" virtual destructor, and empty private section
	" usage: in insert mode type class and press / twice e.g class//
	" It will expand class in that line and search for template _Class_Name_
	" in the class, and promt you a :%s///g command where you can type a class name
	" so it could be set automatically
	iabbr class/ <Esc>:-1read $HOME/.vim/snippets/class<CR><Esc>/_Class_Name_<CR>:noh<CR>:%s//g<left><left>

	" Generates getter and setter for C++ private class items.
	" Input  format: type name; e.g. unsigned int* name;
	" Output format: type obtainName() {return name;}
	"                void establishName(type Name) {name = Name;}
	" For example: unsigned char* name; will produce:
	" unsigned char* obtainName() {return name;}
	" void establishName(unsigned char* Name) {name = Name;}
	" just above private: keyword
	" WARNING: must be used below private: keyword
	nnoremap ,gen<Tab> <Esc>0:set nohlsearch<CR>/;<CR>y^:silent!?private<CR>:-1read $HOME/.vim/snippets/obtain<CR>0Pa()<Esc>bbyw~hiobtain<Esc>/;<CR>P:noh<CR>>>j0>>/)<CR>bPnbb~hiestablish<Esc>nPnb~/ =<CR>P/;<CR>Pnb~?obtain<CR>y^j/(<CR>p^:set hlsearch<CR>:noh<CR>

	" bunch of for(;;) {} snippets.
	" for/ acts like a class snippet and lets you to define iterator name
	iabbr for/ for(_Iterator_ = 0; _Iterator_ <; _Iterator_++) {}<Esc>/_Iterator_<CR>:noh<CR>:%s//g<left><left>
	" fori and forj generates a simple for cycle and puts cursor to position where amount is being set
	iabbr fori for(i = 0; i <; i++) {}<Esc>8hi
	iabbr forj for(j = 0; j <; j++) {}<Esc>8hi

	" Simple main() snip
	nnoremap ,main<Tab> <Esc>:-1read $HOME/.vim/snippets/main<CR>2ji<Tab>


