" Map Settings:
	" Highlights word under cursor by placing it in @/ register
		nnoremap <silent> * :set hlsearch<Cr>:exe "let @/='\\<".expand("<cword>")."\\>'"<Cr>

	" Rename word under cursor in whole document
		nnoremap <silent><F2> :call RenameCWord()<Cr>
		inoremap <silent><F2> <Esc>:call RenameCWord()<Cr>

	" Open file under cursor
		"nnoremap gf <C-w>gf

	" Toggle terminal on/off (neovim)
		nnoremap <silent><A-t> :call TermToggle(12)<CR>
		inoremap <silent><A-t> <Esc>:call TermToggle(12)<CR>
		tnoremap <silent><A-t> <C-\><C-n>:call TermToggle(12)<CR>

	" Terminal go back to normal mode
		tnoremap <Esc> <C-\><C-n>
		tnoremap :q! <C-\><C-n>:q!<CR>

	" Tagbar
		noremap <A-b> <Esc>:TagbarToggle<CR>
		tnoremap <A-b> <C-\><C-n>:TagbarToggle<CR>

	" NERDTree
		noremap <A-n> <Esc>:NERDTreeToggle<CR>
		tnoremap <A-n> <C-\><C-n>:NERDTreeToggle<CR>

" Common fixes
	nnoremap gb :bn<Cr>
	nnoremap gB :bp<Cr>

	nmap <F1> <nop>
	imap <F1> <nop>

	" visual mode from insert mode
	inoremap <C-v> <Esc>l<C-v>

" Move lines around
	nnoremap <A-j> :m .+1<CR>==
	nnoremap <A-k> :m .-2<CR>==
	inoremap <A-j> <Esc>:m .+1<CR>==gi
	inoremap <A-k> <Esc>:m .-2<CR>==gi
	vnoremap <A-j> :m '>+1<CR>gv=gv
	vnoremap <A-k> :m '<-2<CR>gv=gv

" Visual Selection Macro
	xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

" Encoding
	" <F7> EOL format (dos <CR><NL>,unix <NL>,mac <CR>)
		set  wcm=<Tab>
		menu EOL.unix :set fileformat=unix<CR>
		menu EOL.dos  :set fileformat=dos<CR>
		menu EOL.mac  :set fileformat=mac<CR>
		noremap  <F7> :emenu EOL.<Tab>

	" <F8> Change encoding
		set  wcm=<Tab>
		menu Enc.cp1251  :e! ++enc=cp1251<CR>
		menu Enc.koi8-r  :e! ++enc=koi8-r<CR>
		menu Enc.cp866   :e! ++enc=ibm866<CR>
		menu Enc.utf-8   :e! ++enc=utf-8<CR>
		menu Enc.ucs-2le :e! ++enc=ucs-2le<CR>
		menu Enc.koi8-u  :e! ++enc=koi8-u<CR>
		noremap  <F8> :emenu Enc.<Tab>

	" <F20> Convert file encoding
		set  wcm=<Tab>
		menu FEnc.cp1251  :set fenc=cp1251<CR>
		menu FEnc.koi8-r  :set fenc=koi8-r<CR>
		menu FEnc.cp866   :set fenc=ibm866<CR>
		menu FEnc.utf-8   :set fenc=utf-8<CR>
		menu FEnc.ucs-2le :set fenc=ucs-2le<CR>
		menu FEnc.koi8-u  :set fenc=koi8-u<CR>
		noremap  <F20> :emenu FEnc.<Tab>

" Commands
	command! Wq w|bd
	command! Q bd

" Mapjitsu
	" Getter and setter generation

	" WARN: This is total mess. Don't try this at home. Requires ninja skills.

	" Generates getter and setter for C++ private class items.

	"     Input  Format: type name;
	"     Output Format: type obtainName() {return name;}
	"                    void establishName(type Name) {name = Name;}

	" For example: 'unsigned char* letter;' will produce:
	" unsigned char* obtainLetter() {return letter;}
	" void establishLetter(unsigned char* Letter) {letter = Letter;}
	" just above 'private:' keyword.
	" You can change function names in this string only
	" Search for hiobtain and hiestablish and change
	" to higet hiset or to any hi* pattern
	" WARNING: must be used *below* 'private' keyword in a class scope
		autocmd FileType cpp,h,hpp nnoremap <F3> <Esc>0:set nohlsearch<CR>
					\/;<CR>y^?private<CR>:-1read $HOME/.vim/snippets/getSet.cpp<CR>
					\0Pa()<Esc>bbyw~hiobtain<Esc>/;<CR>P:noh<CR>==j0==/)<CR>bPnbb~h
					\iestablish<Esc>nPnb~/ =<CR>P/;<CR>Pnb~?obtain<CR>y^j/(<CR>p^
					\:set hlsearch<CR>:noh<CR>

