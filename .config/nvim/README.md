# NeoVim config

Fancy screenshot for you:
![Neovim](https://user-images.githubusercontent.com/19470159/38351495-64efb4da-38b8-11e8-8454-f2e3d597b82c.png)
## Features:

### Plugins and stuff

#### Plugins
  - Linting C/C++ code via [ALE](https://github.com/w0rp/ale).
  - Autocompleting C/C++ via [deoplete.nvim](https://github.com/Shougo/deoplete.nvim) + [deoplete-clang](https://github.com/zchee/deoplete-clang).
  - Linting and autocompleting Rust via [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim) + [deoplete.nvim](https://github.com/Shougo/deoplete.nvim).
  - Generating ctags on the fly via [vim-autotag](https://github.com/craigemery/vim-autotag) and browsing current file tags with [tagbar](https://github.com/majutsushi/tagbar).
  - Snippets provided by [SimpleSnippets](https://github.com/andreyorst/SimpleSnippets.vim)
  - Browsing files with [Nerdtree](https://github.com/scrooloose/nerdtree).
  - Automatically inserting delimiters with [delimitMate](https://github.com/Raimondi/delimitMate).
  - Fuzzy finding any file, mru, tag via [ctrlp.vim](https://github.com/ctrlpvim/ctrlp.vim)

#### Some C/C++ cool stuff
  - Special regular expressions, to highlight language facilities like
  ```cpp
  typedef struct container {
  	int item1;
  	int item2;
  } container_t;
  
  container_t c;
  container_t* pc;
  
  c.item1;
  pc->item2;
  ```
  will look like so:
  
  ![image](https://user-images.githubusercontent.com/19470159/38468381-8797eeca-3b4d-11e8-9536-e82d79df3a75.png)
  - Some color fixes, so no end of buffer `~`s and less distracting signcolumn, and some nontext color fixes.
  
### Mappings and functions

#### Mappings:

  - <kbd>F1</kbd> - NOTHING
  - <kbd>F2</kbd> - Search and replace word under cursor with `RenameCWord()`
  - <kbd>*</kbd> - Highlight all occuriences of word under cursor, but dont move. No jumps added to jumplist.
  - <kbd>g</kbd><kbd>f</kbd> - Open file under cursor in new tab
  - <kbd>Alt</kbd>+<kbd>t</kbd> - Toggle bottom terminal window
  - <kbd>Alt</kbd>+<kbd>b</kbd> - Toggle tagbar window
  - <kbd>Alt</kbd>+<kbd>n</kbd> - Toggle nerdtree window
  - <kbd>g</kbd><kbd>b</kbd> - Go to next buffer
  - <kbd>g</kbd><kbd>B</kbd> - Go to privious buffer
  
#### Functions:
  - `HighlightTypes()` - **WIP** not ready to use. Automatically highlights C/C++ user types as vim Type highlight group.
  - `RemoveTrailingSpaces()` - Removes all trailing whitespace on filesave
  - `Term_toggle(height)` - Toggless terminal on and off
  - `ExecuteMacroOverVisualRange()` - allows to execute <kbd>@</kbd> macro over visual selection
  - `RenameCWord()` - Renames all occuriences of word under cursor in current file.\
  
Other configurations may be found in config files itself. They are provided with comments, so it won't be big problem for you, if you will desire to try my setup, to figure out what is going on here and there.

Some plugins have huge comments like this one:
```vim
" NOTE: This piece of code is used to generate clang options for ALE,
" because we aren't using any build system. You may delete this, or
" rewrite to your project needs. Basically you can refer to a specific
" file in your project root, and automatically pass desired options to
" ALE, and later to Clang. But the main reason I wrote it because, we
" have special config file, that contains current includepath, for our
" own build system, so I need to pass it to ALE somehow and detect if
" it was changed. You can go further and have a separate if() for each
" project, I have two for now. I understand that this is not the most
" beautiful way of doing this, but, still, it works fine, and I'm kinda
" happy with this variant for now.
if filereadable("./testkit.settings")
	let g:includepath = system('echo -n
				\ -I $(pwd)/include
				\ -I $(pwd)/testpacks/SPW_TESTS/spw_lib_src
				\ -I $(pwd)/testpacks/CAN/can_lib_src
				\ -I $(pwd)/platforms/$(cat ./testkit.settings | grep "?=" |  sed -E "s/.*= //")/include
				\')
elseif filereadable("./startf.S")
	let g:includepath = system('echo -n
				\ -I $(pwd)/include
				\ -I $(pwd)/include/cp2
				\ -I $(pwd)/include/hdrtest
				\ -I $(pwd)../../include
				\')
endif

let g:ale_c_clang_options.= g:includepath
let g:ale_c_gcc_options.= g:includepath
```
You should pay attention to colored marks in comment sections, like `WARNING:`, `NOTE:` etc.

If you encounter any problem with my setup, feel free to open issue, we'll se what we can do here.
