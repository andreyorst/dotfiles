# NeoVim Config

Fancy screenshot for you:
![NeoVim](https://user-images.githubusercontent.com/19470159/38351495-64efb4da-38b8-11e8-8454-f2e3d597b82c.png)
## Features

### Plugins

List of plugins (not all, but essential ones) that extend or change workflow in some way. Basically, I've tried to make nvim as close to C/C++ IDE as possible.
  - Linting and autocompleting C/C++/Rust via [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim) + [Deoplete.nvim](https://github.com/Shougo/deoplete.nvim).
  - Generating ctags on the fly via [vim-autotag](https://github.com/craigemery/vim-autotag) and browsing current file tags with [tagbar](https://github.com/majutsushi/tagbar).
  - Snippets provided by [SimpleSnippets.vim](https://github.com/andreyorst/SimpleSnippets.vim)
  - Browsing files with [Nerdtree](https://github.com/scrooloose/nerdtree).
  - Automatically inserting delimiters with [delimitMate](https://github.com/Raimondi/delimitMate).
  - Fuzzy finding, and doing lots of other cool stuff with [Denite](https://github.com/Shougo/denite.nvim)
  - Distraction-free typing of Markdown with [Goyo](https://github.com/junegunn/goyo.vim)
  - Jump to any location specified by two characters with [vim-sneak](https://github.com/justinmk/vim-sneak)

### Some C/C++ cool stuff

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
  - Some color fixes, so no end of buffer `~`s and less distracting `signcolumn`, and some `nontext` color fixes.


### Mappings

  - <kbd>F1</kbd> - NOTHING.
  - <kbd>F3</kbd>/<kbd>F4</kbd> - Search and replace word under cursor with `RenameCWord()` function.
  - <kbd>\*</kbd> - Highlight all occurrences of word under cursor, but dont move. No jumps added to jumplist.
  - <kbd>Alt</kbd>+<kbd>t</kbd> - Toggle bottom terminal window.
  - <kbd>Alt</kbd>+<kbd>b</kbd> - Toggle Tagbar window.
  - <kbd>Alt</kbd>+<kbd>n</kbd> - Toggle Nerdtree window.
  - <kbd>Alt</kbd>+<kbd>j</kbd>/<kbd>k</kbd> - Move current line or visual block accordingly to cursor movements
  - <kbd>g</kbd><kbd>b</kbd> - Go to next buffer.
  - <kbd>g</kbd><kbd>B</kbd> - Go to previous buffer.

### Functions

Some functions, that I use. Some of which i've created myself, some were taken from other Vim users. Feel free to use, modify, extend them.
  - `RemoveTrailingSpaces()` - Removes all trailing whitespace on file-save.
  - `Term_toggle(height)` - Toggles terminal on and off.
  - `ExecuteMacroOverVisualRange()` - allows to execute <kbd>@</kbd> macro over visual selection.
  - `RenameCWord(word type)` - Renames all occurrences of word under cursor in current file. For `word type` see `:h word` and `:h WORD`
  - `FindProjectRootByFile(filename)` - goes up from current directory untill it reaches root, and returns path to the file if found.
  - `IsTermux()` - Determinate if nvim is running inside [Termux](https://github.com/termux/termux-app)

### Termux support

This NeoVim config can be adjusted between powerful PC and less powerful phone, assuming you're using [Termux](https://github.com/termux/termux-app) to run neovim. You can find special `if` statements in some files that change settings accordingly to platform where you're running neovim. For example, I'm using LanguageClient-neovim (LCN) on my PC, but there is no way to run it in Termux (yet) because there is no compiled version for ARM for now. So instead of LCN in Termux I use ALE, and deoplete-clang, to achieve closest completion behavior. Or for example due the small screen of my phone, compared to my laptop's one, I don't really want vertical splits to appear so I disable automatic triggering of Tagbar on C/C++ fileopen. And so on. The usage is simple, just use `if` statement like so if you need to run something only in Termux:

```vim
if IsTermux()
	" Do some less CPU hungry actions here if we're in termux
esle
	" Or go full out when you're on your powerful PC
endif
```

Or you can exclude some actions in Termux neovim session by using `if !IsTermux()` statement.
#

Other configurations may be found in configuration files itself. They are provided with comments, so it won't be big problem for you, if you will desire to try my setup, to figure out what is going on here and there. You should pay attention to colored marks in comment sections, like `WARNING:`, `NOTE:` etc.

If you encounter any problem with my setup, feel free to open issue, we'll see what we can do here.
