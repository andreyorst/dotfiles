# NeoVim Config

Fancy screenshot for you:
![NeoVim](https://user-images.githubusercontent.com/19470159/38351495-64efb4da-38b8-11e8-8454-f2e3d597b82c.png)

## Plugins

List of plugins (not all, but essential ones) that extend or change workflow in
some way. Basically, I've tried to make nvim as close to C/C++ IDE as possible.

- Linting and autocompleting C/C++/Rust via
  - On PC:
    [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim) +
    [Deoplete.nvim](https://github.com/Shougo/deoplete.nvim).
  - In Termux:
    [ALE](https://github.com/wr0p/ale),
    [Deoplete.nvim](https://github.com/Shougo/deoplete.nvim) +
    [deoplete-clang](https://github.com/zchee/deoplete-clang)
- Browsing current file tags with
  [tagbar](https://github.com/majutsushi/tagbar).
- Snippets provided by
  [SimpleSnippets.vim](https://github.com/andreyorst/SimpleSnippets.vim)
- Browsing files with [Nerdtree](https://github.com/scrooloose/nerdtree).
- Automatically inserting delimiters with
  [delimitMate](https://github.com/Raimondi/delimitMate).
- Fuzzy finding, and doing lots of other cool stuff with
  [Denite](https://github.com/Shougo/denite.nvim)
- Jump to any location specified by two characters with
  [vim-sneak](https://github.com/justinmk/vim-sneak)
- Using various textobjects with
  [targets-vim](https://github.com/wellle/targets.vim)
- Surrounding stuff with
  [vim-surround](https://github.com/tpope/vim-surround)
- Commenting in and out blocks of code with
  [TComment](https://github.com/tomtom/tcomment_vim)
- Managing workspaces with
  [SimpleWorkspaces.vim](https://github.com/andreyorst/SimpleWorkspaces.vim)

## Mappings

- <kbd>F1</kbd> - NOTHING.
- <kbd>F2</kbd> - LanguageClient-neovim rename symbol.
- <kbd>F3</kbd>/<kbd>F4</kbd> - Search and replace word under cursor with
  `RenameCWord()` function.
- <kbd>\*</kbd> - Search for all occurrences of word under cursor, but don't
  move. No jumps added to jumplist.
- <kbd>Alt</kbd>+<kbd>t</kbd> - Toggle bottom terminal window.
- <kbd>Alt</kbd>+<kbd>b</kbd> - Toggle Tagbar window.
- <kbd>Alt</kbd>+<kbd>n</kbd> - Toggle Nerdtree window.
- <kbd>Alt</kbd>+<kbd>j</kbd>/<kbd>k</kbd> - Move current line or visual block
  accordingly to cursor movements
- <kbd>g</kbd><kbd>b</kbd> - Go to next buffer.
- <kbd>g</kbd><kbd>B</kbd> - Go to previous buffer.
- <kbd>g</kbd><kbd>e</kbd> - Create and edit file under cursor.
- <kbd>Ctrl</kbd>+<kbd>/</kbd> - Un/Comment current line
- <kbd>Ctrl</kbd>+<kbd>p</kbd> - Invoke `:Denite file/rec`

## Functions

Some functions, that I use. Some of which I've created myself, some were taken
from other Vim users. Feel free to use, modify, extend them.

- `RemoveTrailingSpaces()` - Removes all trailing whitespace on file-save.
- `Term_toggle(height)` - Toggles terminal on and off.
- `ExecuteMacroOverVisualRange()` - allows to execute <kbd>@</kbd> macro over
  visual selection.
- `RenameCWord(word type)` - Renames all occurrences of word under cursor in
  current file. For `word type` see `:h word` and `:h WORD`
- `FindProjectRootByFile(filename)` - goes up from current directory until it
  reaches root, and returns path to the file if found.
- `IsTermux()` - Determinate if nvim is running inside
  [Termux](https://github.com/termux/termux-app)
- `DefineSyntaxRegion(lang, start, end, hl)` - define nested highlighting
  region for different language.

## Termux support

This NeoVim config can be adjusted between powerful PC and less powerful  phone,
assuming  you're  using  [Termux](https://github.com/termux/termux-app)  to  run
neovim.  You can find special `if` statements in some files that change settings
accordingly to platform where you're running neovim.   For  example,  I'm  using
LanguageClient-neovim (LCN) on my PC, but there is no way to run  it  in  Termux
(yet) because there is no compiled version for ARM for now. So instead of LCN in
Termux I use ALE, and deoplete-clang, to achieve closest completion behavior. Or
for example due the small screen of my phone, compared to  my  laptop's  one,  I
don't really want vertical splits to appear so I  disable  automatic  triggering
of Tagbar on C/C++ fileopen.  And so on.  The usage is  simple,  just  use  `if`
statement like so if you need to run something only in Termux:

```vim
if IsTermux()
	" Do some less CPU hungry actions here if we're in Termux
else
	" Or go full out when you're on your powerful PC
endif
```

Or  you  can  exclude  some  actions  in  Termux   neovim   session   by   using
`if !IsTermux()` statement.

## Language Specific Stuff

I have several autocmd groups for different languages.  The reason for not using
ft plugin, is that I may want to disable some autogroups for some languages, and
may want to use some autogroups in some languages.

### Cpp autogroup

Cpp  autogroup  defines  special  regular  expressions,  to  highlight  language
facilities like:
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

Will look like so:

![image](https://user-images.githubusercontent.com/19470159/38468381-8797eeca-3b4d-11e8-9536-e82d79df3a75.png)

### Rust autogroup

Rust autogroup add highlighting of leading spaces.

### Markdown autogroup

Markdown autogroup defines some nested highlightings for languages that I'm
using when writing my notes:

![image](https://user-images.githubusercontent.com/19470159/43099218-24295600-8eca-11e8-847b-bc7e9da29c81.png)

#

Other configurations may be found  in  configuration  files  itself.   They  are
provided with comments, so it won't be big problem for you, if you  will  desire
to try my setup, to figure out what is going on here and there.  You should  pay
attention to colored marks in comment sections, like `WARNING:`, `NOTE:` etc.

If you encounter any problem with my setup, feel free to open issue,  we'll  see
what we can do here.
