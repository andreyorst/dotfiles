# Kakoune configuration

Fancy screenshot for you: ![Kakoune][0]

Kakoune is amazing code editor, with multiple cursors as a central way of
interaction with text. It was inspired by Vim, and I quite like it. Currently it
is my favorite editor. I've configured it for my needs, but if you find this
configuration useful feel free to use and modify it for your needs.

## Plugins
I use a lot of plugins that may or may not be listed here (because I'm lazy and
I rarely update this document). For managing those plugins, e.g. installing new
ones, updating existed, isolating configurations I use my own plugin manager
called [plug.kak][15], that was inspired by [vim-plug][21] and
[use-package][22]. It is capable of loading plugin configurations, evaluating
post-update commands, checking for updates, and it is asynchronous. I try to
make it as fast as possible.

This is the list of plugins that I use basically every time I do something in
Kakoune:

- [auto-pairs.kak][1] - Automatically insert pair symbols, like braces, quotes,
  etc. Also allows surrounding text.
- [move-line.kak][2] - Move selections up or down with keybindings.
- [replace.kak][3] - Vim replace mode for Kakoune.
- [kakoune-text-objects][4] - Additional text objects, works nice with
  `kakoune-vertical-selection`.
- [kakoune-find][5] - Find and replace text in all opened buffers in a grep
  format.
- [kakoune-gdb][6] - Awesome GDB integration for Kakoune.
- [kakoune-snippets][7] - Finally good snippet plugin.
- [kakoune-sudo-write][8] - A command that pretty much about writing `sudo`
  protected files.
- [kakoune-vertical-selection][9] - Add cursors up or down based on text under
  the cursor.
- [kakoune-edit-or-dir][10] - Sort of a file explorer for Kakoune.
- [kak-lsp][11] - A client for Language Server Protocol implemented in Rust.

These plugins are written by me for personal use, but I tend to share everything
with the community so you're more than welcome to use them:

- [base16-gruvbox.kak][12] - Base16 Gruvbox inspired color-scheme.
- [fzf.kak][13] - Integration of [Fzf][19] tool to Kakoune.
- [kakoune-snippet-collection][14] - Collection of snippets for
  [kakoune-snippets][7] plugin
- [plug.kak][15] - This is my plugin management solution for Kakoune.
- [powerline.kak][16] - A modeline but with passion.
- [smarttab.kak][17] - Vim has a nice features, called `expandtab`,
  `noexpandtab`, and `smarttab`. This plugin implements those.
- [tagbar.kak][18] - A Vim [tagbar][20] like plugin for Kakoune.

## Custom Mappings
Kakoune, like Vim, provides  a nice way to modify or  add mappings. I've defined
some in my configuration files, and some are being defined by plugins.

### General purpose mappings
- <kbd>Ctrl</kbd>+<kbd>/</kbd> - to comment/uncomment selection
- <kbd>Ctrl</kbd>+<kbd>d</kbd> - add cursor/jump on current word
- <kbd>,</kbd> <kbd>t</kbd> - convert leading spaces to tabs
- <kbd>,</kbd> <kbd>T</kbd> - convert leading tabs to spaces

### Goto mode mappings
- <kbd>g</kbd> <kbd>Alt</kbd>+<kbd>f</kbd> - file non-recursive
- <kbd>g</kbd> <kbd>f</kbd> - file recursive
- <kbd>g</kbd> <kbd>b</kbd> - next buffer
- <kbd>g</kbd> <kbd>B</kbd> - previous buffer
- <kbd>g</kbd> <kbd>[</kbd> - search tag in current file
- <kbd>g</kbd> <kbd>]</kbd> - search tag in global tags file

### System clipboard mappings
Kakoune  doesn't provide  a system  clipboard  integration by  default, so  I've
defined  these mappings  to be  able  to copy,  paste, and  replace from  system
keyboard, in a way Kakoune's clipboard works. They are defined in `user` mode.

- <kbd>,</kbd> <kbd>y</kbd> - copy to system clipboard
- <kbd>,</kbd> <kbd>d</kbd> - cut to system clipboard
- <kbd>,</kbd> <kbd>c</kbd> - cut to system clipboard, enter insert mode
- <kbd>,</kbd> <kbd>P</kbd> - paste from system clipboard before cursor
- <kbd>,</kbd> <kbd>p</kbd> - paste from system clipboard after cursor
- <kbd>,</kbd> <kbd>R</kbd> - replace selection with system clipboard

## Custom commands
I've defined these commands because Kakoune doesn't work quite how I want it
to. Feel free to modify it for your needs.

- `format-c` - You probably don't need this command. It executes a search over
  the buffer, and wraps each line that has `tos()` or `ton()` function with
  `clang-format off/on` comment, executes the `format` command and removes those
  comments.
- `leading-spaces-to-tabs` - Converts leading spaces to tabs based on
  `indentwidth` option
- `leading-tabs-to-spaces` - Converts leading tabs to spaces based on `tabstop`
  option
- `search-file` - A recursive file search.
- `select-or-add-cursor` - A supplement command for the
  <kbd>Ctrl</kbd>+<kbd>d</kbd> mapping.
- `smart-select` - A supplement command for selecting `word` or `WORD`, and
  passing it to the `search-file` or `select-or-add-cursor`.
- `symbol` - Generates tags for current buffer, and allows searching and
  jumping.

### Tmux integration
Commands to provide nice integration with tmux:

- `vsplit` - splits window vertically by creating pane to the right.
- `split` - splits window horizontally by creating pane below.
- `tabnew` - creates new window.

These commands can be used with other Kakoune commands, like `edit`, `buffer`, etc.

## Language specific settings
There's a file [languages.kak](./languages.kak) that defines some language
specific settings that you sould check for yourself, but in general it does
these things:

- Highlights operators and delimiters
- Extends highlighters of C/Cpp/Rust by regexps to highlight user types in C/Cpp, and various language facilities,
  like methods, or `struct` fields.
- Adds Perl region highlighting to `sh` language with `perl[^']+'` regexp.

[0]:https://user-images.githubusercontent.com/19470159/49689293-45a14900-fb30-11e8-915b-9a660aa1a233.png
[1]:https://github.com/alexherbo2/auto-pairs.kak
[2]:https://github.com/alexherbo2/move-line.kak
[3]:https://github.com/alexherbo2/replace.kak
[4]:https://github.com/delapouite/kakoune-text-objects
[5]:https://github.com/occivink/kakoune-find
[6]:https://github.com/occivink/kakoune-gdb
[7]:https://github.com/occivink/kakoune-snippets
[8]:https://github.com/occivink/kakoune-sudo-write
[9]:https://github.com/occivink/kakoune-vertical-selection
[10]:https://github.com/TeddyDD/kakoune-edit-or-dir
[11]:https://github.com/ul/kak-lsp
[12]:https://github.com/andreyorst/base16-gruvbox.kak
[13]:https://github.com/andreyorst/fzf.kak
[14]:https://github.com/andreyorst/kakoune-snippet-collection
[15]:https://github.com/andreyorst/plug.kak
[16]:https://github.com/andreyorst/powerline.kak
[17]:https://github.com/andreyorst/smarttab.kak
[18]:https://github.com/andreyorst/tagbar.kak
[19]:https://github.com/junegunn/fzf
[20]:https://github.com/majutsushi/tagbar
[21]:https://github.com/junegunn/vim-plug
[22]:https://github.com/jwiegley/use-package
