# Kakoune config

Fancy screenshot for you:
![image](https://user-images.githubusercontent.com/19470159/49689293-45a14900-fb30-11e8-915b-9a660aa1a233.png)


## Plugins
- [kakoune-text-objects](https://github.com/delapouite/kakoune-text-objects) - additional text objects.
- [kakoune-vertical-selection](https://github.com/occivink/kakoune-vertical-selection) - handling vertical selection.
- [kakoune-gdb](https://github.com/occivink/kakoune-gdb) - gdb integration.
- [kakoune-edit-or-dir](https://github.com/TeddyDD/kakoune-edit-or-dir) - open directories with edit command.
- [kak-lsp](https://github.com/ul/kak-lsp) - language server protocol for Kakoune.
- [auto-pairs.kak](https://github.com/alexherbo2/auto-pairs.kak) - insert pared symbol automatically and surround selections.
- [parinfer-rust](https://github.com/eraserhd/parinfer-rust) - parinfer support for Kakoune.

### My plugins
- [plug.kak](https://github.com/andreyorst/plug.kak) - a plugin manager.
- [base16-gruvbox.kak](https://github.com/andreyorst/base16-gruvbox.kak) - base16 Gruvbox Dark Soft colorscheme.
- [fzf.kak](https://github.com/andreyorst/fzf.kak) - fzf integration plugin.
- [powerline.kak](https://github.com/andreyorst/powerline.kak) - powerline like plugin.
- [smarttab.kak](https://github.com/andreyorst/smarttab.kak) - smarttab plugin for different tab styles handling.

## Custom Mappings
- <kbd>Ctrl+/</kbd>: <c-/> to comment/uncomment selection.
- <kbd>Ctrl+d</kbd>: add currsor/jump on current word.
- <kbd>t</kbd>: convert leading spaces to tabs.
- <kbd>T</kbd>: convert leading tabs to spaces.
- <kbd>Alt+f</kbd>: file non-recursive.
- <kbd>f</kbd>: file recursive.
- <kbd>b</kbd>: next buffer.
- <kbd>B</kbd>: previous buffer.
- <kbd>[</kbd>: search tag in current file.
- <kbd>]</kbd>: search tag in global tags file.
- <kbd>y</kbd>: copy to system clipboard.
- <kbd>d</kbd>: cut to system clipboard.
- <kbd>c</kbd>: cut to system clipboard.
- <kbd>P</kbd>: paste from system clipboard.
- <kbd>p</kbd>: paste from system clipboard.
- <kbd>n</kbd>: next error.
- <kbd>r</kbd>: replace word.
- <kbd>c</kbd>: exit spell mode.
- <kbd>S</kbd>: spell mode.

## Custom commands
- `smart-select` - select `WORD` or `word` if current selection is only one character.
- `find` - search for file recusively under path option: `%opt{path}`.
- `select-or-add-cursor` - select a `word` under cursor, or add cursor on next occurrence of current selection.
- `leading-spaces-to-tabs` - Convert all leading spaces to tabs.
- `leading-tabs-to-spaces` - Convert all leading tabs to spaces.
- `symbol` - jump to symbol definition in current file. If no symbol given, current selection is used as a symbol name.

### Tmux commands
- `vspl[it]` `vert[ical]` - split screen vertically
- `spl[it]` - split screen horizontally
- `tabn[ew]` - create new Kakoune window in new tmux window
- `term[inal]` `tmux-new-terminal` - split screen horizontally opening shell at the bottom
- `init-tmux` `restore-tmux` - modify tmux's statusline
- `rename-tmux` - rename current tmux window to opened buffer filename.

## Language specific settings
- Highlighted operators and delimiters
- Regexes to highlight user types in C/Cpp, and various language facilities,
  like methods, or `struct` fields.
