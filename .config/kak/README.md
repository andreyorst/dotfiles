# Kakoune config

Fancy screenshot for you:
![kakoune.png](https://user-images.githubusercontent.com/19470159/45776447-91dc9b80-bc5b-11e8-90ee-a3c5208e7dfb.png)

## Plugins
- [plug.kak](https://github.com/andreyorst/plug.kak) - Asynchronous plugin manager for Kakoune editor
- [kakoune-text-objects](https://github.com/Delapouite/kakoune-text-objects) - Kakoune plugin providing extra text-objects
- [kakoune-gdb](https://github.com/occivink/kakoune-gdb) - kakoune plugin to provide integration with gdb.
- [kak-lsp](https://github.com/ul/kak-lsp) - kak-lsp is a Language Server Protocol client for Kakoune implemented in Rust.
- [auto-pairs.kak](https://github.com/alexherbo2/auto-pairs.kak) - Kakoune extension to enable automatic closing of pairs.

## Mappings:
- <kbd>Ctrl</kbd>+<kbd>/</kbd> - comment/uncomment lines
- <kbd>g</kbd><kbd>b</kbd>/<kbd>g</kbd><kbd>B</kbd> - next/previous buffer
- <kbd>g</kbd><kbd>f</kbd> - Search file recursively in path
- <kbd>g</kbd><kbd>alt+f</kbd> - standard Kakoune goto file
- <kbd>Alt</kbd>+<kbd>s</kbd> - surround selection with delimiters

## Tmux support
Tmux is being used as a  Vim-like  airline  tabline.   Upon  Kakoune  open  tmux
configuration gets modified, moving statusline to the top, and  recoloring  it's
elements to make it look like Kakoune's statusline.
![tmux tabline](https://user-images.githubusercontent.com/19470159/45776728-72923e00-bc5c-11e8-82dd-48729df3f74f.gif)


### Tmux mappings
- <kbd>g</kbd><kbd>t</kbd>/<kbd>g</kbd><kbd>T</kbd> - next/previous tmux window

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

## Expandtab trick
```kak
hook global WinSetOption filetype=(rust|kak) %{
    hook -group expandtab global InsertChar \t %{
        exec -draft h@
    }

    hook -group expandtab global InsertDelete ' ' %{ try %{
        execute-keys -draft 'h<a-h><a-k>\A\h+\z<ret>i<space><esc><lt>'
    }}
}
```
