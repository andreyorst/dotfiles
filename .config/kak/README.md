# Kakoune config

Fancy screenshot for you:
![kakoune.png](https://user-images.githubusercontent.com/19470159/44391197-e47d4780-a537-11e8-8f66-62764627fc2d.png)

## Mappings:
- <kbd>Ctrl</kbd>+<kbd>/</kbd> - comment/uncomment lines
- <kbd>g</kbd><kbd>b</kbd>/<kbd>g</kbd><kbd>B</kbd> - next/previous buffer
- <kbd>Alt</kbd>+<kbd>s</kbd> - surround selection with delimiters

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
## Tmux support
Tmux is being used as a  Vim-like  airline  tabline.   Upon  Kakoune  open  tmux
configuration gets modified, moving statusline to the top, and  recoloring  it's
elements to make it look like Kakoune's statusline.

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
