# ╭─────────────╥──────────────────────────╮
# │ Author:     ║ File:                    │
# │ Andrey Orst ║ mnemonic-user-mode.kak   │
# ╞═════════════╩══════════════════════════╡
# │ Mnemonic mappings for Kakoune commands │
# ╞════════════════════════════════════════╡
# │ Rest of .dotfiles:                     │
# │ GitHub.com/andreyorst/dotfiles         │
# ╰────────────────────────────────────────╯

declare-user-mode mnemonics
map global user m ":<space>enter-user-mode mnemonics<ret>" -docstring "mnemonic command mode"

#
# file specific commands
#
declare-user-mode file
map global mnemonics f ': enter-user-mode file<ret>' -docstring "file"
map global file o      '<esc>: edit '                -docstring "open file"
map global file e      '<esc>: edit -existing '      -docstring "open existing file"
map global file s      '<esc>: w<ret>'               -docstring "save file"

#
# spell mode mappings
#
declare-user-mode spell
map global mnemonics s ': enter-user-mode spell<ret>'                           -docstring "spell"
map global spell <a-e> ': spell en<ret>: enter-user-mode spell<ret>'            -docstring 'ENG'
map global spell <a-r> ': spell ru<ret>: enter-user-mode spell<ret>'            -docstring 'ENG'
map global spell n     ': spell-next<ret>_: enter-user-mode spell<ret>'         -docstring 'next'
map global spell f     ': spell-replace<ret><ret> : enter-user-mode spell<ret>' -docstring 'lucky fix'
map global spell F     ': spell-replace<ret>'                                   -docstring 'manual fix'
map global spell c     ': spell-clear<ret>'                                     -docstring 'clear'

#
# git commands
#
declare-user-mode git
map global mnemonics g ': enter-user-mode git<ret>'                 -docstring "git"
map global git c ': git commit<ret>'                                -docstring "commit - Record changes to the repository"
map global git d ': git diff<ret>'                                  -docstring "diff - Show changes between HEAD and working tree"
map global git t ': git diff --staged<ret>'                         -docstring "staged - Show staged changes"
map global git w ': write<ret>: git add<ret>: git update-diff<ret>' -docstring "write - Write and stage the current file"

#
# buffers
#
declare-user-mode buffer
map global mnemonics b ': enter-user-mode -lock buffer<ret>' -docstring "buffer"
map global buffer n ': buffer-next<ret>'              -docstring "next"
map global buffer p ': buffer-previous<ret>'          -docstring "previous"
map global buffer d ': delete-buffer<ret>'            -docstring "delete"
map global buffer r ': rename-buffer '                -docstring "rename"
