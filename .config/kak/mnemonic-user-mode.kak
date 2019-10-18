# ╭─────────────╥──────────────────────────╮
# │ Author:     ║ File:                    │
# │ Andrey Orst ║ mnemonic-user-mode.kak   │
# ╞═════════════╩══════════════════════════╡
# │ Mnemonic mappings for Kakoune commands │
# ╞════════════════════════════════════════╡
# │ Rest of .dotfiles:                     │
# │ GitHub.com/andreyorst/dotfiles         │
# ╰────────────────────────────────────────╯

#
# file specific commands
#
declare-user-mode file

map global user f ":<space>enter-user-mode file<ret>"
map global file o "<esc>:<space>edit<space>"           -docstring "open file"
map global file e "<esc>:<space>edit -existing<space>" -docstring "open existing file"
map global file s "<esc>:<space>w<ret>"                -docstring "save file"

#
# spell mode mappings
#
declare-user-mode spell

map global user s ":<space>enter-user-mode spell<ret>"
map global spell E ': spell en<ret>'                                        -docstring 'ENG'
map global spell n ': spell-next<ret>_: enter-user-mode spell<ret>'         -docstring 'next'
map global spell f ': spell-replace<ret><ret> : enter-user-mode spell<ret>' -docstring 'lucky fix'
map global spell F ': spell-replace<ret>'                                   -docstring 'manual fix'
map global spell c ': spell-clear<ret>'                                     -docstring 'clear'

#
# git commands
#
declare-user-mode git

map global user g ":<space>enter-user-mode git<ret>"
map global git c ": git commit<ret>"                                -docstring "commit - Record changes to the repository"
map global git d ": git diff<ret>"                                  -docstring "diff - Show changes between HEAD and working tree"
map global git t ": git diff --staged<ret>"                         -docstring "staged - Show staged changes"
map global git w ": write<ret>: git add<ret>: git update-diff<ret>" -docstring "write - Write and stage the current file"
