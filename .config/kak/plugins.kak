# ╭─────────────╥───────────────────╮
# │ Author:     ║ File:             │
# │ Andrey Orst ║ plugins.kak       │
# ╞═════════════╩═══════════════════╡
# │ plugins for Kakoune with        │
# │ settings                        │
# ╞═════════════════════════════════╡
# │ Rest of .dotfiles:              │
# │ GitHub.com/andreyorst/dotfiles  │
# ╰─────────────────────────────────╯

# Plugin manager
plug andreyorst/plug.kak

# Extended text objects
plug Delapouite/kakoune-text-objects

# Wrapper for GDB
plug occivink/kakoune-gdb

# fzf mode
plug andreyorst/fzf.kak
    map -docstring "fzf mode" global normal '<c-p>' ':fzf-mode<ret>'
    set-option global fzf_file_command 'rg'

# automatic pair insertion and surroundig
plug alexherbo2/auto-pairs.kak
    hook global WinCreate .* %{ auto-pairs-enable }
    map global normal <a-s> :auto-pairs-surround<ret>
