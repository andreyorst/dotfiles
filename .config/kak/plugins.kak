# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ plugins.kak      │
# ╞═════════════╩══════════════════╡
# │ plugins for Kakoune with their │
# │ settings handled by plug.kak   │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

# Plugin manager
plug "andreyorst/plug.kak" noload %{
    set-option global plug_profiler false
}

# Extended text objects and selections
plug "delapouite/kakoune-text-objects"
plug "occivink/kakoune-vertical-selection"

# Wrapper for GDB
plug "occivink/kakoune-gdb"

# fzf integration
plug "andreyorst/fzf.kak" %{
    map -docstring 'fzf mode' global normal '<c-p>' ': fzf-mode<ret>'
    set-option global fzf_preview_width '65%'
    evaluate-commands %sh{
        if [ ! -z "$(command -v fd)" ]; then
            echo "set-option global fzf_file_command 'fd . --no-ignore --type f --follow --hidden --exclude .git --exclude .svn'"
        else
            echo "set-option global fzf_file_command \"find . \( -path '*/.svn*' -o -path '*/.git*' \) -prune -o -type f -follow -print\""
        fi
        if [ ! -z "$(command -v bat)" ]; then
            echo "set-option global fzf_highlighter 'bat --theme gruvbox\ \(Dark\)\ \(Soft\) --color=always --style=plain {}'"
        elif [ ! -z "$(command -v highlight)" ]; then
            echo "set-option global fzf_highlighter 'highlight'"
        fi
    }
}

# automatic pair insertion and surroundig
nop  plug "alexherbo2/auto-pairs.kak" %{
    hook global WinCreate .* %{ auto-pairs-enable }
    map global user -docstring 'surround selection' 's' ': auto-pairs-surround<ret>'
}

plug "TeddyDD/kakoune-edit-or-dir" %{
    unalias global e
    alias global e edit-or-dir
}

plug "ul/kak-lsp" noload do %{cargo build --release} %{
    hook global WinSetOption filetype=(c|cpp|rust) %{
        evaluate-commands %sh{ kak-lsp --kakoune -s $kak_session }
        lsp-auto-hover-enable
        set-option global lsp_hover_anchor true
    }
}

plug "andreyorst/powerline.kak" noload %{
    source "%val{config}/plugins/powerline.kak/rc/powerline.kak"
    source "%val{config}/plugins/powerline.kak/rc/themes/base16.kak"
    source "%val{config}/plugins/powerline.kak/rc/themes/default.kak"
    source "%val{config}/plugins/powerline.kak/rc/themes/gruvbox.kak"

    set-option global powerline_separator ''
    set-option global powerline_separator_thin ''

    set-option -add global powerline_themes "base16-gruvbox"

    define-command powerline-theme-base16-gruvbox %{
        set-option global powerline_git_fg         "rgb:d5c4a1"
        set-option global powerline_git_bg         "rgb:3c3836"
        set-option global powerline_bufname_bg     "rgb:a89984"
        set-option global powerline_bufname_fg     "rgb:282828"
        set-option global powerline_line_column_fg "rgb:bdae93"
        set-option global powerline_line_column_bg "rgb:504945"
        set-option global powerline_mode_info_fg   "rgb:3c3836"
        set-option global powerline_mode_info_bg   "rgb:3c3836"
        set-option global powerline_filetype_fg    "rgb:d5c4a1"
        set-option global powerline_filetype_bg    "rgb:504945"
        set-option global powerline_client_fg      "rgb:ebdbb2"
        set-option global powerline_client_bg      "rgb:665c54"
        set-option global powerline_session_fg     "rgb:fbf1c7"
        set-option global powerline_session_bg     "rgb:7c6f64"
        set-option global powerline_position_fg    "rgb:282828"
        set-option global powerline_position_bg    "rgb:a89984"
    }

    hook -once global WinCreate .* %{
        powerline-theme base16-gruvbox
    }
}
