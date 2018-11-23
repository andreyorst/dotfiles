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

plug "andreyorst/plug.kak" noload

plug "delapouite/kakoune-text-objects"
plug "occivink/kakoune-vertical-selection"

plug "occivink/kakoune-gdb"

plug "andreyorst/base16-gruvbox.kak"

plug "andreyorst/fzf.kak" %{
    map -docstring 'fzf mode' global normal '<c-p>' ': fzf-mode<ret>'
    set-option global fzf_preview_width '65%'
    evaluate-commands %sh{
        if [ ! -z "$(command -v fd)" ]; then
            echo "set-option global fzf_file_command %{fd . --no-ignore --type f --follow --hidden --exclude .git --exclude .svn}"
        else
            echo "set-option global fzf_file_command %{find . \( -path '*/.svn*' -o -path '*/.git*' \) -prune -o -type f -follow -print}"
        fi
        if [ ! -z "$(command -v bat)" ]; then
            echo "set-option global fzf_highlighter %{bat --theme gruvbox\ \(Dark\)\ \(Soft\) --color=always --style=plain {}}"
        elif [ ! -z "$(command -v highlight)" ]; then
            echo "set-option global fzf_highlighter highlight"
        fi
    }
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

plug "andreyorst/powerline.kak" %{
    set-option global powerline_separator ''
    set-option global powerline_separator_thin ''
    hook -once global WinCreate .* %{
        powerline-theme base16-gruvbox
        powerline-toggle line_column off
    }
}

plug "andreyorst/smarttab.kak" %{
    set-option global softtabstop 4
}

plug "alexherbo2/auto-pairs.kak" %{
    map global user 's' ': auto-pairs-surround<ret>' -docstring "surround selection"
}
