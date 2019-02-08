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

plug "andreyorst/base16-gruvbox.kak" noload do %{
    find -type f -name "*.kak" -print0 | xargs -0 cp -t $HOME/.config/kak/colors
} config %{
    colorscheme base16-gruvbox-dark-soft
}

plug "andreyorst/fzf.kak" %{
    map -docstring 'fzf mode' global normal '<c-p>' ': fzf-mode<ret>'
    set-option global fzf_preview_width '65%'
    evaluate-commands %sh{
        if [ -n "$(command -v fd)" ]; then
            echo "set-option global fzf_file_command %{fd . --no-ignore --type f --follow --hidden --exclude .git --exclude .svn}"
        else
            echo "set-option global fzf_file_command %{find . \( -path '*/.svn*' -o -path '*/.git*' \) -prune -o -type f -follow -print}"
        fi
        if [ -n "$(command -v bat)" ]; then
            echo "set-option global fzf_highlighter bat"
        elif [ -n "$(command -v highlight)" ]; then
            echo "set-option global fzf_highlighter highlight"
        fi
    }
}

plug "TeddyDD/kakoune-edit-or-dir" %{
    unalias global e
    alias global e edit-or-dir
}

plug "ul/kak-lsp" do %{
    cargo build --release --locked
    cargo install --force
} config %{
    set-option global lsp_diagnostic_line_error_sign "!"
    set-option global lsp_diagnostic_line_warning_sign "?"
    hook global WinSetOption filetype=(c|cpp|rust) %{
        map window user "l" ": enter-user-mode lsp<ret>" -docstring "LSP mode"
        lsp-enable-window
        lsp-auto-hover-enable
    }
    hook global WinSetOption filetype=rust %{
        set-option window lsp_server_configuration rust.clippy_preference="on"
        lsp-auto-hover-disable
    }
    hook global KakEnd .* lsp-exit
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
    hook global WinSetOption filetype=(rust|markdown|kak|lisp|scheme) expandtab
    hook global WinSetOption filetype=(makefile) noexpandtab
    hook global WinSetOption filetype=(c|cpp) smarttab
}

plug "alexherbo2/auto-pairs.kak" %{
    map global user 's' ': auto-pairs-surround<ret>' -docstring "surround selection"
    # hook global WinCreate .* %{ auto-pairs-enable }
}

plug "alexherbo2/replace.kak" config %{
    map global user r -docstring 'Replace mode' ':<space>replace<ret>'
}

plug "alexherbo2/move-line.kak" config %{
    map global normal "<c-b>" ': move-line-below %val{count}<ret>'
    map global normal "<c-a>" ': move-line-above %val{count}<ret>'
}

plug "occivink/kakoune-snippets" config %{
    set-option -add global snippets_directories "%opt{plug_install_dir}/kakoune-snippet-collection/snippets"
    set-option global snippets_auto_expand false
    map global insert '<tab>' "z<a-;>: snippets-expand-or-jump 'tab'<ret>"

    hook global InsertCompletionShow .* %{
        try %{
            execute-keys -draft 'h<a-K>\h<ret>'
            map window insert '<ret>' "z<a-;>: snippets-expand-or-jump 'ret'<ret>"
        }
    }

    hook global InsertCompletionHide .* %{
        unmap window insert '<ret>' "z<a-;>: snippets-expand-or-jump 'ret'<ret>"
    }

    define-command snippets-expand-or-jump -params 1 %{
        execute-keys <backspace>
        try %{
            snippets-expand-trigger %{
                set-register / "%opt{snippets_triggers_regex}\z"
                execute-keys 'hGhs<ret>'
            }
        } catch %{
            snippets-select-next-placeholders
        } catch %sh{
            case $1 in
                ret|tab)
                    printf "%s\n" "execute-keys -with-hooks <$1>" ;;
                *)
                    printf "%s\n" "execute-keys -with-hooks $1" ;;
            esac
        }
    }
}

plug "andreyorst/kakoune-snippets-collection"

plug "occivink/kakoune-find"
