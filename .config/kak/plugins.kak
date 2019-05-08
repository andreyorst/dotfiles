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

# source the plugin manager itself
source "%val{config}/plugins/plug.kak/rc/plug.kak"

# Plugin configurations
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
plug "andreyorst/plug.kak" branch "dev" noload config %{
    hook global WinSetOption filetype=plug %{
        remove-highlighter buffer/numbers
        remove-highlighter buffer/matching
        remove-highlighter buffer/wrap
        remove-highlighter buffer/show-whitespaces
    }
}

plug "andreyorst/kakoune-snippet-collection"
plug "delapouite/kakoune-text-objects"
plug "occivink/kakoune-vertical-selection"
plug "occivink/kakoune-sudo-write"
plug "occivink/kakoune-find"

plug "andreyorst/base16-gruvbox.kak" theme %{
    colorscheme base16-gruvbox-dark-soft
}

plug "andreyorst/fzf.kak" %{
    map -docstring 'fzf mode' global normal '<c-p>' ': fzf-mode<ret>'
    set-option global fzf_preview_width '65%'
    set-option global fzf_project_use_tilda true
    evaluate-commands %sh{
        if [ -n "$(command -v fd)" ]; then
            echo "set-option global fzf_file_command %{fd . --no-ignore --type f --follow --hidden --exclude .git --exclude .svn}"
        else
            echo "set-option global fzf_file_command %{find . \( -path '*/.svn*' -o -path '*/.git*' \) -prune -o -type f -follow -print}"
        fi
        [ -n "$(command -v bat)" ] && echo "set-option global fzf_highlight_cmd bat"
        [ -n "${kak_opt_grepcmd}" ] && echo "set-option global fzf_sk_grep_command %{${kak_opt_grepcmd}}"
    }
}

plug "ul/kak-lsp" do %{
    cargo build --release --locked
    cargo install --force --path .
} config %{
    define-command lsp-restart %{ lsp-stop; lsp-start }
    set-option global lsp_completion_trigger "execute-keys 'h<a-h><a-k>\S[^\h\n,=;*(){}\[\]]\z<ret>'"
    set-option global lsp_diagnostic_line_error_sign "!"
    set-option global lsp_diagnostic_line_warning_sign "?"
    hook global WinSetOption filetype=(c|cpp|rust) %{
        map window user "l" ": enter-user-mode lsp<ret>" -docstring "LSP mode"
        lsp-enable-window
        lsp-auto-hover-enable
        lsp-auto-hover-insert-mode-disable
        set-option window lsp_hover_anchor true
        set-face window DiagnosticError default+u
        set-face window DiagnosticWarning default+u
    }
    hook global WinSetOption filetype=rust %{
        set-option window lsp_server_configuration rust.clippy_preference="on"
    }
    hook global KakEnd .* lsp-exit
}

plug "andreyorst/powerline.kak" %{
    set-option global powerline_ignore_warnings true
    set-option global powerline_format 'git bufname smarttab mode_info filetype client session position'
    hook -once global WinDisplay .* %{
        powerline-theme base16-gruvbox
    }
}

plug "andreyorst/smarttab.kak" %{
    set-option global softtabstop 4
    hook global WinSetOption filetype=(rust|markdown|kak|lisp|scheme|sh|perl) expandtab
    hook global WinSetOption filetype=(makefile|gas) noexpandtab
    hook global WinSetOption filetype=(c|cpp) smarttab
}

plug "alexherbo2/auto-pairs.kak" %{
    map global user 's' ': auto-pairs-surround<ret>' -docstring "surround selection"
}

plug "alexherbo2/replace.kak" config %{
    map global user r -docstring 'Replace mode' ':<space>replace<ret>'
}

plug "alexherbo2/move-line.kak" config %{
    map global normal '<a-up>'   ': move-line-above %val{count}<ret>'
    map global normal '<a-down>' ': move-line-below %val{count}<ret>'
}

plug "occivink/kakoune-snippets" branch "auto-discard" config %{
    set-option -add global snippets_directories "%opt{plug_install_dir}/kakoune-snippet-collection/snippets"
    set-option global snippets_auto_expand false
    map global insert '<ret>' "<a-;>: expand-or-jump-or-key ret<ret>"
    map global normal '<ret>' ":      expand-or-jump-or-key ret<ret>"

    define-command -docstring "expand-or-jump-or-key <key>: expand snippet or jump to the placeholder or execute <key>" \
    expand-or-jump-or-key -params 1 %{
        try %{ snippets-expand-trigger %{
            set-register / "%opt{snippets_triggers_regex}\z"
            execute-keys 'hGhs<ret>'
        }} catch %{
            snippets-select-next-placeholders
        } catch %sh{
            printf "%s\n" "execute-keys -with-hooks <$1>"
        }
    }
}

plug "andreyorst/tagbar.kak" config %{
    set-option global tagbar_sort false
    set-option global tagbar_size 40
    set-option global tagbar_display_anon false
    map global user 't' ": tagbar-toggle<ret>" -docstring "toggle tagbar panel"
    hook global WinSetOption filetype=(c|cpp|rust|gas|markdown) %{
        tagbar-enable
    }
    hook global WinSetOption filetype=tagbar %{
        remove-highlighter buffer/numbers
        remove-highlighter buffer/matching
        remove-highlighter buffer/wrap
        remove-highlighter buffer/show-whitespaces
    }
}

plug "alexherbo2/word-movement.kak" config %{
    word-movement-map next w
    word-movement-map previous b
}

plug "occivink/kakoune-expand" config %{
    declare-user-mode expand
    map -docstring "expand selection" global expand "e"       ": expand<ret>"
    map -docstring "expand selection" global object "e"       "<esc>: expand; enter-user-mode -lock expand<ret>"
    set-option -add global expand_commands 'execute-keys <a-i>w' # select word if possible
    set-option -add global expand_commands 'execute-keys <a-i>q' # select inside single quotes
    set-option -add global expand_commands 'execute-keys <a-a>q' # select around single quotes
    set-option -add global expand_commands 'execute-keys <a-i>Q' # select inside double quotes
    set-option -add global expand_commands 'execute-keys <a-a>Q' # select around double quotes
    set-option -add global expand_commands 'execute-keys <a-i>g' # select inside backticks
    set-option -add global expand_commands 'execute-keys <a-a>g' # select around graves
    set-option -add global expand_commands 'execute-keys <a-a>a' # select around angle block
}

plug "alexherbo2/yank-ring.kak" config %{
    map -docstring "yank-ring" global user 'Y' ': yank-ring<ret>'
}

plug "alexherbo2/split-object.kak" config %{
    map -docstring "split object" global normal '<a-I>' ': enter-user-mode split-object<ret>'
}

plug "screwtapello/kakoune-inc-dec" domain "GitLab.com" config %{
    map -docstring "decrement selection" global normal '<C-x>' ': inc-dec-modify-numbers - %val{count}<ret>'
    map -docstring "increment selection" global normal '<C-a>' ': inc-dec-modify-numbers + %val{count}<ret>'
}
