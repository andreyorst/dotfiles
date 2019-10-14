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
plug "andreyorst/plug.kak" noload config %{
    set-option global plug_always_ensure true
    set-option global plug_profile true
    hook global WinSetOption filetype=plug %{
        remove-highlighter buffer/numbers
        remove-highlighter buffer/matching
        remove-highlighter buffer/wrap
        remove-highlighter buffer/show-whitespaces
    }
}

plug "delapouite/kakoune-text-objects"
plug "occivink/kakoune-vertical-selection"
plug "occivink/kakoune-sudo-write"
plug "occivink/kakoune-find" config %{
    define-command -docstring "grep-apply-changes: apply changes specified in current *grep* buffer to their respective files" \
    grep-apply-changes %{ find-apply-changes -force }
}

plug "andreyorst/base16-gruvbox.kak" theme %{
    if %[ -n "${PATH##*termux*}" ] %{
        colorscheme base16-gruvbox-dark-soft
    } else %{
        colorscheme base16-gruvbox-dark-hard
    }
}

plug "andreyorst/fzf.kak" config %{
    map -docstring 'fzf mode' global normal '<c-p>' ': fzf-mode<ret>'
} defer fzf %{
    set-option global fzf_preview_width '65%'
    set-option global fzf_project_use_tilda true
    declare-option str-list fzf_exclude_files "*.o" "*.bin" "*.obj" ".*cleanfiles"
    declare-option str-list fzf_exclude_dirs ".git" ".svn" "rtlrun*"
    set-option global fzf_file_command %sh{
        if [ -n "$(command -v fd)" ]; then
            eval "set -- $kak_quoted_opt_fzf_exclude_files $kak_quoted_opt_fzf_exclude_dirs"
            while [ $# -gt 0 ]; do
                exclude="$exclude --exclude '$1'"
                shift
            done
            cmd="fd . --no-ignore --type f --follow --hidden $exclude"
        else
            eval "set -- $kak_quoted_opt_fzf_exclude_files"
            while [ $# -gt 0 ]; do
                exclude="$exclude -name '$1' -o"
                shift
            done
            eval "set -- $kak_quoted_opt_fzf_exclude_dirs"
            while [ $# -gt 1 ]; do
                exclude="$exclude -path '*/$1' -o"
                shift
            done
            exclude="$exclude -path '*/$1'"
            cmd="find . \( $exclude \) -prune -o -type f -follow -print"
        fi
        echo "$cmd"
    }
    if %[ -n "$(command -v bat)" ] %{
        set-option global fzf_highlight_command bat
    }
    if %[ -n "${kak_opt_grepcmd}" ] %{
        set-option global fzf_sk_grep_command %{${kak_opt_grepcmd}}
    }
}

if %[ -n "${PATH##*termux*}" ] %{
    plug "ul/kak-lsp" do %{
        cargo install --force --path . --locked
        cargo clean
    } config %{
        define-command lsp-restart %{ lsp-stop; lsp-start }
        set-option global lsp_completion_trigger "execute-keys 'h<a-h><a-k>\S[^\s,=;*(){}\[\]]\z<ret>'"
        set-option global lsp_diagnostic_line_error_sign "!"
        set-option global lsp_diagnostic_line_warning_sign "?"
        hook global WinSetOption filetype=(c|cpp|rust) %{
            map window user "l" ": enter-user-mode lsp<ret>" -docstring "LSP mode"
            map window lsp "n" "<esc>: lsp-find-error --include-warnings<ret>" -docstring "find next error or warning"
            map window lsp "p" "<esc>: lsp-find-error --previous --include-warnings<ret>" -docstring "find previous error or warning"
            lsp-enable-window
            hook -always global KakEnd .* lsp-exit
            lsp-auto-hover-enable
            lsp-auto-hover-insert-mode-disable
            set-option window lsp_hover_anchor true
            set-face window DiagnosticError default+u
            set-face window DiagnosticWarning default+u
        }
        hook global WinSetOption filetype=rust %{
            set-option window lsp_server_configuration rust.clippy_preference="on"
        }
    }
} else %{
    hook global WinSetOption filetype=(c|cpp) %{
        clang-find-and-parse-compile-flags
        clang-enable-autocomplete
        clang-enable-diagnostics
        hook window BufWritePre .* %{ clang-parse }
        hook window InsertEnd .* %{ clang-parse }
        map -docstring "next diagnostics error" window goto n '<esc>: clang-diagnostics-next<ret>'
    }
    if %[ -n "$(command -v racer)" ] %{
        hook global WinSetOption filetype=rust %{
            racer-enable-autocomplete
            map -docstring "go to definition" window goto d '<esc>: racer-go-definition<ret>'
        }
    }
}

plug "andreyorst/powerline.kak" defer powerline %{
    set-option global powerline_ignore_warnings true
    set-option global powerline_format 'git bufname langmap smarttab mode_info filetype client session position'
    set-option global powerline_shorten_bufname 'short'
    if %[ ! -n "${PATH##*termux*}" ] %{
        set-option global powerline_separator ''
        set-option global powerline_separator_thin ''
    }
    powerline-theme base16-gruvbox
} config %{
    powerline-start
}

plug "andreyorst/smarttab.kak" defer smarttab %{
    set-option global softtabstop 4
    set-option global smarttab_expandtab_mode_name '⋅t⋅'
    set-option global smarttab_noexpandtab_mode_name '→t→'
    set-option global smarttab_smarttab_mode_name '→t⋅'
} config %{
    hook global WinSetOption filetype=(rust|markdown|kak|lisp|scheme|sh|perl) expandtab
    hook global WinSetOption filetype=(makefile|gas) noexpandtab
    hook global WinSetOption filetype=(c|cpp) smarttab
}

plug "alexherbo2/auto-pairs.kak" %{
    map global user 's' ': auto-pairs-surround<ret>' -docstring "surround selection"
    hook global WinCreate .* auto-pairs-enable
}

plug "alexherbo2/replace.kak" config %{
    map global user r -docstring 'Replace mode' ':<space>replace<ret>'
}

plug "alexherbo2/move-line.kak" config %{
    map global normal '<a-up>'   ': move-line-above %val{count}<ret>'
    map global normal '<a-down>' ': move-line-below %val{count}<ret>'
}

if %[ -n "${PATH##*termux*}" ] %{
    plug "andreyorst/tagbar.kak" defer tagbar %{
        set-option global tagbar_sort false
        set-option global tagbar_size 40
        set-option global tagbar_display_anon false
        set-option global tagbar_powerline_format ""
    } config %{
        map global user 't' ": tagbar-toggle<ret>" -docstring "toggle tagbar panel"
        hook global WinSetOption filetype=(c|cpp|rust|gas) %{
            tagbar-enable
        }
        hook global WinSetOption filetype=tagbar %{
            remove-highlighter buffer/numbers
            remove-highlighter buffer/matching
            remove-highlighter buffer/wrap
            remove-highlighter buffer/show-whitespaces
        }
    }
}

plug "alexherbo2/word-movement.kak" config %{
    word-movement-map next w
    word-movement-map previous b
}

plug "alexherbo2/yank-ring.kak" config %{
    map -docstring "yank-ring" global user 'Y' ': yank-ring<ret>'
}

plug "alexherbo2/split-object.kak" config %{
    map -docstring "split object" global normal '<a-I>' ': enter-user-mode split-object<ret>'
}

plug "screwtapello/kakoune-inc-dec" domain GitLab.com config %{
    map -docstring "decrement selection" global normal '<C-x>' ': inc-dec-modify-numbers - %val{count}<ret>'
    map -docstring "increment selection" global normal '<C-a>' ': inc-dec-modify-numbers + %val{count}<ret>'
}

plug "andreyorst/langmap.kak" defer langmap %{
    set-option global langmap %opt{langmap_ru_jcuken}
    map -docstring "toggle layout (C-\)" global normal '' ':      toggle-langmap<ret>'
    map -docstring "toggle layout (C-\)" global insert '' '<a-;>: toggle-langmap<ret>'
    map -docstring "toggle layout (C-\)" global prompt '' '<a-;>: toggle-langmap prompt<ret>'
}

plug "delapouite/kakoune-select-view" %{
    map global normal <a-%> ': select-view<ret>' -docstring 'select view'
    map global view s '<esc>: select-view<ret>' -docstring 'select view'
}

plug "andreyorst/kaktree" defer kaktree %{
    map global user 'f' ": kaktree-toggle<ret>" -docstring "toggle filetree panel"
    if %[ -n "${PATH##*termux*}" ] %{
        set-option global kaktree_double_click_duration '0.5'
        set-option global kaktree_indentation 1
        set-option global kaktree_dir_icon_open  '▾ 🗁 '
        set-option global kaktree_dir_icon_close '▸ 🗀 '
        set-option global kaktree_file_icon      '⠀⠀🖺'
    }
} config %{
    map global user 'f' ": kaktree-enable<ret>" -docstring "enable filetree panel"
    hook global WinSetOption filetype=kaktree %{
        remove-highlighter buffer/numbers
        remove-highlighter buffer/matching
        remove-highlighter buffer/wrap
        remove-highlighter buffer/show-whitespaces
    }
}

plug "occivink/kakoune-gdb"

plug "KJ_Duncan/kakoune-racket.kak" domain "bitbucket.org" config %{
    hook global WinSetOption filetype=racket %{ require-module lisp }
}

plug "eraserhd/parinfer-rust" do %{
    cargo install --force --path . --locked
    cargo clean
} config %{
    hook -group parinfer global WinSetOption filetype=(clojure|lisp|scheme|racket) %{
        require-module parinfer
        parinfer -if-enabled -paren
        hook -group parinfer window NormalKey .* %{ parinfer -if-enabled -smart }
        hook -group parinfer window InsertChar (?!\n).* %{ parinfer -if-enabled -smart }
        hook -group parinfer window InsertDelete .* %{ parinfer -if-enabled -smart }
    }

    hook -group parinfer global WinSetOption filetype=(?!clojure)(?!lisp)(?!scheme)(?!racket).* %{
        remove-hooks window parinfer
    }
}
