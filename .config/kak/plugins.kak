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

# bootstrap the plugin manager
evaluate-commands %sh{
    plugins="$HOME/.config/kak/plugins"
    mkdir -p $plugins
    [ ! -e "$plugins/plug.kak" ] && \
        git clone -q https://gitlab.com/andreyorst/plug.kak.git "$plugins/plug.kak"
    printf "%s\n" "source '$plugins/plug.kak/rc/plug.kak'"
}

# Plugin configurations
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
plug "andreyorst/plug.kak" domain gitlab.com noload config %{
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

plug "andreyorst/base16-gruvbox.kak" domain gitlab.com theme %{
    if %[ -n "${PATH##*termux*}" ] %{
        colorscheme base16-gruvbox-dark-soft
    } else %{
        colorscheme base16-gruvbox-dark-hard
    }
}

plug "andreyorst/fzf.kak" domain gitlab.com config %{
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
            while [ $# -gt 0 ]; do
                exclude="$exclude -path '*/$1' -o"
                shift
            done
            cmd="find . \( ${exclude% -o} \) -prune -o -type f -follow -print"
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

plug "andreyorst/powerline.kak" domain gitlab.com defer powerline %{
    set-option global powerline_ignore_warnings true
    set-option global powerline_format 'git bufname langmap smarttab mode_info filetype client session position'
    set-option global powerline_shorten_bufname 'short'
    set-option global powerline_separator '║'
    set-option global powerline_separator_thin ''
    if %[ ! -n "${PATH##*termux*}" ] %{
        set-option global powerline_separator ''
        set-option global powerline_separator_thin ''
    }
    powerline-theme base16-gruvbox
} config %{
    powerline-start
}

plug "andreyorst/smarttab.kak" domain gitlab.com defer smarttab %{
    set-option global softtabstop 4
    set-option global smarttab_expandtab_mode_name   '⋅t⋅'
    set-option global smarttab_noexpandtab_mode_name '→t→'
    set-option global smarttab_smarttab_mode_name    '→t⋅'
} config %{
    hook global WinSetOption filetype=(rust|markdown|kak|lisp|scheme|sh|perl) expandtab
    hook global WinSetOption filetype=(makefile|gas) noexpandtab
    hook global WinSetOption filetype=(c|cpp) smarttab
}

plug "alexherbo2/auto-pairs.kak" %{
    hook global WinCreate .* auto-pairs-enable
}

plug "alexherbo2/surround.kak" %{
    map global user 's' ': surround<ret>' -docstring "surround selection"
    set-option global surround_begin auto-pairs-disable
    set-option global surround_end auto-pairs-enable
}

plug "alexherbo2/replace.kak" config %{
    map global user r -docstring 'Replace mode' ':<space>replace<ret>'
}

if %[ -n "${PATH##*termux*}" ] %{
    plug "andreyorst/tagbar.kak" domain gitlab.com defer tagbar %{
        set-option global tagbar_sort false
        set-option global tagbar_size 40
        set-option global tagbar_display_anon false
        set-option global tagbar_powerline_format ""
    } config %{
        hook global WinSetOption filetype=tagbar %{
            remove-highlighter buffer/numbers
            remove-highlighter buffer/matching
            remove-highlighter buffer/wrap
            remove-highlighter buffer/show-whitespaces
        }
    }
}

plug "alexherbo2/word-select.kak" config %{
    map global normal w     ': word-select-next-word<ret>'
    map global normal <a-w> ': word-select-next-big-word<ret>'
    map global normal b     ': word-select-previous-word<ret>'
    map global normal <a-b> ': word-select-previous-big-word<ret>'
}

plug "alexherbo2/split-object.kak" config %{
    map -docstring "split object" global normal '<a-I>' ': enter-user-mode split-object<ret>'
}

plug "screwtapello/kakoune-inc-dec" domain gitlab.com config %{
    map -docstring "decrement selection" global normal '<C-x>' ': inc-dec-modify-numbers - %val{count}<ret>'
    map -docstring "increment selection" global normal '<C-a>' ': inc-dec-modify-numbers + %val{count}<ret>'
}

plug "andreyorst/langmap.kak" domain gitlab.com defer langmap %{
    set-option global langmap %opt{langmap_ru_jcuken}
    map -docstring "toggle layout (C-\)" global normal '' ':      toggle-langmap<ret>'
    map -docstring "toggle layout (C-\)" global insert '' '<a-;>: toggle-langmap<ret>'
    map -docstring "toggle layout (C-\)" global prompt '' '<a-;>: toggle-langmap prompt<ret>'
}

plug "andreyorst/kaktree" domain gitlab.com defer kaktree %{
    map global user 'f' ": kaktree-toggle<ret>" -docstring "toggle filetree panel"
    set-option global kaktree_show_help false
    if %[ -n "${PATH##*termux*}" ] %{
        set-option global kaktree_double_click_duration '0.5'
        set-option global kaktree_indentation 1
        set-option global kaktree_dir_icon_open  '▾ 🗁 '
        set-option global kaktree_dir_icon_close '▸ 🗀 '
        set-option global kaktree_file_icon      '⠀⠀🖺'
        # set-option global kaktree_dir_icon_open  ''
        # set-option global kaktree_dir_icon_close ''
        # set-option global kaktree_file_icon      ''
    } else %{
        set-option global kaktree_split vertical
        set-option global kaktree_size 30%
    }
} config %{
    hook global WinSetOption filetype=kaktree %{
        remove-highlighter buffer/numbers
        remove-highlighter buffer/matching
        remove-highlighter buffer/wrap
        remove-highlighter buffer/show-whitespaces
    }
    kaktree-enable
}

plug "listentolist/kakoune-table" domain "gitlab.com"

plug "eraserhd/parinfer-rust" do %{
    cargo install --force --path . --locked
    cargo clean
} config %<
    hook global WinSetOption filetype=(clojure|lisp|scheme|racket) %<
        parinfer-enable-window
        try %{ set-option buffer auto_pairs '"' '"' "'" "'" ` ` }

        # prefer Default color on all parens
        add-highlighter window/parinfer-parens regex [\[\](){}] 0:Default

        # highlight parens that are inferred by Parinfer
        hook window WinSetOption parinfer_current_mode=.+ %< evaluate-commands %sh<
            if [ ! "$kak_opt_parinfer_current_mode" = "paren" ]; then
                printf "%s\n" "remove-highlighter window/parinfer-inferred
                               add-highlighter window/parinfer-inferred regex [\])}]+\h*$ 0:comment"
            else
                printf "%s\n" "remove-highlighter window/parinfer-inferred
                               add-highlighter window/parinfer-inferred regex [\])}]+\h*$ 0:Default"
            fi
        >>
    >
>
