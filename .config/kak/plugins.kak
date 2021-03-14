# bootstrap the plugin manager
evaluate-commands %sh{
    plugins="${kak_config:?}/plugins"
    mkdir -p "$plugins"
    [ ! -e "$plugins/plug.kak" ] && \
        git clone -q git@github.com:andreyorst/plug.kak.git --branch develop "$plugins/plug.kak"
    printf "%s\n" "source '$plugins/plug.kak/rc/plug.kak'"
}

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
plug "occivink/kakoune-find" config %{
    define-command -docstring "grep-apply-changes: apply changes specified in current *grep* buffer to their respective files" \
    grep-apply-changes %{ find-apply-changes -force }
}

plug "git@github.com:andreyorst/base16-gruvbox.kak" theme %{
    if %[ -n "${PATH##*termux*}" ] %{
        colorscheme base16-gruvbox-dark-soft
    } else %{
        colorscheme base16-gruvbox-dark-hard
    }
}

plug "git@github.com:andreyorst/fzf.kak" config %{
    map -docstring 'fzf mode' global normal '<c-p>' ': fzf-mode<ret>'
} defer fzf %{
    set-option global fzf_preview_width '65%'
    if %[ -n "$(command -v bat)" ] %{
        set-option global fzf_highlight_command bat
    }
} defer fzf-project %{
    set-option global fzf_project_use_tilda true
} defer fzf-file %{
    declare-option str-list fzf_exclude_files "*.o" "*.bin" "*.obj" ".*cleanfiles"
    declare-option str-list fzf_exclude_dirs ".git" ".svn"
    set-option global fzf_file_command %sh{
        if [ -n "$(command -v fd)" ]; then
            eval "set -- ${kak_quoted_opt_fzf_exclude_files:-} ${kak_quoted_opt_fzf_exclude_dirs:-}"
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
}

plug "git@github.com:andreyorst/powerline.kak" defer powerline %{
    set-option global powerline_ignore_warnings true
    set-option global powerline_format 'git bufname langmap smarttab mode_info filetype client session position'
    set-option global powerline_separator '║'
    set-option global powerline_separator_thin ''
} defer powerline_bufname %{
    set-option global powerline_shorten_bufname 'short'
} defer powerline_base16_gruvbox %{
    powerline-theme base16-gruvbox
} config %{
    powerline-start
}

plug "git@github.com:andreyorst/smarttab.kak" defer smarttab %{
    set-option global softtabstop 4
    set-option global smarttab_expandtab_mode_name   '⋅a⋅'
    set-option global smarttab_noexpandtab_mode_name '→a→'
    set-option global smarttab_smarttab_mode_name    '→a⋅'
} config %{
    hook global WinSetOption filetype=(rust|markdown|kak|lisp|scheme|sh|perl) expandtab
    hook global WinSetOption filetype=(makefile|gas) noexpandtab
    hook global WinSetOption filetype=(c|cpp) smarttab
}

plug "alexherbo2/surround.kak" demand surround %{
    map global user 's' ': enter-user-mode surround<ret>' -docstring "surround selection"
}

plug "alexherbo2/replace.kak" demand replace-mode %{
    map global user r -docstring 'Replace mode' ':<space>enter-replace-mode<ret>'
}

plug "alexherbo2/word-select.kak" demand word-select %{
    word-select-add-mappings
}

plug "git@github.com:andreyorst/langmap.kak" config %{
    set-option global langmap %opt{langmap_ru_jcuken}
} demand langmap %{
    map -docstring "toggle layout" global normal '<c-\>' ':      toggle-langmap<ret>'
    map -docstring "toggle layout" global insert '<c-\>' '<a-;>: toggle-langmap<ret>'
    map -docstring "toggle layout" global prompt '<c-\>' '<a-;>: toggle-langmap prompt<ret>'
}

plug "git@github.com:andreyorst/kaktree" defer kaktree %{
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

plug "git@github.com:andreyorst/pmanage.kak"
