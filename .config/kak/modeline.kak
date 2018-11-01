# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ modeline.kak     │
# ╞═════════════╩══════════════════╡
# │ Custom modeline for Kakoune    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

# Options
declare-option -hidden str modeline_separator_left ''
declare-option -hidden str modeline_separator_left_thin ''
declare-option -hidden str modeline_separator_right ''
declare-option -hidden bool modeline_bidirectional_separators false

declare-option -hidden str modeline_pos_percent
declare-option -hidden str modeline_git_branch
declare-option -hidden str modeline_readonly

declare-option bool modeline_module_git true
declare-option bool modeline_module_bufname true
declare-option bool modeline_module_line_column true
declare-option bool modeline_module_mode_info true
declare-option bool modeline_module_filetype true
declare-option bool modeline_module_client true
declare-option bool modeline_module_session true
declare-option bool modeline_module_position true

# Commands
define-command -override -hidden \
modeline-update-position %{ evaluate-commands %sh{
    echo "set-option window modeline_pos_percent $(($kak_cursor_line * 100 / $kak_buf_line_count))%"
}}

define-command -override -hidden \
modeline-update-readonly %{ set-option global modeline_readonly %sh{
    if [ -w ${kak_buffile} ]; then
        echo ''
    else
        echo ' '
    fi
}}

define-command -override -hidden \
modeline-update-branch %{ set-option global modeline_git_branch %sh{
    branch=$(cd "${kak_buffile%/*}" 2>/dev/null && git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if [ ! -z $branch ]; then
        echo "$kak_opt_modeline_separator_left_thin $branch "
    else
        echo ""
    fi
}}

# Hooks
hook global WinCreate .* %{
    modeline-update-position
    hook window NormalKey (j|k) modeline-update-position
    hook window NormalIdle .* modeline-update-position
}

hook global WinDisplay .* %{modeline-update-branch; modeline-update-readonly}

# Modeline
define-command -override -docstring "Build modeline with new separators" \
modeline-rebuild %{
        set-option global modelinefmt %sh{
        bg0="rgb:282828"
        bg1="rgb:3c3836"
        bg2="rgb:504945"
        bg3="rgb:665c54"
        bg4="rgb:7c6f64"

        fg0="rgb:fbf1c7"
        fg1="rgb:ebdbb2"
        fg2="rgb:d5c4a1"
        fg3="rgb:bdae93"
        fg4="rgb:a89984"

        left=$kak_opt_modeline_separator_left
        if [ "$kak_opt_modeline_bidirectional_separators" = "true" ]; then
            right1=$kak_opt_modeline_separator_right
            right2=$right1
        else
            right1="{$bg2,$fg4}$kak_opt_modeline_separator_left{$bg2}"
            right2="{$bg1,$bg2}$kak_opt_modeline_separator_left{$bg2}"
        fi

        if [ "$kak_opt_modeline_module_git" = "true" ]; then
            git="{$fg3}%opt{modeline_git_branch} "
            next_bg=$bg2
            next_fg=$bg1
        fi
        if [ "$kak_opt_modeline_module_bufname" = "true" ]; then
            bufname="{$fg4}$left{$bg0,$fg4} %val{bufname}{{context_info}}%opt{modeline_readonly} "
            next_bg=$bg2
            next_fg=$fg4
        fi
        if [ "$kak_opt_modeline_module_line_column" = "true" ]; then
            line_column="{${next_fg:-$bg1},${next_bg:-$bg2}}$right1{default,$bg2} {$fg2,$bg2}%val{cursor_line}{$fg2,$bg2}:{$fg2,$bg2}%val{cursor_char_column} "
            next_bg=$bg2
            next_fg=$bg2
        fi
        if [ "$kak_opt_modeline_module_mode_info" = "true" ]; then
            mode_info="{${next_fg:-default},default}$right2 {{mode_info}} "
            next_bg=default
        fi
        if [ "$kak_opt_modeline_module_filetype" = "true" ]; then
            filetype="{$bg2,$next_bg}$left{$fg2,$bg2} %opt{filetype} "
            next_bg=$bg2
        fi
        if [ "$kak_opt_modeline_module_client" = "true" ]; then
            client="{$bg3,$next_bg}$left{$fg1,$bg3} %val{client} "
            next_bg=$bg3
        fi
        if [ "$kak_opt_modeline_module_session" = "true" ]; then
            session="{$bg4,$next_bg}$left{$fg0,$bg4} %val{session} "
            next_bg=$bg4
        fi
        if [ "$kak_opt_modeline_module_position" = "true" ]; then
            position="{$fg4,$next_bg}$left{$bg0,$fg4} ≣ %opt{modeline_pos_percent} "
        fi

        echo "$git$bufname$line_column$mode_info$filetype$client$session$position "
    }
    modeline-update-branch
    modeline-update-readonly
}

define-command -override -docstring "change separators for modeline" \
modeline-separator -params 1 -shell-script-candidates %{ echo "arrow
curve
flame
triangle"} %{ evaluate-commands %sh{
    separator=$1
    case $separator in
    arrow)
        echo "set-option global modeline_separator_left ''"
        echo "set-option global modeline_separator_left_thin ''"
        echo "set-option global modeline_bidirectional_separators false"
        ;;
    curve)
        echo "set-option global modeline_separator_left ''"
        echo "set-option global modeline_separator_left_thin ''"
        echo "set-option global modeline_bidirectional_separators false"
        ;;
    triangle)
        echo "set-option global modeline_separator_left ''"
        echo "set-option global modeline_separator_left_thin ''"
        echo "set-option global modeline_separator_right ''"
        echo "set-option global modeline_bidirectional_separators true"
        ;;
    flame)
        echo "set-option global modeline_separator_left ''"
        echo "set-option global modeline_separator_left_thin ''"
        echo "set-option global modeline_bidirectional_separators false"
        ;;
    *)
        ;;
    esac
    echo "modeline-rebuild"
}}

modeline-rebuild

