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
declare-option str modeline_separator_left ''
declare-option str modeline_separator_left_thin ''
declare-option str modeline_separator_right ''
declare-option bool modeline_bidirectional_separators true

declare-option str modeline_pos_percent
declare-option str modeline_git_branch
declare-option str modeline_readonly

# Commands
define-command -override -hidden \
update-modeline-pos %{ evaluate-commands %sh{
    echo "set-option window modeline_pos_percent $(($kak_cursor_line * 100 / $kak_buf_line_count))%"
}}

define-command -override -hidden \
check-readonly %{ set-option global modeline_readonly %sh{
    if [ -w ${kak_buffile} ]; then
        echo ''
    else
        echo ' '
    fi
}}

define-command -override -hidden \
update-modeline-branch %{ set-option global modeline_git_branch %sh{
    branch=$(cd "${kak_buffile%/*}" 2>/dev/null && git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if [ ! -z $branch ]; then
        echo "$kak_opt_modeline_separator_left_thin $branch  "
    else
        echo ""
    fi
}}

# Hooks
hook global WinCreate .* %{
    update-modeline-pos
    hook window NormalKey (j|k) update-modeline-pos
    hook window NormalIdle .* update-modeline-pos
}

hook global WinDisplay .* %{update-modeline-branch; check-readonly}

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
        echo "{$fg3}%opt{modeline_git_branch}{$fg4}$left{$bg0,$fg4} %val{bufname}{{context_info}}%opt{modeline_readonly} {$fg4,$bg2}$right1{default,$bg2} {$fg2,$bg2}%val{cursor_line}{$fg2,$bg2}:{$fg2,$bg2}%val{cursor_char_column} {$bg2,default}$right2 {{mode_info}} {$bg2}$left{$fg2,$bg2} %opt{filetype} {$bg3,$bg2}$left{$fg1,$bg3} %val{client} {$bg4,$bg3}$left{$fg0,$bg4} %val{session} {$fg4,$bg4}$left{$bg0,$fg4} ≣ %opt{modeline_pos_percent} "
    }
    update-modeline-branch
    check-readonly
}

define-command -override -docstring "change separators for modeline" \
modeline-separator -params 1 -shell-script-candidates %{ echo "arrow
curve
flame
triangle"} %{ evaluate-commands %sh{
    separator=$1
    case $separator in
    arrow)
        echo "set-option window modeline_separator_left ''"
        echo "set-option window modeline_separator_left_thin ''"
        echo "set-option window modeline_bidirectional_separators false"
        ;;
    curve)
        echo "set-option window modeline_separator_left ''"
        echo "set-option window modeline_separator_left_thin ''"
        echo "set-option window modeline_bidirectional_separators false"
        ;;
    triangle)
        echo "set-option window modeline_separator_left ''"
        echo "set-option window modeline_separator_left_thin ''"
        echo "set-option window modeline_separator_right ''"
        echo "set-option window modeline_bidirectional_separators true"
        ;;
    flame)
        echo "set-option window modeline_separator_left ''"
        echo "set-option window modeline_separator_left_thin ''"
        echo "set-option window modeline_bidirectional_separators false"
        ;;
    *)
        ;;
    esac
    echo "modeline-rebuild"
}}

modeline-rebuild

