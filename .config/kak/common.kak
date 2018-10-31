# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ common.kak       │
# ╞═════════════╩══════════════════╡
# │ Common settings for Kakoune    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

# Common options
set-option global scrolloff 3,3
evaluate-commands %sh{
    [ ! -z $(command -v rg) ] && echo "set-option global grepcmd 'rg -L --with-filename --column'"
}
set-option global tabstop 4
set-option global indentwidth 4

hook global WinCreate .* %{
    update-statusline-pos-percent
    hook window NormalKey (j|k) update-statusline-pos-percent
    hook window NormalIdle .* update-statusline-pos-percent
}

hook global FocusIn .* %{update-statusline-git-branch}
hook global WinDisplay .* %{update-statusline-git-branch}

# UI
colorscheme base16-guvbox-dark-soft
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

    echo "{$fg2}%opt{statusline_git_branch}{$fg4}{$bg0,$fg4} %val{bufname}{{context_info}} {$fg4,$bg2}{default,$bg2} {$fg2,$bg2}%val{cursor_line}{$fg2,$bg2},{$fg2,$bg2}%val{cursor_char_column} {$bg2,default} {{mode_info}} {$bg2}{$fg2,$bg2} %opt{filetype} {$bg3,$bg2}{$fg1,$bg3} %val{client} {$bg4,$bg3}{$fg0,$bg4} %val{session} {$fg4,$bg4}{$bg0,$fg4} %opt{statusline_pos_percent} "

#{$fg3,$bg4}{$bg4,$fg3}  "
}

# Highlighters
hook global KakBegin .* %{
    add-highlighter global/numbers number-lines -relative -hlcursor -separator '  '
    add-highlighter global/matching show-matching
    add-highlighter global/whitespace show-whitespaces -tab "▏" -lf " " -nbsp "⋅" -spc " "
    add-highlighter global/wrap wrap -word -indent -marker ↪
}

# Maps
map global normal ''     ': comment-line<ret>'               -docstring "<c-/> to comment/uncomment selection"
map global normal '<c-r>' 'U'                                 -docstring "vim-like redo"
map global goto   '<a-f>' '<esc><a-i><a-w>gf'                 -docstring "file non-recursive"
map global goto   'f'     '<esc>: smart-gf<ret>'              -docstring "file recursive"
map global goto   'b'     '<esc>:bn<ret>'                     -docstring "next buffer"
map global goto   'B'     '<esc>:bp<ret>'                     -docstring "previous buffer"
map global normal '<c-d>' ': select-or-add-cursor<ret>'       -docstring "add currsor on current word, and jump to the next match"
map global user   't'     ': leading-spaces-to-tabs<ret>'     -docstring "convert leading spaces to tabs"
map global user   'T'     ': leading-tabs-to-spaces<ret>'     -docstring "convert leading tabs to spaces"

# Hooks
hook global InsertCompletionShow .* %{ map   window insert <tab> <c-n>; map   window insert <s-tab> <c-p> }
hook global InsertCompletionHide .* %{ unmap window insert <tab> <c-n>; unmap window insert <s-tab> <c-p> }
hook global BufOpenFile .* editorconfig-load
hook global BufNewFile  .* editorconfig-load

# Aliases
alias global h doc
