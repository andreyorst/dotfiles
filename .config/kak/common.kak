# Common options
    set-option global scrolloff 3,3
    set-option global grepcmd 'rg -L --with-filename --column'
    set-option global tabstop 4
    set-option global indentwidth 4

# UI
    colorscheme base16-guvbox-dark-soft
    set-option global ui_options ncurses_status_on_top=yes ncurses_assistant=none
    set-option global modelinefmt '{rgb:83a598}{rgb:3c3836,rgb:83a598+b} %val{bufname}{{context_info}} {default,rgb:3c3836} {{mode_info}} {rgb:83a598+b}%val{cursor_line}{default}:{rgb:83a598+b}%val{cursor_char_column} {rgb:83a598}{rgb:3c3836,rgb:83a598+b} %opt{filetype} {rgb:3c3836,rgb:83a598}{rgb:83a598} {rgb:83a598,default+b}%val{client}{default} at {magenta,default+b}[%val{session}] '

# Highlighters
    set-face global delimiters rgb:af3a03,default

    add-highlighter global/ number-lines -relative -hlcursor
    add-highlighter global/ show-matching
    add-highlighter global/ show-whitespaces -tab "▏" -lf " " -nbsp "⋅" -spc " "
    add-highlighter global/ wrap -word -indent -marker ↪

    # Highlight operators and delimiters
    add-highlighter global/ regex (\+|-|\*|=|\\|\?|%|\|-|!|\||->|\.|,|<|>|::|\^) 0:operator
    add-highlighter global/ regex (\(|\)|\[|\]|\{|\}|\;|') 0:delimiters

# Maps and hooks
    # maps <c-/> to comment/uncomment line
    map global normal '' :comment-line<ret>

    # tab-completion
    hook global InsertCompletionShow .* %{map   window insert <tab> <c-n>; map   window insert <s-tab> <c-p>}
    hook global InsertCompletionHide .* %{unmap window insert <tab> <c-n>; unmap window insert <s-tab> <c-p>}

    hook global NormalIdle .* %{nop %sh{command tmux rename-window ${kak_bufname##*/}}}
    hook global KakBegin .* %{nop %sh{command tmux set-option -g status-position top}}
    hook global KakEnd .* %{nop %sh{
    command tmux rename-window zsh
    command tmux set-option -g status-position bottom}}

# Aliases
    alias global vert tmux-new-vertical
    alias global vertical tmux-new-vertical
    alias global win tmux-new-window
    alias global window tmux-new-window
