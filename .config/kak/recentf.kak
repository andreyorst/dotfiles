hook global BufOpenFile .* recentf-add-file
hook global BufWritePost .* recentf-add-file

declare-option -docstring "Path to the file where to store recent file list." \
str recentf_file "%val{config}/.recentf"
declare-option -docstring "Maximum amount of entries in the recent file list." \
int max_recentf_files 45

define-command -hidden recentf-add-file %{ nop %sh{
    if grep -q "${kak_buffile:?}" "${kak_opt_recentf_file:?}"; then
        # store full recentf list without current entry
        old=$(grep -v "${kak_buffile}" "${kak_opt_recentf_file}")
        # move current entry to the beginning of the list
        printf "%s\n%s\n" "${kak_buffile}" "${old}" > "${kak_opt_recentf_file}"
    else
        # put current entry to the beginning of the list
        printf "%s\n%s\n" "${kak_buffile}" "$(cat "$kak_opt_recentf_file")" > "$kak_opt_recentf_file"
        # remove everyting past the `max_recentf_files' count
        printf "%s\n" "$(head -"${kak_opt_max_recentf_files:?}" "$kak_opt_recentf_file")" > "${kak_opt_recentf_file}"
    fi
}}

# unfortunately `-shell-script-candidates` does sorting to arguments thus list of recently edited files
# becomes sorted and recent files are no longer on the top. `-shell-script-completion' doesn't have this
# problem but it doesn't allow fuzzy matching through completion items, therefore it is recommended to keep
# sane amount of recent files to be able to use this comfortably.
define-command recentf -params 1 -shell-script-completion %{ cat "${kak_opt_recentf_file}" } %{ edit -existing %arg{1} }
alias global & recentf

# fzf.kak support
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
hook global ModuleLoaded fzf %§

define-command -hidden fzf-recentf %{ evaluate-commands %sh{
    cmd="cat ${kak_quoted_opt_recentf_file:?} 2>/dev/null"
    message="Open single or multiple recent files.
<ret>: open file in new buffer.
${kak_opt_fzf_window_map:?}: open file in new terminal"
    [ -n "${kak_client_env_TMUX:-}" ] && tmux_keybindings="
${kak_opt_fzf_horizontal_map:-}: open file in horizontal split
${kak_opt_fzf_vertical_map:-}: open file in vertical split"

    printf "%s\n" "info -title 'fzf recentf' '$message$tmux_keybindings'"
    [ -n "${kak_client_env_TMUX}" ] && additional_flags="--expect $kak_opt_fzf_vertical_map --expect $kak_opt_fzf_horizontal_map"
    printf "%s\n" "fzf -preview -kak-cmd %{edit -existing} -items-cmd %{$cmd} -fzf-args %{-m --expect $kak_opt_fzf_window_map $additional_flags --reverse}"
}}

map global fzf -docstring "recent files" 'r' '<esc>: fzf-recentf<ret>'

§
