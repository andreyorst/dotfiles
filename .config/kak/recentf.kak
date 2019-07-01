# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ recentf.kak      │
# ╞═════════════╩══════════════════╡
# │ Populates recentf file with    │
# │ recently edited files.         │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

hook global BufOpenFile .* recentf-add-file
hook global BufWritePost .* recentf-add-file

declare-option str recentf_file "%val{config}/.recentf"
declare-option int max_recentf_files 45

define-command -hidden recentf-add-file %{ nop %sh{
    if ! grep -q "${kak_buffile}" "${kak_opt_recentf_file}"; then
        printf "%s\n%s\n" "${kak_buffile}" "$(cat ${kak_opt_recentf_file})" > ${kak_opt_recentf_file}
        printf "%s\n" "$(head -${kak_opt_max_recentf_files} ${kak_opt_recentf_file})" > ${kak_opt_recentf_file}
    fi
}}

define-command recentf -params 1 -shell-script-completion %{ cat ${kak_opt_recentf_file} } %{ edit -existing %arg{1} }

# fzf.kak support
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
hook global ModuleLoaded fzf %§

map global fzf -docstring "recent files" 'r' '<esc>: fzf-recentf<ret>'

define-command -hidden fzf-recentf %{ evaluate-commands %sh{
    cmd="cat '${kak_opt_recentf_file}' 2>/dev/null"
    message="Open single or multiple recent files.
<ret>: open file in new buffer.
$kak_opt_fzf_window_map: open file in new terminal"
    [ ! -z "${kak_client_env_TMUX}" ] && tmux_keybindings="
$kak_opt_fzf_horizontal_map: open file in horizontal split
$kak_opt_fzf_vertical_map: open file in vertical split"

    printf "%s\n" "info -title 'fzf recentf' '$message$tmux_keybindings'"
    [ ! -z "${kak_client_env_TMUX}" ] && additional_flags="--expect $kak_opt_fzf_vertical_map --expect $kak_opt_fzf_horizontal_map"
    printf "%s\n" "fzf -preview -kak-cmd %{edit -existing} -items-cmd %{$cmd} -fzf-args %{-m --expect $kak_opt_fzf_window_map $additional_flags --reverse}"
}}

§
