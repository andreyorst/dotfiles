# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ recentf.kak      │
# ╞═════════════╩══════════════════╡
# │ Populates *scratch* buffer     │
# │ with recently edited files.    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

hook global BufOpenFile .* recentf-add-file
hook global BufWritePost .* recentf-add-file

declare-option str recentf_file "%val{config}/.recentf"
declare-option int max_recentf_files 15

define-command recentf-add-file %{ nop %sh{
    if ! grep -q "${kak_buffile}" "${kak_opt_recentf_file}"; then
        printf "%s\n" "${kak_buffile}" >> ${kak_opt_recentf_file}
        printf "%s\n" "$(tail -${kak_opt_max_recentf_files} ${kak_opt_recentf_file})" > ${kak_opt_recentf_file}
    fi
}}

define-command recentf-populate-scratch %{ evaluate-commands -buffer *scratch* %sh{
    printf "%s\n" "execute-keys -draft 'ggIWelcome back. Here''s your recently visited files:<ret>'"
    printf "%s\n" "execute-keys -draft %{!cat '$kak_opt_recentf_file'<ret>}"
}}

