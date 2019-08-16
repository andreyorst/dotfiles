# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ beacon.kak       │
# ╞═════════════╩══════════════════╡
# │ Highlight cursor position with │
# │ smooth transition effect.      │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

declare-option -hidden int beacon__current_line 0
declare-option str beacon_final_bg "32302F"
declare-option str beacon_delay "0.03"
declare-option -hidden str beacon__transition %sh{
    bg=$kak_opt_beacon_final_bg
    while true; do
        bg=$(echo "obase=16; ibase=16; $bg+111111" | bc)
        if [ ${#bg} -eq 6 ] && [ "$bg" != "FFFFFF" ]; then
            colors="rgb:$bg $colors"
        else
            break
        fi
    done
    echo "$colors"
}

define-command beacon %{ nop %sh{ (
    for color in $kak_opt_beacon__transition; do
        printf "%s\n" "
            evaluate-commands -client $kak_client %{
                eval -draft %{
                    exec <a-l>
                    declare-option range-specs beacon_range %val{timestamp} \"%val{selection_desc}|default,$color\"
                }
            add-highlighter buffer/beacon-$color ranges beacon_range
        }" | kak -p $kak_session
        sleep $kak_opt_beacon_delay
    done
    for color in $kak_opt_beacon__transition; do
        printf "%s\n" "evaluate-commands -client $kak_client %{ remove-highlighter buffer/beacon-$color }" | kak -p $kak_session
    done
) >/dev/null 2>&1 </dev/null & }}

define-command beacon-jump %{
    evaluate-commands %sh{
        if [ $kak_cursor_line -gt $kak_opt_beacon__current_line ]; then
            [ $(($kak_cursor_line - $kak_opt_beacon__current_line)) -gt 10 ] && echo "beacon"
        else
            [ $(($kak_opt_beacon__current_line - $kak_cursor_line)) -gt 10 ] && echo "beacon"
        fi
    }
    set-option buffer beacon__current_line "%val{cursor_line}"
}

hook global -group beacon FocusIn .* beacon
hook global -group beacon WinDisplay .* beacon
hook global -group beacon NormalIdle .* beacon-jump
