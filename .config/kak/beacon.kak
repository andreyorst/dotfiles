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

declare-option str beacon_final_bg "32302F"
declare-option str beacon_delay "0.03"
declare-option str beacon_transition %sh{
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

define-command -override beacon %{ nop %sh{ (
    for color in $kak_opt_beacon_transition; do
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
    for color in $kak_opt_beacon_transition; do
        printf "%s\n" "evaluate-commands -client $kak_client %{ remove-highlighter buffer/beacon-$color }" | kak -p $kak_session
    done
) >/dev/null 2>&1 </dev/null & }}

map global user b ": beacon<ret>" -docstring "beacon"
