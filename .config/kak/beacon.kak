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

declare-option -docstring "color of background that beacon will fade to." \
str beacon_final_bg "32302F"

declare-option -docstring "interval between color transitions of beacon" \
str beacon_refresh_interval "0.03"

declare-option -hidden bool beacon__in_process false
declare-option -hidden str beacon__transition %sh{
    bg=$kak_opt_beacon_final_bg
    while true; do
        bg=$(echo "obase=16; ibase=16; $bg+222222" | bc)
        if [ ${#bg} -eq 6 ] && [ "$bg" != "FFFFFF" ]; then
            colors="rgb:$bg $colors"
        else
            break
        fi
    done
    printf "%s\n" "$colors"
}

define-command beacon %{ nop %sh{ (
    printf "%s\n" "evaluate-commands -client $kak_client %{ try %{
                       add-highlighter buffer/beacon group
                   }}" | kak -p $kak_session

    for color in $kak_opt_beacon__transition; do
        printf "%s\n" "
            evaluate-commands -client $kak_client %{
                eval -draft %{
                    exec <a-l>
                    declare-option range-specs beacon_range %val{timestamp} \"%val{selection_desc}|default,$color\"
                }
            try %{ add-highlighter buffer/beacon/$color ranges beacon_range }
        }" | kak -p $kak_session
        sleep $kak_opt_beacon_refresh_interval
    done

    printf "%s\n" "evaluate-commands -client $kak_client %{
                       try %{ remove-highlighter buffer/beacon }
                   }" | kak -p $kak_session
) >/dev/null 2>&1 </dev/null & }}


if %[ -n "${PATH##*termux*}" ] %{
    hook global KakBegin .* %{
        hook global -group beacon FocusIn .* beacon
        hook global -group beacon WinDisplay .* beacon
    }
}
