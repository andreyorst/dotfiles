# gruvbox theme

%sh{
    bg0_h="rgb:1d2021"
    bg0="rgb:282828"
    bg0_s="rgb:32302f"
    bg1="rgb:3c3836"
    bg2="rgb:504945"
    bg3="rgb:665c54"
    bg4="rgb:7c6f64"
    bg4_256="rgb:7c6f64"

    gray_245="rgb:928374"
    gray_244="rgb:928374"

    fg0_h="rgb:f9f5d7"
    fg0="rgb:fbf1c7"
    fg0_s="rgb:f2e5bc"
    fg1="rgb:ebdbb2"
    fg2="rgb:d5c4a1"
    fg3="rgb:bdae93"
    fg4="rgb:a89984"
    fg4_256="rgb:a89984"

    # Normal Colors
    red="rgb:fb4934"
    green="rgb:b8bb26"
    yellow="rgb:fabd2f"
    blue="rgb:83a598"
    purple="rgb:d3869b"
    aqua="rgb:8ec07c"
    orange="rgb:fe8019"

    # Bright Colors
    d_red="rgb:cc241d"
    d_green="rgb:98971a"
    d_yellow="rgb:d79921"
    d_blue="rgb:458588"
    d_purple="rgb:b16286"
    d_aqua="rgb:689d6a"
    d_orange="rgb:d65d0e"

    # Faded Colors
    f_red="rgb:9d0006"
    f_green="rgb:79740e"
    f_yellow="rgb:b57614"
    f_blue="rgb:076678"
    f_purple="rgb:8f3f71"
    f_aqua="rgb:427b58"
    f_orange="rgb:af3a03"

    echo "
        # Custom faces
        set-face global Child ${red},default+b
        set-face global Condition ${purple},default
        set-face global Delimiters ${f_orange},default

        # Code highlighting
        face global value     ${orange}
        face global type      ${yellow}
        face global variable  ${blue}
        face global module    ${green}
        face global function  ${blue}
        face global string    ${green}
        face global keyword   ${purple}
        face global operator  ${aqua}
        face global attribute ${orange}
        face global comment   ${gray_245}
        face global meta      ${aqua}
        face global builtin   default+b

        # Markdown highlighting
        face global title     ${green}+b
        face global header    ${orange}
        face global bold      ${fg0}+b
        face global italic    ${fg1}
        face global mono      ${fg2}
        face global block     default
        face global link      default
        face global bullet    default
        face global list      default

        face global Default            ${fg0},${bg0_s}
        face global PrimarySelection   ${fg0},${blue}
        face global SecondarySelection ${bg0},${blue}
        face global PrimaryCursor      ${bg0},${fg0}
        face global SecondaryCursor    ${bg0},${fg0}
        face global PrimaryCursorEol   ${bg0},${fg2}
        face global SecondaryCursorEol ${bg0},${fg2}
        face global LineNumbers        ${gray_245},rgb:3c3836
        face global LineNumberCursor   ${fg0},${bg2}
        face global MenuForeground     ${bg2},${blue}
        face global MenuBackground     default,${bg2}
        face global StatusLine         default
        face global StatusLineMode     ${yellow}+b
        face global StatusLineInfo     ${purple}
        face global StatusLineValue    ${red}
        face global StatusCursor       ${bg0},${fg0}
        face global Prompt             ${yellow}
        face global MatchingChar       default+b
        face global BufferPadding      ${bg0_s},${bg0_s}
        face global Whitespace         ${bg2}
    "
}
