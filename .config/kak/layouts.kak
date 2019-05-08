# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ layouts.kak      │
# ╞═════════════╩══════════════════╡
# │ Insert mode layout switcher    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

declare-option -hidden str langmap "us"

define-command -docstring "toggle-layout: toggle between keyboard layouts in insert mode only" \
toggle-layout -params 1 %{ evaluate-commands %sh{
    export map_mode=$1
    perl -Mutf8 -CS -e 'use strict;
        use utf8;
        my $mode = $ENV{map_mode};
        my $us_qwerty = q{`~@#$^&|qQwWeErRtTyYuUiIoOpP[{]}aAsSdDfFgGhHjJkKlL;:''"zZxXcCvVbBnNmM,<.>/?};
        my $ru_jcuken = q{ёЁ"№;:?/йЙцЦуУкКеЕнНгГшШщЩзЗхХъЪфФыЫвВаАпПрРоОлЛдДжЖэЭяЯчЧсСмМиИтТьЬбБюЮ.,};
        my $map;

        if ($ENV{kak_opt_langmap} eq "us") {
            $map = "map";
            print "set-option global langmap ru\n";
        } else {
            $map = "unmap";
            print "set-option global langmap us\n";
        }

        for my $key (split //, $us_qwerty) {
            $_ = $key;
            eval sprintf "tr/%s/%s/", map quotemeta, $us_qwerty, $ru_jcuken;
            print "$map buffer $mode -- %🦀$key🦀 %🦀$_🦀\n";
        }'
}}

map global normal '' ': toggle-layout insert<ret>'
map global insert '' '<a-;>: toggle-layout insert<ret>'
map global prompt '' '<a-;>: toggle-layout prompt<ret>'

