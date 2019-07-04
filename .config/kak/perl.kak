try %{ require-module perl }
try %{ require-module kak }

add-highlighter shared/kakrc/code/perl_commands regex '(^|\h)define-perl-command\h' 0:keyword
add-highlighter shared/kakrc/perl_command1 region -recurse '\{' 'define-perl-command\h.*?\K%\{' '\}' ref perl
add-highlighter shared/kakrc/perl_command2 region -recurse '\(' 'define-perl-command\h.*?\K%\(' '\)' ref perl
add-highlighter shared/kakrc/perl_command3 region -recurse '\[' 'define-perl-command\h.*?\K%\[' '\]' ref perl
add-highlighter shared/kakrc/perl_command4 region -recurse '<'  'define-perl-command\h.*?\K%<'  '>'  ref perl

define-command define-perl-command -params 1.. %§ evaluate-commands %sh{
    # filtering arguments
    while [ $# -gt 0 ]; do
        case $1 in
            -docstring)
                shift
                docstring=$(printf "%s\n" "$1" | sed "s/&/&&/g")
                docstring="-docstring %&$docstring&" ;;
            -params)
                shift
                params="-params $1" ;;
            -shell-script-completion)
                shift
                completion=$(printf "%s\n" "$1" | sed "s/&/&&/g")
                completion="-shell-script-completion $&$completion&" ;;
            -shell-script-candidates)
                shift
                candidates=$(printf "%s\n" "$1" | sed "s/&/&&/g")
                candidates="-shell-script-candidates $&$candidates&" ;;
            -override|-hidden|-menu|-file-completion|-client-completion|-buffer-completion|-command-completion|-shell-completion)
                switches="$switches $1" ;;
            *)
                args="$args '$1'" ;;
        esac
        shift
    done

    eval "set -- $args"
    # at this point only name and body should be left as unhandled args
    if [ $# -ne 2 ]; then
        printf "%s\n" "fail %{'define-perl-command' wrong argument count}"
        exit
    fi

    command_name="$1"
    shift

    # creating command body file
    tmp=$(mktemp "${TMPDIR:-/tmp}/kakoune_perl_cmd_${command_name}.XXXXXXXXX")
    printf "%s\n" "$1" >> "$tmp"

    # extracting kakoune variables from command body
    vars=$(grep -o 'kak_\w*' $tmp | uniq)

    # creating body of the command
    printf "%s\n" "
        define-command $switches $docstring $command_name $completion $candidates $params %{
            evaluate-commands %sh{
                # $vars
                perl $tmp \$@
            }
        }
        hook global -always KakEnd .* %{ nop %sh{ rm $tmp }}
    "
}§

define-perl-command -docstring "docstring supported" -override test2 -params .. %{
    my $variable = 'test';
    my $env_variable = $ENV{"kak_bufname"};
    my $parameter = $ARGV[1];
    print("echo %{$variable, $env_variable, $parameter}\n");
}
