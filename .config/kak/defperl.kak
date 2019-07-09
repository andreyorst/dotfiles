require-module perl
require-module kak

add-highlighter shared/kakrc/code/perl_commands regex '(^|\h)define-perl-command\h' 0:keyword
add-highlighter shared/kakrc/perl_command1 region -recurse '\{' 'define-perl-command\h.*?\K%\{' '\}' ref perl
add-highlighter shared/kakrc/perl_command2 region -recurse '\(' 'define-perl-command\h.*?\K%\(' '\)' ref perl
add-highlighter shared/kakrc/perl_command3 region -recurse '\[' 'define-perl-command\h.*?\K%\[' '\]' ref perl
add-highlighter shared/kakrc/perl_command4 region -recurse '<'  'define-perl-command\h.*?\K%<'  '>'  ref perl

define-command define-perl-command \
-docstring "define-perl-command [<switches>] <name> <cmds>: define a command <name> executing <cmds>
 Switches:
     -params <arg>                  take parameters, accessible to each shell escape as $0..$N
     parameter should take the form <count> or <min>..<max> (both omittable)
     -override                      allow overriding an existing command
     -hidden                        do not display the command in completion candidates
     -docstring <arg>               define the documentation string for command
     -file-completion               complete parameters using filename completion
     -client-completion             complete parameters using client name completion
     -buffer-completion             complete parameters using buffer name completion
     -command-completion            complete parameters using kakoune command completion
     -shell-completion              complete parameters using shell command completion
     -shell-script-completion <arg> complete parameters using the given shell-script
     -shell-script-candidates <arg> get the parameter candidates using the given shell-script" \
-params 1.. %{ evaluate-commands %sh{
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
                completion="-shell-script-completion %&$completion&" ;;
            -shell-script-candidates)
                shift
                candidates=$(printf "%s\n" "$1" | sed "s/&/&&/g")
                candidates="-shell-script-candidates %&$candidates&" ;;
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
    vars=$(grep -o 'kak_\w*' $tmp | uniq | sed "s/^/# /")
    # creating body of the command
    printf "%s\n" "
        define-command $switches $docstring $command_name $completion $candidates $params %{
            evaluate-commands %sh{
                $vars
                perl $tmp \$@
            }
        }
        hook global -always KakEnd .* %{ nop %sh{ rm $tmp }}
    "
}}

alias global def-perl define-perl-command
