# getpasswd function relies on `gpg' and password file located at
# $HOME/.passwords.gpg by default .It can either print passwords, or
# copy those via `xsel' asking for choice if multiple results found.
getpasswd() {
    (
        # declare inline function here, that will be destroyed when
        # `getpasswd' exits.  This function reads `stdin' line by line,
        # building categories for the password_tools.sh based on Org
        # Mode format. Each nested heading is being pushed into the
        # stack, if heading has the same level we pop previous stack
        # item. When the line matches the queue we print the stack and
        # the result to `stdout'.
        filter() {
            perl -e '
                use strict;
                use warnings;

                my @results;

                my $old_depth = 0;
                my $cur_depth = 0;
                my @path;

                foreach (<STDIN>) {
                    foreach my $name (@ARGV) {
                        if ($_ =~ /^(\*+)\s(.*)$/) {
                            $cur_depth = length $1;
                            if ($cur_depth == 1) {
                                @path = ();
                                push @path, $2;
                            } elsif ($cur_depth > $old_depth) {
                                push @path, $2;
                            } elsif ($cur_depth == $old_depth) {
                                pop @path;
                                push @path, $2;
                            } else {
                                pop @path;
                            }
                            $old_depth = $cur_depth;
                        } elsif ($_ =~ /^-\s($name)\s::\s(.*)$/) {
                            my $password = $2;
                            my $p = join "/", @path;
                            # @path = ();
                            print "$p/$name: $2\n";
                        }
                    }
                }
            ' $@
        }
        copy="true"
        file="$HOME/.passwords.gpg"
        # argument handling
        while [ $# -gt 0 ]; do
            case $1 in
                (-file)    shift; file="$1" ;;
                (-file=*)  file="${1#-file=}";;
                (-copy|-c) copy="true" ;;
                (-print|-p) copy="false" ;;
                (-help|-h)
                    printf "%s\n" "usage: getpasswd [-file=<filename>] [-copy, -c|-print, -p] [-help, -h] [keyname] ...\n" >&2
                    printf "%s\n" "  -c -copy: copy password to clipboard. If specified only one key is used." >&2
                    printf "%s\n" "  -p -print: print password even if there's only one found" >&2
                    printf "%s\n" "     -file=filename: look for passwords in specified file." >&2
                    printf "%s\n" "  -h -help: print this message" >&2
                    return 0 ;;
                # if we did not match to anything treat current item
                # as args for later usage. This way we can keep args
                # in original order
                (*) args="'$1' $args" ;;
            esac
            shift
        done
        # if file was not specified or not found continuously ask for
        # vile via user input.
        while [ -z "$file" ] || [ ! -e "$file" ]; do
            [ -n "$file" ] && printf "No such file '%s'\n" "$file" >&2
            printf "Please specify a file: " >&2
            read -r file
        done

        eval "set -- $args"
        if [ $# -lt 1 ]; then
            printf "Enter service name(s) (separated by space): " >&2
            read -r inputs
            set -- $inputs
        fi
        # in case when there's only one password queue was provided,
        # and if no `-print' option was specified we're going to copy
        # the password via `xsel'.
        if [ $# -eq 1 ] && [ -n "$(command -v xsel)" ] && [ "$copy" = "true" ]; then
            name="$1"
            result=$(gpg --decrypt "$file" 2>/dev/null | filter $name)
            amount=$(printf "%s\n" "$result" | wc -l)
            # if multiple passwords found in the search results we
            # have to select 1 to copy
            if [ $amount -gt 1 ]; then
                printf "Multiple passwords found for '%s':\n" "$name" >&2
                for i in $(seq 1 $amount); do
                    item=$(printf "%s\n" "$result" | head -n $i | tail -n +$i | tr -d '\n' | sed "s|/\w\+: .*$||")
                    printf "%s) %s\n" "$i" "$item" >&2
                done
                printf "Select which to copy [%s]: " "$(seq -s ', ' 1 $amount)" >&2
                read -r choice
                # this `case' checks if user input is a number and
                # that it is in range of available passwords
                case $choice in
                    ''|*[!0-9]*) printf "Bad choice '%s', expected one of these: %s.\n" "$choice" "$(seq -s ', ' 1 $amount)" >&2
                                 return 1 ;;
                    *) if [ $choice -lt 1 ] || [ $choice -gt $amount ]; then
                           printf "Bad choice '%s', expected one of these: %s.\n" "$choice" "$(seq -s ', ' 1 $amount)" >&2
                           return 1
                       fi ;;
                esac
                # filter the input by specific line
                name="$(printf "%s\n" "$result" | head -n $choice | tail -n +$choice | tr -d '\n' | sed "s|/\w\+: .*$||")"
                printf "%s\n" "$result" | head -n $choice | tail -n +$choice | tr -d '\n' | sed "s/.*: //" | xsel -b -i
            else
                printf "%s" "$result" | sed "s/.*: //" | xsel -b -i
            fi
            printf "%s\n" "Password for '$name' copied to clipboard" >&2
        else
            # multiple passwords were specified
            for name in $@; do
                names="${names} $name"
            done
            gpg --decrypt "$file" 2>/dev/null | filter $names
        fi
    )
}

# `genpasswd' function generates passwords by reading `/dev/random' and stripping
# away everything that are not presented in `allowed' list of symbols.
genpasswd () {
    (
        allowed='A-Za-z0-9!`~!@#$%^&*()-_=+\|[{]};:'\''",<.>/?'
        while [ $# -gt 0 ]; do
            case $1 in
                (-copy|-c) copy="true" ;;
                (-a) shift; allowed=$1 ;;
                (-allowed=*) allowed=${1#-allowed=} ;;
                (-help|-h)
                    printf "%s\n" "usage: genpasswd [length] [-copy] [-help] [-allowed=<symbols>] ...\n" >&2
                    printf "%s\n" "  -a -allowed: symbols that are allowed to be in password." >&2
                    printf "%s\n" "  -c -copy:    copy password to clipboard. If specified only one key is used." >&2
                    printf "%s\n" "  -h -help:    print this message" >&2
                    return 0 ;;
                (*) size=$1 ;;
            esac
            shift
        done
        if [ "$copy" = "true" ]; then
            cat /dev/random | tr -dc $allowed | head -c ${size:-22} | xsel -b -i
            printf "password copied to clipboard\n" >&2
        else
            cat /dev/random | tr -dc $allowed | head -c ${size:-22} && echo
        fi
    )
}
