# getpasswd function relies on `gpg' and password file located at
# $HOME/.passwords.gpg by default .It can either print passwords, or
# copy those via `xsel' asking for choice if multiple results found.
getpasswd() {
    (
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
                    printf "%s\n" "usage: getpasswd [-file <filename>] [-copy] [-help] [keyname] ...\n" >&2
                    printf "%s\n" "  -c -copy: copy password to clipboard. If specified only one key is used." >&2
                    printf "%s\n" "  -p -print: print password even if there's only one found" >&2
                    printf "%s\n" "     -file=filename: look for passwords in specifiled file." >&2
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
            [ -n "$file" ] && printf "No such file '%s'\n" "$file"
            printf "Please specify a file: "
            read -r file
        done
        eval "set -- $args"
        if [ $# -lt 1 ]; then
            printf "Enter service name(s) (^D to finish): " >&2
            set -- $(</dev/stdin)
            printf "\n"
        fi
        # in case when there's only one password queue was provided,
        # and if no `-print' option was specified we're going to copy
        # the password via `xsel'.
        if [ $# -eq 1 ] && [ -n "$(command -v xsel)" ] && [ "$copy" = "true" ]; then
            name="$1"
            result=$(gpg --decrypt "$file" 2>/dev/null | perl ~/.dotfiles/scripts/search_password.pl $name)
            amount=$(printf "%s\n" "$result" | wc -l)
            # if multiple passwords found in the search results we
            # have to select 1 to copy
            if [ $amount -gt 1 ]; then
                printf "Multiple passwords found for '%s':\n" "$name" >&2
                for i in $(seq 1 $amount); do
                    item=$(printf "%s\n" "$result" | head -n $i | tail -n +$i | tr -d '\n' | sed "s|/\w\+: .*$||")
                    printf "%s) %s\n" "$i" "$item"
                done
                printf "Select which to copy [%s]: " "$(seq -s ', ' 1 $amount)"
                read -r choice
                if [ $choice -lt 1 ] || [ $choice -gt $amount ]; then
                    printf "Bad choice '%s'.\n" "$choice" >&2
                    return 1
                fi
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
            gpg --decrypt "$file" 2>/dev/null | perl ~/.dotfiles/scripts/search_password.pl $names
        fi
    )
}

# `genpasswd' function generates passwords by reading `/dev/random' and stripping
# avay everything that are not presented in `allowed' list of symbols.
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
