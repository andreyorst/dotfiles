# getpasswd function relyes on `gpg' and password file located
# at $HOME/.passwords.gpg by default .It can eiher print passwods,
# or copy those via `xsel' asking for choice if multiple results found.
getpasswd() {
    (
        file="$HOME/.passwords.gpg"
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
                (*) args="'$1' $args" ;;
            esac
            shift
        done
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
        if [ -n "$(command -v xsel)" ] && { [ "$copy" = "true" ] || [ $# -eq 1 ]; }; then
            name="$1"
            result=$(gpg --decrypt "$file" 2>/dev/null | grep -Po -- "(?<=^- $name :: ).*")
            amount=$(printf "%s\n" "$result" | wc -l)
            if [ $amount -gt 1 ]; then
                printf "Multiple passwords found for '%s'. Select which to copy [%s]: " "$name" "$(seq -s ', ' 1 $amount)" >&2
                read -r choice
                if [ $choice -lt 1 ] || [ $choice -gt $amount ]; then
                    printf "Bad choice '%s'.\n" "$choice" >&2
                    return 1
                fi
                printf "%s\n" "$result" | head -n $choice | tail -n +$choice | tr -d '\n' | xsel -b -i
            else
                printf "%s" "$result" | xsel -b -i
            fi
            printf "%s\n" "Password for '$1' copied to clipboard" >&2
        else
            for name in $@; do
                names="^- ${name} :: \|${names}"
            done
            names="${names%\\|}"
            gpg --decrypt "$file" 2>/dev/null | grep -- "${names}"
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
