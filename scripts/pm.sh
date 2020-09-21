# Package manager wrappers

for pm in yay pacman rpm-ostree dnf yum apt; do
    if [ -n "$(command -v $pm)" ]; then
        alias pm="pm_\$(echo \$pm | sed 's/[^a-zA-Z_]/_/g')"
        break
    fi
done

pm_yay() {
    cmd="$1"
    shift
    case "$cmd" in
        (update)     yay -Syyu \
                         --noconfirm \
                         --devel \
                         --nodiffmenu \
                         --noeditmenu \
                         --noupgrademenu \
                         --removemake "$@" ;;
        (install)    yay -Sy "$@" ;;
        (remove)     yay -Rsc "$@" ;;
        (autoremove) yay -C "$@" ;;
        (search)     yay -Ss "$@" ;;
        (*)          yay "$cmd" "$@" ;;
    esac
}

pm_packman() {
    cmd="$1"
    shift
    case "$cmd" in
        (install)    sudo pacman -Sy "$@" ;;
        (update)     sudo pacman -Syyu --noconfirm "$@" ;;
        (remove)     sudo pacman -Rsc "$@" ;;
        (autoremove) pacman -Qtdq "$@" | sudo pacman -Rns - ;;
        (search)     pacman -Ss "$@" ;;
        (list)       case $1 in
                         (all) pacman -Qe ;;
                         (*)   echo "wrong argument for 'list'" >&2
                               return 1 ;;
                     esac ;;
        (*)          pacman "$cmd" "$@" ;;
    esac
}

pm_dnf() {
    cmd="$1"
    shift
    case "$cmd" in
        (install)    sudo dnf install "$@" ;;
        (update)     sudo dnf upgrade "$@" ;;
        (remove)     sudo dnf remove "$@" ;;
        (autoremove) sudo dnf autoremove "$@" ;;
        (search)     dnf search "$@" ;;
        (copr)       sudo dnf copr "$@" ;;
        (list)       case $1 in
                         (all)       dnf list all ;;
                         (installed) dnf list installed ;;
                         (*)         echo "wrong argument for 'list'" >&2
                                     return 1 ;;
                     esac ;;
        (*)          dnf "$cmd" "$@" ;;
    esac
}

pm_yum() {
    cmd="$1"
    shift
    case "$cmd" in
        (install) sudo yum install "$@" ;;
        (update)  sudo yum update "$@" ;;
        (remove)  sudo yum remove "$@" ;;
        (search)  yum search "$@" ;;
        (*)       yum "$cmd" "$@" ;;
    esac
}

pm_rpm_ostree() {
    cmd="$1"
    shift
    case "$cmd" in
        (install) rpm-ostree install "$@" ;;
        (update)  rpm-ostree upgrade "$@" ;;
        (remove)  rpm-ostree remove "$@" ;;
        (*)       rpm-ostree "$cmd" "$@" ;;
    esac
}

pm_apt() {
    cmd="$1"
    shift
    case "$cmd" in
        (install)     sudo apt install "$@" ;;
        (update)      sudo apt update "$@" ;;
        (remove)      sudo apt remove "$@" ;;
        (autoremove)  sudo apt autoremove "$@" ;;
        (search)      apt search "$@" ;;
        (*)           sudo apt "$cmd" "$@" ;;
    esac
}
