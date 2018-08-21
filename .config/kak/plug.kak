declare-option -docstring "path where plugins should be installed" \
    str plugin_install_dir '$HOME/.config/kak/plugins'

define-command plug -params 1..2 %{
    %sh{
        if [[ -d $HOME/.config/kak/plugins/$(basename $1)/rc ]]; then
            for file in $(echo $HOME/.config/kak/plugins/$(basename $1)/rc/$2*.kak); do
                echo source "$file"
            done
        fi
    }
}

define-command plug-install -docstring 'Install all uninstalled plugins' %{
    echo -markup "{Information}Installing plugins in the background"
    nop %sh{ (
        if [[ ! -d %opt{plugin_install_dir} ]]; then
            mkdir %opt{plugin_install_dir}
        fi

        for plugin in $(grep -oP "(?<=plug\s).*" $HOME/.config/kak/kakrc); do
            if [[ ! -d %opt{plugin_install_dir}/$(basename $plugin) ]]; then
                (cd %opt{plugin_install_dir}; git clone https://github.com/$plugin >&2) &
            fi
        done
        wait
        printf %s\\n "evaluate-commands -client $kak_client echo -markup '{Information}Done installing plugins'" | kak -p ${kak_session}
    ) > /dev/null 2>&1 < /dev/null & }
}

define-command plug-update -docstring 'Update installed plugins' %{
    echo -markup "{Information}Updating plugins in the background"
    nop %sh{ (
        for plugin in $(echo $HOME/.config/kak/plugins/*); do
           (cd $plugin && git pull >&2) &
        done
        wait
        printf %s\\n "evaluate-commands -client $kak_client echo -markup '{Information}Done updating plugins'" | kak -p ${kak_session}
    ) > /dev/null 2>&1 < /dev/null & }
}

