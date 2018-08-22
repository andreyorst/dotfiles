declare-option -docstring "path where plugins should be installed" \
    str plugin_install_dir '$HOME/.config/kak/plugins'
declare-option -hidden -docstring "Array of plugins" \
    str plugins

define-command plug -params 1..2 %{
    %sh{
        if [[ -d $(eval echo $kak_opt_plugin_install_dir) ]]; then
            if [[ -d $(eval echo $kak_opt_plugin_install_dir)/$(basename $1) ]]; then
                for file in $(find -L $(eval echo $kak_opt_plugin_install_dir/$(basename $1)) -type f -name '*.kak'); do
                    echo source "$file"
                done
            fi
        fi
    }
    set-option -add global plugins %arg{1}
    set-option -add global plugins " "
}

define-command plug-install -docstring 'Install all uninstalled plugins' %{
    echo -markup "{Information}Installing plugins in the background"
    nop %sh{ (
        if [[ ! -d $(eval echo $kak_opt_plugin_install_dir) ]]; then
            mkdir $(eval echo $kak_opt_plugin_install_dir)
        fi

        for plugin in $kak_opt_plugins; do
            if [[ ! -d $($(eval echo $kak_opt_plugin_install_dir)/$(basename $plugin)) ]]; then
                (cd $(eval echo $kak_opt_plugin_install_dir); git clone https://github.com/$plugin >&2) &
            fi
        done
        wait
        printf %s\\n "evaluate-commands -client $kak_client echo -markup '{Information}Done installing plugins'" | kak -p ${kak_session}
    ) > /dev/null 2>&1 < /dev/null & } 
}

define-command plug-update -docstring 'Update all installed plugins' %{
    echo -markup "{Information}Updating plugins in the background"
    nop %sh{ (
        for plugin in $kak_opt_plugins; do
           (cd $(eval echo $kak_opt_plugin_install_dir)/$(basename $plugin) && git pull >&2) &
        done
        wait
        printf %s\\n "evaluate-commands -client $kak_client echo -markup '{Information}Done updating plugins'" | kak -p ${kak_session}
    ) > /dev/null 2>&1 < /dev/null & }
}

