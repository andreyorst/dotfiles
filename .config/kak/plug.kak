declare-option -docstring "path where plugins should be installed" \
    str plugin_install_dir '$HOME/.config/kak/plugins'
declare-option -hidden -docstring "Array of plugins" \
    str plugins

define-command -hidden plug -params 1..2 %{
    %sh{
        if [[ -d $(eval echo $kak_opt_plugin_install_dir) ]]; then
            if [[ -d $(eval echo $kak_opt_plugin_install_dir/$(basename $1)) ]]; then
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
    ) & } 
}

define-command plug-update -docstring 'Update all installed plugins' %{
    echo -markup "{Information}Updating plugins in the background"
    nop %sh{ (
        for plugin in $kak_opt_plugins; do
           (cd $(eval echo $kak_opt_plugin_install_dir/$(basename $plugin)) && git pull >&2) &
        done
        wait
        printf %s\\n "evaluate-commands -client $kak_client echo -markup '{Information}Done updating plugins'" | kak -p ${kak_session}
    ) & }
}

define-command plug-clean -docstring 'Delete all plugins that not present in config files' %{
	nop %sh{ (
		for plugin in $(echo $(eval echo $kak_opt_plugin_install_dir)/*); do
			skip=
			for enabled_plugin in $kak_opt_plugins; do
				[[ $(basename $plugin) == $(basename $enabled_plugin) ]] && { skip=1; break; }
			done
			[[ -n $skip ]] || plugins_to_remove+=("$plugin")
		done
		for plugin in $plugins_to_remove; do
			rm -rf $plugin
		done
	) & }
}
