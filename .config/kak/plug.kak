def plug -params 1..2 %{
    %sh{
        if [[ ! -d $HOME/.config/kak/plugins/$(basename $1) ]]; then
            for file in $(echo $HOME/.config/kak/plugins/$(basename $1)/rc/$2*.kak); do
                echo source "$file"
            done
        fi
    }
}

define-command plug-install %{
    %sh{
        if [[ ! -d $HOME/.config/kak/plugins ]]; then
            mkdir $HOME/.config/kak/plugins
        fi
        for plugin in $(grep -oP "(?<=plug\s).*" $HOME/.config/kak/kakrc); do
            if [[ ! -d $HOME/.config/kak/plugins/$(basename $1) ]]; then
                (cd $HOME/.config/kak/plugins; git clone https://github.com/$1 >> /dev/null)
            fi
        done
    }
}

define-command plug-update %{
    %sh{
        for plugin in $(echo $HOME/.config/kak/plugins/*); do
           (cd $plugin; git pull >> /dev/null)
        done
    }
}

