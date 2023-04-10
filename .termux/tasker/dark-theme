#!/data/data/com.termux/files/usr/bin/sh
emacsclient -e '(progn
                  (disable-theme local-config-light-theme)
                  (load-theme local-config-dark-theme t))'

echo -n "" > ~/.termux/colors.properties
echo "dark" > ~/.termux/theme-variant
termux-reload-settings
