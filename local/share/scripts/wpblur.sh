#!/bin/bash
sleep 10
while true; do
    inotifywait -q ~/.config/plasma-org.kde.plasma.desktop-appletsrc -e delete_self | while read; do
        sleep 1
        convert $(cat ~/.config/plasma-org.kde.plasma.desktop-appletsrc | grep "Image=file" | sed 's/.*\/\//\//') -filter Gaussian -resize 5% -define filter:sigma=2.5 -resize 2000% -attenuate 0.2 +noise Gaussian ~/.bg.png
        cp ~/.bg.png /usr/share/sddm/themes/breeze/.bg.png
    done
done
