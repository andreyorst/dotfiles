#!/bin/bash
while true; do
    inotifywait -mq ~/.config/plasma-org.kde.plasma.desktop-appletsrc -e access -e modify -e delete || sleep 1 | while read; do
         convert $(cat ~/.config/plasma-org.kde.plasma.desktop-appletsrc | grep "Image=file" | sed 's/.*\/\//\//') -filter Gaussian -resize 5% -define filter:sigma=2.5 -resize 2000% -attenuate 0.3 +noise Gaussian ~/.bg.png
         cp ~/.bg.png /usr/share/sddm/themes/breeze/components/artwork/background.png
    done
done
