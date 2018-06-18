if ! pgrep -x "xfce4-appfinder" > /dev/null; then
    xfce4-appfinder --disable-server $1 &
else
    pkill xfce4-appfinder
fi
