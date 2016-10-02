if [ -x /usr/bin/gnome-power-manager ] ; then
   gnome-power-manager &
fi

unity-settings-daemon &

nm-applet &
feh --bg-scale ~/.xmonad/Wallpaper.jpg &

setxkbmap -option ctrl:swapcaps
xmodmap -e "keycode 37 ="

synclient AreaBottomEdge=1600
synclient AreaTopEdge=400
synclient AreaLeftEdge=700
synclient AreaRightEdge=3350

xbacklight -set 25

xcompmgr &
