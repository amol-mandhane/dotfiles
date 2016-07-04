if [ -x /usr/bin/gnome-power-manager ] ; then
   gnome-power-manager &
fi

unity-settings-daemon &

nm-applet &
feh --bg-scale /home/amol/.xmonad/Wallpaper.jpg &

# xautolock -time 5 -locker /home/amol/.xmonad/lock.sh &

# xmodmap -e "keycode 117 = End"
# xmodmap -e "keycode 115 = Prior"
# xmodmap -e "keycode 112 = Next"

synclient AreaBottomEdge=1600
synclient AreaTopEdge=400
synclient AreaLeftEdge=700
synclient AreaRightEdge=3350

xbacklight -set 25

