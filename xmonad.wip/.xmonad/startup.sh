feh --bg-scale /home/mandhane/Wallpaper.jpg
xcompmgr -fF -D 5 &

if [ ! $(pgrep -x "nm-applet") > /dev/null ]; then
  nm-applet &
fi

# if [ -x "$(command -v mate-settings-daemon)" ]; then
#   mate-settings-daemon &
# fi

if [ -f $HOME/.xsettingsd ]; then
  xsettingsd &
fi

# Swap Control_L with Caps_Lock
setxkbmap -option ctrl:swapcaps
# Disable Caps_Lock
xmodmap -e "keysym Caps_Lock = NoSymbol"


if [ -x "$(command -v synclient)" ]; then
  synclient AreaBottomEdge=1600
  synclient AreaTopEdge=400
  synclient AreaLeftEdge=700
  synclient AreaRightEdge=3350

  # xinput --set-prop 14 "Synaptics Finger" 25 60 0

  xbacklight -set 25
fi

if [ -x "$(command -v xautolock)" ]; then
  xautolock -time 3 -locker "dm-tool lock" &
fi
