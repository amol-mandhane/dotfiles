# Cleanup
killall nm-applet compton xsettingsd xautolock polybar dunst

# Start
if [ -x "$(command -v dunst)" ]; then
    dunst -conf ~/.dunstrc &
else
    echo "Dunst not present on PATH"
fi

if [ -x "$(command -v polybar)" ]; then
    if [ -d "/sys/module/battery/" ]; then
        polybar laptop &
    else
        polybar desktop &
    fi
else
    echo "Polybar not found on PATH"
fi

feh --bg-scale ~/Wallpaper.jpg
compton -fF -D 5 &

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
  synclient AreaLeftEdge=2071
  synclient AreaRightEdge=3871

  # xinput --set-prop 14 "Synaptics Finger" 25 60 0

  xbacklight -set 25
fi

if [ -x "$(command -v xautolock)" ]; then
  xautolock -time 3 -locker "xsecurelock" &
fi

/usr/share/goobuntu-indicator/goobuntu_indicator.py &
