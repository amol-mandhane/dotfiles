feh --bg-scale /home/mandhane/Wallpaper.jpg
xcompmgr -fF -D 5 &

if [ ! $(pgrep -x "nm-applet") > /dev/null ]; then
  nm-applet &
fi

if [ -x "$(command -v mate-settings-daemon)" ]; then
  mate-settings-daemon &
fi

if [ -f $HOME/.xsettingsd ]; then
  xsettingsd &
fi

# Swap Control_L with Caps_Lock
setxkbmap -option ctrl:swapcaps
# Disable Caps_Lock
xmodmap -e "keysym Caps_Lock = NoSymbol"
