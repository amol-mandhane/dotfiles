xsettingsd &

nm-applet &
feh --bg-scale ~/.xmonad/Wallpaper.jpg &

# Swap Control_L with Caps_Lock
setxkbmap -option ctrl:swapcaps
# Disable Caps_Lock
xmodmap -e "keysym Caps_Lock = NoSymbol"

xcompmgr &
