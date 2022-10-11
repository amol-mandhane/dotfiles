killall -w redshift-gtk

# Swap Control_L with Caps_Lock
setxkbmap -option ctrl:swapcaps
# Disable Caps_Lock
xmodmap -e "keysym Caps_Lock = NoSymbol"

redshift-gtk &
