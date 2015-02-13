killall nm-applet
ps ax | grep jupiter | awk '{ print $1 }' | xargs kill
dropbox stop
killall dzen2
killall trayer
xmonad --restart
