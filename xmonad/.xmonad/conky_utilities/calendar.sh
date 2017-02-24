#/bin/bash
#
LOCKFILE=/tmp/dzen_calendar.txt
if [ -e ${LOCKFILE} ] && kill -0 `cat ${LOCKFILE}`; then
    exit
fi

trap "rm -f ${LOCKFILE}; exit" INT TERM EXIT
echo $$ > ${LOCKFILE}

(python ~/.xmonad/conky_utilities/dzen_calendar.py; sleep 5) | dzen2 \
-e 'onstart=uncollapse;button1=exit;button3=exit;button2=exit' \
-x "904" -y "20" -w "420" -l "16" -sa 'l' -ta 'l' \
-fg '#BBBBBB' \
-bg '#444444' \
-fn 'Clean:size=10' \
-title-name 'popup_calendar'

rm -f ${LOCKFILE}
