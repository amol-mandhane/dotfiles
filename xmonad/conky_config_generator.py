# -*- coding: utf-8 -*-

import sys

color1 = "\\#020202"
color2 = "\\#44AACC"
color3 = "\\#9D9D9D"
color4 = "\\#444444"
color5 = "\\#101010"
color_green = "\\#66FF66"
color_red = "\\#FF6666"
color_yellow = "\\#FFD727"


def fg(c=None):
    if c is None:
        c = color4
    return "^fg(%s)" % c


def bg(c=None):
    if c is None:
        c = color1
    return "^bg(%s)" % c


# spacer =
# "^fn(Clean:size=2)" + bg(color1) + " ^fn(Clean:size=10)" + bg(color5)
spacer = ""


def cpu_init():
    return """
template1 ${cpu cpu1}
template2 ${cpu cpu2}
template3 ${cpu cpu3}
template4 ${cpu cpu4}
"""


def cpu():
    def f(cpus):
        ret = ""
        for d in cpus:
            ret += "${if_match ${template%d} >= 50}" % d
            ret += fg(color_red)
            ret += "${else}"
            ret += "${if_match ${template%d} >= 20}" % d
            ret += fg(color_yellow)
            ret += "${else}"
            ret += fg(color2)
            ret += "${endif}"
            ret += "${endif}"
            ret += "${template%d}%% " % d
            ret += fg() + "\\\n"
        return ret
    return " " + bg(color5) + fg(color4) + " CPU \\\n" + spacer + \
        " " + f(range(1, 5)) + fg() + bg() + "\\\n\\"


def temp_init():
    return "template5 ${hwmon temp 1}"


def temp():
    ret = " "
    ret += bg(color5) + fg(color4)
    ret += " TEMP " + spacer
    ret += "${if_match ${template5} >= 60}"
    ret += fg(color_red)
    ret += "${else}"
    ret += "${if_match ${template5} >= 50}"
    ret += fg(color_yellow)
    ret += "${else}"
    ret += fg(color2)
    ret += "${endif}"
    ret += "${endif}"
    ret += "${template5}°C "
    ret += bg() + fg() + "\\\n\\"
    return ret


def mem_init():
    return "template6 ${memperc}"


def mem():
    ret = " "
    ret += bg(color5) + fg(color4)
    ret += " MEM " + spacer
    ret += "${if_match ${template6} >= 60}"
    ret += fg(color_red)
    ret += "${else}"
    ret += "${if_match ${template6} >= 40}"
    ret += fg(color_yellow)
    ret += "${else}"
    ret += fg(color2)
    ret += "${endif}"
    ret += "${endif}"
    ret += "${template6}% ${mem} "
    ret += bg() + fg() + "\\\n\\"
    return ret


def swap_init():
    return "template7 ${swapperc}"


def swap():
    ret = " "
    ret += bg(color5) + fg(color4)
    ret += " SWAP " + spacer
    ret += "${if_match ${template7} >= 10}"
    ret += fg(color_red)
    ret += "${else}"
    ret += "${if_match ${template7} >= 2}"
    ret += fg(color_yellow)
    ret += "${else}"
    ret += fg(color2)
    ret += "${endif}"
    ret += "${endif}"
    ret += "${template7}% "
    ret += bg() + fg() + "\\\n\\"
    return ret


def wifi_init():
    return ""


def wifi():
    ret = " "
    ret += bg(color5) + fg(color4)
    ret += " WiFi " + spacer
    ret += fg(color2)
    ret += " ${wireless_link_qual_perc wlan0}%"
    ret += bg() + fg() + "\\\n\\"
    return ret


def battery_init():
    return "template8 ${battery_percent BAT1}"


def battery():
    ret = " "
    ret += fg(color1)
    ret += "${if_match \"${acpiacadapter}\" == \"on-line\"}"
    ret += bg(color_green) + " CHARGE "
    ret += "${else}"
    ret += bg(color_red) + " BATTERY "
    ret += "${endif}" + spacer + bg(color5)
    ret += "${if_match ${template8} >= 75}"
    ret += fg(color_green)
    ret += "${else}"
    ret += "${if_match ${template8} > 25}"
    ret += fg(color2)
    ret += "${else}"
    ret += fg(color_red)
    ret += "${endif}"
    ret += "${endif}"
    ret += " ${template8}% "
    ret += bg() + fg() + "\\\n\\"
    return ret


def volume_init():
    return ""


def volume():
    ret = " "
    ret += bg(color5) + fg(color4)
    ret += " VOLUME " + spacer
    ret += fg(color2)
    ret += " ${exec pactl list sinks | grep \"Volume: 0\"" + \
        "| awk '{print $3}' | tail -1} "
    ret += bg() + fg() + "\\\n\\"
    return ret


def brightness_init():
    return ""


def brightness():
    ret = " "
    ret += bg(color5) + fg(color4)
    ret += " BRIGHT " + spacer
    ret += fg(color2)
    ret += " ${exec xbacklight | cut -d \".\" -f1}% "
    ret += bg() + fg() + "\\\n\\"
    return ret


def timestamp_init():
    return ""


def timestamp():
    ret = " "
    ret += bg(color3) + fg(color1)
    ret += "^ca(1,/home/amol/.xmonad/calendar.sh) " +\
        "${time %a %b %d %Y %l:%M:%S %p} ^ca()"
    ret += bg() + fg() + "\\\n\\"
    return ret


def time_init():
    return ""


def time():
    ret = " "
    ret += bg(color3) + fg(color1)
    ret += " ${time %A} "
    ret += bg() + " " + bg(color5) + fg(color3)
    ret += "^ca(1,/home/amol/.xmonad/calendar.sh) "
    ret += "${time %Y.%m.}"
    ret += fg(color2)
    ret += "${time %d} ^ca()"
    ret += bg() + " " + bg(color5) + fg(color3)
    ret += " ${time %I:%M:}"
    ret += fg(color_green)
    ret += "${time %S}"
    ret += fg(color4)
    ret += " ${time %p} "
    ret += bg() + fg() + "\\\n\\"
    return ret


def uptime_init():
    return ""


def uptime():
    ret = " "
    ret += bg(color5) + fg(color4)
    ret += " UPTIME "
    ret += fg(color3)
    ret += "${uptime} "
    ret += bg() + fg() + "\\\n\\"
    return ret


def wnet_init():
    return ""


def wnet():
    ret = " "
    ret += bg(color5) + fg(color4)
    ret += " NET "
    ret += fg(color2)
    ret += " ${downspeed wlan0} ${upspeed wlan0} "
    ret += bg() + fg() + "\\\n\\"
    return ret


def main():
    print """background no
out_to_console yes
out_to_x no
update_interval 1.0
total_run_times 0
use_spacer left
pad_percents 2
short_units yes
"""

    for arg in sys.argv[1:]:
        try:
            print eval(arg + "_init()")
        except:
            pass

    print "TEXT"
    for arg in sys.argv[1:]:
        try:
            print eval(arg + "()")
        except:
            pass

if __name__ == "__main__":
    main()
