#!/usr/bin/env python2
import datetime


def tomorrow_badge(y, m, d):
    date = datetime.datetime(y, m, d)
    diff = datetime.datetime.now() - date
    days = diff.days
    hours = diff.seconds // 3600
    minutes = diff.seconds % 3600 // 60
    print "%dd %dh %dm" % (days, hours, minutes)


if __name__ == "__main__":
    tomorrow_badge(2015, 8, 14)