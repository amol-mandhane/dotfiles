import datetime
import sys
import inspect
import os
import stat


def tomorrow_badge(y, m, d):
    date = datetime.datetime(y, m, d)
    diff = datetime.datetime.now() - date
    days = diff.days
    hours = diff.seconds // 3600
    minutes = diff.seconds % 3600 // 60
    print "%dd %dh %dm" % (days, hours, minutes)


def main():
    if len(sys.argv) == 1 or sys.argv[1] == "tomorrow_badge":
        date = datetime.date.today() + datetime.timedelta(hours=24)
        filename = "tomorrow_badge.py"
        f = open(filename, "w")
        code = "#!/usr/bin/env python2\nimport datetime\n\n\n"
        code += "".join(inspect.getsourcelines(tomorrow_badge)[0])
        code += "\n\n"
        code += "if __name__ == \"__main__\":\n    "
        code += "tomorrow_badge({d.year}, {d.month}, {d.day})".format(d=date)
        f.write(code)
        f.close()
        os.chmod(filename, os.stat(filename).st_mode | stat.S_IEXEC)

if __name__ == "__main__":
    main()
