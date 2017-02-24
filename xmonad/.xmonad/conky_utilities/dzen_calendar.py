import calendar
import datetime


def main():
    today = datetime.datetime.now()
    bday = datetime.datetime(1993, 6, 29, 8, 30, 0)
    diff = today - bday
    year = today.year
    month = today.month
    date = today.day
    finder = str(date) if date >= 10 else (" " + str(date))
    finder1 = finder + " "
    finder2 = " " + finder
    next_month = month + 1
    next_month_year = year
    last_month = month - 1
    last_month_year = year

    if next_month == 13:
        next_month = 1
        next_month_year += 1
    if last_month == 0:
        last_month = 12
        last_month_year -= 1

    cal = calendar.TextCalendar(6)
    this_month_cal = cal.formatmonth(year, month)
    next_month_cal = cal.formatmonth(next_month_year, next_month)
    last_month_cal = cal.formatmonth(last_month_year, last_month)

    last_month_cal = last_month_cal.split("\n")
    this_month_cal = this_month_cal.split("\n")
    next_month_cal = next_month_cal.split("\n")

    for cal in [last_month_cal, this_month_cal, next_month_cal]:
        while len(cal) != 9:
            cal.append("")
        if not cal[0].startswith("    "):
            cal[0] = " " + cal[0]

        cal[1] = " S  M  T  W  T  F  S"

    total_cal = ""

    for i, (l, t, n) in enumerate(zip(last_month_cal, this_month_cal, next_month_cal)):
        total_cal += "  " + l + "".join([" "] * (23 - len(l)))
        nt = t.replace(finder1, "^fg(#FFFFFF)" + finder1 + "^fg()") if i >= 1 else t
        nt = nt.replace(finder2, "^fg(#FFFFFF)" + finder2 + "^fg()") if i >= 1 else t
        total_cal += nt + "".join([" "] * (23 - len(t)))
        total_cal += n + "  \n"

    format = "%a %b %d %Y %I:%M:%S %p"

    print "".join([" "] * 30) + "^fg(#FFFFFF)^fn(Clean:size=12)Calendar^fg()^fn()"
    print total_cal
    print "  Day of the year: ^fg(#FFFFFF)%d^fg()" % today.timetuple().tm_yday
    print "  Week of the year: ^fg(#FFFFFF)%d^fg()" % today.isocalendar()[1]
    print "  New York time: ", (today - datetime.timedelta(hours=10, minutes=30)).strftime(format)
    print "  London Time: ", (today - datetime.timedelta(hours=5, minutes=30)).strftime(format)
    print "  You have been alive for ^fg(#FFFFFF){:,.0f} seconds^fg() or ^fg(#FFFFFF){:,.0f} days^fg()  ".format(diff.total_seconds(), diff.days)
    print ""
if __name__ == '__main__':
    main()
