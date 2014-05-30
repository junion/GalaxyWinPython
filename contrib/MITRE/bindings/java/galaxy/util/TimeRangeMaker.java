/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */

package galaxy.util;

import java.util.Calendar;

public class TimeRangeMaker {
    public static final int THIS = 0;
    public static final int NEXT = 1;

    public static final int SUNDAY = Calendar.SUNDAY;
    public static final int MONDAY = Calendar.MONDAY;
    public static final int TUESDAY = Calendar.TUESDAY;
    public static final int WEDNESDAY = Calendar.WEDNESDAY;
    public static final int THURSDAY = Calendar.THURSDAY;
    public static final int FRIDAY = Calendar.FRIDAY;
    public static final int SATURDAY = Calendar.SATURDAY;

    public static final int YESTERDAY = 0;
    public static final int TODAY = 1;
    public static final int TOMORROW = 2;

    public static final int AM = 0;
    public static final int PM = 1;

    public static final int EARLY_MORNING   = 0;
    public static final int MORNING         = 1;
    public static final int AFTERNOON       = 2;
    public static final int TONIGHT         = 3;
    public static final int EVENING         = 4;
    public static final int NIGHT           = 5;
    public static final int LATE_NIGHT      = 6;

    TimeRange range;

    boolean rangeComputed;

    boolean before;
    boolean after;
    int year;                   //e.g. 1998
    int  month;                 //1-12
    int   dom;                  //1-31
    int week_which;             //THIS, NEXT
    int weekend_which;          //THIS, NEXT
    int dow;                    //SUNDAY, MONDAY, ...
    int dow_which;              //THIS, NEXT
    int rday;                   //YESTERDAY, TODAY, TOMORROW
    int ampm;                   //AM, PM
    int partOfDay;              //MORNING, AFTERNOON, EVENING, ...
    int hour;                   //1-12
    int min;                    //0-59

    public TimeRangeMaker()
    {
        year = month = dom = week_which = weekend_which = dow = dow_which = rday = -1;
        ampm = partOfDay = hour = min = -1;
    }

    public boolean isEmpty()
    {
        return year == -1 && month == -1 && dom == -1 && week_which == -1
            && weekend_which == -1 && dow == -1 && dow_which == -1
            && rday == -1 && ampm == -1 && partOfDay == -1
            && hour == -1 && min == -1;
    }

    void computeRange() throws TimeRangeMakerException
    {
        Calendar cal = Calendar.getInstance();
        boolean giveDefaults = true;

        if (year != -1 && giveDefaults) {
            giveDefaults = false;
        }
        if (month != -1 && giveDefaults) {
            year = cal.get(Calendar.YEAR);
            giveDefaults = false;
        }
        if (dom != -1 && giveDefaults) {
            year = cal.get(Calendar.YEAR);
            month = cal.get(Calendar.MONTH)+1;
            giveDefaults = false;
        }
        if (weekend_which != -1 || week_which != -1) {
            giveDefaults = false;
        }
        if (dow != -1 && dow_which == -1 && giveDefaults) {
            dow_which = THIS;
            giveDefaults = false;
        }
        if ((partOfDay != -1 || ampm != -1) && giveDefaults) {
            year = cal.get(Calendar.YEAR);
            rday = TODAY;
            giveDefaults = false;
        }
        if (hour != -1 && giveDefaults) {
            year = cal.get(Calendar.YEAR);
            rday = TODAY;
            if (min == -1)  // If they say "at five".  That implies 0 minutes.
                min = 0;
            giveDefaults = false;
        }
        if (min != -1 && giveDefaults) {
            rday = TODAY;
            hour = cal.get(Calendar.HOUR);
            ampm = (cal.get(Calendar.AM_PM) == Calendar.AM ? AM : PM);
            giveDefaults = false;
        }

        if (year != -1)
            range = yearRange(year);

        if (month != -1) {
            range.intersect(monthRange(year, month));
        }

        if (week_which != -1) {
            range.intersect(weekRange(week_which));
        }

        if (dom != -1) {
            range.intersect(domRange(dom, year, month));
        }

        if (weekend_which != -1) {
            range.intersect(weekendRange(weekend_which));
        }

        if (dow != -1) {
            if (dow_which == -1)
                dowRange(dow, range);
            else
                range.intersect(dowRange(dow, dow_which));
        }

        if (rday != -1) {
            TimeRange r = rdayRange(rday);
            if (range != null)
                range.intersect(r);
            else
                range = r;
        }

        if (ampm != -1) {
            ampmRange(ampm, range);
        }

        if (partOfDay != -1) {
            partOfDayRange(partOfDay, range);
        }

        if (hour != -1) {
            hourRange(hour, range);
        }

        if (min != -1) {
            minuteRange(min, range);
        }

        if (before) {
            range.end = range.start;
            range.start = null;
        }

        if (after) {
            range.start = range.end;
            range.end  = null;
        }

        rangeComputed = true;
    }

    TimeRange yearRange(int year)
    {
        Calendar c;
        TimeRange tr = new TimeRange();

        c = Calendar.getInstance();
        c.set(Calendar.YEAR, year);
        c.set(Calendar.MONTH, 0);
        c.set(Calendar.DAY_OF_MONTH, 1);
        c.set(Calendar.HOUR, 0);
        c.set(Calendar.MINUTE, 0);
        tr.setStart(c);

        c.clear();
        c.set(Calendar.YEAR, year+1);
        c.set(Calendar.MONTH, 0);
        c.set(Calendar.DAY_OF_MONTH, 1);
        c.add(Calendar.MINUTE, -1);
        tr.setEnd(c);

        return tr;
    }

    public TimeRange monthRange(int year, int month)
    {
        TimeRange tr = new TimeRange();

        Calendar cal = Calendar.getInstance();
        cal.clear();
        cal.set(Calendar.YEAR, year);
        cal.set(Calendar.MONTH, month);
        cal.set(Calendar.DAY_OF_MONTH, 1);
        cal.set(Calendar.HOUR, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        tr.setStart(cal);

        cal.add(Calendar.MONTH, 1);
        cal.add(Calendar.MINUTE, -1);
        tr.setEnd(cal);

        return tr;
    }

    TimeRange weekRange(int which)
    {
        TimeRange tr = new TimeRange();

        Calendar now = Calendar.getInstance();
        Calendar cal = Calendar.getInstance();
	    cal.clear();
	    cal.set(Calendar.YEAR, now.get(Calendar.YEAR));
	    cal.set(Calendar.WEEK_OF_YEAR, now.get(Calendar.WEEK_OF_YEAR));

	    cal.set(Calendar.DAY_OF_WEEK, 1);
	    if (which == NEXT)
    	    cal.add(Calendar.DATE, 7);
        tr.setStart(cal);

	    cal.add(Calendar.DATE, 7);
	    cal.add(Calendar.MINUTE, -1);
        tr.setEnd(cal);

        return tr;
    }

    TimeRange weekendRange(int which)
    {
        TimeRange tr = new TimeRange();

        Calendar cal = Calendar.getInstance();

        for(;;) {
            if (cal.get(Calendar.DAY_OF_WEEK) == SATURDAY)
                break;

            if (cal.get(Calendar.DAY_OF_WEEK) == SUNDAY) {
                cal.add(Calendar.DATE, -1);
                break;
            }

            cal.add(Calendar.DATE, 1);
        }

        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);

        if (which == NEXT)
            cal.add(Calendar.DATE, 7);

        tr.setStart(cal);
	    cal.add(Calendar.DATE, 2);
	    cal.add(Calendar.MINUTE, -1);
        tr.setEnd(cal);

        return tr;
    }

    void dowRange(int dow, TimeRange trin) throws TimeRangeMakerException
    {
        int cdow = trin.start.get(Calendar.DAY_OF_WEEK);
        int delta = dow - cdow;
        if (delta < 0)
            delta += 7;

        generalHourRange(delta*24, 24, 7*24, false, trin);
    }

    TimeRange dowRange(int dow, int which)
    {
        TimeRange tr = new TimeRange();

        Calendar cal = Calendar.getInstance();

        while(cal.get(Calendar.DAY_OF_WEEK) != dow) {
            cal.add(Calendar.DATE, 1);
        }

        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);

        if (which == NEXT)
            cal.add(Calendar.DATE, 7);

        tr.setStart(cal);
	    cal.add(Calendar.DATE, 1);
	    cal.add(Calendar.MINUTE, -1);
        tr.setEnd(cal);

        return tr;
    }

    TimeRange domRange(int dom, int baseyear, int basemonth)
    {
        Calendar c = Calendar.getInstance();
        c.clear();
        c.set(Calendar.YEAR, baseyear);
        c.set(Calendar.MONTH, basemonth);
        c.set(Calendar.DAY_OF_MONTH, dom);

        TimeRange tr = new TimeRange();
        tr.setStart(c);

        c.add(Calendar.DATE, 1);
        c.add(Calendar.MINUTE, -1);
        tr.setEnd(c);

        return tr;
    }

    TimeRange rdayRange(int rday)
    {
        int delta=0;

        switch(rday) {
        case YESTERDAY: delta = -1; break;
        case TODAY:     delta =  0; break;
        case TOMORROW:  delta =  1; break;
        }

        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.add(Calendar.DATE, delta);

        TimeRange tr = new TimeRange();
        tr.setStart(cal);

        cal.add(Calendar.DATE, 1);
        cal.add(Calendar.MINUTE, -1);
        tr.setEnd(cal);

        return tr;
    }

    void ampmRange(int ampm, TimeRange trin) throws TimeRangeMakerException
    {
        generalHourRange(ampm == AM ? 0 : 12, 12, 24, false, trin);
    }

    void partOfDayRange(int partOfDay, TimeRange trin) throws TimeRangeMakerException
    {
        int shour=0, ehour=0;

        switch(partOfDay) {
        case EARLY_MORNING: shour =  0;  ehour =  7;    break;
        case MORNING:       shour =  6;  ehour = 12;    break;
        case AFTERNOON:     shour = 12;  ehour = 18;    break;
        case TONIGHT:       shour = 17;  ehour = 28;    break;
        case EVENING:       shour = 17;  ehour = 20;    break;
        case NIGHT:         shour = 19;  ehour = 22;    break;
        case LATE_NIGHT:    shour = 22;  ehour = 28;    break;
        }

        generalHourRange(shour, ehour-shour, 24, partOfDay == LATE_NIGHT, trin);
    }

    void hourRange(int hour, TimeRange trin) throws TimeRangeMakerException
    {
        generalHourRange(hour, 1, 12, false, trin);
    }

    void generalHourRange(int shour, int len, int delta, boolean lateNight, TimeRange trin)
        throws TimeRangeMakerException
    {
        Calendar cal = Calendar.getInstance();
        cal.setTime(trin.start.getTime());

        cal.set(Calendar.HOUR_OF_DAY, shour);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);

        TimeRange tr = new TimeRange();
        tr.setStart(cal);
        cal.add(Calendar.HOUR_OF_DAY, len);
        cal.add(Calendar.MINUTE, -1);
        tr.setEnd(cal);

        TimeRange tr2 = new TimeRange(tr);
        tr2.intersect(trin);
        if (tr2.isEmpty()) {
            tr.add(Calendar.HOUR_OF_DAY, delta);
        }

        // Check for ambiguity
        tr2.set(tr);
        tr2.add(Calendar.HOUR_OF_DAY, delta);
        tr2.intersect(trin);
        if (!tr2.isEmpty())
            throw new TimeRangeMakerException();

        //If they are talking about LATE_NITE, that really extends in to the next day.
        //So if the end of trin is the end of the day, we assume they really want to
        //extend beyond the current technical day.
        if (lateNight
         && trin.end.get(Calendar.HOUR_OF_DAY) == 23
         && trin.end.get(Calendar.MINUTE) == 59) {
            trin.intersect(tr);
            trin.setEnd(tr.end);
        } else
            trin.intersect(tr);
    }

    void minuteRange(int min, TimeRange trin) throws TimeRangeMakerException
    {
        Calendar cal = Calendar.getInstance();
        cal.setTime(trin.start.getTime());

        cal.set(Calendar.MINUTE, min);
        cal.set(Calendar.SECOND, 0);

        TimeRange tr = new TimeRange();
        tr.setStart(cal);
        tr.setEnd(cal);

        TimeRange tr2 = new TimeRange(tr);
        tr2.intersect(trin);
        if (tr2.isEmpty()) {
            tr.add(Calendar.HOUR_OF_DAY, 1);
        }

        // Check for ambiguity
        tr2.set(tr);
        tr2.add(Calendar.HOUR_OF_DAY, 1);
        tr2.intersect(trin);
        if (!tr2.isEmpty())
            throw new TimeRangeMakerException();

        trin.intersect(tr);
    }

    public Calendar getStart() throws TimeRangeMakerException
    {
        if (!rangeComputed)
            computeRange();

        return range.start;
    }

    public TimeRange getRange() throws TimeRangeMakerException
    {
        if (!rangeComputed)
            computeRange();

        return range;
    }

    public Calendar getEnd() throws TimeRangeMakerException
    {
        if (!rangeComputed)
            computeRange();

        return range.end;
    }

    public void setBefore()
    {
        before = true;
    }

    public void setAfter()
    {
        after = true;
    }

    public void setYear(int year)
    {
        this.year = year;
    }

    public void setMonth(int month)
    {
        this.month = month-1;
    }

    /**
     * THIS, NEXT
     */
    public void setWeek(int week)
    {
        this.week_which = week;
    }

    /**
     * THIS, NEXT
     */
    public void setWeekend(int week)
    {
        this.weekend_which = week;
    }

    public void setDayOfMonth(int dom)
    {
        this.dom = dom;
    }

    /**
     * YESTERDAY, TODAY, TOMORROW
     */

    String rdayNames[] = {"yesterday", "today", "tomorrow"};
    int    rdayIDs[] = {YESTERDAY, TODAY, TOMORROW};

    public boolean isRelativeDay(String s)
    {
        for(int i=0; i<rdayNames.length; ++i) {
            if (rdayNames[i].equals(s))
                return true;
        }

        return false;
    }

    public void setRelativeDay(String rday)
    {
        for(int i=0; i<rdayNames.length; ++i) {
            if (rdayNames[i].equals(rday)) {
                this.rday = rdayIDs[i];
                break;
            }
        }
    }

    public void setRelativeDay(int rday)
    {
        this.rday = rday;
    }

    String dayNames[] = {"sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"};
    int dayID[] = {SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY};

    public boolean isDayOfWeek(String s)
    {
        for(int i=0; i<dayNames.length; ++i) {
            if (dayNames[i].equals(s))
                return true;
        }

        return false;
    }

    public void setDayOfWeek(String dowName)
    {
        for(int i=0; i<7; ++i) {
            if (dayNames[i].equals(dowName)) {
                this.dow = dayID[i];
                return;
            }
        }
    }

    public void setDayOfWeek(int dow)
    {
        this.dow = dow;
    }

    public void setDayOfWeek(int dow, int which)
    {
        this.dow = dow;
        this.dow_which = which;
    }

    public void setAMPM(int ampm)
    {
        this.ampm = ampm;
    }

    /**
     * MORNING, AFTERNOON, EVENING, NIGHT
     */
    String podNames[] = {"morning", "afternoon", "evening", "night", "tonight"};
    int podID[] = {MORNING, AFTERNOON, EVENING, NIGHT, TONIGHT};

    public void setPartOfDay(String partOfDayName)
    {
        for(int i=0; i<podNames.length; ++i) {
            if (podNames[i].equals(partOfDayName)) {
                this.partOfDay = podID[i];
                break;
            }
        }
    }

    public void setPartOfDay(int partOfDay)
    {
        this.partOfDay = partOfDay;
    }

    public void setHour(int hour)
    {
        this.hour = hour;
    }

    public void setHour24(int hour)
    {
        if (hour >= 12) {
            this.hour = hour - 12;
            this.ampm = PM;
        } else {
            this.hour = hour;
            ampm = AM;
        }
    }

    public void setMinute(int min)
    {
        this.min = min;
    }

    public String toString()
    {
        try {
            if (!rangeComputed)
                computeRange();
        } catch(TimeRangeMakerException ex) {
            return "{TimeRange error: " + ex + "}";
        }

        return range.toString();
    }

    public static void main(String args[])
    {
	    TimeRangeMaker tc;

	    tc = new TimeRangeMaker();
	    tc.setMonth(8);
        System.out.println("month 8="+tc);

	    tc = new TimeRangeMaker();
	    tc.setWeek(TimeRangeMaker.THIS);
        System.out.println("this week="+tc);

        tc = new TimeRangeMaker();
	    tc.setWeek(TimeRangeMaker.NEXT);
        System.out.println("next week="+tc);

        tc = new TimeRangeMaker();
	    tc.setWeekend(TimeRangeMaker.THIS);
        System.out.println("this weekend="+tc);

        tc = new TimeRangeMaker();
	    tc.setWeekend(TimeRangeMaker.NEXT);
        System.out.println("next weekend="+tc);

        tc = new TimeRangeMaker();
        tc.setWeek(TimeRangeMaker.THIS);
	    tc.setDayOfWeek(TimeRangeMaker.THURSDAY);
        System.out.println("thursday this week="+tc);

        tc = new TimeRangeMaker();
        tc.setWeek(TimeRangeMaker.THIS);
	    tc.setDayOfWeek(TimeRangeMaker.SUNDAY);
        System.out.println("sunday this week="+tc);

        tc = new TimeRangeMaker();
	    tc.setDayOfWeek(TimeRangeMaker.THURSDAY, TimeRangeMaker.THIS);
        System.out.println("this thursday="+tc);

        tc = new TimeRangeMaker();
	    tc.setDayOfWeek(TimeRangeMaker.WEDNESDAY, TimeRangeMaker.THIS);
        System.out.println("this wednesday="+tc);

        tc = new TimeRangeMaker();
	    tc.setDayOfWeek(TimeRangeMaker.THURSDAY, TimeRangeMaker.NEXT);
        System.out.println("next thursday="+tc);

        tc = new TimeRangeMaker();
	    tc.setDayOfWeek(TimeRangeMaker.WEDNESDAY, TimeRangeMaker.NEXT);
        System.out.println("next wednesday="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.YESTERDAY);
        System.out.println("yesterday="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.TODAY);
        System.out.println("today="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.TOMORROW);
        System.out.println("tomorrow="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.TOMORROW);
	    tc.setAMPM(TimeRangeMaker.AM);
        System.out.println("tomorrow am="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.YESTERDAY);
	    tc.setAMPM(TimeRangeMaker.PM);
        System.out.println("yesterday pm="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.TODAY);
	    tc.setPartOfDay(TimeRangeMaker.LATE_NIGHT);
        System.out.println("today late night="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.TODAY);
	    tc.setPartOfDay(TimeRangeMaker.AFTERNOON);
        System.out.println("today afternoon="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.TOMORROW);
	    tc.setPartOfDay(TimeRangeMaker.MORNING);
        System.out.println("tomorrow morning="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.YESTERDAY);
	    tc.setPartOfDay(TimeRangeMaker.EVENING);
        System.out.println("yesterday evening="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.YESTERDAY);
	    tc.setAMPM(TimeRangeMaker.PM);
	    tc.setHour(7);
        System.out.println("yesterday hour 7="+tc);

        tc = new TimeRangeMaker();
	    tc.setRelativeDay(TimeRangeMaker.YESTERDAY);
	    tc.setAMPM(TimeRangeMaker.PM);
	    tc.setHour(7);
	    tc.setMinute(15);
        System.out.println("yesterday 7:15="+tc);
    }
}
