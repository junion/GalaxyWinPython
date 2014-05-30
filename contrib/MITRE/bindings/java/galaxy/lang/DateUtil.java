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

package galaxy.lang;

import java.text.ParseException;

import java.util.GregorianCalendar;
import java.util.Calendar;
import java.util.StringTokenizer;
import java.util.Date;

import galaxy.util.TimeRange;
import galaxy.util.TimeRangeMaker;
import galaxy.util.TimeRangeMakerException;
import galaxy.util.Logger;

import galaxy.server.MainServer;

/**
 * This class encapsulates a set of static functions for manipulating
 * dates and semantic frames.
 *
 * @see Predicate
 */
public class DateUtil {
    /**
     * The current year.<!> For example: 1998.
     */
    public final static int defaultYear = new GregorianCalendar().get(Calendar.YEAR);

    static final Symbol DATE        = Symbol.getSymbol("date");
    static final Symbol DAY         = Symbol.getSymbol("day");
    static final Symbol DAY_NUMBER  = Symbol.getSymbol("day_number");
    static final Symbol MONTH       = Symbol.getSymbol("month");
    static final Symbol STR         = Symbol.getSymbol("str");
    static final Symbol YEAR        = Symbol.getSymbol("year");
    static final Symbol WDAY        = Symbol.getSymbol("wday");
    static final Symbol YDAY        = Symbol.getSymbol("yday");

    static final String dayNames[] = {"sunday","monday","tuesday","wednesday",
		       "thursday","friday","saturday",null};

    static final String monthNames[] = {"january","february","march","april","may","june",
			 "july","august","september","october","november","december",null};

    private DateUtil()
    {
    }

    /**
     * Locate time predicates, and convert them from relative time to absolute time
     * useful for sql queries.  Absolute times are represented by absolute_tod (absolute
     * time of day) qsets.   If a time range is needed, absolute_start_time and absolute_end_time
     * predicates are used with absolute_tod topics.  If a point in time is needed, it is represented
     * by a absolute_time predicate.
     * <P>
     * Predicates which are converted are: start_time, end_time, before, after,
     * at and time_interval.
     * <p>
     * For example, assume today is Friday May 15, 1998
     * <P>
     * <table border=1>
     *  <tr>
     *    <td valign=top>
     *<PRE>
     *:pred {p month_date
     *          :topic {q date
     *                :day "saturday"
     *                :exists 1 } }
     *</PRE>
     *    </td><td valign=top>
     *<PRE>
     *:pred {p absolute_start_time
     *      :topic {q absolute_tod
     *              :year 1998
     *              :month "may"
     *              :dom 16
     *              :hour 0
     *              :minute 0 } }
     *:pred {p absolute_end_time
     *      :topic {q absolute_tod
     *              :year 1998
     *              :month "may"
     *              :dom 16
     *              :hour 23
     *              :minute 59 } }
     *</PRE>
     *  </td></tr><tr>
     *    <td valign=top>
     *<PRE>
     *:pred {p month_date
     *      :topic {q date
     *                 :day "weekend"
     *                 :exists 1 }  } }
     *</PRE>
     *    </td><td valign=top>
     *<PRE>
     *:pred {p absolute_start_time
     *      :topic {q absolute_tod
     *              :year 1998
     *              :month "may"
     *              :dom 16
     *              :hour 0
     *              :minute 0 } }
     *:pred {p absolute_end_time
     *      :topic {q absolute_tod
     *              :year 1998
     *              :month "may"
     *              :dom 17
     *              :hour 23
     *              :minute 59 } }
     *</PRE>
     *  </td></tr><tr>
     *    <td valign=top>saturday 3:00   pm
     *    </td><td valign=top>
     *<PRE>
     *:pred {p absolute_time
     *      :topic {q absolute_tod
     *              :year 1998
     *              :month "may"
     *              :dom 16
     *              :hour 3
     *              :minute 0 } }
     *</PRE>
     *</td>/</tr></table>
     *<P>
     *This is a grammar of time predicates we handle as input.
     *<P>
     *<PRE>
     *   &lt;tina_time&gt;       :=&lt;time_preds&gt;
     *                      |&lt;time_relation&gt;
     *                      |&lt;time_preds&gt; &lt;time_relation&gt;
     *   &lt;time_relation&gt; := &lt;start_end&gt; | &lt;after&gt; | &lt;before&gt; | &lt;at&gt;
     *   &lt;start_end&gt; :=                         ;;Generated when the user says "between".
     *              :pred {p start_time         ;;Inclusive
     *                  &lt;complex_time&gt;}
     *              :pred {p end_time
     *                  &lt;complex_time&gt;}
     *   &lt;after&gt; := :pred {p after              ;;Exclusive
     *                  &lt;complex_time&gt;}
     *   &lt;before&gt; := :pred {p before            ;;Exclusive
     *                  &lt;complex_time&gt;}
     *   &lt;at&gt; := :pred {p at
     *                  :topic &lt;time&gt;}
     *   &lt;complex_time&gt; :=
     *      [(&lt;at&gt; | :topic &lt;time&gt;)]
     *      [(&lt;month_date&gt; | :topic &lt;date&gt;)]
     *      [(&lt;time_interval&gt; | :topic &lt;time_of_day&gt;)]
     *   &lt;time_preds&gt; := &lt;month_date&gt; &lt;time_interval&gt; &lt;at&gt;   ;; One or more of these.
     *   &lt;month_date&gt; := :pred {p month_date
     *                              :topic  &lt;date&gt; }
     *   &lt;date&gt; := {q date
     *                  :day &lt;dow&gt; }
     *   &lt;time_interval&gt; := :pred {p time_interval
     *                              :topic {q time_of_day
     *                                          :name &lt;interval&gt; } }
     *   &lt;time&gt; = {q time
     *              &lt;hour&gt;
     *              :minutes #
     *              [:xm "a m"]}
     *   &lt;hour&gt; := :hour # | :military #
     *   &lt;interval&gt; := "morning" | "night" | "afternoon" | "evening"
     *   &lt;dow&gt; := "weekend" | "monday" | "tuesday" | "wednesday" | "thursday" | ...
     *<PRE>
     */
    static public void prepareTimesForSQL(GFrame f, int zoneOffset) throws TimeRangeMakerException
    {
        // Prepare time predicates at this level.
        prepareTimePredicates(f, zoneOffset);

        // If the topic remains, then prepare time predicates in the topic.
        QSet topic = (QSet)f.getProperty(Symbol.TOPIC);
        if (topic != null)
            prepareTimesForSQL(topic, zoneOffset);
    }

    static void prepareTimePredicates(GFrame f, int zoneOffset) throws TimeRangeMakerException
    {
        TimeRange tr;
        TimeRange start = prepareStartEnd(Symbol.START_DATE, Symbol.START_TIME, f);
        TimeRange end = prepareStartEnd(Symbol.END_DATE, Symbol.END_TIME, f);
        
        if (start != null) {
            if (end != null) {
                tr = new TimeRange(start);
                tr.betweenInclusive(end);
            } else {
                tr = start;
                tr.end = null;
            }
        } else {
            if (end != null) {
                tr = end;
                tr.start = null;
            } else {
                tr = null;
            }
        }
        
        if (tr == null) {
            TimeRangeMaker trm = new TimeRangeMaker();
            try {
            prepareNormalTimePredicates(f, trm);
            tr = trm.getRange();
            } catch (Exception ex) {
		Logger.getLogger().logErrorMessage("DateUtil.prepareTimePredicates caught exception: " + ex.toString(), ex, "DateUtil.prepareTimePredicates(GFrame, int)");
            }
            if (tr == null)
                return;
        }
          
        if (tr != null)
            tr.add(Calendar.MILLISECOND, zoneOffset);
        
        if (tr.start != null) {
            // Experimenting with using predicates without topics.
            Predicate p = new Predicate(Symbol.ABSOLUTE_START_TIME_P);
            p.setProperty(YEAR, tr.start.get(Calendar.YEAR));
            p.setProperty(MONTH, tr.start.get(Calendar.MONTH)+1);
            p.setProperty(Symbol.DAY_OF_MONTH, tr.start.get(Calendar.DAY_OF_MONTH));
            p.setProperty(Symbol.HOUR, tr.start.get(Calendar.HOUR));
            p.setProperty(Symbol.XM, tr.start.get(Calendar.HOUR_OF_DAY) < 12 ? "AM" : "PM" );
            p.setProperty(Symbol.MINUTES, tr.start.get(Calendar.MINUTE));
            f.setPredicate(p);
        }

        if (tr.end != null) {
            // Experimenting with using predicates without topics.
            Predicate p = new Predicate(Symbol.ABSOLUTE_END_TIME_P);
            p.setProperty(YEAR, tr.end.get(Calendar.YEAR));
            p.setProperty(MONTH, tr.end.get(Calendar.MONTH)+1);
            p.setProperty(Symbol.DAY_OF_MONTH, tr.end.get(Calendar.DAY_OF_MONTH));
            p.setProperty(Symbol.HOUR, tr.end.get(Calendar.HOUR));
            p.setProperty(Symbol.XM, tr.end.get(Calendar.HOUR_OF_DAY) < 12 ? "AM" : "PM" );
            p.setProperty(Symbol.MINUTES, tr.end.get(Calendar.MINUTE));
            f.setPredicate(p);
        }
    }
     
    static TimeRange prepareStartEnd(Symbol pname1, Symbol pname2, GFrame f) throws TimeRangeMakerException
    {
        Predicate p1 = f.getPredicate(pname1);
        Predicate p2 = f.getPredicate(pname2);
        
        if (p1 == null && p2 == null)
            return null;
            
        TimeRangeMaker trm = new TimeRangeMaker();
        if (p1 != null) {
            prepareNormalTimePredicates(p1, trm);
            f.removePredicate(pname1);
        }
        if (p2 != null) {
            prepareNormalTimePredicates(p2, trm);
            f.removePredicate(pname2);
        }
        TimeRange tr = trm.getRange();                
        if (pname1 == Symbol.START_TIME || pname1 == Symbol.START_DATE)
            tr.end = null;
        else
            tr.start = null;
        
        return tr;
    }
    
    static void prepareNormalTimePredicates(GFrame f, TimeRangeMaker trm) throws TimeRangeMakerException
    {
        GFrame topic = (GFrame)f.getProperty("topic");
        if (topic != null) {
            getTimeRange(topic, trm);
            if (!trm.isEmpty())       // If it's a time topic, remove it.
                f.removeProperty("topic");
        }
        for(TimePredicateEnumeration e=f.timePredicates(); e.hasMorePredicates(); ) {
            Predicate p = e.nextPredicate();
            getTimeRange(p, trm);
            f.removePredicate(p.getName());
        }
    }

    static void getTimeRange(GFrame f, TimeRangeMaker trm)
    {
        Symbol name = f.getName();

        if (f instanceof QSet) {
            if (name == Symbol.TIME) {
                if (f.getProperty(Symbol.HOUR) != null)
                    trm.setHour(((Integer)f.getProperty(Symbol.HOUR)).intValue());
                if (f.getProperty(Symbol.MINUTES) != null)
                    trm.setMinute(((Integer)f.getProperty(Symbol.MINUTES)).intValue());
                if (f.getProperty(Symbol.XM) != null)
                    trm.setMinute(((String)f.getProperty(Symbol.XM)).equals("am") ? TimeRangeMaker.AM : TimeRangeMaker.PM);
                if (f.getProperty(Symbol.MILITARY) != null)
                    trm.setHour24(((Integer)f.getProperty(Symbol.MILITARY)).intValue());
            } else
            if (name == DATE) {
                String day = (String)f.getProperty(DAY);
                if (day != null) {
                        trm.setDayOfWeek(day);
                }
                String n = (String)f.getProperty(Symbol.NAME);
                if (n != null) {
                    if (trm.isRelativeDay(n))
                        trm.setRelativeDay(n);
                    else
                        trm.setPartOfDay(n);
                }
            } else
            if (name == Symbol.TIME_OF_DAY) {
                if (f.getProperty(Symbol.NAME) != null) {

                    trm.setPartOfDay(((String)f.getProperty(Symbol.NAME)));
                }
            }
        } else
        if (f instanceof Predicate) {
            if (name == Symbol.AT) {
                getTimeRange((GFrame)f.getProperty(Symbol.TOPIC), trm);
            } else
            if (name == Symbol.BEFORE) {
                trm.setBefore();
                getTimeRange((GFrame)f.getProperty(Symbol.TOPIC), trm);
            } else
            if (name == Symbol.AFTER) {
                trm.setAfter();
                getTimeRange((GFrame)f.getProperty(Symbol.TOPIC), trm);
            } else
            if (name == Symbol.MONTH_DATE) {
                getTimeRange((GFrame)f.getProperty(Symbol.TOPIC), trm);
            } else
            if (name == Symbol.TIME_INTERVAL) {
                getTimeRange((GFrame)f.getProperty(Symbol.TOPIC), trm);
            }
        }
    }

    //-----------------------------------------------

    /**
     * Generate a GVector of date frames from a tina frame.  If the tina frame does not
     * contain a month_date predicate, null is returned. It's a GVector, not a single value
     * because of cases like <i>weekend</i> that can return multiple dates.  If the tina
     * frame designates a specific date, then the following fields are created in the Date frame.
     * <P>
     * <table>
     * <tr><td>year </td><td>The full year.</td></tr>
     * <tr><td>month </td><td>The number of the month.  1 == January </td></tr>
     * <tr><td>day </td><td>Day of the month.</td></tr>
     * <tr><td>wday </td><td>Day of the week. 1 == Sunday.</td></tr>
     * <tr><td>yday </td><td>Day of the year.</td></tr>
     * </table>
     * <P>
     * If the tina frame designates a day of the week, then <i>wday</i> field is added designating
     * the day of the week as an integer.  The name of the day is carried over.  Note a name for
     * the day of the week is not generated if the date is given as month, day.  Presumably this is
     * for language generation reasons.  Answer in the manner the question was asked.
     * <P>
     *  <table border=1>
     *  <tr><th>Tina frame</th><th>Date frame</th></tr>
     *  <tr><td><PRE>{p date :day "saturday"}</PRE></td>
     *      <td><PRE>({p date :wday 7 :day_name "saturday"})</td></tr>
     *  <tr><td><PRE>{p date :name "today"}</PRE></td>
     *      <td>
     *<PRE>
     * ({p date
     *     :year 1998
     *     :name "today"
     *     :wday 3
     *     :yday 153
     *     :day 2
     *     :month 6})
     *</PRE>
     *  </td></tr><tr><td><PRE>{p date :name "weekend"}</PRE></td>
     *      <td>
     *<PRE>
     * ({p date
     *     :wday 7
     *     :day_name "saturday"}
     *  {p date
     *     :wday 1
     *     :day_name "sunday"} )
     *</PRE>
     *</td></tr><TR><TD><PRE>{p date :month "may" :day_number 15}</PRE></td>
     *<TD>
     * <PRE>
     * ({p date
     *    :year 1998
     *    :wday 6
     *    :yday 135
     *    :day 15
     *    :month 5})
     * </PRE>
     *  </td></tr></table>
     *
     * @param tinaFrame  The tina frame
     * @param silent     If true insert <code>:silent 1</code> in to the frame.  Otherwise remove <code>:silent</code>
     * @return A GVector of date frames.
     */
    public static GVector prepareDates(GFrame tinaFrame, boolean silent)
    {
        GFrame pred, res=null;

        if ((pred = tinaFrame.findPredicate("month_date")) != null) {
            if (silent)
                pred.setProperty("silent", new Integer(1)); /* propagate silencing */
            else
                pred.removeProperty("silent");

            return dateFromTinaFrame((GFrame)pred.getProperty("topic")); /* for effect */
        }

        return null;
    }

    static GVector dateFromTinaFrame(GFrame fr)
    {
        GFrame and_fr, date_fr;
        GFrame res = null;
        GFrame fakeTimeFrame = null;
        GVector tlist = new GVector();
        int num = 0, todays_yday = -1;
        int todays_index = -1;
        GFrame mydate = null;

        String name = (String)fr.getProperty("name");
        if (name != null && name.equals("weekend"))
            weekend(fr);
        date_fr = makeDateFrame(fr);
        if (date_fr != null)
            tlist.addElement(date_fr);

        processTimeConjunction(fr, "and", tlist);
        processTimeConjunction(fr, "or", tlist);

        if (tlist.size() == 0) return null;

        return tlist;
    }


    /**
     * Handle dates with :AND and :OR. The date from the input frame is already in the
     * vector.  Work on the frame associated with the conjunction, then recurse.
     */
    static void processTimeConjunction(GFrame fr, String conjName, GVector gvec)
    {
        GFrame conjFrame = (GFrame)fr.getProperty(conjName);
        if (conjFrame == null)
            return;

        gvec.addElement(makeDateFrame(conjFrame));

        processTimeConjunction(conjFrame, conjName, gvec);
    }

    static int getIndex(String array[], String str)
    {
        int i;
        String lstr = str.toLowerCase();

        for(i=0; array[i] != null; ++i) {
            if (array[i].equalsIgnoreCase(lstr)) return i;
            if (array[i].startsWith(lstr)) return i;
        }

        return -1;
    }

    /**
     * Create a Date from a GFrame containing :year, :month and :day_number.
     */
    static Date getDate(GFrame date)
    {
        GregorianCalendar cal = new GregorianCalendar();

        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        //!! This was DAY_NUMBER in the C code.  But that doesn't match parseDate().
        Integer day = (Integer)date.getProperty(DAY);
        if (day != null)
            cal.set(Calendar.DATE, day.intValue());

        Integer month = (Integer)date.getProperty(MONTH);
        if (month != null)
            cal.set(Calendar.MONTH, month.intValue()-1);

        Integer year = (Integer)date.getProperty(YEAR);
        if (year != null)
            cal.set(Calendar.YEAR, year.intValue());

        return cal.getTime();
    }

    /**
     * Parse a date string and produce the corresponding GFrame.
     * A Predicate with the name "date" will be returned. The following fields will be set if the
     * corresponding value was in the input string.
     * <br>
     * :year<br>
     * :month<br>
     * :day<br>
     * :wday<br>
     * <P>
     * The date string can
     * contain a month name, day of the week name, and/or an integer.  If the integer is less than
     * 32, it is considered the day of the month.  If it is less than 100 it is considered the
     * last two digits of the year.  Otherwise it is considered the year.
     *
     * @see Predicate
     */
    public static Predicate parseDate(String str)
    {
        StringTokenizer st = new StringTokenizer(str);
        int month = -1;
        int year = -1;
        int day = -1;
        int wday = -1;
        int i;

        while(st.hasMoreTokens()) {
            String token = st.nextToken();

            if ((i = getIndex(dayNames, token)) >= 0)
                wday = i;
            else if( (i = getIndex(monthNames, token)) >= 0)
                month = i;
            else {
                try {
                    i = Integer.parseInt(token);
                    if (i < 32) day = i;
                    else if (i < 100) year = 1900+i;
                    else year = i;
                } catch(NumberFormatException nfe) {
                }
            }
        }

        Predicate frame = new Predicate(DATE);
        frame.setProperty(STR, str);
        if (year >= 0)
            frame.setProperty(YEAR, new Integer(year));
        if (month >= 0)
            frame.setProperty(MONTH, new Integer(month + 1));
        if( day >= 0)
            frame.setProperty(DAY, new Integer(day));
        if( wday >= 0)
            frame.setProperty(WDAY, new Integer(wday + 1));

        return frame;
    }

    /**
     * Return true if the month in both frames is non-null and the same value.
     */
    public static boolean monthEquals(GFrame dfr, GFrame mfr)
    {
        Integer dmonth = (Integer)dfr.getProperty(MONTH);
        Integer mmonth = (Integer)mfr.getProperty(MONTH);

        return dmonth != null && mmonth != null
            && dmonth.intValue() == mmonth.intValue();
    }

    /**
     * Return true if the first date frame is before the second date frame.
     */
    public static boolean before(GFrame dfr1, GFrame dfr2)
    {
      return getDate(dfr1).before(getDate(dfr2));
    }

    /**
     * Return true if the first date frame is after the second date frame.
     */
    public static boolean after(GFrame dfr1, GFrame dfr2)
    {
      return getDate(dfr1).after(getDate(dfr2));
    }

    /**
     * Return true if the first date frame is the same as the second date frame.
     */
    public static boolean equals(GFrame dfr1, GFrame dfr2)
    {
      return getDate(dfr1).equals(getDate(dfr2));
    }

    /**
     * Make a date Predicate for the given Date. Feilds in the resulting frame are:<p>
     *
     * <table>
     * <tr><td>:year</td><td>Year.  For example 1998.</td></tr>
     * <tr><td>:month</td><td>Month.  Jan == 1</td></tr>
     * <tr><td>:day</td><td>Day of the month.  First day == 1.</td></tr>
     * <tr><td>WDAY</td><td>Day of the week. Sunday == 1.</td></tr>
     * <tr><td>YDAY</td><td>Day of the year.</td></tr>
     * </table>
     */
    static Predicate makeDateFrame(Date date)
    {
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        return makeDateFrame(cal);
    }

    static Predicate makeDateFrame(Calendar cal)
    {
        Predicate frame = new Predicate(DATE);

        frame.setProperty(YEAR, new Integer(cal.get(Calendar.YEAR)));
        frame.setProperty(MONTH, new Integer(cal.get(Calendar.MONTH)+1));
        frame.setProperty(DAY, new Integer(cal.get(Calendar.DATE)));
        frame.setProperty(WDAY, new Integer(cal.get(Calendar.DAY_OF_WEEK)));
        frame.setProperty(YDAY, new Integer(cal.get(Calendar.DAY_OF_YEAR)));

        return frame;
    }

    /**
     * Make a date frame from .
     * <pre>
     *  {q date
     *      :day_number 4   //Required
     *      :exists 1
     *      :month "july"   //Required
     *      :year 1997 }    //Optional, defaults to current year.
     * </pre>
     */
    static Predicate explicitDate(GFrame frame)
    {
        int month, date, year;

        String monthName = (String)frame.getProperty("month");

        Integer yearInteger = (Integer)frame.getProperty("year");
        if (yearInteger != null)
            year = yearInteger.intValue();
        else
            year = defaultYear;

        month = getIndex(monthNames, monthName);

        Integer dateInteger = (Integer)frame.getProperty("day_number");
        date = dateInteger.intValue();

        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.YEAR, year);
        cal.set(Calendar.MONTH, month);
        cal.set(Calendar.DATE, date);

        return makeDateFrame(cal);
    }

    static Predicate today()
    {
        Calendar cal = Calendar.getInstance();

        Predicate f = makeDateFrame(cal.getTime());

        f.setProperty("name", "today");
        return f;
    }

    static Predicate yesterday()
    {
        Calendar cal = Calendar.getInstance();

        cal.add(Calendar.DATE, -1);
        Predicate f = makeDateFrame(cal.getTime());

        f.setProperty("name", "yesterday");
        return f;
    }

    static Predicate tonight()
    {
        Calendar cal = Calendar.getInstance();

        if (cal.get(Calendar.HOUR_OF_DAY) <= 4)
            cal.add(Calendar.DATE, -1);
        Predicate f = makeDateFrame(cal.getTime());

        f.setProperty("name", "tonight");
        return f;
    }

    static Predicate tomorrow()
    {
        Calendar cal = Calendar.getInstance();

        cal.add(Calendar.DATE, 1);
        Predicate f = makeDateFrame(cal.getTime());

        f.setProperty("name", "tomorrow");
        return f;
    }

    static Predicate dayAfterTomorrow()
    {
        Calendar cal = Calendar.getInstance();

        if (cal.get(Calendar.HOUR_OF_DAY) >= 4)
            cal.add(Calendar.DATE, 1);
        cal.add(Calendar.DATE, 1);
        Predicate f = makeDateFrame(cal.getTime());

        f.setProperty("name", "day_after_tomorrow");
        return f;
    }


    /**
     * This resolves definite weekday references (eg returning on monday).
     * The input is the ":wday" property of the frame.  The value is an Integer
     * designating a day of the week.  A date frame is returned for the named day of the week.
     * The date will be after today.
     * <p>
     * Indefinite references (eg returing on a monday or flights on mondays) need different
     * treatment.
     */
    static Predicate weekday(GFrame frame)
    {
        Calendar cal = Calendar.getInstance();
        int dayOfWeek = cal.get(Calendar.DAY_OF_WEEK);

        int frameDayOfWeek = ((Integer)frame.getProperty(WDAY)).intValue();;
        int delta = frameDayOfWeek - dayOfWeek;
        if (delta <= 0)
            delta += 7;
        cal.add(Calendar.DATE, delta);
        return  makeDateFrame(cal.getTime());
    }

    /**
     * Construct a date frame from the frame that tina generates.
     *<br>
     * Here are the cases handled.
     * <ul>
     * <li>:month and :day
     * <li>:name is today, this, tonight, yesterday, overnight, last, day after tomorrow, tomorrow or any
     * <li>Otherwise, process optional components
     * <table border=1>
     * <tr><th>If input frame contains</th><th>then process output frame</th></tr>
     * <tr><td>:day_number</td><td>add :day, and :day_name</td></tr>
     * <tr><td>:year</td><td> copy to date frame.</td></tr>
     * <tr><td>:month with name</td><td>add :month with number.</td></tr>
     * <tr><td>:day</td><td>add :wday and :day_name to date frame.</td></tr>
     * </table>
     * </ul>
     */
    public static Predicate makeDateFrame(GFrame fr)
    {
        if (fr.getProperty("month") != null) {
            if (fr.getProperty("day_number") == null)
                return null; /* pathological: month and no day */
            return explicitDate(fr);
        }

        String name = (String)fr.getProperty("name");
        if (name != null) {
            if(name.equals("today")) return today();
            if(name.equals("this")) return today();
            if(name.equals("tonight")) return tonight();
            if(name.equals("yesterday")) return yesterday();
            if(name.equals("overnight")) return tonight();
            if(name.equals("last")) return today(); /* this is bogus: "last night" */
            if(name.equals("day after tomorrow")) return dayAfterTomorrow();
            if(name.equals("tomorrow")) return tomorrow();

            if (name.indexOf("any ") != -1)
                return null; /* could be "any date", "any time", etc. */
        }

        // if :day_number then add :day, and :day_name
        Predicate date = new Predicate(DATE);
        Integer dayInteger = (Integer)fr.getProperty("day_number");
        if (dayInteger != null) {
            date.setProperty("day", dayInteger);
            int dayInt = dayInteger.intValue();
            if (dayInt > 0)
                date.setProperty("day_name", dayNames[dayInt-1]);
        }

        // Copy :year over to date frame.
        Integer yearInteger = (Integer)fr.getProperty("year");
        if (yearInteger != null)
            date.setProperty("year", yearInteger);

    /* Can't happen.  month is caught above.
        // if :month with name in input, add :month with number in date.
        String monthName = (String)fr.getProperty("month");
        if (monthName != null) {
            int i = getIndex(monthNames, monthName);
            if (i >= 0)
                date.setProperty("month", new Integer(i+1));
        }
    */

        // if :day, add :wday and :day_name to date frame.
        String dayString = (String)fr.getProperty("day");
        if (dayString != null) {
            int i = getIndex(dayNames, dayString);
            if (i >= 0) {
                /* we should consolidate these two */
                date.setProperty("wday", new Integer(i+1));
                date.setProperty("day_name", dayNames[i]);
            }
        }

        return date;
    }

    static GVector datesFromTinaFrame(GFrame fr)
    {
        /* This is from the C code.  It seems architecturally inappropriate to be
         * in this package.  So I'm not porting it.
         *
        if (SQLhost)
        {
          if (todays_date)
            todays_index = (int) fr_getsvalue(todays_date, ":day_index");
          else if(Static_Weather_Directory == Weather_Directory) {
            fake_time_frame = setup_fake_date(Batch_Date);
            todays_index = (int) fr_getsvalue(fake_time_frame, ":day_index");
          }
          else
          { mydate= today();
            todays_index = (int) fr_getsvalue(mydate, ":day_index");
            Substitute_Today = 1;
            if (patricks_bug(todays_index))
            {	todays_index -= 1;
        	Substitute_Today = 0;
            }
          }
        }
        */
        GVector gvec = new GVector();

        String nameString = (String)fr.getProperty("name");
        if(nameString != null && nameString.equals("weekend"))
            weekend(fr);

        Predicate dateFrame = makeDateFrame(fr);
        if (dateFrame != null)
            gvec.addElement(dateFrame);

        processTimeConjunction(fr, "and", gvec);
        processTimeConjunction(fr, "or",  gvec);

        if (gvec.size() == 0) return null;

        /*
        if (SQLhost)
        { if (mydate) todays_yday = (int) fr_getsvalue(mydate, ":yday");
          relativize_dates(fr, gvec, num, todays_index, todays_yday); \* subtract off today's index modulo 7. *\
          return(fr); \* it doesn't use the res *\
        }
        */

        return gvec;
    }

    /**
     *  Insert a frame fragment representing "saturday and sunday".
     * <pre>
     *  :day "saturday"
     *  :and {p date
     *          :day "sunday"}
     * <pre>
     */
    static void weekend(GFrame fr)
    {
        GFrame andFrame = new Predicate(DATE);
        andFrame.setProperty("day", "sunday");
        fr.setProperty("day", "saturday");
        fr.setProperty("and", andFrame);
    }

    /**
     * For testing only.
     */
    public static void main(String args[])
    {
        test();
    }

    static void test()
    {
        GFrame sf = DateUtil.parseDate("Mon jul 27");
        System.out.println("result from parseDate="+sf);

        Date d = DateUtil.getDate(sf);
        System.out.println("d=getDate()="
            +java.text.DateFormat.getDateTimeInstance(java.text.DateFormat.LONG,java.text.DateFormat.LONG).format(d));

        sf = DateUtil.makeDateFrame(d);
        System.out.println("makeDateFrame(d)="+sf);

        System.out.println("yesterday="+yesterday());
        System.out.println("today="+today());
        System.out.println("tonight="+tonight());
        System.out.println("tomorrow="+tomorrow());
        System.out.println("dayAfterTomorrow="+dayAfterTomorrow());

        Calendar cal = Calendar.getInstance();
        sf = makeDateFrame(cal.getTime());
        System.out.println("Today's weekday date="+weekday(sf));
        cal.add(Calendar.DATE, 1);
        sf = makeDateFrame(cal.getTime());
        System.out.println("Tomorrows's weekday date ="+weekday(sf));

        sf = new Predicate(DATE);
        sf.setProperty("name", "weekend");
        GVector gv = datesFromTinaFrame(sf);
        System.out.println("weekend="+gv);

        testName("tomorrow");
        testName("today");
        testName("yesterday");

        testDay("sunday");
        testDay("monday");
        testDay("tuesday");
        testDay("wednesday");
        testDay("thursday");
        testDay("friday");
        testDay("saturday");

        testMonth("may", 15);

        System.exit(0);
    }

    static void testName(String name)
    {
        GFrame sf;
        sf = new Predicate(DATE);
        sf.setProperty("name", name);
        GVector gv = datesFromTinaFrame(sf);
        System.out.println(name+"="+gv);
    }

    static void testDay(String name)
    {
        GFrame sf;
        sf = new Predicate(DATE);
        sf.setProperty("day", name);
        GVector gv = datesFromTinaFrame(sf);
        System.out.println(name+"="+gv);
    }

    static void testMonth(String month, int day)
    {
        GFrame sf;
        sf = new Predicate(DATE);
        sf.setProperty("month", month);
        sf.setProperty("day_number", new Integer(day));
        GVector gv = datesFromTinaFrame(sf);
        System.out.println(month+" "+day+"="+gv);
    }

    
    /**
     * Parse dateString and insert the appropriate predicates into the frame representing
     * that date and time.  An attempt is made to relativize the.  We check for yesterday, today
     * tomorrow, and then names of days of the week for the following six days.  Otherwise
     * we just express the date directly.  12:00 is converted to noon, 0:00 to midnight.
     *
     * This method is useful when a date retrieved from the database needs to be expressed
     * to the user.
     */
    public static void insertDate(GFrame frame, Calendar cal) throws ParseException
    {
        String name = null;
        String day = null;
        String noon_midnight = null;
        Calendar now = Calendar.getInstance();
        int delta;
        
        //
        // Day
        //
        if (equalsNowPlusDays(cal, -1))
            name = "yesterday";
        else
        if (equalsNowPlusDays(cal, 0))
            name = "today";
        else
        if (equalsNowPlusDays(cal, 1))
            name = "tomorrow";
        else
        // The next line does not work well in the last week of the year.
        if ((delta = cal.get(Calendar.DAY_OF_YEAR) - now.get(Calendar.DAY_OF_YEAR)) > 0 && delta < 7) {
            day = dayNames[now.get(Calendar.DAY_OF_WEEK) + delta % 7];
        }
        Predicate p = new Predicate(Symbol.MONTH_DATE);
        QSet topic = new QSet(DATE);
        p.setProperty(Symbol.TOPIC, topic);
        frame.setPredicate(p);
        if (name != null)
            topic.setProperty(Symbol.NAME, name);
        else
        if (day != null)
            topic.setProperty(DAY, day);
        else {
            topic.setProperty(MONTH, monthNames[cal.get(Calendar.MONTH)]);
            topic.setProperty(DAY_NUMBER, cal.get(Calendar.DAY_OF_MONTH));
        }

        if (cal.get(Calendar.MINUTE) == 0) {
            if (cal.get(Calendar.HOUR_OF_DAY) == 0)
                noon_midnight = "midnight";
            else
            if (cal.get(Calendar.HOUR_OF_DAY) == 12)
                noon_midnight = "noon";
        }

        //
        // Time
        //
        p = new Predicate(Symbol.AT);
        frame.setPredicate(p);
        topic = new QSet(Symbol.TIME);
        p.setProperty(Symbol.TOPIC, topic);
        if (noon_midnight != null)
            topic.setProperty(Symbol.MILITARY, noon_midnight);
        else {
            topic.setProperty(Symbol.HOUR, cal.get(Calendar.HOUR));
            topic.setProperty(Symbol.MINUTES, cal.get(Calendar.MINUTE));
            topic.setProperty(Symbol.XM, cal.get(Calendar.AM_PM) == Calendar.AM ? "am" : "pm");
        }
    }
    
    static boolean equalsNowPlusDays(Calendar cal, int dayDelta)
    {
        Calendar cal2 = Calendar.getInstance();
        cal2.add(Calendar.DATE, dayDelta);
        
        return cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR)
            && cal.get(Calendar.MONTH) == cal2.get(Calendar.MONTH)
            && cal.get(Calendar.DAY_OF_MONTH) == cal2.get(Calendar.DAY_OF_MONTH);
    }
}


