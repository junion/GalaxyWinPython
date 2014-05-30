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
import java.util.Date;

public class TimeRange {
    public Calendar start, end;
    
    public TimeRange()
    {
    }
    
    public TimeRange(TimeRange trin)
    {
        setStart(trin.start);
        setEnd(trin.end);
    }
    
    public void set(TimeRange trin)
    {
        setStart(trin.start);
        setEnd(trin.end);
    }
    
    public void setStart(Date d)
    {
        start = Calendar.getInstance();
        start.setTime(d);
    }
    
    public void setStart(Calendar c)
    {
        start = (Calendar)c.clone();
    }
    
    public void setEnd(Date d)
    {
        end = Calendar.getInstance();
        end.setTime(d);
    }

    public void setEnd(Calendar c)
    {
        end = (Calendar)c.clone();
    }
    
    public void add(int which, int v)
    {
        start.add(which, v);
        end.add(which, v);
    }

    public void intersect(TimeRange r) 
    {
        if (r.start != null) {
            if (start == null)
                start = (Calendar)r.start.clone();
            else {
                if (r.start.after(start))
                    start = (Calendar)r.start.clone();
            }
        }
               
        if (r.end != null) {
            if (end == null)
                end = (Calendar)r.end.clone();
            else {
                if (r.end.before(end))
                    end = (Calendar)r.end.clone();
            }
        }
        
        if (start != null && end != null && end.before(start))
            start = end = null;
    }
    
    public void betweenInclusive(TimeRange r) 
    {
        if (start != null && r.start != null) {
            if (r.start.getTime().before(start.getTime()))
                start = (Calendar)r.start.clone();
        } else
            start = null;

        if (end != null && r.end != null) {
            if (r.end.getTime().after(end.getTime()))
                end = (Calendar)r.end.clone();
        } else
            end = null;
    }
    
    public boolean isEmpty()
    {
        return start == null && end == null;
    }
    
    public String toString()
    {
        String s = "{TimeRange ";
        if (start == null)
            s += "null";
        else
            s += start.getTime();
            
        s += " -- ";
        
        if (end == null)
            s += "null";
        else
            s += end.getTime();        
            
        s += "}";
        
        return s;
    }
}
