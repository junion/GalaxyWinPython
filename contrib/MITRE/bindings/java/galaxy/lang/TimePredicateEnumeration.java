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

import java.util.Enumeration;

public class TimePredicateEnumeration implements Enumeration {
    GFrame f;
    int i;
    Symbol timePredicates[] = {Symbol.AT, Symbol.BEFORE, Symbol.AFTER, Symbol.MONTH_DATE, Symbol.TIME_INTERVAL};
    
    public TimePredicateEnumeration(GFrame f)
    {
        this.f = f;
        i = findNext(-1);
    }

    public boolean hasMoreElements()
    {
        return i >= 0;
    }

    public Object nextElement()
    {
        Object o = f.getPredicate(timePredicates[i]);
        i = findNext(i);
        return o;
    }

    public boolean hasMorePredicates()
    {
        return hasMoreElements();
    }

    Predicate nextPredicate()
    {
        return (Predicate)nextElement();
    }

    int findNext(int i)
    {
        for(++i; i<timePredicates.length; ++i) {
            Predicate p = f.getPredicate(timePredicates[i]);
            if (p != null)
                return i;
        }
        
        return -1;
    }
}
