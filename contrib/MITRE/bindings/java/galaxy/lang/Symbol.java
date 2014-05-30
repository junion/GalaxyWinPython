/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: Symbol.java,v 1.5 2002/03/14 19:06:02 wohlever Exp $
 */

/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */

package galaxy.lang;

import java.util.Map;
import java.util.HashMap;

public class Symbol
{
    //-------------------------------------------------------------------------
    // Static attibutes and methods.
    //-------------------------------------------------------------------------

    private static Map symbolMap = new HashMap();

    public final static Symbol ABSOLUTE_END_TIME_P = Symbol.getSymbol("absolute_end_time_p");
    public final static Symbol ABSOLUTE_START_TIME_P = Symbol.getSymbol("absolute_start_time_p");
    public final static Symbol ADVERB = Symbol.getSymbol("adverb");
    public final static Symbol AND = Symbol.getSymbol("and");
    public final static Symbol AFTER = Symbol.getSymbol("after");
    public final static Symbol AT = Symbol.getSymbol("at");
    public final static Symbol AUX = Symbol.getSymbol("aux");
    public final static Symbol BEFORE = Symbol.getSymbol("BEFORE");
    public final static Symbol COMPLEMENT = Symbol.getSymbol("complement");
    public final static Symbol DATE = Symbol.getSymbol("date");
    public final static Symbol DAY_OF_MONTH = Symbol.getSymbol("day_of_month");
    public final static Symbol DEAD = Symbol.getSymbol("*DEAD*");
    public final static Symbol EMPTY = Symbol.getSymbol("");
    public final static Symbol END_DATE = Symbol.getSymbol("END_DATE");
    public final static Symbol END_TIME = Symbol.getSymbol("END_TIME");
    public final static Symbol ENTER_TOPIC_NAME = Symbol.getSymbol("enter_topic_name");
    public final static Symbol EXISTS = Symbol.getSymbol("exists");
    public final static Symbol GENERIC_ACTION = Symbol.getSymbol("generic_action");
    public final static Symbol HOUR = Symbol.getSymbol("hour");
    public final static Symbol MILITARY = Symbol.getSymbol("military");
    public final static Symbol MINUTES = Symbol.getSymbol("minutes");
    public final static Symbol MONTH_DATE = Symbol.getSymbol("month_date");
    public final static Symbol NAME = Symbol.getSymbol("name");
    public final static Symbol NAMED_TOPICS = Symbol.getSymbol("named_topics");
    public final static Symbol NULL = Symbol.getSymbol("NULL");
    public final static Symbol NUMBER = Symbol.getSymbol("number");
    public final static Symbol OR = Symbol.getSymbol("or");
    public final static Symbol PLURAL = Symbol.getSymbol("plural");
    public final static Symbol PRONOUN = Symbol.getSymbol("pronoun");
    public final static Symbol QUANTIFIER = Symbol.getSymbol("quantifier");
    public final static Symbol START_DATE = Symbol.getSymbol("start_date");
    public final static Symbol START_TIME = Symbol.getSymbol("start_time");
    public final static Symbol TIME = Symbol.getSymbol("time");
    public final static Symbol TIME_INTERVAL = Symbol.getSymbol("time_interval");
    public final static Symbol TIME_OF_DAY = Symbol.getSymbol("time_of_day");
    public final static Symbol TOPIC = Symbol.getSymbol("topic");
    public final static Symbol TRUE = Symbol.getSymbol("true");
    public final static Symbol WELCOME = Symbol.getSymbol("welcome");
    public final static Symbol WHICH = Symbol.getSymbol("which");
    public final static Symbol XM = Symbol.getSymbol("xm");

    public static Symbol getSymbol(String name)
    {
	Symbol sym;
	
	synchronized(symbolMap) {
	    sym = (Symbol) symbolMap.get(name);
	    if (sym == null) {
		sym = new Symbol(name);
		symbolMap.put(name, sym);
	    }
	}
        return sym;
    }


    //-------------------------------------------------------------------------
    // Non-static attributes and methods.
    //-------------------------------------------------------------------------
    
    /** Symbol's name. */
    private String name;

    public Symbol(String name)
    {
        this.name = name;
    }

    public String getString()
    {
        return name;
    }
    
    public String toString()
    {
        return name;
    }

    public String toFormattedString()
    {
	return name;
    }

    public boolean isConjunction()
    { 
        return this == AND || this == OR;
    }
}
