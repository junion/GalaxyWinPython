/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: GalaxyObject.java,v 1.9 2002/04/03 21:56:55 wohlever Exp $
 */

/** 
 * These Java bindings were originally produced by Intel Corp.,
 * which has granted permission to the Communicator program to
 * use and modify them. The preceding MITRE copyright refers to
 * whatever changes the MITRE Corporation has made to the code. 
 */

package galaxy.lang;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.Iterator;

public abstract class GalaxyObject implements Cloneable 
{
    public final static int GAL_FREE = 0;
    public final static int GAL_FRAME = 1;
    public final static int GAL_STRING = 2;
    public final static int GAL_INT = 3;
    public final static int GAL_FLOAT = 4;
    public final static int GAL_SYMBOL = 5;
    public final static int GAL_LIST = 6;
    public final static int GAL_PTR = 7;
    public final static int GAL_TOPIC_FRAME = 8;
    public final static int GAL_CLAUSE_FRAME = 9;
    public final static int GAL_PRED_FRAME = 10;
    public final static int GAL_BINARY = 11;
    public final static int GAL_INT_16 = 12;
    public final static int GAL_INT_32 = 13;
    public final static int GAL_INT_64 = 14;
    public final static int GAL_FLOAT_32 = 15;
    public final static int GAL_FLOAT_64 = 16;
    public final static int GAL_KEYWORD = 17;
    public final static int GAL_TAG = 18;
    public final static int GAL_TOKEN = 19;
    public final static int GAL_PROXY = 20;

    public static Map typeToName = new HashMap();
    public static Map nameToType = new HashMap();

    // Initialize type and name mappings
    static {
	// Build the type-to-name mapping.
	typeToName.put(new Integer(GAL_FREE),"GAL_FREE");
	typeToName.put(new Integer(GAL_FRAME),"GAL_FRAME");
	typeToName.put(new Integer(GAL_STRING),"GAL_STRING");
	typeToName.put(new Integer(GAL_INT),"GAL_INT");
   	typeToName.put(new Integer(GAL_FLOAT),"GAL_FLOAT");
	typeToName.put(new Integer(GAL_SYMBOL),"GAL_SYMBOL");
	typeToName.put(new Integer(GAL_LIST),"GAL_LIST");
	typeToName.put(new Integer(GAL_PTR),"GAL_PTR");
	typeToName.put(new Integer(GAL_TOPIC_FRAME),"GAL_TOPIC_FRAME");
	typeToName.put(new Integer(GAL_CLAUSE_FRAME),"GAL_CLAUSE_FRAME");
	typeToName.put(new Integer(GAL_PRED_FRAME),"GAL_PRED_FRAME");
	typeToName.put(new Integer(GAL_BINARY),"GAL_BINARY");
	typeToName.put(new Integer(GAL_INT_16),"GAL_INT_16");
	typeToName.put(new Integer(GAL_INT_32),"GAL_INT_32");
	typeToName.put(new Integer(GAL_INT_64),"GAL_INT_64");
	typeToName.put(new Integer(GAL_FLOAT_32),"GAL_FLOAT_32");
	typeToName.put(new Integer(GAL_FLOAT_64),"GAL_FLOAT_64");
	typeToName.put(new Integer(GAL_KEYWORD),"GAL_KEYWORD");
	typeToName.put(new Integer(GAL_TAG),"GAL_TAG");
	typeToName.put(new Integer(GAL_TOKEN),"GAL_TOKEN");
	typeToName.put(new Integer(GAL_PROXY),"GAL_PROXY");

	// Build the name-to-type mapping.
	Set keys = typeToName.keySet();
	Iterator iterator = keys.iterator();
	Object key;
	while(iterator.hasNext()) {
	    key = iterator.next();
	    nameToType.put(typeToName.get(key), key);
	}
    }

    protected int type;

    public int getType() 
    {
	return this.type;
    }
    
    public String getTypeName() 
    {
	return (String)typeToName.get(new Integer(type));
    }
    
    public static boolean isArrayType(int type) 
    {
	switch(type) {
	case GAL_BINARY:
	case GAL_INT_16:
	case GAL_INT_32:
	case GAL_INT_64:
	case GAL_FLOAT_32:
	case GAL_FLOAT_64:
	    return true;
	default:
	}
	return false;
    }  

    public static String nameType(int type) 
    {
	return (String)typeToName.get(new Integer(type));
    }

    public static int getTypeForObject(Object obj) 
    {
	if(obj == null)
	    return -1;

	if (obj instanceof GalaxyObject) {
	    return ((GalaxyObject)obj).getType();
	} else if (obj instanceof Integer) {
	    return GAL_INT;
	} else if (obj instanceof Float) {
	    return GAL_FLOAT;
	} else if (obj instanceof String) {
	    return GAL_STRING;
	} else if (obj instanceof Symbol) {
	    return GAL_SYMBOL;
	}

	return -1;
    }

    public static String getNameForObject(Object obj) 
    {
	return GalaxyObject.nameType(GalaxyObject.getTypeForObject(obj));
    }

    /*
     * The only reason we need this function at all is because of the
     * dual behavior of the GBinary object, that requires a special
     * encoded String version for GFrames.
     */
    static void appendValue(StringBuffer buff, int indent, Object value)
    {
        if (value instanceof GVector) {
            GVector gvector = (GVector)value;
            buff.append(gvector.toEncodedString());
        } else if (value instanceof Symbol) {
	    buff.append(((Symbol)value).toString());
	} else if (value instanceof Integer) {
	    buff.append(((Integer)value).toString());
	} else if (value instanceof Float) {
	    buff.append(((Float)value).toString());
	} else if (value instanceof String) {
	    putString(buff, (String)value, 0);
	} else if (value instanceof GFrame) {
	    ((GFrame)value).toStringBuffer(buff);
	} else if (value instanceof GBinary) {
	    buff.append(((GBinary)value).toEncodedString());
	} else if (value instanceof Int16) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Int16)value).getSize() + " elements]");
	} else if (value instanceof Int32) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Int32)value).getSize() + " elements]");
	} else if (value instanceof Int64) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Int64)value).getSize() + " elements]");
	} else if (value instanceof Float32) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Float32)value).getSize() + " elements]");
	} else if (value instanceof Float64) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Float64)value).getSize() + " elements]");
	} else if (value instanceof BrokerProxy) {
	    buff.append(((BrokerProxy)value).toString());
	} else {
	    buff.append("\"-unexpected data type-\"" + value);
	} 
    }

    /** 
     * Convert value to a string, and append it to the buffer usign the given indentation.
     */
    static void appendFormattedValue(StringBuffer buff, int indent, Object value)
    {
        if (value instanceof GVector) {
            GVector gvector = (GVector)value;
            gvector.format(buff, indent);
        } else if (value instanceof Symbol) {
	    putIndent(buff, indent);
	    buff.append(((Symbol)value).toString());
	} else if (value instanceof Integer) {
	    putIndent(buff, indent);
	    buff.append(((Integer)value).toString());
	} else if (value instanceof Float) {
	    putIndent(buff, indent);
	    buff.append(((Float)value).toString());
	} else if (value instanceof String) {
	    putString(buff, (String)value, indent);
	} else if (value instanceof GFrame) {
	    ((GFrame)value).toFormattedStringBuffer(buff, indent);
	} else if (value instanceof GBinary) {
	    putIndent(buff, indent);
	    buff.append(((GBinary)value).toFormattedString());
	} else if (value instanceof Int16) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Int16)value).getSize() + " elements]");
	} else if (value instanceof Int32) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Int32)value).getSize() + " elements]");
	} else if (value instanceof Int64) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Int64)value).getSize() + " elements]");
	} else if (value instanceof Float32) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Float32)value).getSize() + " elements]");
	} else if (value instanceof Float64) {
	    buff.append("[array: type " + GalaxyObject.getNameForObject(value) + ", " + ((Float64)value).getSize() + " elements]");
	} else if (value instanceof BrokerProxy) {
	    buff.append(((BrokerProxy)value).toString());
	} else {
	    buff.append("-unexpected data type-");
	} 
    }

    static void putString(StringBuffer buff, String st, int indent)
    {
        putIndent(buff, indent);
        buff.append('"');
        for(int i=0; i<st.length(); ++i) {
            char c = st.charAt(i);
            switch(c) {
            case '\n':
                buff.append(c);
                putIndent(buff, indent);
                break;
            case '"':
                buff.append('\\');
                buff.append(c);
                break;
	    case '\\':
                buff.append('\\');
                buff.append(c);
                break;
            default:
                buff.append(c);
                break;
            }
        }
        buff.append('"');
    }

    static void putIndent(StringBuffer buff, int indent)
    {
        int currentIndent=0;
        for(int i=buff.length()-1; i >= 0 && buff.charAt(i) != '\n'; --i) {
            ++currentIndent;
        }
        for(int i=currentIndent; i<indent; ++i)
            buff.append(' ');
    }

}
