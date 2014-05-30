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

import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashMap;

import galaxy.server.MainServer;

/**
 * The argument parser processes an array of arguments (key/values pairs).
 */
public class ArgParser 
{
    /** 
     * Collection of arguments stored as key/value pairs. Key is the argument 
     * name (represented as a <code>String</code>). The value is stored as
     * a <code>java.util.List</code> containing <code>String</code>s.
     */
    private Map argTable;

    /**
     * Construct an argument parser for the given arguments. Argument names
     * must be prepended with a dash (-) character, and all argument names and
     * values must be separated with whitespace.
     *
     * @param args the argument list (consisting of argument keys and values)
     */
    public ArgParser(String args[])
    {
	argTable = new HashMap();

	String currentStr;
	String argName = null;
	List argValueList = null;

        for(int idx=0; idx<args.length; ++idx) {
	    currentStr = args[idx];
            if(currentStr.charAt(0) == '-') {
		// Found a new argument name. Store the previous argument name
		// and values if they exist.
		if(argName != null) {
		    if(argValueList.size() == 0)
			argValueList = null;
		    argTable.put(argName, argValueList);  
		}

		argName = currentStr;
		argValueList = new ArrayList();
	    } else if(idx == 0) {
		// First item in argument list must be an argument name.
		Logger.getLogger().logWarningMessage("Argument parser expected an argument name at the beginning of the argument list but it found a value: "+ currentStr, "ArgParser(String[])");
	    } else {
		// Found a new argument value for the current argument name.
		argValueList.add(currentStr);
	    }
	}

	// Handle the last argument.
	if(argName != null) {
	    if(argValueList.size() == 0)
		argValueList = null;
	    argTable.put(argName, argValueList);  
	}
    }


    /**
     * Tests if the specified argument exists. Note that the dash must be 
     * included in the argument name.
     *
     * @param argName the argument name
     * @return true if the named argument exists, false otherwise
     */
    public boolean isArg(String argName)
    {
	return argTable.containsKey(argName);
    }
    
    /**
     * Return the value of the specified argument. Note that the dash must be 
     * included in the argument name.
     *
     * @param argName the argument name
     * @return the value of the argument or null if it has no value (or the
     *         argument does not exist)
     */
    public List getArg(String argName)
    {
	return (List) argTable.get(argName);
    }

    /**
     * Return the value of the specified argument. Note that the dash must be 
     * included in the argument name. If the argument does not exist, the 
     * specified default value is returned.
     *
     * @param argName the argument name
     * @param defaultValue the default argument value
     * @return the value of the argument or the default value if it has no 
     *         value (or the argument does not exist)
     */
    public List getArg(String argName, List defaultValue)
    {
	List value = getArg(argName);
	
	if(value == null)
	    value = defaultValue;
	
	return value;
    }

    /**
     * Return the string value of the specified argument. Note that the dash 
     * must be included in the argument name. If the argument does not exist, 
     * the specified default value is returned. Note that if this method is
     * called for an argument that consists of multiple argument values, only
     * the first argument value will be processed.
     *
     * @param argName the argument name
     * @param defaultValue the default argument value
     * @return the value of the argument or the default value if it has no 
     *         value or the argument does not exist
     */
    public String getStringArg(String argName, String defaultValue)
    {
	List value = getArg(argName);
	if (value == null)
	    return defaultValue;
	
	return (String) value.get(0);
    }

    /**
     * Return the integer value of the specified argument. Note that the dash 
     * must be included in the argument name. If the argument does not exist 
     * or there is an error while converting the argument value to an integer,
     * the specified default value is returned. Note that if this method is
     * called for an argument that consists of multiple argument values, only
     * the first argument value will be processed.
     *
     * @param argName the argument name
     * @param defaultValue the default argument value
     * @return the value of the argument or the default value if it has no 
     *         value, the argument does not exist, or there is an error while
     *         converting the argument value to an integer
     */
    public int getIntegerArg(String argName, int defaultValue)
    {
	List value = getArg(argName);
	if (value == null)
	    return defaultValue;

	String strValue = null;
	try {
	    strValue = (String) value.get(0);
	    return Integer.parseInt(strValue);
	} catch(NumberFormatException nfe) {
	    Logger.getLogger().logWarningMessage("Argument parser read an invalid integer for " + argName + ": " + strValue, "ArgParser.getIntegerArg(String, int)");
	    return defaultValue;
	}
    }
}
