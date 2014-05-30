/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.io;

/**
 * Utility class for I/O classes.
 */
public class Util 
{    
    /**
     * Creates a string representaion of a byte array. ASCII characters
     * between 32 and 126 (inclusive) are written to the string. All other
     * characters are represented as a 0-padded octal sequence, \ooo.
     *
     * @param bytes the byte array to stringify
     * @return the stringified byte array
     */
    public static String bytesToString(byte[] bytes) 
    {
	// Preallocate 4 characters per byte (non-printable bytes are 
	// displayed in octal format, e.g., \xxx).
	StringBuffer buffer = new StringBuffer(bytes.length * 4);
	String octalStr;
	int value;
	for(int idx=0; idx < bytes.length; ++idx) {
	    if(bytes[idx] < 32 || bytes[idx] > 126) {
		buffer.append("\\");
		value = (int) bytes[idx];
		if(value < 0) {
		    value += 256;
		}
		octalStr = Integer.toOctalString(value);
		if(octalStr.length() == 1)
		    buffer.append("00");
		else if(octalStr.length() == 2)
		    buffer.append("0");
		buffer.append(octalStr);
	    } else {
		buffer.append((char) bytes[idx]);
	    }
	}
	return buffer.toString();
    }
}
