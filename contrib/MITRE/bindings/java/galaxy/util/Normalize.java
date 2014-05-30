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

public class Normalize 
{    
    /**
     * Normalize from the C convention of seperating words in method and
     * attribute names with underscores, to the Java convention of using 
     * initial caps to start a word. 
     *
     * @param str the string to normalize
     * @return the normalized string
     */
    static public String normalize(String str)
    {
        String newStr = "";
        
        for(int i=0; i<str.length(); ++i) {
            char c = str.charAt(i);
            if(c == '_' && i+1 < str.length()) {
                c = str.charAt(++i);
                newStr += Character.toUpperCase(c);
            } else
                newStr += c;
        }
        
        return newStr;
    }

    /**
     * Like normalize, but the first character will be capitalized.
     *
     * @param str the string to normalize
     * @return the normalized string
     */
    static public String normalizeWithInitialCap(String str)
    {
        String newStr = normalize(str);
	
        if(str.length() > 0) {
            char c = str.charAt(0);
            c = Character.toUpperCase(c);
            newStr = c + newStr.substring(1);
        }
        
        return newStr;
    }
}
