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

/**
 * A predicate says something about the topic of the frame.
 */
public class Predicate extends GFrame 
{
    public Predicate(Symbol name)
    {
        super(name, GFrame.GAL_PRED);
    }

    public Predicate(String name)
    {
	this(Symbol.getSymbol(name));
    }
}
