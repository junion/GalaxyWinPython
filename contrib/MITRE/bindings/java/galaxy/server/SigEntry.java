/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: SigEntry.java,v 1.4 2001/06/05 20:35:42 wohlever Exp $
 */
package galaxy.server;

import galaxy.lang.GVector;

/**
 * This class defines each entry for input and output keys of a Signature.
 *
 * @author Sasha P. Caskey
 * @see galaxy.server.Signature
 * @see galaxy.lang.GalaxyObject
 */
public class SigEntry {
    String name;
    int type;
    int existence;


    /**
     * Constructor.
     *
     * @param name the name of the key
     * @param type type of the key (one of the types defined in GalaxyObject)
     * @param existence existence contraint (defined in Signature)
     */
    public SigEntry(String name, int type, int existence) {
      this.name = name;
      this.type = type;
      this.existence = existence;
    }

    public GVector toGVector() {
	GVector v = new GVector(3);
	v.addElement(name);
	v.addElement(new Integer(type));
	v.addElement(new Integer(existence));
	return v;
    }
}
