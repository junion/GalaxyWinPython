/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: NotGalaxyStreamable.java,v 1.1.1.1 1999/08/31 15:19:35 sam Exp $
 */

package galaxy.io;

import java.io.IOException;

public class NotGalaxyStreamable extends IOException {

    public NotGalaxyStreamable(String s) {
	super(s);
    }

}

