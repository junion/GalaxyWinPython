/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: DispatchError.java,v 1.3 2001/06/05 20:35:35 wohlever Exp $
 */

package galaxy.server;

import galaxy.lang.GFrame;

public class DispatchError extends Exception {
    
    GFrame data;

    public DispatchError() {
	super();
    }

    public DispatchError(String s) {
	super(s);
    }

    public DispatchError(GFrame frame) {
	super();
	data = frame;
    }

    public GFrame getData() {
	return data;
    }

}

