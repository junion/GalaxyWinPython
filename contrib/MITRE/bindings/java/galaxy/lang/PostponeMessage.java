/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: PostponeMessage.java,v 1.1 2000/11/01 15:16:44 wohlever Exp $
 */


/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */

package galaxy.lang;

/**
 * This creates a <code>GalaxyMessage</code> of type <b>GAL_POSTPONE_MSG_TYPE</b>.
 *
 * @see galaxy.lang.GalaxyMessage
 * @see galaxy.lang.Clause
 */
public class PostponeMessage extends GalaxyMessage {

    public PostponeMessage(Object data) {
	this.data = data;
	msgType = GAL_POSTPONE_MSG_TYPE;
    }
}
	
