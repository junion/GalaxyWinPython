/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: NewMessage.java,v 1.1 1999/10/28 18:33:20 sasha Exp $
 */


/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */

package galaxy.lang;

/**
 * This creates a <code>GalaxyMessage</code> of type <b>GAL_MESSAGE_MSG_TYPE</b>.
 *
 * @author Sasha P. Caskey
 * @see galaxy.lang.GalaxyMessage
 * @see galaxy.lang.Clause
 */
public class NewMessage extends GalaxyMessage {

    public NewMessage(Object data) {
	this.data = data;
	msgType = GAL_MESSAGE_MSG_TYPE;
    }
}
	
