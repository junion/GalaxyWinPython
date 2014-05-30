/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: ErrorMessage.java,v 1.4 2000/11/15 19:54:20 wohlever Exp $
 */


/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */

package galaxy.lang;

/**
 * This object creates a Clause frame with the special name 
 * <tt>system_error</tt>. It sets <b>:errno</b> with an <tt>int</tt> and the 
 * <b>:err_description</b> with the <tt>String</tt>.
 *
 * @see galaxy.lang.GalaxyMessage
 * @see galaxy.lang.Clause
 */
public class ErrorMessage extends GalaxyMessage {

    public static final int GAL_APPLICATION_ERROR = 0;
    public static final int GAL_NO_OPNAME_ERROR = 1;
    public static final int GAL_TRANSMISSION_ERROR = 2;
    public static final int GAL_RECEPTION_ERROR = 3;
    public static final int GAL_SERVER_DOWN_ERROR = 4;
    public static final int GAL_NO_FRAME_ERROR = 5;
    public static final int GAL_CONN_REJECTION_ERROR = 6;

    public ErrorMessage(String description, int errno) {
	data = new Clause("system_error");
	((Clause)data).setProperty(GFrame.GAL_ERROR_NUMBER_FRAME_KEY, errno);
	((Clause)data).setProperty(GFrame.GAL_ERROR_DESCRIPTION_FRAME_KEY, 
				   description);
	msgType = GAL_ERROR_MSG_TYPE;
    }

    public ErrorMessage(String description) {
	this(description, GAL_APPLICATION_ERROR);
    }

    public ErrorMessage(Object data) {
	this.data = data;
	msgType = GAL_ERROR_MSG_TYPE;
	
    }
}
	
