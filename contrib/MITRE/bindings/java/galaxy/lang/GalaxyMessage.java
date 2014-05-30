/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/*
 * $Id: GalaxyMessage.java,v 1.3 2002/02/25 22:52:45 wohlever Exp $
 */

package galaxy.lang;

import java.util.Map;
import java.util.HashMap;

public abstract class GalaxyMessage {

    /**
     * Message Type information
     */
    public static final int GAL_OBJECT_MSG_TYPE = 0;
    public static final int GAL_MESSAGE_MSG_TYPE = 1;
    public static final int GAL_REPLY_MSG_TYPE = 2;
    public static final int GAL_DESTROY_MSG_TYPE = 3;
    public static final int GAL_BROKER_START_MSG_TYPE = 4;
    public static final int GAL_BROKER_END_MSG_TYPE = 5;
    public static final int GAL_ERROR_MSG_TYPE = 6;
    public static final int GAL_DISCONNECT_MSG_TYPE = 7;
    public static final int GAL_POSTPONE_MSG_TYPE = 8;

    protected int msgType;
    protected Object data;

    public Object getData() {
	return data;
    }

    public int getType() {
	return msgType;
    }

    public static final GalaxyMessage createMessage(Object data, int type) 
    throws Exception {
	switch(type) {
	case GAL_OBJECT_MSG_TYPE:
	    return new ObjectMessage(data);
	case GAL_MESSAGE_MSG_TYPE:
	    return new NewMessage(data);
	case GAL_REPLY_MSG_TYPE:
	    return new ReplyMessage(data);
	case GAL_DESTROY_MSG_TYPE:
	    return new DestroyMessage(data);
	case GAL_BROKER_START_MSG_TYPE:
	    return new BrokerStartMessage(data);
	case GAL_BROKER_END_MSG_TYPE:
	    return new BrokerEndMessage(data);
	case GAL_ERROR_MSG_TYPE:
	    return new ErrorMessage(data);
	case GAL_DISCONNECT_MSG_TYPE:
	    return new DisconnectMessage();
	case GAL_POSTPONE_MSG_TYPE:
	    return new PostponeMessage(data);
	default:
	    throw new Exception("GalaxyMessage doesn't know of any type: "+type);
	}
    } 

    public static Map typeToName = new HashMap();
    
    // Initialize type and name mappings
    static {
	// Build the type-to-name mapping.
	typeToName.put(new Integer(GalaxyMessage.GAL_OBJECT_MSG_TYPE),"GAL_OBJECT_MSG_TYPE");
	typeToName.put(new Integer(GalaxyMessage.GAL_MESSAGE_MSG_TYPE),"GAL_MESSAGE_MSG_TYPE");
	typeToName.put(new Integer(GalaxyMessage.GAL_REPLY_MSG_TYPE),"GAL_REPLY_MSG_TYPE");
	typeToName.put(new Integer(GalaxyMessage.GAL_DESTROY_MSG_TYPE),"GAL_DESTROY_MSG_TYPE");
   	typeToName.put(new Integer(GalaxyMessage.GAL_BROKER_START_MSG_TYPE),"GAL_BROKER_START_MSG_TYPE");
	typeToName.put(new Integer(GalaxyMessage.GAL_BROKER_END_MSG_TYPE),"GAL_BROKER_END_MSG_TYPE");
	typeToName.put(new Integer(GalaxyMessage.GAL_ERROR_MSG_TYPE),"GAL_ERROR_MSG_TYPE");
	typeToName.put(new Integer(GalaxyMessage.GAL_DISCONNECT_MSG_TYPE),"GAL_DISCONNECT_MSG_TYPE");
	typeToName.put(new Integer(GalaxyMessage.GAL_POSTPONE_MSG_TYPE),"GAL_POSTPONE_MSG_TYPE");
    }
    
    public static String getTypeName(int type) 
    {
	return (String) typeToName.get(new Integer(type));
    }
}
