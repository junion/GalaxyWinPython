/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.lang;

/**
 * This class encapsulates connection and type information for an out broker.
 */
public class BrokerProxy extends GalaxyObject 
{
    /** The type of the object brokered by the associated out broker. */
    private int objectType = -1;

    /** The out broker call id. */
    private String callId = null;

    /** The name of the machine hosting the out broker. */
    private String host = null;

    /** The port being used by the out broker. */
    private int port = -1;

   /**
     * Creates an out broker proxy object.
     *
     * @param callId call ID of the target out broker
     * @param host host address of the target out broker
     * @param port port of the target out broker
     * @param objectType type of object being served up by the target out 
     *                   broker. Valid values are -1 (untyped) or a
     *                   GalaxyObjcet type as defined in <code>
     *                   galaxy.lang.GalaxyObject</code>.
     */
    public BrokerProxy(String callId, String host, int port, int objectType)
    {
	type = GAL_PROXY;
	this.callId = callId;
	this.host = host;
	this.port = port;
	this.objectType = objectType;
    }

    public String toString()
    {
	return new String("[callId: " + callId + ", host: " + host +
			  ", port: " + port + ", objectType: " + objectType +
			  " (" + GalaxyObject.nameType(objectType) + ")]");
    }

    //=========================================================================
    // Gettor methods
    //=========================================================================
    
    public int getObjectType()
    {
	return objectType;
    } 
    
    public String getCallId()
    {
	return callId;
    }

    public String getHost()
    {
	return host;
    }

    public int getPort() 
    {
	return port;
    }
}
