/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

import java.util.Enumeration;

import galaxy.lang.*;

/**
 * This class encapsulates a <code>DataOutBroker</code>.
 */
public class DataOutBrokerProxy
{
    private Object brokeredObject = null;
    private int brokeredObjectType = -1;
    
    private DataOutBroker outBroker = null; 
    private BrokerProxy proxy = null;

    private Server server = null;

    private int pollMilliseconds = 0;
    private int timeoutSeconds = 0;

    /**
     * Creates an out broker proxy object.
     *
     * @param obj the object that is to be brokered
     * @param server the <code>Server</code> that created this proxy object
     * @param pollMilliseconds the polling cycle in milliseconds of the 
     *                         underlying out broker. If less than
     *                         or equal to zero, default poll is used.
     * @param timeoutSeconds timeout in seconds for the broker. If zero,
     *                       default timeout is used. If less than zero,
     *                       broker never times out.
     *
     * @throws IllegalArgumentException If an object that cannot be brokered
     *                                  is passed in.
     */
    public DataOutBrokerProxy(Object obj, Server server,
			      int pollMilliseconds, 
			      int timeoutSeconds)
	throws IllegalArgumentException
    {
	brokeredObjectType = GalaxyObject.getTypeForObject(obj);
	if(brokeredObjectType == -1) {
	    String objectString = "null";
	    if(obj != null)
		objectString = obj.toString();
	    throw new IllegalArgumentException("Cannot broker specified object: " + objectString);
	}

	brokeredObject = obj;	
	this.server = server;
	this.pollMilliseconds = pollMilliseconds;
	this.timeoutSeconds = timeoutSeconds;

	initBroker();
	brokerObject(obj, true);
    }

    /**
     * Starts the underlying <code>DataOutBroker</code> and creates
     * the associated <code>BrokerProxy</code> object.
     */
    private void initBroker()
    {
	outBroker = new OutBroker(server, pollMilliseconds, 
				  timeoutSeconds, this);

	proxy = new BrokerProxy(outBroker.getCallId(), server.getHostAddress(),
				server.getMainServer().getPort(), 
				brokeredObjectType);
    }

    /**
     * Brokers an object. If the object is a <code>GVector</code>, it
     * is brokered as one object (e.g., the list elements are not
     * brokered as individual objects).
     *
     * @param obj the object to broker. If null, this method does nothing.
     */
    private void brokerObject(Object obj)
    {
	brokerObject(obj, false);
    }

    /**
     * Brokers an object. 
     *
     * @param obj the object to broker. If null, this method does nothing.
     * @param encodeListElements if true and the object is a 
     *                           <code>GVector</code>, the list elements are
     *                           brokered as individual objects.
     */
    private void brokerObject(Object obj, boolean encodeListElements) 
    {
	if(obj == null)
	    return;
      
	switch(brokeredObjectType) {
	case GalaxyObject.GAL_LIST:
	    if(encodeListElements) {
		Enumeration elements = ((GVector) obj).elements();
		Object listElement;
		while(elements.hasMoreElements()) {
		    listElement = elements.nextElement();
		    outBroker.write(listElement);
		}
	    } else {
		outBroker.write(obj);
	    }
	    break;
	case GalaxyObject.GAL_BINARY:
	case GalaxyObject.GAL_INT_16:
	case GalaxyObject.GAL_INT_32:
	case GalaxyObject.GAL_INT_64:
	case GalaxyObject.GAL_FLOAT_32:
	case GalaxyObject.GAL_FLOAT_64:
	    outBroker.write(obj);
	    break;
	default:
	    outBroker.write(obj);

	    // Since these objects are not expandable, we can close the broker
	    // now.
	    close();
	}
    }

    /**
     * Creates an out broker proxy object.
     *
     * @param objectType the type of object that is to be brokered. If set to
     *                   -1 any type of object can be brokered, including 
     *                   heterogeneous objects (e.g., both Int32 and Float64 
     *                   objects).
     * @param server the <code>Server</code> that created this proxy object
     * @param pollMilliseconds the polling cycle in milliseconds of the 
     *                         underlying out broker. If less than
     *                         or equal to zero, default poll is used.
     * @param timeoutSeconds timeout in seconds for the broker. If zero,
     *                       default timeout is used. If less than zero,
     *                       broker never times out.
     *
     * @throws IllegalArgumentException If an object type that cannot be 
     *                                  brokered is passed in.
     */
    public DataOutBrokerProxy(int objectType, Server server,
			      int pollMilliseconds, 
			      int timeoutSeconds)
	throws IllegalArgumentException
    {
	
	if(objectType != -1 && 
	   GalaxyObject.nameType(objectType) == null)
	    throw new IllegalArgumentException("Cannot broker specified object type: " + objectType);
	
	brokeredObjectType = objectType;
	this.server = server;
	this.pollMilliseconds = pollMilliseconds;
	this.timeoutSeconds = timeoutSeconds;
	
	initBroker();
    }

    /**
     * Closes the out broker.
     *
     * @return true if the out broker was closed, false if there is no
     *         out broker
     */
    public boolean close()
    {
	if(outBroker == null)
	    return false;

	outBroker.close();
	return true;
    }

    /**
     * Brokers an obj.
     * 
     * @param obj the object to broker
     */
    public void write(Object obj)
    {
	int type = GalaxyObject.getTypeForObject(obj);
	if(brokeredObjectType == -1)
	    outBroker.write(obj);
	else if(brokeredObjectType == GalaxyObject.GAL_LIST)
	    addObjectToList(obj);
	else if(GalaxyObject.isArrayType(brokeredObjectType) &&
		obj instanceof ArrayObject)
	    addArrayToArray((ArrayObject) obj);
	else if(type == brokeredObjectType) {
	    outBroker.write(obj);
	    outBroker.close();
	}
    }

    /**
     * Adds an object to the list (i.e., <code>GVector</code>) that is
     * being brokered by this proxy. If the object is not a valid
     * <code>GalaxyObject</code> or if this proxy is not brokering
     * a list, this method does nothing.
     * 
     * @param obj the object to broker
     */
    public void addObjectToList(Object obj)
    {
	if(GalaxyObject.getTypeForObject(obj) == -1) {
	    String objectString = "null";
	    if(obj != null)
		objectString = obj.toString();
	    server.logWarningMessage("Cannot broker specified object: " + objectString, "DataOutBrokerProxy.addObjectToList(Object)");
	    return;
	}
	
	if(brokeredObjectType !=  GalaxyObject.GAL_LIST) { 
	    server.logWarningMessage("This broker is brokering an object of type " + brokeredObjectType + " (" + GalaxyObject.nameType(brokeredObjectType) + "), which is not a list type.", "DataOutBrokerProxy.addObjectToList(Object)");
	    return;
	}
	
	brokerObject(obj);
    }

    /**
     * Adds an array to the array (i.e., instance of a class that
     * implements the <code>ArrayObject</code> interface) that is
     * being brokered by this proxy. If the specified array is not of the
     * same type as the array that is being brokered by this proxy or if this 
     * proxy is not brokering an array, this method does nothing.
     * 
     * @param array the array to broker
     */
    public void addArrayToArray(ArrayObject array)
    {
	if(!GalaxyObject.isArrayType(brokeredObjectType)) {
	    server.logWarningMessage("This broker is brokering an object of type " + brokeredObjectType + " (" + GalaxyObject.nameType(brokeredObjectType) + "), which is not an array type.", "DataOutBrokerProxy.addArrayToArray(Object)");
	    return;
	}
	
	if(GalaxyObject.getTypeForObject(array) != brokeredObjectType) {
	    server.logWarningMessage("Tried to broker an object of a type other than " + brokeredObjectType + " (" + GalaxyObject.nameType(brokeredObjectType) + ").", "DataOutBrokerProxy.addArrayToArray(Object)");
	    return;
	}
	
	brokerObject(array);
    }
     
    //=========================================================================
    // Gettor methods
    //=========================================================================

    public Object getObject()
    {
	return brokeredObject;
    }
    
    public int getObjectType()
    {
	return brokeredObjectType;
    } 
   
    public DataOutBroker getOutBroker()
    {
	return outBroker;
    }

    public String getCallId()
    {
	return proxy.getCallId();
    }

    public String getHost()
    {
	return proxy.getHost();
    }

    public int getPort() 
    {
	return proxy.getPort();
    }

    public Server getServer() 
    {
	return server;
    }

    public BrokerProxy getProxy() 
    {
	return proxy;
    }

    /**
     * This method is called when this proxy's broker establishes a connection
     * with a new inbound broker.
     */
    protected void connectionEstablished()
    {
    }

    /**
     * This method is called when this proxy's broker is told to disconnect 
     * from its client inbound brokers (once it times out).
     */
    protected void disconnectReceived()
    {
    }

    private class OutBroker extends DataOutBroker
    {
	private DataOutBrokerProxy proxy;
	
	public OutBroker(Server server, int pollMs, int timeoutSecs, 
			 DataOutBrokerProxy proxy) throws IllegalStateException
	{
	    super(server, pollMs, timeoutSecs);
	    this.proxy = proxy;
	}
	
	protected void connectionEstablished()
	{
	    proxy.connectionEstablished();
	} 

	protected void disconnectReceived()
	{
	    proxy.disconnectReceived();
	}
    }
}
