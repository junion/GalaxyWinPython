/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

import java.io.IOException;

import galaxy.lang.*;

/**
 * This class encapsulates a <code>DataInBroker</code>.
 */
public class DataInBrokerProxy
{
    protected BrokerProxy proxy = null;
    protected Server server = null;
    private InBroker inBroker = null;
    protected boolean immediate = false;
    private boolean brokerSet = false;
    private final int BROKER_START_WAIT_MS = 1;

    /**
     * Creates an inbound broker proxy.
     *
     * @param server the <code>Server</code> that is creating this proxy
     * @param proxy the <code>Proxy</code> that is associated with the
     *              target <code>DataOutBroker</code>.
     * @param immediate if true, the <code>receivedObject</code> method of
     *                  this proxy is called each time the broker receives
     *                  an object's worth of data.
     * @param startBroker if true, the <code>DataInBroker</code> associated
     *                    with this proxy is started
     */
    public DataInBrokerProxy(Server server, BrokerProxy proxy, 
			     boolean immediate, boolean startBroker) 
	throws IOException
    {
	this.proxy = proxy;
	this.server = server;
	this.immediate = immediate;
	brokerSet = true;
	inBroker = new InBroker(server, proxy.getHost(), 
				proxy.getPort(),
				proxy.getCallId(), 
				proxy.getObjectType(),
				this, immediate, startBroker);
    } 

    /**
     * If this proxy is configured to handle data immediately as it is read
     * into the associated <code>DataInBroker</code>, this method is called 
     * each time the broker receives an object's worth of data. If this proxy 
     * is not configured to handle data as it comes in, this method is only 
     * called when the broker receives a disconnect message from the outgoing 
     * broker. Override this method to provide your own data handling routine.
     */
    protected void receivedObject(Object obj)
    {
    }

    /**
     * This method is called when the <code>DataInBroker</code> that is
     * associated with this proxy receives a disconnect message
     * from the outgoing broker (indicates all data has been received).
     * Override this method to provide your own customized finalization 
     * routine. This broker is stopped after this method is called.
     */
    protected void disconnectReceived()
    {	
    }

    /**
     * This method is called when the <code>DataInBroker</code> that is
     * associated with this proxy catches an unexpected exception while 
     * receiving brokered data.
     * Override this method to provide your own customized error handling 
     * routine. This broker is stopped after this method is called.
     */
    protected void abortReceived()
    {
    }

    /**
     * This cleanup method is called when the <code>DataInBroker</code> that is
     * associated with this proxy is preparing to stop. Override this method to
     * provide your own customized cleanup routine.
     * The generic <code>DataInBroker</code> cleanup will still take place.
     */
    protected void cleanup()
    {
    }

    /**
     * This method starts the underlying <code>DataInBroker</code>, if it 
     * exists.
     */
    public void start()
    {
	if(inBroker != null)
	    inBroker.start();
    }
	    
    /**
     * Returns the object associated with this broker. If the target
     * outbound broker is brokering an "any" stream (e.g., the
     * <code>DataOutBrokerProxy</code> object is brokering an object of type
     * -1), this method returns null. Use the <code>receivedObject</code>
     * method to retrieve and processe the brokered data in this case.
     *
     * @param wait if true, this method will not return until a non-null
     *             object is received, else it will return immediately
     *             (returning the object even if it is still null).
     * @return the object associated with this broker (may be null).
     * @throws IOException if there was a problem while creating the in broker
     *         and retrieving the object
     */
    public Object getObject(boolean wait) throws IOException
    {
	return inBroker.getObject(wait);
    }

    private class InBroker extends DataInBroker
    {
	private Object obj = null;
	private int objectType = -1;
	private boolean immediate = false;

	private GalaxyObject expandableObj = null;
	private DataInBrokerProxy proxy = null;

	private final int GET_OBJECT_WAIT_MS = 1;

	public InBroker(Server server, String host, int port, String callId,
			int objectType, DataInBrokerProxy proxy,
			boolean immediate, boolean startBroker)
	    throws IOException 
	{
	    super(server, host, port, callId);
	    this.objectType = objectType;

	    switch(this.objectType) {
	    case GalaxyObject.GAL_LIST:
		expandableObj = new GVector();
		break;
	    case GalaxyObject.GAL_BINARY:
		expandableObj = new GBinary();
		break;
	    case GalaxyObject.GAL_INT_16:
		expandableObj = new Int16();
		break;
	    case GalaxyObject.GAL_INT_32:
		expandableObj = new Int32();
		break;
	    case GalaxyObject.GAL_INT_64:
		expandableObj = new Int64();
		break;
	    case GalaxyObject.GAL_FLOAT_32:
		expandableObj = new Float32();
		break;
	    case GalaxyObject.GAL_FLOAT_64:
		expandableObj = new Float64();
		break;
	    }

	    this.proxy = proxy;
	    this.immediate = immediate;
	    if(objectType == -1)
	      this.immediate = true;

	    if(startBroker)
		start();
	}

	public void start()
	{
	    super.start();
	    while(!wasStarted()) {
		try {
		    Thread.sleep(BROKER_START_WAIT_MS);
		} catch(InterruptedException iex) {
		    this.server.logWarningMessage("Interruption occurred while for broker to start.", "DataInBrokerProxy.InBroker constructor");
		}
	    }
	}

	/**
	 * Returns the object associated with this broker.
	 *
	 * @param wait if true, this method will not return until a non-null
	 *             object is received, else it will return immediately
	 *             (returning the object even if it is still null). In
	 *             either case, if the underlying broker is stopped, the 
	 *             object is returned immediately.
	 *
	 * @return the object (if available) or null if a) wait is true and
	 *         the object is not available or b) if the object type of
	 *         the target outbound broker is -1 (i.e., any data), in
	 *         which case this method always returns null (use
	 *         receivedObject instead)
	 *             
	 */
	public Object getObject(boolean wait)
	{
	    // If reading an "any stream", return null.
	    if(objectType == -1)
		return null;

	    if(wait) {
		while(obj == null && !isStopped()) {
		    try {
			Thread.sleep(GET_OBJECT_WAIT_MS);
		    } catch(InterruptedException iex) {
			this.server.logWarningMessage("Interruption occurred while broker was waiting for object.", "DataInBrokerProxy.InBroker.getObject(boolean)");
		    }
		}
	    }
	    return obj;
	}

	protected void disconnectReceived()
	{	
	    if(expandableObj != null) {
		obj = expandableObj;	
	    }

	    if(!immediate)
		proxy.receivedObject(obj);
	    
	    proxy.disconnectReceived();
	}

	protected void abortReceived()
	{
	    proxy.abortReceived();
	}

	protected void cleanup()
	{
	    proxy.cleanup();
	}

	private void receivedObject(Object obj)
	{
	    switch(objectType) {
	    case -1:
		break;
	    case GalaxyObject.GAL_STRING:
	    case GalaxyObject.GAL_INT:
	    case GalaxyObject.GAL_FLOAT:
	    case GalaxyObject.GAL_SYMBOL:
	    case GalaxyObject.GAL_PROXY:
	    case GalaxyObject.GAL_FRAME:
		this.obj = obj;
		break;
	    case GalaxyObject.GAL_BINARY:
		((GBinary) expandableObj).append((GBinary) obj);
		break;
	    case GalaxyObject.GAL_INT_16:
		((Int16) expandableObj).append((Int16) obj);
		break;
	    case GalaxyObject.GAL_INT_32:
		((Int32) expandableObj).append((Int32) obj);
		break;
	    case GalaxyObject.GAL_INT_64:
		((Int64) expandableObj).append((Int64) obj);
		break;
	    case GalaxyObject.GAL_FLOAT_32:
		((Float32) expandableObj).append((Float32) obj);
		break;
	    case GalaxyObject.GAL_FLOAT_64:
		((Float64) expandableObj).append((Float64) obj);
		break;
	    case GalaxyObject.GAL_LIST:
		((GVector) expandableObj).addElement(obj);
		break;
	    }
	    
	    if(immediate)
		proxy.receivedObject(obj);
	}
	
	public void receivedString(String str) 
	{ 
	    receivedObject(str);
	}
	
	public void receivedBinary(GBinary bin)  
	{
	    receivedObject(bin);
	}
	
	public void receivedFrame(GFrame frame) 
	{
	    receivedObject(frame);
	}
	
	public void receivedList(GVector list) 
	{	  
	    receivedObject(list);
	}
	
	public void receivedInt16(Int16 int16) 
	{
	    receivedObject(int16);
	}
	
	public void receivedInt32(Int32 int32) 
	{
	    receivedObject(int32);
	}
	
	public void receivedInt64(Int64 int64) 
	{
	    receivedObject(int64);
	}
	
	public void receivedFloat32(Float32 float32) 
	{
	    receivedObject(float32);
	}
	
	public void receivedFloat64(Float64 float64) 
	{
	    receivedObject(float64);
	}
	
	public void receivedInteger(Integer integerObject) 
	{
	    receivedObject(integerObject);
	}
	
	public void receivedFloat(Float floatObject) 
	{
	    receivedObject(floatObject);
	}
	
	public void receivedSymbol(Symbol symbol) 
	{
	    receivedObject(symbol);
	}
	
	public void receivedProxy(BrokerProxy proxy) 
	{
	    receivedObject(proxy);
	}
    }
}
