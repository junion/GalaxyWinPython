/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */

/**
 * $Id: DataOutBroker.java,v 1.25 2002/06/03 22:00:00 wohlever Exp $
 */

package galaxy.server;

import java.net.Socket;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;
import java.util.Collection;
import java.util.Iterator;

import java.io.IOException;

import galaxy.io.GalaxyOutputStream;

import galaxy.lang.GFrame;
import galaxy.lang.DisconnectMessage;
import galaxy.lang.GalaxyMessage;

/**
 * This class represents the supplier of a brokering connection. It accepts
 * multiple client connections and serves data to these clients. A timeout
 * can be specified for <code>DataOutBroker</code>. Once the timeout expires,
 * no new client connections are accepted, but the broker will continue to
 * serve up data until it is done (i.e., when a <code>DisconnectMessage</code>
 * is written to its output data structure). The brokered data is cached and
 * the broker can serve this data to new clients as they connect.
 */
public class DataOutBroker implements Runnable
{
    /** This broker's call id. */
    private String callId;

    /** <code>Server</code> that created this broker. */
    private Server server = null;

    /** Polling thread. */
    private Thread thread = null; 

    /** Timeout (in seconds). */
    private int timeoutSeconds = 10;

    /** Polling cycle time (in milliseconds). */
    private int pollMilliseconds = 100;

    /** Start time of this broker (in millliseconds since 1/1/1970). */
    private long startTime = 0;

    /** Collection of client sockets served by this broker. */
    private List clientSockets;

    /** Collection of client <code>GalaxyOutputStream</code>s. */
    private List outStreams;

    /** Collection of client sockets keyed on their output streams. */
    private Map clientConnections;

    /** Collection of new client connections to be served by this broker. */
    private Map newClientConnections;
    
    /** Used to initialize size of some data structures. */
    private final int NUM_CLIENTS = 5;

    /** Collection of cached data served up to new clients of this broker. */
    private List cachedData;

    /** Collection of new data to be served up to clients of this broker. */
    private List newData;
    
    /** This flag indicates if this broker has timed out. */
    private volatile boolean timedOut = false;

    /** This flag indicates if this broker has a timeout. */
    private boolean timeoutSet = false;

    /** 
     * This flag indicates if this broker has received all the data it is
     * going to receive.
     */
    private boolean brokerDone = false;

    /** 
     * Initial size (number of object references) of this broker's data cache.
     */
    private final int INITIAL_CACHE_SIZE = 10;

    /** 
     * Initial size (number of object references) of this broker's incoming 
     * data buffer.
     */
    private final int INITIAL_BUFFER_SIZE = 10; 

    /** Flag that indicates if this broker is marked as running. */
    private volatile boolean isRunning = false;

    /** Flag that indicates if this broker is stopped. */
    private volatile boolean stopped = true;

    //=========================================================================
    // Static interface
    //=========================================================================

    /** Call id index. */
    private static int callIdIndex = 0;

    /** Use this has the lock on callIdIndex. */
    private static int callIdIndexLock[] = new int[1];

    /**
     * Returns a call id unique to the current Java virtual machine.
     * This method is only used internally and should not be called by
     * other code.
     *
     * @return the call id
     */
    public static int getCallIdIndex()
    {
	int newCallIdIndex  = 0;
	synchronized(callIdIndexLock) {
	    newCallIdIndex = ++callIdIndex;
	}
	return newCallIdIndex;
    } 

    //=========================================================================
    // Constructors
    //=========================================================================

    /**
     * Constructor.
     *
     * @param server the <code>Server</code> that created this broker
     * @param pollMilliseconds the polling cycle in milliseconds. If less than
     *                         or equal to zero, default poll is used.
     * @param timeoutSeconds timeout in seconds for this broker. If zero,
     *                       default timeout is used. If less than zero,
     *                       broker never times out.
     */
    public DataOutBroker(Server server, int pollMilliseconds, 
			 int timeoutSeconds) 
    {
	// Set the call id for this outgoing broker.
	callId = new String(server.getHostAddress() + ":" + 
			    DataOutBroker.getCallIdIndex());
	
	// Register this outgoing broker with the Server that created it.
	this.server = server;
	server.registerOutBroker(this);

	// If timeout set to 0, use default timeout. If timeout is < 0, never
	// timeout.
	if(timeoutSeconds != 0)
	    this.timeoutSeconds = timeoutSeconds;

	if(this.timeoutSeconds > 0)
	    timeoutSet = true;

	// If poll <= 0, use default poll.
	if(pollMilliseconds > 0)
	    this.pollMilliseconds = pollMilliseconds;

	// Initialize data structures.
	clientSockets = new ArrayList(NUM_CLIENTS);
	newClientConnections = new HashMap(NUM_CLIENTS);
	outStreams = new ArrayList(NUM_CLIENTS);
	cachedData = new ArrayList(INITIAL_CACHE_SIZE);
	newData = new ArrayList(INITIAL_BUFFER_SIZE);

	clientConnections = Collections.synchronizedMap(new HashMap(NUM_CLIENTS));

	// Try to start the main listener thread (in case it is not running).
	if(server.getMainServer().startBrokerListenerThread()) {
	    // Make sure the server has a listener port
	    while(!server.getMainServer().listenerHasPort()) {
		// Wait one second.
		try {
		    Thread.sleep(1000);
		} catch(InterruptedException iex) {
		}
	    }
		
	    // Record the start time and start the thread
	    startTime = System.currentTimeMillis();
	    thread = new Thread(this, "DataOutBrokerThread");
	    thread.start();
	}
    }

    /*
     * Legacy constructor. Only <code>Server</code> is still used. Default
     * polling and timeout values are used.
     * @deprecated as of Galaxy Communicator 3.0
     * @see #DataOutBroker(Server,int,int)
     *
    public DataOutBroker(Server server, int port, GFrame frame) 
    {
 	this(server, 0, 0);
    }
    */

    /*
     * Legacy constructor. Only <code>Server</code> is still used. Default
     * polling and timeout values are used.
     * @deprecated as of Galaxy Communicator 3.0
     * @see #DataOutBroker(Server,int,int)
     *
    public DataOutBroker(Server server, int port, GFrame frame, boolean rport) 
    {
	this(server, 0, 0);
    }   
    */

    //=========================================================================
    // Gettors and testers
    //=========================================================================

    /**
     * Returns the call id associated with this broker.
     *
     * @return the call id
     */
    public String getCallId()
    {
	return callId;
    }

    /**
     * Indicates if this broker has timed out.
     *
     * @return true this broker has timed out, false otherwise
     */
    public boolean isExpired()
    {
	return timedOut;
    }

    /**
     * Forces this broker to time out, preventing any additional client
     * connections from being established.
     */
    public void expire()
    {
	timedOut = true;
    }
  
    /**
     * Indicates if this broker is stopped.
     *
     * @return true if stopped, false otherwise
     */
    public boolean isStopped()
    {
	return stopped;
    }

    //=========================================================================
    // Thread management methods
    //=========================================================================
   
    /**
     * Outgoing broker thread runs a polling loop, waiting for data to
     * write to clients. It also writes cached data to new clients as they
     * connect.
     */
    public void run() 
    {
	stopped = false;
	isRunning = true;
	server.logMessage("Outgoing broker thread started (callId = " + callId + ").", 
			  MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
			  "DataOutBroker.run()");

	long currentTime = 0;
	
	while(true) {
	    
	    if(!isRunning) {
		break;
	    }
	    
	    // Break out of loop if the thread has timed out and the
	    // broker has been directed to disconnect from its clients
	    synchronized(newClientConnections) {
		if(timeoutSet) {
		    currentTime = System.currentTimeMillis();
		    if(!timedOut && (((currentTime-startTime) / 1000) >= timeoutSeconds)) {
			timedOut = true;
			server.logMessage("Outgoing broker thread timed out.", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
					  "DataOutBroker.run()");
		    }
		    
		    if(timedOut && brokerDone) {
			server.logMessage("Outgoing broker thread timed out and broker has received disconnect message.", 
					  MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
					  "DataOutBroker.run()");
			
			// Write the data to new clients that may have
			// connected before the time out but after the
			// disconnect message was sent
			writeDataToNewClients();
			break;
		    }
		}
	    }
	    
	    // Write data to clients
	    writeDataToAllClients();
	    
	    // Sleep
	    try{
		Thread.sleep(pollMilliseconds);
	    } catch(InterruptedException iex) {
		break;
	    }
	}
	
	
	// Write out remaining data
	writeDataToAllClients();

	prepareToStop();
	server.logMessage("Outgoing broker thread stopped (callId = " + callId + ").",
			  MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
			  "DataOutBroker.run()");
    } 

    /**
     * Marks this broker's thread as being stopped. 
     */
    public void stop()
    {	
	isRunning = false;
	close();
    } 

    /**
     * Performs cleanup and closes this broker's socket connections.
     */
    private void prepareToStop()
    {
	isRunning = false;
	
	try {
	    // Unregister this broker from its server.
	    server.unregisterOutBroker(this);

	    // Close the client connections.
	    int numClients = outStreams.size();
	    Socket socket;
	    GalaxyOutputStream out;
	    for(int idx=0; idx<numClients; ++idx) {
		out = (GalaxyOutputStream) outStreams.get(idx);
		socket = (Socket) clientConnections.get(out);
		out.close();
		socket.close();
	    }
	} catch(IOException ioex) {
	    server.logErrorMessage("Caught exception in outgoing broker while doing cleanup: " + ioex.toString(), ioex, "DataOutBroker.prepareToStop()");
	}
	stopped = true;
    }


    //=========================================================================
    // Data management methods
    //=========================================================================

    /**
     * Populates a frame that is announcing the availability of brokered data
     * with the broker's contact information. This includes the name of the
     * machine hosting the broker, the broker's listener port, and the 
     * broker's call id.
     * 
     *
     * @param frame the frame to populate
     * @param hostKey the name of the broker's host name frame key
     * @param portKey the name of the broker's listener port frame key
     */
    public void populateFrame(GFrame frame, String hostKey, String portKey)
    {
	frame.setProperty(hostKey, server.getHostAddress());
	frame.setProperty(portKey, server.getMainServer().getPort());
	frame.setProperty(GFrame.GAL_BROKER_CALL_ID_FRAME_KEY, getCallId());
    }

    /**
     * Writes new data to current clients, caches the data, and writes the
     * cached data to new clients. New clients are also added to the collection
     * of "current" clients.
     */
    private void writeDataToAllClients() 
    {
	writeDataToCurrentClients();
	writeDataToNewClients();
    }

    /**
     * Writes new data to current clients and caches the data.
     */
    private void writeDataToCurrentClients()
    {
	synchronized(newData) {
	    int newDataSize = newData.size();
	    int numClients = clientSockets.size();
	    GalaxyOutputStream out = null;
	    Object data;
	    try {
		for(int idx1=0; idx1<newDataSize; ++idx1) {
		    
		    // Get the next bit of new data.
		    data = newData.get(idx1);
		    
		    if(data instanceof DisconnectMessage) {
			brokerDone = true; 
			disconnectReceived();
		    }

		    // Cache the data.
		    cachedData.add(data);
		    
		    // Send the data to current clients.
		    synchronized(outStreams) {
			for(int idx2=0; idx2<numClients; ++idx2) {
			    out = (GalaxyOutputStream) outStreams.get(idx2);
			    if(data instanceof DisconnectMessage) {
				out.writeEOT((DisconnectMessage) data);
			    } else {
				out.writeObject(data, GalaxyMessage.GAL_OBJECT_MSG_TYPE);
			    }
			}
		    }
		}
	    } catch(IOException ioex) {
		server.logErrorMessage("Caught exception in outgoing broker while sending data to current clients: " + ioex.toString(), ioex,
				       "DataOutBroker.writeDataToCurrentClients()");
		
		// Remove the offending client connection
		removeClientConnection(out);
	    }
	    
	    // Clear the collection of "new" data.
	    newData.clear();
	}
    }

    /**
     * Writes cached data to new clients and adds those clients to the 
     * collection of "current" clients.
     */
    private void writeDataToNewClients()
    {
	synchronized(newClientConnections) {
	    Socket clientSocket;
	    GalaxyOutputStream out = null;
	    Object data;
	    int cachedDataSize = 0;

	    try {
		Collection newClientCollection = newClientConnections.values();
		Iterator iterator = newClientCollection.iterator();
		ClientConnection connection;
		List removeList = new ArrayList();
		while(iterator.hasNext()) {
		    connection = (ClientConnection) iterator.next();
		    if(connection.isHandshakeComplete()) {
			
			// Add the new client to the collection of 
			// "current" clients.
			clientSocket = (Socket) connection.getClientSocket(); 
			
			// This client can be removed from the list of new 
			// clients.
			removeList.add(clientSocket);
			
			try{
			    out = new GalaxyOutputStream(clientSocket.getOutputStream());
			} catch(IOException ioe) {
			    // ...in case the socket has closed since we got it
			    continue;
			}

			synchronized(outStreams) {
			    outStreams.add(out);
			}
			
			clientSockets.add(clientSocket);
			
			clientConnections.put(out, clientSocket);
			
			// Send the cached data, if any.
			cachedDataSize = cachedData.size();
			for(int idx2=0; idx2<cachedDataSize; ++idx2) {
			    data = cachedData.get(idx2);
			    if(data instanceof DisconnectMessage) {
				out.writeEOT((DisconnectMessage) data);
			    } else {
				out.writeObject(data, GalaxyMessage.GAL_OBJECT_MSG_TYPE);
			    }
			}	
		    }
		}

		// Remove processed clients from the list of new clients.
		// Note that this cannot be done in the while loop above
		// since we cannot iterate over the collection and remove
		// from it concurrently.
		int removeCount = removeList.size();
		for(int idx=0; idx<removeCount; ++idx) {
		    newClientConnections.remove(removeList.get(idx));
		}

	    } catch(IOException ioex) {
		server.logErrorMessage("Caught exception in outgoing broker while sending data to new clients: " + ioex.toString(), ioex, "DataOutBroker.writeDataToNewClients()");

		// Remove the offending client connection
		removeClientConnection(out);
	    }
	}
    }

    /**
     * Removes a client connection from this broker.
     *
     * @param out the output stream of the connection to remove
     */
    private void removeClientConnection(GalaxyOutputStream out) 
    {
	Socket clientSocket = (Socket) clientConnections.get(out);
	if(clientSocket != null) {

	    // Close and remove the output stream.
	    synchronized(outStreams) {
		try {
		    out.close();
		    outStreams.remove(out);
		} catch(IOException ioex) {
		}
	    }

	    // Try to close the connection.
	    try {
		clientSocket.close();
	    } catch(IOException ioex) {
	    }

	    // Remove the entry from the out stream/socket mapping.
	    clientConnections.remove(out);

	    // Remove the client socket.
	    synchronized(clientSockets) {
		clientSockets.remove(clientSocket);
	    }
	}
    }
	
    /**
     * Adds data to this broker's incoming data buffer. This data will be
     * written out to the broker's clients during the next polling cycle.
     *
     * @param obj the data object to add to the buffer
     */
    public void write(Object obj)
    {
	synchronized(newData) {
	    newData.add(obj);
	}
    }
    
    /**
     * Tells this broker to disconnect from its clients once it times out.
     */
    public void close()
    {
	synchronized(newData) {
	    newData.add(new DisconnectMessage());
	}
    }

    /**
     * This method is called when this broker establishes a connection with
     * a new inbound broker.
     */
    protected void connectionEstablished()
    {
    }

    /**
     * This method is called when this broker is told to disconnect from 
     * its client inbound brokers (once it times out).
     */
    protected void disconnectReceived()
    {
    }

    /**
     * Adds a new client socket connection to this broker.
     *
     * @param clientSocket the client socket to add
     * @return true if this broker has not timed out and the client was
     *         added, false otherwise
     */
    boolean addClientSocket(Socket clientSocket)
    {
	synchronized(newClientConnections) {
	    if(!timedOut) {
		newClientConnections.put(clientSocket,
					 new ClientConnection(clientSocket));
		return true;
	    }	
	}
	return false;
    }

    /**
     * Marks the specified client as having completed the handshake.
     *
     * @param clientSocket the socket of the client
     * @return true if the client was found, false otherwise
     */
    boolean setClientHandshakeComplete(Socket clientSocket)
    {
	synchronized(newClientConnections) {
	    ClientConnection connection = (ClientConnection) newClientConnections.get(clientSocket);
	    if(connection == null) {
		return false;
	    }
	    connection.setHandshakeComplete(true);
	    connectionEstablished();
	    return true;
	}	
    }

    private class ClientConnection
    {
	private Socket clientSocket;
	private boolean handshakeComplete;

	public ClientConnection(Socket clientSocket)
	{
	    this.clientSocket = clientSocket;
	    handshakeComplete = false;
	}

	public void setHandshakeComplete(boolean handshakeComplete)
	{
	    this.handshakeComplete = handshakeComplete;
	}

	public Socket getClientSocket()
	{
	    return clientSocket;
	}

	public boolean isHandshakeComplete()
	{
	    return handshakeComplete;
	}
    }
}
