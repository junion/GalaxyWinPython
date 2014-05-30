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
 * $Id: DataInBroker.java,v 1.23 2002/03/15 17:49:29 wohlever Exp $
 */

package galaxy.server;

import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;

import java.io.EOFException;
import java.io.IOException;

import galaxy.io.GalaxyInputStream;
import galaxy.io.GalaxyOutputStream;
import galaxy.io.EOTException;

import galaxy.lang.GFrame;
import galaxy.lang.Clause;
import galaxy.lang.GalaxyMessage;
import galaxy.lang.GalaxyObject;
import galaxy.lang.Int16;
import galaxy.lang.Int32;
import galaxy.lang.Int64;
import galaxy.lang.Float32;
import galaxy.lang.Float64;
import galaxy.lang.GBinary;
import galaxy.lang.GVector;
import galaxy.lang.Symbol;
import galaxy.lang.BrokerProxy;

/**
 * This class represents the consumer of a brokering connection. Subclasses of
 * this class try to establish a connection to specified outgoing broker (i.e.,
 * the data supplier), receive the brokered data, and process the data.
 */
public abstract class DataInBroker implements Runnable
{  

    /** The <code>Server</code> that created this broker. */
    protected Server server;

    /** 
     * The IP address of the server that manages access to 
     * the outgoing broker to which this broker wishes to connect.
     */
    private InetAddress ip;

    /**
     * The "listener" port of the target server.
     */
    private int port;
    
    /** This broker's thread. */
    private Thread thread;

    /** This broker's socket. */
    private Socket socket;

    /** This broker's data input stream. */
    private GalaxyInputStream in;

    /** This broker's data output stream. */
    private GalaxyOutputStream out;

    /** 
     * The call environment of the server dispatch function that created this
     * broker.
     */
    protected Environment env;

    /** 
     * Flag that indicates if this broker has established a connection to the 
     * target outgoing broker.
     */
    private volatile boolean brokerConnectionEstablished = false;

    /** Flag that indicates if this broker is running. */
    private volatile boolean isRunning = false;

    /** 
     * Reference to this broker (used in methods that may be executed in the 
     * other threads.
     */
    private DataInBroker inBroker;

    /** 
     * Flag that indicates if this broker is registered with the 
     * <code>Server</code> that started it.
     */
    private volatile boolean registeredWithServer = false;

    /** Frame dispatcher of the <code>Server</code> that started this broker */
    protected FrameDispatcher frameDispatcher;

    /** Duration of sleep in thread loop in milliseconds. */
    private final int SLEEP_MS = 50;

    /** Flag that indicates if this broker is stopped. */
    private volatile boolean stopped = true; 

    /** Flag that indicates if this broker was started. */
    private volatile boolean wasStarted = false;

    /** 
     * The call id of the outgoing broker to which this broker wishes to 
     * connect.
     */
    private String callId = null;

    //=========================================================================
    // Constructors
    //=========================================================================

    /**
     * Constructor.
     *
     * @param server the <code>Server</code> that created this broker
     * @param ip the IP address of the server that manages access to the 
     *           outgoing broker to which this broker wishes to connect
     * @param port the "listener" port of the target server
     * @param frame a <code>GFrame</code> that contains the call id of the
     *              outgoing broker to which this broker wishes to connect
     * @deprecated As of Galaxy Communicator 4.0. Use DataInBroker(Server, 
     *             InetAddress, int, String). 
     */
    public DataInBroker(Server server, InetAddress ip, int port, GFrame frame) 
	throws IOException 
    {
	inBroker = this;

	this.port = port;
	this.ip = ip;
	callId = frame.getString(GFrame.GAL_BROKER_CALL_ID_FRAME_KEY);
	setServer(server);

	// Establish the socket connection with the target server.
	socket = new Socket(ip, port);
	in = new GalaxyInputStream(socket.getInputStream());
	out = new GalaxyOutputStream(socket.getOutputStream());
    }

    /**
     * Constructor.
     *
     * @param server the <code>Server</code> that created this broker
     * @param hostName the host name of the server that manages access to the
     *                 outgoing broker to which this broker wishes to connect
     * @param port the "listener" port of the target server
     * @param frame a <code>GFrame</code> that contains the call id of the
     *              outgoing broker to which this broker wishes to connect
     * @deprecated As of Galaxy Communicator 4.0. Use DataInBroker(Server, 
     *             String, int, String). 
     */
    public DataInBroker(Server server, String hostName, int port, 
			GFrame frame) 
	throws IOException 
    {
	this(server, InetAddress.getByName(hostName), port, frame);
    }

    /**
     * Default constructor.
     */
    public DataInBroker()
    {
    }

    /**
     * Constructor.
     *
     * @param server the <code>Server</code> that created this broker
     * @param ip the IP address of the server that manages access to the 
     *           outgoing broker to which this broker wishes to connect
     * @param port the "listener" port of the target server
     * @param callId the call id of the outgoing broker to which this broker 
     *               wishes to connect
     *
     */
    public DataInBroker(Server server, InetAddress ip, int port, String callId) 
	throws IOException 
    {
	inBroker = this;

	this.port = port;
	this.ip = ip;
	this.callId = callId;
	setServer(server);

	// Establish the socket connection with the target server.
	socket = new Socket(ip, port);
	in = new GalaxyInputStream(socket.getInputStream());
	out = new GalaxyOutputStream(socket.getOutputStream());
    }

    /**
     * Constructor.
     *
     * @param server the <code>Server</code> that created this broker
     * @param hostName the host name of the server that manages access to the
     *                 outgoing broker to which this broker wishes to connect
     * @param port the "listener" port of the target server
     * @param callId the call id of the outgoing broker to which this broker 
     *               wishes to connect
     */
    public DataInBroker(Server server, String hostName, int port, 
			String callId) 
	throws IOException 
    {
	this(server, InetAddress.getByName(hostName), port, callId);
    }

    /**
     * Constructor.
     *
     * @param server the <code>Server</code> that created this broker
     * @param proxy the broker proxy to use to initialize this broker
     */
    public DataInBroker(Server server, BrokerProxy proxy) 
	throws IOException 
    {
	this(server, proxy.getHost(), proxy.getPort(), proxy.getCallId());
    }


    //=========================================================================
    // Settor/gettor and test methods
    //=========================================================================
   
    /**
     * Returns the reference to the <code>Server</code> that created this
     * broker.
     *
     * @return the server reference
     */
    public Server getServer() 
    {
	return server;
    }

    /**
     * Sets the reference to the <code>Server</code> that is associated with
     *  this broker.
     *
     * @param server the server reference
     */
    protected void setServer(Server server)
    {
	this.server = server;
	if(server != null) {
	    frameDispatcher = server.getFrameDispatcher();
	    this.env = server.getCopyOfCurrentEnvironment();
	}
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

    /**
     * Indicates if this broker was started.
     *
     * @return true if this broker was started, false otherwise
     */
    public boolean wasStarted()
    {
	return wasStarted;
    }

    /**
     * Resets (i.e., sets to false) the flag that indicates if this broker 
     * was started.
     */
    public void resetWasStarted()
    {
	wasStarted = false;
    }

    //=========================================================================
    // Incoming broker handshake methods
    //=========================================================================
 
    /**
     * Performs the incoming broker portion of the brokering handshake.
     */
    private void doHandshake()
    {
	GFrame handshakeFrame = new Clause("handshake");
	handshakeFrame.setProperty(GFrame.GAL_CONNECTION_TYPE_FRAME_KEY,
				   MainServer.GAL_BROKER_LISTENER);
       
	if(callId == null) {
	    server.logErrorMessage("Broker client did not receive a call id.",
				   "DataInBroker.doHandshake()");
	    return;
	}

	// ...and set it in the handshake frame that is going to the target
	// server.
	handshakeFrame.setProperty(GFrame.GAL_BROKER_CALL_ID_FRAME_KEY,
				   callId); 
	handshakeFrame.setProperty(":protocol_version", 1);
	
	// Do the handshake with the target server.
	try {

	    // Send the handshake frame to the server.
	    server.logMessage("\nBroker client sending frame:\n" +
			      handshakeFrame.toFormattedString(),
			      MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
				   "DataInBroker.doHandshake()");

	    Environment handshakeEnvironment = new Environment(handshakeFrame,
							       in, out);
	    handshakeEnvironment.writeFrame(handshakeFrame);

	    // Get the handshake reply from the server
	    GalaxyMessage replyMsg = in.readMessage();
	    GFrame replyFrame = (GFrame) replyMsg.getData();

	    server.logMessage("\nBroker client receiving frame:\n" +
			      replyFrame.toFormattedString(),
			      MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
			      "DataInBroker.doHandshake()");
	    
	    // Report handshake error if one occurred.
	    if(replyFrame.getName().toString().equals("system_error")) {
		int errno = ((Integer)replyFrame.getProperty(GFrame.GAL_ERROR_NUMBER_FRAME_KEY)).intValue();
		String msg = (String)replyFrame.getProperty(GFrame.GAL_ERROR_DESCRIPTION_FRAME_KEY);
		server.logErrorMessage("System error during broker client handshake\nError number " + errno + ": " + msg, "DataInBroker.doHandshake()");
		brokerConnectionEstablished = false;
	    } else {
		brokerConnectionEstablished = true;
	    }
	} catch(Exception ex) {
	    server.logErrorMessage("Caught exception during broker client handshake: " + ex.toString(), ex, "DataInBroker.doHandshake()");
	}
    }

    //=========================================================================
    // Thread management methods
    //=========================================================================

    /**
     * Performs the broker client side of the handshake and starts the broker
     * thread, assuming the handshake succeeded.
     */
    public void start()
    {
	// Perform the brokering handshake.
	doHandshake();

	// Start this broker's thread.
	if(brokerConnectionEstablished) {
	    thread = new Thread(this, "DataInBrokerThread");
	    thread.start();
	}
    }

    /**
     * The thread routine for this broker. While this broker's socket 
     * connection is open, data is read off the connection and processed.
     * by the various <code>received</code> methods defined by
     * subclasses of this class.
     */
    public void run() 
    {	
	stopped = false;
	isRunning = true;
	wasStarted = true;

	Object object;

	while(true) {

	    // Break out of outer loop if thread has been stopped.
	    if(!isRunning) {
		break;
	    }

	    object = null;
	    
	    try {
		object = in.readObject();
	    } catch (EOTException eotex) {
		disconnectReceived();
		// Received disconnect message.
		prepareToStop();
		server.logMessage("Incoming broker thread stopped.",
				  MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
				  "DataInBroker.run()");
		return;
	    } catch (EOFException eofex) {
		server.logErrorMessage("Exception in broker client while " +
				       "reading from socket: " + eofex.toString(), eofex, "DataInBroker.run()");
		abortReceived();
		prepareToStop();
		server.logMessage("Incoming broker thread stopped.",
				  MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
				  "DataInBroker.run()");
		return;
	    } catch (SocketException sex) {
		server.logErrorMessage("Exception in broker client while " +
				       "reading from closed socket: " + sex.toString(), sex, "DataInBroker.run()"); 
		abortReceived();
		prepareToStop();
		server.logMessage("Incoming broker thread stopped.",
				  MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
				  "DataInBroker.run()");
		return;
	    } catch (Exception ex) {
		server.logErrorMessage("Exception in broker client while " +
				       "reading from socket: " + ex.toString(), ex, "DataInBroker.run()");
		abortReceived();
		prepareToStop();
		server.logMessage("Incoming broker thread stopped.",
				  MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
				  "DataInBroker.run()");
		return;
	    }
	    

	    if(object instanceof GalaxyObject) {
		int objType = ((GalaxyObject) object).getType();
		
		switch(objType) {
		case GalaxyObject.GAL_FRAME:
		    receivedFrame((GFrame) object);
		    break;
		case GalaxyObject.GAL_LIST:
		    receivedList((GVector) object);
		    break;
		case GalaxyObject.GAL_BINARY:
		    receivedBinary((GBinary) object);
		    break;
		case GalaxyObject.GAL_INT_16:
		    receivedInt16((Int16) object);
		    break;
		case GalaxyObject.GAL_INT_32:
		    receivedInt32((Int32) object);
		    break;
		case GalaxyObject.GAL_INT_64:
		    receivedInt64((Int64) object);
		    break;
		case GalaxyObject.GAL_FLOAT_32:
		    receivedFloat32((Float32) object);
		    break;
		case GalaxyObject.GAL_FLOAT_64:
		    receivedFloat64((Float64) object);
		    break;
		case GalaxyObject.GAL_PROXY:
		    receivedProxy((BrokerProxy) object);
		    break;
		}
	    } else if(object instanceof String) {
		receivedString((String) object);
	    } else if(object instanceof Integer) {
		receivedInteger((Integer) object);
	    } else if(object instanceof Float) {
		receivedFloat((Float) object);
	    } else if(object instanceof Symbol) {
		receivedSymbol((Symbol) object);
	    } else {
		server.logWarningMessage("DataInBroker can't encode object",
					 "DataInBroker.run()");
	    }   
	}

	prepareToStop();
	server.logMessage("Incoming broker thread stopped.",
			  MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
			  "DataInBroker.run()");
    }
    
    /**
     * Marks this broker's thread as being stopped. 
     */
    public void stop()
    {
	isRunning = false;

	// Close the socket. This is needed in order to get the thread to
	// shut down correctly on Windows NT.
	if (socket != null) {
	    try {
		in.close();
		out.close();
		socket.close();	
		socket = null;
	    } catch (IOException ioe) {
	    }
	}
    }

    /**
     * This cleanup method is called when this broker is preparing to stop.
     * This is called before the socket connection is closed.
     * Override this method to provide your own customized cleanup routine.
     * The generic <code>DataInBroker</code> cleanup will still take place.
     */
    protected void cleanup()
    {
    }

    /**
     * This method is called when this broker receives a disconnect message
     * from the outgoing broker (indicates all data has been received).
     * Override this method to provide your own customized finalization 
     * routine. This broker is stopped after this method is called.
     */
    protected void disconnectReceived()
    {
    }

    /**
     * This method is called when this broker catches an unexpected 
     * exception while receiving brokered data.
     * Override this method to provide your own customized error handling 
     * routine. This broker is stopped after this method is called.
     */
    protected void abortReceived()
    {
    }

    /**
     * Performs cleanup and closes this broker's socket connection.
     */
    private void prepareToStop()
    {
	isRunning = false;
	cleanup();

	// Mark the connection as being closed.
	brokerConnectionEstablished = false;
	if (socket != null) {
	    try {
		in.close();
		out.close();
		socket.close();	
		socket = null;
	    } catch (IOException ioex) {
	    }
	}

	if(registeredWithServer)
	    server.unregisterInBroker(inBroker);

	stopped = true;
    }

    /**
     * Registers this broker with the <code>Server</code> that started it.
     * At the moment, this will cause this broker to be stopped if the server
     * loses its connection to the Hub.
     */
    public void registerWithServer()
    {
	server.registerInBroker(inBroker);
	registeredWithServer = true;
    }

    /**
     * Unregisters this broker from the <code>Server</code> that started it.
     */
    public void unregisterFromServer()
    {
	server.unregisterInBroker(inBroker);
	registeredWithServer = false;
    }
    
    //=========================================================================
    // Data management methods
    //=========================================================================

    /** 
     * Executed when a <tt>String</tt> is read by this incoming broker.
     *
     * @param str the string received
     */
    public void receivedString(String str) {}
    
    /** 
     * Executed when a <tt>GBinary</tt> is read by this incoming broker.
     *
     * @param bin the binary object received
     */
    public void receivedBinary(GBinary bin) {}

    /** 
     * Executed when a <tt>GFrame</tt> is read by this incoming broker. 
     *
     * @param frame the frame received
     */
    public void receivedFrame(GFrame frame) {}

    /** 
     * Executed when a <tt>GVector</tt> is read by this incoming broker. 
     *
     * @param list the list received
     */
    public void receivedList(GVector list) {}
    
    /** 
     * Executed when an <tt>Int16</tt> is read by this incoming broker. 
     *
     * @param int16 the 16-bit integer array received
     */
    public void receivedInt16(Int16 int16) {}
    
    /** 
     * Executed when an <tt>Int32</tt> is read by this incoming broker. 
     *
     * @param int32 the 32-bit integer array received
     */
    public void receivedInt32(Int32 int32) {}

    /** 
     * Executed when an <tt>Int64</tt> is read by this incoming broker. 
     *
     * @param int64 the 64-bit integer array received
     */
    public void receivedInt64(Int64 int64) {}

    /** 
     * Executed when a <tt>Float32</tt> is read by this incoming broker.
     *
     * @param float32 the 32-bit floating point number array received
     */
    public void receivedFloat32(Float32 float32) {}

    /** 
     * Executed when a <tt>Float64</tt> is read by this incoming broker.
     *
     * @param float64 the 64-bit floating point number array received
     */
    public void receivedFloat64(Float64 float64) {}

    /** 
     * Executed when an <tt>Integer</tt> is read by this incoming broker.
     *
     * @param integerObject the integer received
     */
    public void receivedInteger(Integer integerObject) {}

    /** 
     * Executed when a <tt>Float</tt> is read by this incoming broker.
     *
     * @param floatObject the float received
     */
    public void receivedFloat(Float floatObject) {}

    /** 
     * Executed when a <tt>Symbol</tt> is read by this incoming broker.
     *
     * @param symbol the symbol received
     */
    public void receivedSymbol(Symbol symbol) {}

    /** 
     * Executed when a <tt>BrokerProxy</tt> is read by this incoming broker.
     *
     * @param proxy the proxy received
     */
    public void receivedProxy(BrokerProxy proxy) {}
}
