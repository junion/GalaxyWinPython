/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: Server.java,v 1.47 2002/06/03 22:00:18 wohlever Exp $
 */

/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */


package galaxy.server;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.io.IOException;
import java.io.EOFException;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Collection;
import java.util.Iterator;
import java.util.Collections;

import java.net.Socket;
import java.net.InetAddress;
import java.net.SocketException;

import galaxy.io.GalaxyInputStream;
import galaxy.io.GalaxyOutputStream;

import galaxy.lang.GVector;
import galaxy.lang.GFrame;
import galaxy.lang.Clause;
import galaxy.lang.Symbol;
import galaxy.lang.GalaxyMessage;
import galaxy.lang.ReplyMessage;
import galaxy.lang.ErrorMessage;
import galaxy.lang.DestroyMessage;

import galaxy.util.FifoMutex;
import galaxy.util.Normalize;
import galaxy.util.DiagnosticLogger;
import galaxy.util.Logger;

/** 
 * This is the base class for all Galaxy servers implemented in Java.  It 
 * implements the basic message loop, logging, and the symbolic invocation of 
 * the dispatch function defined in the received frame.
 * <P>
 * You can add a <code>ServerListener</code> to receive notification of Server
 * events.
 * <P>
 *
 * @see MainServer
 * @see ServerListener
 */
public class Server implements Runnable, DiagnosticLogger
{
    /** Reference to the <code>MainServer</code> that started this server. */
    protected MainServer mainServer = null; 

    /** Session id. */
    private String sessionId = null;

    /** This server's socket connection. */
    private Socket socket = null;

    /** This server's input stream. */
    private GalaxyInputStream in = null;

    /** This server's output stream. */
    private GalaxyOutputStream out = null;

    /** This server's thread. */
    private Thread thread = null;

    /** 
     * Collection of <code>ServerListener</code>s listening for events from 
     * this server.
     */
    private List listeners;
    
    /** Collection of this server's method signatures. */
    private Map signatures = null;

    /** Galaxy-encoded signatures. */
    private GVector encSignatures = null;
    
    /** 
     * Flag that indicates if this server should do method signature 
     * validation when processing frames. 
     */
    private volatile boolean validate = false;

    /** 
     * Flag that indicates if the thread associated with this server is
     * running.
     */
    private volatile boolean isRunning = false;

    /** Flag that indicates if this server contacts Hubs on startup. */
    private boolean actsAsHubClient = false;

    /** 
     * Flag that indicates if this server's <code>reinitialize</code> method 
     * has been invoked.
     */
    private boolean initialized = false;

    /** 
     * The environment of the current dispatch function being executed by this
     * server.
     */
    private Environment currentEnv = null;

    /** Lock used to control access to this server's current envrionment. */
    private int currentEnvLock[] = new int[1];

    /** This server's frame dispatcher. */
    private FrameDispatcher frameDispatcher;  

    /** Collection of continuations. */
    private Map continuations;

    /** 
     * List of <code>DataInBroker</code>s to stop if this server loses its
     * connection to the Hub.
     */
    private List inBrokers;

    /** This server's properties. */
    private GFrame serverProperties;

    /** This server's extra service types. */
    private GVector extraServiceTypes;

    /** 
     * Bit flag that defines server's policy for reconnecting to Hub.
     * When setting up initial connection to Hub, default is to keep polling
     * Hub until connection is established (alternative is to give up after
     * first unsuccessful attempt).
     * In case the connection is lost, default is for <code>MainServer</code>
     * to poll Hub until connection is reestablished. Other alternatives are 
     * to a) shutdown server or b) do nothing (i.e., continue to run).
     */
    private int hubContactPolicy = -1;

    /** 
     * Flag that indicates if this server's Hub contact policy has been
     * modified.
     */
    private boolean hubContactPolicyModified = false;

    /**
     * The type of this server. Either MainServer.GAL_CONNECTION_LISTENER or
     * MainServer.GAL_HUB_CLIENT.
     */
    private int serverConnectionType;

    /**
     * If this is a MainServer.GAL_HUB_CLIENT, this is the name of the machine
     * hosting the Hub to which this server is connected.
     */ 
    private String hubHost;

    /**
     * If this is a MainServer.GAL_HUB_CLIENT, this is the listener port of 
     * the Hub to which this server is connected.
     */ 
    private int hubPort;

    /** Input stream mutex. */
    private FifoMutex mutex;

    private Logger logger = null;

    //=========================================================================
    // Constructors and other initialization methods.
    //=========================================================================
    
    /**
     * Creates a new server. Note that the server is not started in the 
     * constructor.
     *
     * @param mainServer the <code>MainServer</code> that created this server
     * @param socket the socket connection to the Hub
     */
    public Server(MainServer mainServer, 
		  Socket socket) throws IOException
    {
        this.socket = socket;
        this.mainServer = mainServer;
	logger = Logger.getLogger();

        inBrokers = new ArrayList();
	listeners = new ArrayList();

        out = new GalaxyOutputStream(socket.getOutputStream());
        in = new GalaxyInputStream(socket.getInputStream());
	mutex = in.getMutex();

	signatures = new HashMap();

	initializeSignatures();	

	frameDispatcher = new FrameDispatcher(in);

	continuations = Collections.synchronizedMap(new HashMap());
	
	serverProperties = new Clause("server_properties");
	extraServiceTypes = new GVector();
    }

    /*
     * @deprecated As of Galaxy Communicator 3.0. Use 
     * Server(MainServer, Socket) constructor.
    public Server(MainServer mainServer, Socket socket, 
		  String sessionId, String name) throws IOException
    {
	this(mainServer, socket);
    }
    */

    /**
     * Called before server starts. Subclasses of this class can override
     * this method to provide custom initialization.
     */
    public void init() throws Exception 
    {
    }

    //=========================================================================
    // Settor/gettor methods.
    //=========================================================================

    /**
     * Sets the method signature validation flag.
     *
     * @param validate true if validation should be done, false otherwise
     */
    public void setValidate(boolean validate)
    { this.validate = validate;}

    /**
     * Gets the value of the method signature validation flag.
     *
     * @return true if validation should be done, false otherwise
     */
    public boolean getValidate()
    { return validate; }  


    /**
     * Gets the internet address information for this server's local host.
     *
     * @return the internet address information or null if there was an error
     */
    public InetAddress getIPAddress() 
    {
	try{ 
	    return InetAddress.getLocalHost();
	} catch(Exception ex) {
	    logErrorMessage("Server caught exception while getting IP address: " + ex.toString(), ex, "Server.getIPAddress()");
	}
       return null;
    }

    /**
     * Gets the host address for this server's local host. 
     *
     * @return the host address or an empty string if there was an error
     */
    public String getHostAddress() 
    {
	InetAddress ipAddress = getIPAddress();
	if(ipAddress == null)
	    return "";

	return ipAddress.getHostAddress();
    } 

    /**
     * Returns this server current execution environment.
     *
     * @return the environment
     */
    public Environment getCurrentEnvironment()
    {
	Environment env;
	synchronized(currentEnvLock) {
	    env = currentEnv;
	}
	return env;
    }

    /**
     * Returns a copy of this server's current execution environment.
     *
     * @return the environment
     */
    public Environment getCopyOfCurrentEnvironment()
    {
	Environment env;
	synchronized(currentEnvLock) {
	    env = currentEnv;
	}
	return env.copyEnvironment();
    }
   
    /**
     * Returns the reference to the <code>MainServer</code> that started this
     * server.
     *
     * @return reference to the <code>MainServer</code>
     */
    public MainServer getMainServer()
    {
	return mainServer;
    }

    /**
     * Gets the input stream used by this server.
     *
     * @return this server's input stream
     */
    GalaxyInputStream getInputStream()
    {
	return in;
    }

    /**
     * Gets the output stream used by this server.
     *
     * @return this server's output stream
     */
    GalaxyOutputStream getOutputStream()
    {
	return out;
    }

    /**
     * Gets the frame dispatch used by this server.
     *
     * @return this server's frame dispatcher
     */
    FrameDispatcher getFrameDispatcher()
    {
	return frameDispatcher;
    }

    /**
     * Gets this server's properties.
     *
     * @return the server's properties
     */
    public GFrame getServerProperties()
    {
	return serverProperties;
    }

    /**
     * Sets the specified server properties.
     *
     * @param props the properties to set
     */
    protected void setServerProperties(GFrame props)
    {
	Enumeration keys = props.propertyKeys();
	Symbol key;
	while(keys.hasMoreElements()) {
	    key = (Symbol) keys.nextElement();
	    serverProperties.setProperty(key, props.getProperty(key));
        }
    }

    /**
     * Deletes the specified server properties.
     *
     * @param keys the keys of the properties to delete
     */
    protected void deleteServerProperties(String keys[])
    {
	for(int idx=0; idx<keys.length; ++idx) {
	    serverProperties.removeProperty(keys[idx]);
	}
    } 

    /**
     * Adds a service type to this server's lists of supported service types
     * if it is not already in the list.
     *
     * @param serviceType the service type to add
     */
    public void addServiceType(String serviceType)
    {
	if(serviceType != null && !extraServiceTypes.contains(serviceType))
	    extraServiceTypes.addElement(serviceType);
    }

    /**
     * Gets the service types supported by this server.
     *
     * @return the service types
     */
    public GVector getServiceTypes()
    {
	return extraServiceTypes;
    }

    /**
     * Sets the Hub contact policy (used if this server is a Hub client).
     * If an invalid policy is specified, no change is made. Valid values
     * include -1 (use the policy of the parent MainServer object), 
     * MainServer.DEFAULT_HUB_CONTACT_POLICY, and any or'ed
     * combination of one of a)
     * MainServer.GAL_HUB_CLIENT_CONNECT_FAILURE_RETRY,
     * MainServer.GAL_HUB_CLIENT_CONNECT_FAILURE_SHUTDOWN, or
     * MainServer.GAL_HUB_CLIENT_CONNECT_FAILURE_NOOP, and one of b)
     * MainServer.GAL_HUB_CLIENT_DISCONNECT_RETRY,
     * MainServer.GAL_HUB_CLIENT_DISCONNECT_SHUTDOWN, and
     * MainServer.GAL_HUB_CLIENT_DISCONNECT_NOOP.
     *
     * @param hubContactPolicy the policy
     */
    public void setHubContactPolicy(int hubContactPolicy)
    {
	if(hubContactPolicy == -1 ||
	   hubContactPolicy == MainServer.DEFAULT_HUB_CONTACT_POLICY ||
	   (hubContactPolicy & (MainServer.GAL_HUB_CLIENT_CONNECT_FAILURE_RETRY | 
				MainServer.GAL_HUB_CLIENT_CONNECT_FAILURE_SHUTDOWN |
				MainServer.GAL_HUB_CLIENT_CONNECT_FAILURE_NOOP |
				MainServer.GAL_HUB_CLIENT_DISCONNECT_RETRY |
				MainServer.GAL_HUB_CLIENT_DISCONNECT_SHUTDOWN | 
				MainServer.GAL_HUB_CLIENT_DISCONNECT_NOOP)) != 0) {
	    this.hubContactPolicy = hubContactPolicy;
	    hubContactPolicyModified = true;
	}
    } 

    /** 
     * Tests if this server's Hub contact policy has been modified since the
     * the server was created.
     * 
     * @return true if the policy has been modified and false otherwise
     */
    protected boolean wasHubContactPolicyModified() 
    {
	return hubContactPolicyModified;
    }

    /**
     * Gets the Hub contact policy (used if this server is a Hub client).
     *
     * @return the policy
     */
    public int getHubContactPolicy()
    {
	if(hubContactPolicy == -1)
	    return mainServer.getHubContactPolicy();
	return hubContactPolicy;
    }

    /**
     * Sets this server's connection type.
     *
     * @param serverConnectionType either MainServer.GAL_CONNECTION_LISTENER
     *                             or MainServer.GAL_HUB_CLIENT
     */
    void setServerConnectionType(int serverConnectionType)
    {
	this.serverConnectionType = serverConnectionType;
    }

    /**
     * Sets the session id.
     *
     * @param sessionId the sessionId
     */
    void setSessionId(String sessionId) 
    { this.sessionId = sessionId; }

    /**
     * Gets the session id.
     *
     * @return the sessionId
     */
    public String getSessionId() 
    { return sessionId; }

    /**
     * Records the name of the machine hosting the Hub to which this server is
     * connected. This is only relevant if this server is of type
     * MainServer.GAL_HUB_CLIENT.
     *
     * @param hubHost the machine name of the Hub's host
     */ 
    void setHubHost(String hubHost)
    { this.hubHost = hubHost; }

    /**
     * Records the listener port of the Hub to which this server is connected.
     * This is only relevant if this server is of type
     * MainServer.GAL_HUB_CLIENT.
     *
     * @param hubPort the Hub's listener port
     */ 
    void setHubPort(int hubPort)
    { this.hubPort = hubPort; }
    
    void setActsAsHubClient(boolean actsAsHubClient)
    {
	this.actsAsHubClient = actsAsHubClient;
    }

    //=========================================================================
    // Broker management methods.
    //=========================================================================

    /*
     * @deprecated As of Galaxy Communicator 3.0. Use registerOutBroker.
     *
    protected void register(DataOutBroker broker) 
    { registerOutBroker(broker); }
    */

    /** 
     * Registers an outgoing broker that was started by this server.
     *
     * @param broker the broker
     */
    protected void registerOutBroker(DataOutBroker broker) 
    { 
	logMessage("Server registering out broker (callId = " + broker.getCallId() + ").",
		   MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
		   "Server.registerOutBroker(DataOutBroker)");
	mainServer.registerOutBroker(broker);
    }

    /*
     * @deprecated As of Galaxy Communicator 3.0. Use unregisterOutBroker.
     *
    protected boolean unregister(DataOutBroker broker) 
    { return unregisterOutBroker(broker); }
    */

    /** 
     * Unregisters an outgoing broker that was started by this server.
     *
     * @param broker the broker
     * @return true if the broker was found and unregistered, false if the
     *         broker was not found
     */
    protected boolean unregisterOutBroker(DataOutBroker broker) 
    {
	logMessage("Server unregistering out broker (callId = " + broker.getCallId() + ").",
		   MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
		   "Server.unregisterOutBroker(DataOutBroker)");
	return mainServer.unregisterOutBroker(broker);
    }

    
    //=========================================================================
    // Server thread management methods.
    //=========================================================================

    /**
     * Sets the thread that runs this server.
     *
     * @param thread the thread
     */
    synchronized void setThread(Thread thread)
    { this.thread = thread; } 

    /**
     * Marks this server's thread as being stopped. The thread is not
     * actually stopped until this condition is tested in the thread's
     * main loop.
     */
    public void stop()
    {	
	isRunning = false;

	// Close the socket. This is needed in order to get the thread to
	// shut down correctly on Windows NT.
	if(socket != null) {
            try {
		in.close();
		out.close();
                socket.close();	
		socket = null;
            } catch(Exception ex) {
	    }
	}
    }

    /**
     * Indicates if this server's thread is marked as running or stopped.
     * Note that this does not return the actual status of the thread (which
     * is only affected when the associated flag is tested in the thread's
     * main loop).
     *
     * @return true if the thread is marked as running, false otherwise
     */
    public boolean isRunning()
    {
	return isRunning;
    } 

    /**
     * This cleanup method is called when this server is preparing to stop.
     * This is called before the socket connection is closed and this 
     * server is unregisterd with the main server.
     * Override this method to provide your own customized cleanup routine.
     * The generic <code>Server</code> cleanup will still take place.
     */
    protected void cleanup()
    {
    }

    /**
     * Called when this server's thread is about to stop. This performs
     * custom cleanup, closes the server's socket connection, and unregisters
     * the server from the main server.
     */
    private void prepareToStop()
    {
	cleanup();
        if(socket != null) {
            try {
		in.close();
		out.close();
                socket.close();	
		socket = null;
            } catch(IOException ioe) {
	    }
	}
	
	fireStopped();
	mainServer.unregisterServer(thread);

	// Stop in brokers that want to be stopped.
	stopInBrokers();
	isRunning = false;
    }   

    /**
     * This implements the basic message loop.  It receives a frame, then calls
     * evaluateHubFrame to process the frame.  The result is sent back to the 
     * hub.
     *
     * @see #evaluateHubFrame
     */
    public void run()
    {
	boolean clientHandshakeSucceeded = true;
	boolean handshakeSucceeded = true;

	isRunning = true;
        fireStarted();

	if(actsAsHubClient) {
	    if(!doClientHandshake()) {
		logErrorMessage("Server (as Hub client) encountered error in handshake.", "Server.run()");
		clientHandshakeSucceeded = false;
	    }
	} else {
	    if(!sendListenerHandshakeReply()) {
		logErrorMessage("Server (as Hub listener) encountered error while sending reply to handshake.", "Server.run()");
		handshakeSucceeded = false;
	    }
        }

	if(clientHandshakeSucceeded || handshakeSucceeded) {
	    try {
		GalaxyMessage msg = null;
		int replyRequested = 0;
		int serverTidx = -1;
		GFrame frame = null;
		GFrame replyFrame;
		int msgType = 0;
		ContinuationData data;
		Continuation continuation;
		Object continuationState;

		boolean foundIndex = false;
		boolean foundMessageForThisServer = false;
		GFrame hubOpaqueData;

		while(isRunning) {

		    foundMessageForThisServer = false;

		    // This block is synchronized on the input stream in order
		    // to synchronize with FrameDispatcher.dispatchFrame.
		    // Both these blocks of code read messages off the input
		    // stream and put messages of no interest to them on a
		    // shared queue. So, we need to make sure that either
		    // block of code does not get stuck trying to read a
		    // message for itself off the input stream when the
		    // the message they are waiting for has in fact already
		    // been read and stored in the queue. The input stream
		    // is used as the synchronizing object since it is shared
		    // by both blocks of code.
		    
		    try { 
			mutex.lock();

			// Check for any unprocessed messages
			msg = frameDispatcher.getNextNonReplyMessage();
			
			if(msg == null) {
			    
			    // Read in a new message from the input 
			    // stream.
			    msg = in.readMessage();
			}
			
			frame = (GFrame)msg.getData();
			logger.logMessage("\nReceived frame:\n" +
					  frame.toFormattedString(),
					  MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL, 
					  "Server.run()");

			// Create a new Environment based on the incoming frame
			synchronized(currentEnvLock) {
			    currentEnv = new Environment(frame, this);
			}
			
			// Initialize the server token index flag to false.
			foundIndex = false;

			// Check if there is administrative info.
			hubOpaqueData = (GFrame) frame.getProperty(GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY);

			replyRequested = 0;
			if(hubOpaqueData != null) {

			    // Check if a reply is requested
			    if(hubOpaqueData.containsProperty(GFrame.GAL_ROUND_TRIP_FRAME_KEY))
				replyRequested = hubOpaqueData.getInt(GFrame.GAL_ROUND_TRIP_FRAME_KEY).intValue();
			    
			    // Check if there is a server token index (i.e., is 
			    // this a continuation or a reply from a 
			    // dispatch function?).
			    foundIndex = false;
			    if(hubOpaqueData.containsProperty(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY)) {
				foundIndex = true;
				serverTidx = hubOpaqueData.getInt(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY).intValue();
			    }
			}
			
			replyFrame = null;
			if(!foundIndex) {
			    
			    // No server token index. Process normally.
			    foundMessageForThisServer = true;
			    replyFrame = evaluateHubFrame(frame);
			} else {
			    
			    // The input frame is a continuation or a reply to
			    // a dispatch function.
			    msgType = msg.getType();
			    
			    switch(msgType) {
			    case GalaxyMessage.GAL_MESSAGE_MSG_TYPE:
				
				// Server token index belongs to another 
				// server, so ignore it and process frame
				// as usual.
				foundMessageForThisServer = true;
				replyFrame = evaluateHubFrame(frame);
				break;
			    case GalaxyMessage.GAL_REPLY_MSG_TYPE:
			    case GalaxyMessage.GAL_ERROR_MSG_TYPE:
				data = getContinuationData(serverTidx);
				if(data == null) {
				    logWarningMessage("Server could not find continuation data for server token index " + serverTidx + ". Assume it is a reply from a dispatch function and put on the message queue.", "Server.run()");
				    frameDispatcher.queueMessage(msg);
				    break;
				} else {
				    foundMessageForThisServer = true;
				    synchronized(currentEnvLock) {
					currentEnv = data.getEnvironment();
				    }
				    
				    replyRequested = 0;
				    if(currentEnv.getHubOpaqueData().containsProperty(GFrame.GAL_ROUND_TRIP_FRAME_KEY))
					replyRequested = currentEnv.getHubOpaqueData().getInt(GFrame.GAL_ROUND_TRIP_FRAME_KEY).intValue();

				    continuation = data.getContinuation();
				    continuationState = 
					data.getContinuationState();
				    replyFrame = 
					continuation.run(frame, msgType, 
							 continuationState,
							 currentEnv);
				}
				break;
			    case GalaxyMessage.GAL_DESTROY_MSG_TYPE:
				logWarningMessage("Server got incomprehensible destroy request as message reply; ignoring.", "Server.run()");
				break;
			    case GalaxyMessage.GAL_POSTPONE_MSG_TYPE:
				logWarningMessage("Server got incomprehensible postpone request as message reply; ignoring.", "Server.run()");
				break;
			    default:
				logWarningMessage("Server got incomprehensible message of type " + msgType + " as message reply; ignoring.", "Server.run()");
				break;
			    }
			}
			
			if(foundMessageForThisServer) {
			    // Send a reply if one is needed but has not been 
			    // sent.
			    if(replyRequested == 1) {
				if(!currentEnv.isReturnSatisfied()) {
				    if (replyFrame != null) {
					currentEnv.reply(replyFrame);
				    } else {
					replyFrame = frame;
					logMessage("\nServer sending pacifier frame.", MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL, "Server.run()");
					currentEnv.reply(replyFrame);
				    } 
				}
			    } else {
				logMessage("\nNo return necessary.",
					   MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
					   "Server.run()");
			    } 
			    
			    // If the return was postponed, we can now unset 
			    // the "return" flags, allowing the real reply to
			    // be sent eventually.
			    if(currentEnv.isReturnPostponed()) {
				currentEnv.setReturnPostponed(false);
				currentEnv.setReturnSatisfied(false);
			    }
			}
		    } finally {
			mutex.unlock();
		    }		    
		}
	    } catch(EOFException eofex) {
		logWarningMessage("Server caught exception, possibly due to a Hub shutdown: " + eofex.toString(), "Server.run()");
		if(serverConnectionType == MainServer.GAL_HUB_CLIENT) {
		    prepareToStop();
		    int policy = getHubContactPolicy();
		    if((policy & MainServer.GAL_HUB_CLIENT_DISCONNECT_SHUTDOWN) != 0) {
			logMessage("Server telling MainServer to stop.",
				   MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
				   "Server.run()");
			mainServer.stop();
		    } else if((policy & MainServer.GAL_HUB_CLIENT_DISCONNECT_MASK) == 0) {
			logMessage("Server telling MainServer to reestablish connection.",
				   MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
				   "Server.run()");
			mainServer.contactHub(hubHost, hubPort, sessionId, policy);
		    }
		    
		    logMessage("Server thread stopped.",
			       MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
			       "Server.run()");
		    return;
		}
	    } catch (SocketException sex) {
		logWarningMessage("Server caught exception, possibly due to a Hub shutdown or an explicit closing of the server's socket connection: " + sex.toString(), "Server.run()");
		if(serverConnectionType == MainServer.GAL_HUB_CLIENT) {
		    prepareToStop();
		    int policy = getHubContactPolicy();
		    if((policy & MainServer.GAL_HUB_CLIENT_DISCONNECT_SHUTDOWN) != 0) {
			logMessage("Server telling MainServer to stop.",
				   MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
				   "Server.run()");
			mainServer.stop();
		    } else if((policy & MainServer.GAL_HUB_CLIENT_DISCONNECT_MASK) == 0) {
			logMessage("Server telling MainServer to reestablish connection.",
				   MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
				   "Server.run()");
			mainServer.contactHub(hubHost, hubPort, sessionId, policy);
		    }
		    
		    logMessage("Server thread stopped.",
			       MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
			       "Server.run()");
		    return;
		}
	    } catch(Exception ex) {
		logErrorMessage("Server caught exception in main loop: " + ex.toString(), ex, "Server.run()");
	    }
	}
        
        // Clean up and notify listeners that we have died (or will shortly).
        prepareToStop();
	logMessage("Server thread stopped.",
		   MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL,
		   "Server.run()");
    }
    
    /**
     * Gets the continuation data associated with a server token index.
     *
     * @param serverTidx the server token index associated with the 
     *                   continuation
     * @return the continuation data or null if no data was found for the
     *         specified index
     */
    protected ContinuationData getContinuationData(int serverTidx)
    {
	return (ContinuationData) continuations.get(new Integer(serverTidx));
    }  
    
    /**
     * Stores continuation data associated with a server token index.
     *
     * @param serverTidx the server token index associated with the 
     *                   continuation
     * @param data the continuation data
     */
    protected void addContinuationData(int serverTidx, ContinuationData data)
    {
	continuations.put(new Integer(serverTidx), data);
    }   

    //=========================================================================
    // Handshake management methods.
    //=========================================================================
    
    /**
     * Performs the server-side handshake when this server is contacting
     * Hubs on startup (i.e., acting as a Hub "client").
     *
     * @return true if the handshake succeeded, false otherwise
     */
    private boolean doClientHandshake()
    {
	//
	// Since this server is acting as a client initially, it needs to 
	// establish contact with the Hub.
	//
	try {
	    GFrame frame = new Clause(mainServer.getName());
	    if(sessionId != null)
		frame.setProperty(GFrame.GAL_SESSION_ID_FRAME_KEY, sessionId);
	    frame.setProperty(GFrame.GAL_SIGNATURES_FRAME_KEY, encodeSignatures());
	    frame.setProperty(":extra_service_types", extraServiceTypes);
	    frame.setProperty(":properties", serverProperties);
	    frame.setProperty(":protocol_version", 1);

	    logMessage("\nServer sending handshake frame:\n" +
		       frame.toFormattedString(),
		       MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
		       "Server.doClientHandshake()");

	    // Set up a dummy environment to get us through the handshake
	    currentEnv = new Environment(null, in, out);

	    GFrame replyFrame = currentEnv.dispatchFrame(frame);
	    logMessage("\nServer received handshake reply frame:\n" +
		       replyFrame.toFormattedString(),
		       MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
		       "Server.doClientHandshake()");
	    
	    if(replyFrame.getName().toString().equals("system_error")) {
		int errno = ((Integer)replyFrame.getProperty(GFrame.GAL_ERROR_NUMBER_FRAME_KEY)).intValue();
		String msg = (String)replyFrame.getProperty(GFrame.GAL_ERROR_DESCRIPTION_FRAME_KEY);
		logFatalMessage("System error during handshake\n" +
				"Error number " + errno + ": " + msg + "\n",
				"Server.doClientHandshake()");
		return false;
	    } else {
		return true;
	    }
	} catch(Exception ex) {
	    logFatalMessage("Caught exception during \"Hub client\" handshake.", ex, "Server.doClientHandshake()");
	    return false;
	}
    } 

    /**
     * Sends the server-side handshake reply when this server is acting as a
     * listener.
     */
    private boolean sendListenerHandshakeReply()
    {
	try {
	    GFrame replyFrame = null;
	    replyFrame = new Clause(mainServer.getName());
	    replyFrame.setProperty(GFrame.GAL_SIGNATURES_FRAME_KEY, 
				   encodeSignatures());
	    replyFrame.setProperty(":extra_service_types", extraServiceTypes);
	    replyFrame.setProperty(":properties", serverProperties);
	    replyFrame.setProperty(":protocol_version", 1);

	    logMessage("\nServer sending handshake reply frame:\n" +
		       replyFrame.toFormattedString(),
		       MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
		       "Server.sendListenerHandshakeReply()");
	    try {
		out.writeMessage(new ReplyMessage(replyFrame));
	    }  catch (IOException ioe) {
		throw ioe;
	    }
 
	} catch(Exception ex) {
	    logFatalMessage("Caught exception while sending reply to Hub-as-listener handshake", ex, "Server.sendListenerHandshakeReply()");
	    return false;
	} 
	return true;
    }

    //=========================================================================
    // Signature management methods.
    //=========================================================================

    /**
     * This method will add a default signature to all server operations.
     * It is up to the programmer to add correct singatures for their 
     * serverOp's.
     */
    private void initializeSignatures() 
    {
	Method[] methods = getClass().getMethods();
	String opName;
	for(int i=0; i < methods.length; i++) {
	    if (methods[i].getName().startsWith("serverOp")) {
		opName = methods[i].getName().substring(8).toLowerCase();
		logMessage("Adding signature for method \"" + opName + "\".",
			   MainServer.INITIALIZATION_DETAILS_VERBOSITY_LEVEL,
			   "Server.initializeSignatures()");
		addSignature(new Signature(opName));
	    }
	}
    }

    /**
     * Adds new signature information for this server.
     *
     * @param sig the signature information
     */
    protected void addSignature(Signature sig) 
    {
	signatures.put(sig.getName(), sig);
    }

    /**
     * Gets the signature information associated with an operation on this
     * server's interface
     *
     * @param op the name of the operation
     * @return the signature information associated with the specified
     *         operation or null if no match was found
     */
    protected Signature getSignature(String op) 
    {
	return (Signature) signatures.get(op);
    }

    /**
     * Encodes this server's signature information in a <code>GVector</code>.
     *
     * @return the encoded signature information
     */
    protected GVector encodeSignatures() 
    {
	if (encSignatures == null) {
	    encSignatures= new GVector(signatures.size());
	    
	    Collection signatureCollection = signatures.values();
	    synchronized(signatureCollection) {
		Iterator iterator = signatureCollection.iterator();
		Signature sig;
		while(iterator.hasNext()) {
		    sig = (Signature) iterator.next();
		    if (sig != null)
			encSignatures.addElement(sig.toGVector());
		}
	    }
	}
	return encSignatures;
    }


    //=========================================================================
    // Frame evaluation methods.
    //=========================================================================

    /**
     * Evaluates the Hub frame. Otherwise the <code>op_name</code> property 
     * is extracted from the frame and the method name formed by prepending 
     * <code>serverOp</code> when it is called.
     * For example, if <code>serverOp</code> is <code>explode</code>, then the
     * method <code>serverOpExplode</code> will be called.
     */
    protected GFrame evaluateHubFrame(GFrame frame)
    {
	// Remove the opaque data temporarily from the frame. The environment
	// object will take care of adding it back in when needed.
	frame.removeProperty(GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY);

        Method method;
        GFrame resultFrame = null;
        Class methodSignature[] = {GFrame.class};
	String opName = currentEnv.getOpName();

	// Check for the name of a dispatch function
        if (opName == null) {
	    try {
		logAndSendErrorMessage("null operation name supplied", 
				       ErrorMessage.GAL_NO_OPNAME_ERROR,
				       "Server.evaluateHubFrame(GFrame)");
	    } catch (Exception ex) {
		logErrorMessage("Server caught exception while sending \"null operation name supplied\" error: " + ex.toString(), ex, "Server.evaluateHubFrame(GFrame)");
	    }
            return null;
        }

	// Validate dispatch function signature if configured to do so
	if (validate) {
	    getSignature(opName).validateInput(frame,this);
	}

	if(!initialized && !opName.equals("reinitialize")) {
	    try {
		logAndSendErrorMessage("first message must be reinitialize",
				       ErrorMessage.GAL_CONN_REJECTION_ERROR,
				       "Server.evaluateHubFrame(GFrame)"); 
	    } catch (Exception ex) {
		logErrorMessage("Server caught while sending \"first message must be reinitialize\" error: " + ex.toString(), ex, "Server.evaluateHubFrame(GFrame)");
	    }
	    return null;
	}

	
        String methodName = "serverOp" + Normalize.normalizeWithInitialCap(opName);
	
        try {
            method = getClass().getMethod(methodName, methodSignature);
        } catch(Exception ex) {
	    try {
		logAndSendErrorMessage("No operation named " + opName + 
				       ". Server method " + methodName + 
				       " is not defined.", 
				       ErrorMessage.GAL_NO_OPNAME_ERROR,
				       "Server.evaluateHubFrame(GFrame)");
	    } catch (Exception ex2) {
		logErrorMessage("Server caught exception while sending \"No operation named\"error: " + ex2.toString(), ex2, "Server.evaluateHubFrame(GFrame)");
	    }

            return null;
        }
    
        try {
            Object args[] = {frame};
            resultFrame = (GFrame)method.invoke(this, args);
	    if (opName.equals("reinitialize")) {
		initialized = true;
	    }
	} catch(InvocationTargetException ite) {
	    Throwable t = ite.getTargetException();
	    logErrorMessage("InvocationTargetException caught in Server.evaluateHubFrame: " + t.getMessage(), "Server.evaluateHubFrame(GFrame)");
	    t.printStackTrace();
        } catch(Exception ex) {
	    try {
		logAndSendErrorMessage(this.getClass().getName() + ": Error invoking " + methodName + " while processing the \"op_name\" " + opName + ". " + ex.toString(), ErrorMessage.GAL_NO_OPNAME_ERROR, ex, "Server.evaluateHubFrame(GFrame)");
	    } catch (Exception ex2) {
		logErrorMessage("Server caught exception while sending \"Error invoking method\" error: " + ex2.toString(), ex2, "Server.evaluateHubFrame(GFrame)");
	    }
	    return null;
        }
	
	// Validate if necessary
	if(validate)
	    getSignature(opName).validateOutput(frame, this);

	// Return the result frame
	return resultFrame;
    }

    /**
     * Subclasses of <code>Server</code> typically override this method in
     * order to provide custom reinitialization.
     */
    public void serverOpReinitialize(GFrame frame)
    {
    }

    /*
     * Dispatches a frame to the Hub. 
     *
     * @param frame the frame to dispatch
     * @return the reply frame sent from the Hub
     * @deprecated As of Galaxy Communicator 3.0. Use 
     * Environment.dispatchFrame.
     *
    protected GFrame dispatchFrame(GFrame frame) 
	throws DispatchError
    {
	return currentEnv.dispatchFrame(frame);
    }
    */

    /*
     * Writes a new frame to the Hub.
     *
     * @param frame the frame to send
     * @deprecated As of Galaxy Communicator 3.0. Use Environment.writeFrame.
     *
    public void writeFrame(GFrame frame)
    {
	try {
	    currentEnv.writeFrame(frame);
	}  catch (Exception ex) {
	    logErrorMessage("Server caught exception while writing frame: " + ex.toString(), ex, "Server.writeFrame(GFrame)");
	}
    }
    */

    /*
     * @deprecated As of Galaxy Communicator 3.0. Use Environment methods
     * (error, destroyToken, reply, writeFrame).
     *
    public void writeMessage(GalaxyMessage msg) 
    {
	GFrame frame = (GFrame) msg.getData();

	if (msg instanceof ErrorMessage) {
	    writeErrorMessage((ErrorMessage) msg);
	} else if (msg instanceof DestroyMessage) {
	    writeDestroyMessage();
	} else if (msg instanceof ReplyMessage) {
	    try {
		currentEnv.reply(frame);
	    } catch (Exception ex) {
		logErrorMessage("Server caught exception while writing reply message: " + ex.toString(), ex, "Server.writeMessage(GalaxyMessage)");
	    }
	} else {
	    try {
		currentEnv.writeFrame(frame);
		out.writeMessage(msg);
	    } catch (Exception ex) {
		logErrorMessage("Server caught exception while writing message: " + ex.toString(), ex, "Server.writeMessage(GalaxyMessage)");
	    }
	}
    }
    */

    /*
     * @deprecated As of Galaxy Communicator 3.0. Use Environment.error.
     *
    public void writeErrorMessage(ErrorMessage msg) 
    {
	if(currentEnv.isReturnSatisfied())
	    return;
	
	currentEnv.addAdministrativeInfo((GFrame)msg.getData(), false);
	try {
	    out.writeMessage(msg);
	    currentEnv.setReturnSatisfied(true);
	} catch (Exception ex) {
	    logErrorMessage("Server caught exception while writing error message: " + ex.toString(), ex, "Server.writeErrorMessage(ErrorMessage)");
	}
    }
    */

    /*
     * @deprecated As of Galaxy Communicator 3.0. Use Environment.destroyToken.
     *
    public void writeDestroyMessage() 
    {
	try {
	    currentEnv.destroyToken();
	} catch (Exception ex) {
	    logErrorMessage("Server caught exception while writing destroy message: " + ex.toString(), ex, "Server.writeDestroyMessage()");
	}
    }
    */

    //=========================================================================
    // In broker management methods.
    //=========================================================================

    /**
     * Adds a broker to this server's list of in brokers to stop if it
     * loses its connection to the Hub.
     *
     * @param broker the broker to add
     */
    void registerInBroker(DataInBroker broker)
    {
	synchronized(inBrokers) {
	    inBrokers.add(broker);
	}
    }

    /**
     * Removes a broker from this server's list of in brokers to stop if it
     * loses its connection to the Hub.
     *
     * @param broker the broker to remove
     */
    void unregisterInBroker(DataInBroker broker)
    {
	synchronized(inBrokers) {
	    inBrokers.remove(broker);
	}
    }

    /**
     * Stops the in brokers that want to be stopped if this server loses its 
     * connection to the Hub.
     */
    private void stopInBrokers()
    {
	synchronized(inBrokers) {
	    Iterator iterator = inBrokers.iterator();
	    DataInBroker broker;
	    while(iterator.hasNext()) {
		broker = (DataInBroker) iterator.next();
		broker.stop();
	    }
	}
    }

    //=========================================================================
    // ServerListener support methods.
    //=========================================================================
    
    public void addServerListener(ServerListener listener)
    {
	synchronized(listeners) {
	    listeners.add(listener);
	}
    }
    
    public void removeServerListener(ServerListener listener)
    {
	synchronized(listeners) {
	    listeners.remove(listener);
	}
    }

    private boolean fireStarted()
    {	
	List listenersClone;
	synchronized(listeners) {
	    listenersClone = (List) ((ArrayList)listeners).clone();
	}

	if(listenersClone.size() > 0) {
	    Iterator iterator = listenersClone.iterator();
	    ServerListener listener;
	    while(iterator.hasNext()) {
		listener = (ServerListener) iterator.next();
		listener.serverStarted(this);
	    }
	    return true;
	} else
	    return false;
    }
    
    private boolean fireStopped()
    {	
	List listenersClone;
	synchronized(listeners) {
	    listenersClone = (List) ((ArrayList)listeners).clone();
	}

	if(listenersClone.size() > 0) {
	    Iterator iterator = listenersClone.iterator();
	    ServerListener listener;
	    while(iterator.hasNext()) {
		listener = (ServerListener) iterator.next();
		listener.serverStopped(this);
	    }
	    return true;
	} else
	    return false;
    }
    
    private boolean fireMessage(String msg)
    {	
	List listenersClone;
	synchronized(listeners) {
	    listenersClone = (List) ((ArrayList)listeners).clone();
	}

	if(listenersClone.size() > 0) {
	    Iterator iterator = listenersClone.iterator();
	    ServerListener listener;
	    while(iterator.hasNext()) {
		listener = (ServerListener) iterator.next();
		listener.serverMessage(this, msg);
	    }
	    return true;
	} else
	    return false;
    }

    private boolean fireWarningMessage(String msg)
    {	
	List listenersClone;
	synchronized(listeners) {
	    listenersClone = (List) ((ArrayList)listeners).clone();
	}

	if(listenersClone.size() > 0) {
	    Iterator iterator = listenersClone.iterator();
	    ServerListener listener;
	    while(iterator.hasNext()) {
		listener = (ServerListener) iterator.next();
		listener.serverWarningMessage(this, msg);
	    }
	    return true;
	} else
	    return false;
    }

    private boolean fireErrorMessage(String msg)
    {	
	List listenersClone;
	synchronized(listeners) {
	    listenersClone = (List) ((ArrayList)listeners).clone();
	}

	if(listenersClone.size() > 0) {
	    Iterator iterator = listenersClone.iterator();
	    ServerListener listener;
	    while(iterator.hasNext()) {
		listener = (ServerListener) iterator.next();
		listener.serverErrorMessage(this, msg);
	    }
	    return true;
	} else
	    return false;
    }

    private boolean fireErrorMessage(String msg, Exception ex)
    {	
	List listenersClone;
	synchronized(listeners) {
	    listenersClone = (List) ((ArrayList)listeners).clone();
	}

	if(listenersClone.size() > 0) {
	    Iterator iterator = listenersClone.iterator();
	    ServerListener listener;
	    while(iterator.hasNext()) {
		listener = (ServerListener) iterator.next();
		listener.serverErrorMessage(this, msg, ex);
	    }
	    return true;
	} else
	    return false;
    }

    private boolean fireFatalErrorMessage(String msg)
    {	
	List listenersClone;
	synchronized(listeners) {
	    listenersClone = (List) ((ArrayList)listeners).clone();
	}

	if(listenersClone.size() > 0) {
	    Iterator iterator = listenersClone.iterator();
	    ServerListener listener;
	    while(iterator.hasNext()) {
		listener = (ServerListener) iterator.next();
		listener.serverFatalErrorMessage(this, msg);
	    }
	    return true;
	} else
	    return false;
    }
    
    private boolean fireFatalErrorMessage(String msg, Exception ex)
    {	
	List listenersClone;
	synchronized(listeners) {
	    listenersClone = (List) ((ArrayList)listeners).clone();
	}

	if(listenersClone.size() > 0) {
	    Iterator iterator = listenersClone.iterator();
	    ServerListener listener;
	    while(iterator.hasNext()) {
		listener = (ServerListener) iterator.next();
		listener.serverFatalErrorMessage(this, msg, ex);
	    }
	    return true;
	} else
	    return false;
    }

    //=========================================================================
    // Logging methods
    //=========================================================================

    /** @deprecated As of Galaxy Communicator 4.0. Use logMessage(String, int). */
    public void log(String msg)
    {
	if((MainServer.getVerbosityLevel() > MainServer.NIL_VERBOSITY_LEVEL)
	   && !fireMessage(msg))
	    System.out.println(msg);
    }

    /** @deprecated As of Galaxy Communicator 4.0. Use logWarningMessage(String). */
    public void logWarning(String msg)
    {
	if((MainServer.getVerbosityLevel() > MainServer.NIL_VERBOSITY_LEVEL)
	   && !fireWarningMessage(msg))
	    System.out.println("Warning: "+msg);
    }

    /** @deprecated As of Galaxy Communicator 4.0. Use logErrorMessage(String). */
    public void logError(String msg)
    {
      if((MainServer.getVerbosityLevel() > MainServer.NIL_VERBOSITY_LEVEL)
	 && !fireErrorMessage(msg))
	  System.err.println("Error: "+msg);
    }

    /** @deprecated As of Galaxy Communicator 4.0. Use logErrorMessage(String, Exception). */
    public void logError(String msg, Exception ex)
    {
	if((MainServer.getVerbosityLevel() > MainServer.NIL_VERBOSITY_LEVEL) 
	   && !fireErrorMessage(msg, ex)) {
	    System.err.println("Error: "+msg);
	    ex.printStackTrace();
	}
    }
    
    /** @deprecated As of Galaxy Communicator 4.0. Use logFatalMessage(String). */
    public void logFatalError(String msg)
    {
      if((MainServer.getVerbosityLevel() > MainServer.NIL_VERBOSITY_LEVEL) 
	 && !fireFatalErrorMessage(msg))
	System.err.println("Fatal error: "+msg);
    }  

    /** @deprecated As of Galaxy Communicator 4.0. Use logFatalMessage(String, Exception). */
    public void logFatalError(String msg, Exception ex)
    {
      if((MainServer.getVerbosityLevel() > MainServer.NIL_VERBOSITY_LEVEL) &&
	 !fireFatalErrorMessage(msg, ex)) {
	System.err.println("Fatal error: "+msg);
	ex.printStackTrace();
      }
    } 

    public void logMessage(String msg, int level)
    {
	logMessage(msg, level, null);
    } 

    public void logMessage(String msg, int level, String location)
    {
	if(MainServer.getVerbosityLevel() >= level) {
	    String newMsg = msg;
	    if(msg.startsWith("\n"))
		newMsg = new String("\n" + location + ": " + msg.substring(1));
	    else
		newMsg = new String("\n" + location + ": " + msg);
	    if(!fireMessage(newMsg))
		logger.logMessage(msg, level, location);
	}
    } 

    public void logMessage(String msg)
    {
	if(!fireMessage(msg))
	    logger.logMessage(msg);
    }
    
    public void logWarningMessage(String msg)
    {
	logWarningMessage(msg, null);
    }

    public void logWarningMessage(String msg, String location)
    {
	if(MainServer.getVerbosityLevel() >= MainServer.WARNING_VERBOSITY_LEVEL) {
	    String newMsg = msg;
	    if(location != null && MainServer.getVerbosityLevel() >= 4)
		newMsg = new String(location + ": " + msg);
	    if(!fireWarningMessage(newMsg))
		logger.logWarningMessage(msg, location);
	}
    } 
    
    public void logErrorMessage(String msg)
    {
	logErrorMessage(msg, null, null);
    }
    
    public void logErrorMessage(String msg, Exception ex)
    {
	logErrorMessage(msg, ex, null);
    }

    public void logErrorMessage(String msg, String location)
    {
	logErrorMessage(msg, null, location);
    }
    
    public void logErrorMessage(String msg, Exception ex, String location)
    {
	if(MainServer.getVerbosityLevel() >= MainServer.ERROR_VERBOSITY_LEVEL) {
	    String newMsg = msg;
	    if(location != null && MainServer.getVerbosityLevel() >= 4)
		if(msg.startsWith("\n"))
		    newMsg = new String("\n" + location + ": " + msg.substring(1));
		else
		    newMsg = new String("\n" + location + ": " + msg);	    
	    if(!fireErrorMessage(newMsg, ex))  
		logger.logErrorMessage(msg, ex, location);
	}
    }
    
    public void logFatalMessage(String msg)
    {
	logFatalMessage(msg, null, null);
    }
    
    public void logFatalMessage(String msg, Exception ex)
    {
	logFatalMessage(msg, ex, null);
    } 

    public void logFatalMessage(String msg, String location)
    {
	logFatalMessage(msg, null, location);
    }
    
    public void logFatalMessage(String msg, Exception ex, String location)
    {
	if(MainServer.getVerbosityLevel() >= MainServer.ERROR_VERBOSITY_LEVEL) {
	    String newMsg = msg;
	    if(location != null && MainServer.getVerbosityLevel() >= 4)
		if(msg.startsWith("\n"))
		    newMsg = new String("\n" + location + ": " + msg.substring(1));
		else
		    newMsg = new String("\n" + location + ": " + msg);
	    if(!fireFatalErrorMessage(newMsg, ex))
		logger.logFatalMessage(msg, ex, location);
	}
    }
    
    /**
     * Logs an error message locally (if enabled) and sends it to the Hub.
     *
     * @param msg the description of the error 
     * @deprecated As of Galaxy Communicator 4.0. Use logAndSendErrorMessage(String).
     */
    protected void logAndSendError(String msg) 
    {
	try {
	    logAndSendError(msg, ErrorMessage.GAL_APPLICATION_ERROR);
	} catch (IOException ioe) {
	    // should close connection
	}
    }

    /** @deprecated As of Galaxy Communicator 4.0. Use logAndSendErrorMessage(String, int). */
    protected void logAndSendError(String msg, int errno) 
	throws IOException 
    {
      if(currentEnv != null) {
	logError(msg);
	currentEnv.error(msg, errno);
      } else {
	logError("Server.logAndSendError has null current environment");
      }
    } 
    
    protected void logAndSendErrorMessage(String msg) 
	throws IOException
    {
	logAndSendErrorMessage(msg, ErrorMessage.GAL_APPLICATION_ERROR,
			       null, null);
    }

    protected void logAndSendErrorMessage(String msg, Exception ex) 
	throws IOException
    {
	logAndSendErrorMessage(msg, ErrorMessage.GAL_APPLICATION_ERROR, ex,
			       null);
    }

    protected void logAndSendErrorMessage(String msg, int errno) 
	throws IOException 
    {
	logAndSendErrorMessage(msg, errno, null, null);
    }
    
    protected void logAndSendErrorMessage(String msg, int errno, Exception ex) 
	throws IOException 
    {
	logAndSendErrorMessage(msg, errno, ex, null);
    }

    protected void logAndSendErrorMessage(String msg, String location) 
	throws IOException
    {
	logAndSendErrorMessage(msg, ErrorMessage.GAL_APPLICATION_ERROR,
			       null, location);
    }

    protected void logAndSendErrorMessage(String msg, Exception ex,
					  String location) 
	throws IOException
    {
	logAndSendErrorMessage(msg, ErrorMessage.GAL_APPLICATION_ERROR, 
			       ex, location);
    }

    protected void logAndSendErrorMessage(String msg, int errno,
					  String location) 
	throws IOException 
    {
	logAndSendErrorMessage(msg, errno, null, location);
    }
    
    protected void logAndSendErrorMessage(String msg, int errno, Exception ex,
					  String location) 
	throws IOException 
    {
	logErrorMessage(msg, ex, location);
	if(currentEnv != null) {
	    currentEnv.error(msg, errno);
	} else {
	    logErrorMessage("The current environment is null.", "Server.logAndSendErrorMessage(String, int, Exception, String)");
	}
    }
}
