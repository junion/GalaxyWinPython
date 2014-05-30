/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

import java.io.IOException;

import galaxy.lang.GFrame;
import galaxy.lang.Symbol;
import galaxy.lang.Clause;
import galaxy.lang.GalaxyMessage;
import galaxy.lang.NewMessage;
import galaxy.lang.PostponeMessage;
import galaxy.lang.DestroyMessage;
import galaxy.lang.ErrorMessage;
import galaxy.lang.ReplyMessage;
import galaxy.lang.GVector;
import galaxy.lang.Float64;

import galaxy.io.GalaxyInputStream;
import galaxy.io.GalaxyOutputStream;

import galaxy.util.Logger;

/**
 * The class encapsulates the environment of the current execution context.
 * This includes methods for dispatching frames to a Hub (with and without
 * support for continuation) and methods for sending replies to a Hub.
 */
public class Environment
{
    /** Token index (from a <code>GFrame</code>) */
    private int tidx = -1;

    /** Hub opaque data (from a <code>GFrame</code>) */
    private GFrame hubOpaqueData = null;

    /** Session ID (from a <code>GFrame</code>) */
    private String sessionId = null;

    /** Operation name (from a <code>GFrame</code>) */
    private String opName = null;

    /** Message name (from a <code>GFrame</code>) */
    private String msgName = null;

    /** <code>GFrame</code> of the current execution context. */
    private GFrame frame = null;

    /** <code>Server</code> that created this environment */
    private Server server = null;

    /** Input stream associated with the current execution context */
    private GalaxyInputStream in = null;

    /** Output stream associated with the current execution context */
    private GalaxyOutputStream out = null;

    /** 
     * Flag that indicates if the return condition associated with this 
     * environment has been satisfied.
     */
    private volatile boolean returnSatisfied = false;

    /**
     * Flag that indicates if the return associated with this environment
     * has been postponed. 
     */
    private volatile boolean returnPostponed = false;

    /**
     * <code>FrameDispatcher</code> used to dispatch/write frames to
     * to the Hub from this environment.
     */
    private FrameDispatcher frameDispatcher;

    private boolean inputHadTopLevelSessionId = false;

    private Logger logger = null;

    private double inheritedTokenTimestamp = -1;

    //=========================================================================
    // Constructors
    //=========================================================================
    	
    /**
     * Creates an environmens for use by handshake code (i.e., not associated
     * with a server or dispatch function).
     *
     * @param frame <code>GFrame</code> of the current execution context
     * @param in input stream associated with the current execution context
     * @param out output stream associated with the current execution context
     */
    public Environment(GFrame frame, GalaxyInputStream in,
		       GalaxyOutputStream out) 
    {
	this();
	this.frame = frame;
	this.in = in;
	this.out = out;	
	frameDispatcher = new FrameDispatcher(in);

	commonInit(frame);
    }

    /**
     * Creates an environment for use by a server when processing calls to its
     * dispatch functions.
     *
     * @param frame <code>GFrame</code> of the current execution context
     * @param server <code>Server</code> that created this environment
     */
    public Environment(GFrame frame, Server server) 
    {
	this();
	this.frame = frame;
	this.server = server;
	if(server != null) {
	    frameDispatcher = server.getFrameDispatcher();
	    out = server.getOutputStream();	
	}
	commonInit(frame);
    }

    /**
     * Creates an empty environment.
     */ 
    public Environment()
    {
	logger = Logger.getLogger();
    }

    /**
     * Returns a copy of this Environment.
     *
     * @return a new environment with the same contents as the original
     */
    synchronized Environment copyEnvironment()
    {	
	Environment copy = new Environment();
	copy.tidx = tidx;

	if(hubOpaqueData != null)
	    copy.hubOpaqueData = hubOpaqueData.copy();

	if(sessionId != null)
	    copy.sessionId = new String(sessionId);

	if(opName != null)
	    copy.opName = new String(opName);

	if(msgName != null)
	    copy.msgName = new String(msgName);

	if(frame != null)
	    copy.frame = frame.copy();
	
	// These four references are not copies.
	copy.server = server;
	copy.in = in;
	copy.out = out;
	copy.frameDispatcher = frameDispatcher;
	
	copy.returnSatisfied = returnSatisfied;
	copy.returnPostponed = returnPostponed;
	copy.inheritedTokenTimestamp = inheritedTokenTimestamp;

	return copy;
    }

    /**
     * Performs initialization common to all constructors.
     *
     * @param frame <code>GFrame</code> of the current execution context
     */
    private void commonInit(GFrame frame)
    {
	int idx;
	
	if(frame != null) {

	    msgName = frame.getName().toString();
	    idx = msgName.indexOf('.');
	    opName = msgName;
	    
	    // If the message name is of the form <server name>.<op name>,
	    // save the op name only.
	    if(idx != -1)
		opName = opName.substring(idx+1);
	    
	    hubOpaqueData = (GFrame) frame.getProperty(GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY);

	    // Check if the input frame has a session id in the top level.
	    if(frame.containsProperty(GFrame.GAL_SESSION_ID_FRAME_KEY))
		inputHadTopLevelSessionId = true;

	    // The handshake frames do not contain opaque data.
	    if(hubOpaqueData != null) {
		
		if(hubOpaqueData.containsProperty(GFrame.GAL_TOKEN_INDEX_FRAME_KEY))
		    tidx = hubOpaqueData.getInt(GFrame.GAL_TOKEN_INDEX_FRAME_KEY).intValue();
		
		sessionId = (String) hubOpaqueData.getProperty(GFrame.GAL_SESSION_ID_FRAME_KEY);
		if(sessionId != null) {
		    
		    // Add the session id to the top level of the frame if
		    // there is not already a session id there.
		    if(!inputHadTopLevelSessionId)
			frame.setProperty(GFrame.GAL_SESSION_ID_FRAME_KEY, sessionId);
		}
	    }
	}
    }

    /**
     * Adds administrative information from this environment to a frame.
     *
     * @param frame the frame to update
     * @param rename if true, the frame's name is set to this environment's
     *               message name
     */
    void addAdministrativeInfo(GFrame frame, boolean rename) 
    {
	if(frame == null) 
	    return;
	
	if(rename && !msgName.equals(frame.getName().toString()))
	    frame.setName(new Symbol(msgName));
	
	if(sessionId != null) {
	    if(hubOpaqueData == null)
		hubOpaqueData = new Clause("admin_info");
	    
	    hubOpaqueData.setProperty(GFrame.GAL_SESSION_ID_FRAME_KEY, 
				      sessionId);
	}
	
	// Remove the session id from the top level of the frame if the
	// original input frame did not have session id in the top level.
	if(!inputHadTopLevelSessionId)
	    frame.removeProperty(GFrame.GAL_SESSION_ID_FRAME_KEY);
	
	if(hubOpaqueData != null) {
	    // Set the (possibly new) opaque data in the frame.
	    frame.setProperty(GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY, 
			      hubOpaqueData);
	}
    }

    //=========================================================================
    // Gettor and tester methods.
    //=========================================================================

    /**
     * Returns the token index associated with this environment.
     *
     * @return the index or -1 if is was not set
     */
    public int getTidx()
    { return tidx; }

    /**
     * Returns the opaque Hub data associated with this environment.
     *
     * @return the opaque Hub data or null if it was not set
     */
    public GFrame getHubOpaqueData()
    { return hubOpaqueData; }

    /**
     * Returns the session id associated with this environment.
     *
     * @return the session id or null if it was not set
     */
    public String getSessionId()
    { return sessionId; }

    /**
     * Returns the operation name associated with this environment.
     *
     * @return the operation name or null if it was not set
     */
    public String getOpName()
    { return opName; }

    /**
     * Returns the message name associated with this environment.
     *
     * @return the message name or null if it was not set
     */
    public String getMsgName()
    { return msgName; }

    /**
     * Returns the input stream associated with this environment.
     *
     * @return the input stream or null if it was not set
     */
    public GalaxyInputStream getInputStream()
    { return in; } 

    /**
     * Returns the output stream associated with this environment.
     *
     * @return the output stream or null if it was not set
     */
    public GalaxyOutputStream getOutputStream()
    { return out; }


    /**
     * Returns the timestamp associated with this environment.
     *
     * @return the timestamp or -1 if it is not available
     */
    public double getTokenTimestamp()
    {
	if(hubOpaqueData != null) {
	    Float64 timestamp = hubOpaqueData.getFloat64(GFrame.GAL_TOKEN_TIMESTAMP_KEY);
	    if(timestamp != null && timestamp.getSize() == 1) {
		double[] array = timestamp.getDoubleArray();
		return array[0];
	    }
	}
	return -1;
    } 

    /**
     * Inherits the current token timestamp and sends that timestamp with all 
     * new messages.
     */
    public void inheritTokenTimestamp()
    {
	inheritedTokenTimestamp = getTokenTimestamp();
    }

    /**
     * Returns the id of the originating server associated with the message
     * that created this environment.
     *
     * @return the id (may be null)
     */
    public String getOriginatingProvider()
    {
	if(hubOpaqueData != null)
	    return hubOpaqueData.getString(GFrame.GAL_PROVIDER_ID_FRAME_KEY);
	return null;
    }

    void setReturnSatisfied(boolean returnSatisfied)
    { this.returnSatisfied = returnSatisfied; }

    boolean isReturnSatisfied()
    { return returnSatisfied; }

    void setReturnPostponed(boolean returnPostponed)
    { this.returnPostponed = returnPostponed; }

    boolean isReturnPostponed()
    { return returnPostponed; } 


    //=========================================================================
    // From FrameDispatcher interface
    //=========================================================================
 
    /**
     * Returns the next message on the queue of unprocessed messages
     * encountered during calls to <code>dispatchFrame</code>.
     *
     * @return the next message on the queue or null if the queue is empty
     */
    public GalaxyMessage getNextMessage()
    { 
	return frameDispatcher.getNextMessage();
    }

    /**
     * Dispatches a frame to the Hub. This adds the keys
     * GFrame.GAL_ROUND_TRIP_FRAME_KEY (value of 1) and
     * GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY (set to next server index value)
     * to the frame before it is sent.
     *
     * @param frame the frame to dispatch
     * @return the reply frame sent from the Hub
     * @throws <code>DispatchError</code> exception if it reads an error
     *         back from the Hub
     */
    public GFrame dispatchFrame(GFrame frame)
	throws DispatchError
    {
	return dispatchFrameToProvider(frame, null);
    }

    /**
     * Dispatches a frame to the Hub. This adds the keys
     * GFrame.GAL_ROUND_TRIP_FRAME_KEY (value of 1) and
     * GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY (set to next server index value)
     * to the frame before it is sent.
     *
     * @param frame the frame to dispatch
     * @param providerID the id of the target service provider. If null, it
     *                   is ignored.
     * @return the reply frame sent from the Hub
     * @throws <code>DispatchError</code> exception if it reads an error
     *         back from the Hub
     */
    public GFrame dispatchFrameToProvider(GFrame frame, String providerID)
	throws DispatchError
    {
	return frameDispatcher.dispatchFrame(frame, providerID, this);
    }

    /**
     * Writes a new frame to the Hub. Note that this method does not return
     * anything as it does not wait for a reply from the Hub as does
     * <code>dispatchFrame</code>. The session ID (if it is set) of the 
     * current call environment is added to incoming frame before it is
     * written to the Hub.
     * @throws <code>IOException</code> exception if there is an error writing
     *         to the Hub
     *
     * @param frame the frame to write
     */
    public void writeFrame(GFrame frame) 
	throws IOException
    {
	writeFrameToProvider(frame, null);
    }

    /**
     * Writes a new frame to the Hub. Note that this method does not return
     * anything as it does not wait for a reply from the Hub as does
     * <code>dispatchFrame</code>. The session ID (if it is set) of the 
     * current call environment is added to incoming frame before it is
     * written to the Hub.
     * @throws <code>IOException</code> exception if there is an error writing
     *         to the Hub
     *
     * @param frame the frame to write
     * @param providerID the id of the targer service provider. If null, it
     *                   is ignored.
     */
    public void writeFrameToProvider(GFrame frame, String providerID) 
	throws IOException
    {
	// Don't modify the input frame.
	GFrame frameCopy = frame.copy();

	// Create the Hub opaque data for the new frame.
	GFrame opaqueData = new Clause("admin_info");

	if(providerID != null)
	    opaqueData.setProperty(GFrame.GAL_PROVIDER_ID_FRAME_KEY, providerID);  

	frameCopy.setProperty(GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY, opaqueData);

	// Set the session id.
	String frameSessionId = (String) frameCopy.getProperty(GFrame.GAL_SESSION_ID_FRAME_KEY);
	if(frameSessionId != null)
	    opaqueData.setProperty(GFrame.GAL_SESSION_ID_FRAME_KEY, frameSessionId);
	else if(sessionId != null)
	    opaqueData.setProperty(GFrame.GAL_SESSION_ID_FRAME_KEY, sessionId);

	// Possibly set the timestamp with an inherited value.
	if(inheritedTokenTimestamp != -1) {
	    double[] array = {inheritedTokenTimestamp};
	    Float64 float64 = new Float64(array);
	    opaqueData.setProperty(GFrame.GAL_TOKEN_TIMESTAMP_KEY, float64);
	}

	// Set the server token index if present.
	if(frameCopy.containsProperty(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY)) {
	    opaqueData.setProperty(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY,
				   frameCopy.getInt(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY));
	    
	    // Remove from the top level.
	    frameCopy.removeProperty(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY);
	}
	
	// Set the reply requested flag if present.
	if(frameCopy.containsProperty(GFrame.GAL_ROUND_TRIP_FRAME_KEY)) {
	    opaqueData.setProperty(GFrame.GAL_ROUND_TRIP_FRAME_KEY, 1);

	    // Remove from the top level.
	    frameCopy.removeProperty(GFrame.GAL_ROUND_TRIP_FRAME_KEY);
	}

	if(server != null)
	    server.logMessage("\nServer environment sending frame:\n" +
			      frameCopy.toFormattedString(),
			      MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
			      "Server.writeFrame(GFrame)");
	else
	    logger.logMessage("\nEnvironment object sending frame:\n" +
					frameCopy.toFormattedString(),
					MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
					"Server.writeFrame(GFrame)");
	out.writeMessage(new NewMessage(frameCopy));
    }

    //=========================================================================
    // "Frame dispatching with continuation" methods
    //=========================================================================
 
    /**
     * Dispatches a frame to the Hub, and tells the Hub that the reply does not
     * have to be sent immediately. This allows the Hub to invoke other
     * dispatch functions on the server associated with this environment. When
     * the reply to the frame dispatched by this server does arrive, it will
     * be processed using the continuation information supplied to the call
     * to this method. This method adds the keys 
     * GFrame.GAL_ROUND_TRIP_FRAME_KEY (value of 1) and
     * GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY (set to next server index value)
     * to the frame before it is sent.
     *
     * @param frame the frame to dispatch
     * @param continuation defines the action to take once the reply to this
     *                     dispatch arrives at the server
     * @param continuationState state information used to process the reply
     * @return true if the frame was sent or false if there was an error
     *         while sending the frame
     */
    public boolean dispatchFrameWithContinuation(GFrame frame,
						 Continuation continuation,
						 Object continuationState)
    { 
	return dispatchFrameToProviderWithContinuation(frame, continuation, continuationState, null);
    }

    /**
     * Dispatches a frame to the Hub, and tells the Hub that the reply does not
     * have to be sent immediately. This allows the Hub to invoke other
     * dispatch functions on the server associated with this environment. When
     * the reply to the frame dispatched by this server does arrive, it will
     * be processed using the continuation information supplied to the call
     * to this method. This method adds the keys 
     * GFrame.GAL_ROUND_TRIP_FRAME_KEY (value of 1) and
     * GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY (set to next server index value)
     * to the frame before it is sent.
     *
     * @param frame the frame to dispatch
     * @param continuation defines the action to take once the reply to this
     *                     dispatch arrives at the server
     * @param continuationState state information used to process the reply
     * @param providerID the id of the target service provider. If null, it
     *                   is ignored.
     * @return true if the frame was sent or false if there was an error
     *         while sending the frame
     */
    public boolean dispatchFrameToProviderWithContinuation(GFrame frame,
							   Continuation continuation,
							   Object continuationState, 
							   String providerID)
    { 
	if(server == null) {
	    logger.logWarningMessage("Current server is null.", "Environment.dispatchFrameWithContinuation(GFrame, Continuation, Object)");
	    return false;
	}

	// Indicate that we want a reply.
	frame.setProperty(GFrame.GAL_ROUND_TRIP_FRAME_KEY, 1);

	// Set the server token id.
	int serverTidx = frameDispatcher.getServerTidx();
	frame.setProperty(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY,
			  serverTidx);

	// Send the frame.
	try {
	    writeFrameToProvider(frame, providerID);
	} catch(IOException ioex) {
	    server.logErrorMessage("Caught exception while writing frame: " + ioex.toString(), ioex, "Environment.dispatchFrameWithContinuation(GFrame, Continuation, Object)");
	    return false;
	}
  
	// If the write didn't fail, store the continuation.
	ContinuationData data = new ContinuationData(copyEnvironment(),
						     continuation,
						     continuationState,
						     serverTidx);
	// Postpone the result.
	try {
	    postponeReply();
	} catch(IOException ioex) {
	    server.logErrorMessage("Caught exception while postponing reply: " + ioex.toString(), ioex, "Environment.dispatchFrameWithContinuation(GFrame, Continuation, Object)");
	}
	
	// Store the continuation.
	server.addContinuationData(serverTidx, data);

	return true;
    }
   

    //=========================================================================
    // Frame and message writer methods
    //=========================================================================

    /**
     * Sends a "postpone" reply.
     */
    public void postponeReply() 
	throws IOException
    {
	if(isReturnSatisfied())
	    return;
	
	PostponeMessage msg = new PostponeMessage(new Clause("postpone"));
	addAdministrativeInfo((GFrame)msg.getData(), false);
	
	try {
	    if(server != null)
		server.logMessage("\nServer environment sending \"postpone\" reply.", 
				  MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
				  "Server.postponeReply()");
	    else
		logger.logMessage("\nEnvironment object sending \"postpone\" reply.",
					    MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
					    "Server.postponeReply()");
	    out.writeMessage(msg);
	    setReturnPostponed(true);
	    setReturnSatisfied(true);
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Sends a "destroy token" reply.
     */
    public void destroyToken() 
	throws IOException
    {
	if(isReturnSatisfied())
	    return;
	
	DestroyMessage msg = new DestroyMessage(new Clause("destroy"));
	addAdministrativeInfo((GFrame)msg.getData(), false);
	
	try {
	    if(server != null)
		server.logMessage("\nServer environment sending \"destroy token\" reply.", 
				  MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
				  "Server.destroyToken()");
	    else
		logger.logMessage("\nEnvironment object sending \"destroy token\" reply.",
					    MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
					    "Server.destroyToken()");
	    out.writeMessage(msg);
	    setReturnSatisfied(true);
	} catch (IOException ioe) {
	    throw ioe;
	}
    }
    
    /**
     * Sends an error reply.
     *
     * @param description textual error message (error number is set to
     *                    galaxy.lang.ErrorMessage.GAL_APPLICATION_ERROR)
     */
    public void error(String description)
	throws IOException
    {
	if(isReturnSatisfied())
	    return;
	
	ErrorMessage msg = new ErrorMessage(description);
	addAdministrativeInfo((GFrame)msg.getData(), false);
	try {
	    if(server != null)
		server.logMessage("\nServer environment sending \"error\" reply: " + description, 
				  MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
				  "Server.error(String)");
	    else
		logger.logMessage("\nEnvironment object sending \"postpone\" reply: " + description,
					    MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
					    "Server.error(String)");
	    out.writeMessage(msg);
	    setReturnSatisfied(true);
	} catch (IOException ioe) {
	    throw ioe;
	} 
    }

    /**
     * Sends an error reply.
     *
     * @param description textual error message 
     * @param errno error number as defined in galaxy.lang.ErrorMessage
     */
    public void error(String description, int errno)
	throws IOException
    {
	if(isReturnSatisfied())
	    return;
	
	ErrorMessage msg = new ErrorMessage(description, errno);
	addAdministrativeInfo((GFrame)msg.getData(), false);
	try {
	    if(server != null)
		server.logMessage("\nServer environment sending \"error\" reply: " + description + " (" + errno + ")", 
				  MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
				  "Server.error(String, int)");
	    else
		logger.logMessage("\nEnvironment object sending \"postpone\" reply: " + description + " (" + errno + ")",
					    MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
					    "Server.error(String, int)");
	    out.writeMessage(msg);
	    setReturnSatisfied(true);
	} catch (IOException ioe) {
	    throw ioe;
	} 
    }
    
    /**
     * Sends a normal reply.
     *
     * @param frame the frame to send in the reply
     */
    public void reply(GFrame frame)
	throws IOException
    {
	if(isReturnSatisfied())
	    return;
	
	// Don't modify the input frame.
	GFrame frameCopy = frame.copy();
	ReplyMessage msg = new ReplyMessage(frameCopy);
	addAdministrativeInfo(frameCopy, true);
	try {
	    if(server != null)
		server.logMessage("\nServer environment sending frame:\n" +
				  frameCopy.toFormattedString(),
				  MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
				  "Server.reply(GFrame)");
	    else
		logger.logMessage("\nEnvironment object sending frame:\n" +
					    frameCopy.toFormattedString(),
					    MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
					    "Server.reply(GFrame)");
	    out.writeMessage(msg);
	    setReturnSatisfied(true);
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    //=========================================================================
    // Session and server property methods
    //=========================================================================

    /**
     * Gets the specified session properties.
     *
     * @param keys the keys of the properties to get
     * @return the reply frame with the session property information
     */
    public GFrame getSessionProperties(String keys[])
	throws DispatchError
    {
	if(keys == null || keys.length <= 0)
	    return null;

	GVector props = new GVector(keys.length);
	for(int idx=0; idx<keys.length; ++idx)
	    props.addElement(keys[idx]);

	GFrame frame = new Clause("builtin.get_properties");
	frame.setProperty(":namespace", "session");
	frame.setProperty(":properties", props);
	return dispatchFrame(frame);
    }

    /**
     * Sets the specified session properties.
     *
     * @param props the properties to set
     * @throws <code>IOException</code> exception if there is an error writing
     *         to the Hub
     */
    public void setSessionProperties(GFrame props)
	throws IOException
    {
	if(props != null) {
	    GFrame frame = new Clause("builtin.modify_properties");
	    frame.setProperty(":namespace", "session");
	    frame.setProperty(":properties_to_set", props);
	    writeFrame(frame);
	}
    }

    /**
     * Deletes the specified session properties.
     *
     * @param keys the keys of the properties to delete
     * @throws <code>IOException</code> exception if there is an error writing
     *         to the Hub
     */
    public void deleteSessionProperties(String keys[])
	throws IOException
    {	
	if(keys == null || keys.length <= 0)
	    return;

	GVector props = new GVector(keys.length);
	for(int idx=0; idx<keys.length; ++idx)
	    props.addElement(keys[idx]);
	
	GFrame frame = new Clause("builtin.modify_properties");
	frame.setProperty(":namespace", "session");
	frame.setProperty(":properties_to_delete", props);
	writeFrame(frame);
    }

    /**
     * Modifies the specified session properties.
     *
     * @param props the properties to set
     * @param keys the keys of the properties to delete
     * @throws <code>IOException</code> exception if there is an error writing
     *         to the Hub
     */
    public void modifySessionProperties(GFrame props, String keys[])
	throws IOException
    {
	GFrame frame = new Clause("builtin.modify_properties");
	frame.setProperty(":namespace", "session");

	if(props != null) {
	    frame.setProperty(":properties_to_set", props);
	}

	if(keys != null && keys.length > 0) {
	    GVector deleteProps = new GVector(keys.length);
	    for(int idx=0; idx<keys.length; ++idx)
		deleteProps.addElement(keys[idx]);
	
	    frame.setProperty(":properties_to_delete", deleteProps);
	}

	writeFrame(frame);
    }

    /**
     * Gets the specified server properties.
     *
     * @param keys the keys of the properties to get
     * @return the reply frame with the server property information
     */
    public GFrame getServerProperties(String keys[])
	throws DispatchError
    {
	if(keys == null || keys.length <= 0)
	    return null;
	
	GVector props = new GVector(keys.length);
	for(int idx=0; idx<keys.length; ++idx)
	    props.addElement(keys[idx]);

	GFrame frame = new Clause("builtin.get_properties");
	frame.setProperty(":namespace", "server");
	frame.setProperty(":properties", props);
	return dispatchFrame(frame);
    }

    /**
     * Sets the specified server properties.
     *
     * @param props the properties to set
     * @throws <code>IOException</code> exception if there is an error writing
     *         to the Hub
     */
    public void setServerProperties(GFrame props)
	throws IOException
    {
	if(props != null) {
	    GFrame frame = new Clause("builtin.modify_properties");
	    frame.setProperty(":namespace", "server");
	    frame.setProperty(":properties_to_set", props);
	    writeFrame(frame);
	    server.setServerProperties(props);
	}
    }

    /**
     * Deletes the specified server properties.
     *
     * @param keys the keys of the properties to delete
     * @throws <code>IOException</code> exception if there is an error writing
     *         to the Hub
     */
    public void deleteServerProperties(String keys[])
	throws IOException
    {
	if(keys == null || keys.length <= 0)
	    return;
	
	GVector props = new GVector(keys.length);
	for(int idx=0; idx<keys.length; ++idx)
	    props.addElement(keys[idx]);
	
	GFrame frame = new Clause("builtin.modify_properties");
	frame.setProperty(":namespace", "server");
	frame.setProperty(":properties_to_delete", props);
	writeFrame(frame);
	server.deleteServerProperties(keys);
    } 

    /**
     * Modifies the specified server properties.
     *
     * @param props the properties to set
     * @param keys the keys of the properties to delete
     * @throws <code>IOException</code> exception if there is an error writing
     *         to the Hub
     */
    public void modifyServerProperties(GFrame props, String keys[])
	throws IOException
    {
	GFrame frame = new Clause("builtin.modify_properties");
	frame.setProperty(":namespace", "server");

	if(props != null) {
	    frame.setProperty(":properties_to_set", props);
	}

	if(keys != null && keys.length > 0) {
	    GVector deleteProps = new GVector(keys.length);
	    for(int idx=0; idx<keys.length; ++idx)
		deleteProps.addElement(keys[idx]);
	
	    frame.setProperty(":properties_to_delete", deleteProps);
	}

	writeFrame(frame);

	if(props != null)
	    server.setServerProperties(props);

	if(keys != null)
	    server.deleteServerProperties(keys);
    }

    /**
     *  Sets the session id local to this environment. The Hub is not informed.
     *
     * @param sessionId the new session id
     */
    public void updateSessionId(String sessionId)
    {
	this.sessionId = sessionId;
    }

    /**
     * Sets the session id and informs the Hub.
     *
     * @param sessionId the new session id
     * @param lockInfo the locking information (OR'ed combination of
     *                 GFrame server/session lock values)
     * @throws <code>IOException</code> exception if there is an error writing
     *         to the Hub
     */
    public void setSessionId(String sessionId, int lockInfo)
	throws IOException
    {
	if(sessionId != null) {
	    GFrame frame = new Clause("builtin.set_session");

	    if(this.sessionId != null) {
		frame.setProperty(":previous_session_id" , this.sessionId);
	    }

	    frame.setProperty(GFrame.GAL_SESSION_ID_FRAME_KEY, sessionId);
	    frame.setProperty(":lock_info", lockInfo);
	    writeFrame(frame);

	    this.sessionId = sessionId;
	}
    }
}
