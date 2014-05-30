/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

import java.util.LinkedList;
import java.util.Iterator;

import java.io.IOException;

import galaxy.lang.GalaxyMessage;
import galaxy.lang.GFrame;
import galaxy.lang.ErrorMessage;
import galaxy.lang.ReplyMessage;

import galaxy.io.GalaxyInputStream;

import galaxy.util.FifoMutex;
import galaxy.util.Logger;

/**
 * This class encapsulates methods for dispatching and writing frames to
 * a Hub.
 */
public class FrameDispatcher
{
    //=========================================================================
    // Static interface
    //=========================================================================

    /** Server token index. */
    private static int serverTidx = 0;

    /** Use this has the lock on serverTidx. */
    private static int serverTidxLock[] = new int[1];
    
    /**
     * Returns the next free server token index.
     *
     * @return the server token index
     */
    public static int getServerTidx()
    {
	int newServerTidx = 0;
	synchronized(serverTidxLock) {
	   newServerTidx  = ++serverTidx;
	}
	return newServerTidx;
    } 

    
    //=========================================================================
    // Non-static interface
    //=========================================================================
    
    /** Queue of unprocessed messages (GalaxyMessage objects). */
    private LinkedList messageQueue;

    /** The input stream used by this dispatcher. */
    private GalaxyInputStream in;

    /** Input stream mutex. */
    private FifoMutex mutex;

    private Logger logger = null;

    //=========================================================================
    // Constructors
    //=========================================================================

    /**
     * Constructor.
     *
     * @param in the input stream to use
     */
    public FrameDispatcher(GalaxyInputStream in)
    {
	this.in = in;
	mutex = in.getMutex();

	messageQueue = new LinkedList();
	logger = Logger.getLogger();
    }

    //=========================================================================
    // Frame dispatch/writing methods
    //=========================================================================

    /**
     * Returns the next message on the queue of unprocessed messages
     * encountered during calls to <code>dispatchFrame</code>.
     *
     * @return the next message on the queue or null if the queue is empty
     */
    protected GalaxyMessage getNextMessage()
    { 
	GalaxyMessage msg = null;
	synchronized(messageQueue) {
	    if(messageQueue.size() != 0) {
		msg = (GalaxyMessage) messageQueue.removeFirst();
	    }
	}
	return msg;
    }

    /**
     * Removes the next non-reply message off the queue and returns it.
     *
     * @return the message or null if no non-reply message was found
     */
    protected GalaxyMessage getNextNonReplyMessage()
    {  
	synchronized(messageQueue) {
	    Iterator iterator = messageQueue.iterator();
	    GalaxyMessage msg = null;
	    GFrame frame;
	    boolean foundNonReply = false;
	    GFrame hubOpaqueData;
	    while(iterator.hasNext()) {
		msg = (GalaxyMessage) iterator.next();
		frame = (GFrame) msg.getData();
		hubOpaqueData = (GFrame) frame.getProperty(GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY);
		if(!hubOpaqueData.containsProperty(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY)) {
		    foundNonReply = true;
		    break;
		}
	    }

	    if(foundNonReply) {
		messageQueue.remove(msg);
		return msg;
	    }
	    
	    return null;
	}
    }

    /**
     * Removes the reply message with the specified server token index
     * off the queue and returns it.
     *
     * @param tidx the server token index
     * @return the reply or null if no reply message was found
     */
    private GalaxyMessage getReplyMessage(int tidx)
    {  
	synchronized(messageQueue) {
	    Iterator iterator = messageQueue.iterator();
	    GalaxyMessage msg = null;
	    GFrame frame;
	    int replyServerTidx = 0;
	    boolean foundReply = false;
	    GFrame hubOpaqueData;
	    while(iterator.hasNext()) {
		msg = (GalaxyMessage) iterator.next();
		frame = (GFrame) msg.getData();
		hubOpaqueData = (GFrame) frame.getProperty(GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY);
		if(hubOpaqueData.containsProperty(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY)) {
		    replyServerTidx = hubOpaqueData.getInt(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY).intValue();
		
		    if(replyServerTidx == tidx) {
			foundReply = true;
			break;
		    }
		}
	    }

	    if(foundReply) {
		messageQueue.remove(msg);
		return msg;
	    }

	    return null;
	}
    }

    /**
     * Adds a message to the end of the queue.
     *
     * @param msg the message to add
     */
    protected void queueMessage(GalaxyMessage msg)
    { 
	synchronized(messageQueue) {
	    messageQueue.addLast(msg);
	}
    }

    /**
     * Dispatches a frame to the Hub. This adds the keys
     * GFrame.GAL_ROUND_TRIP_FRAME_KEY (value of 1) and
     * GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY (set to next server index value)
     * to the frame before it is sent.
     *
     * @param frame the frame to dispatch
     * @param providerID the id of the the target service provider
     * @param env environment in which to dispatch the frame
     * @return the reply frame sent from the Hub
     * @throws <code>DispatchError</code> exception if it reads an error
     *         back from the Hub
     */
    public GFrame dispatchFrame(GFrame frame, String providerID, Environment env)
	throws DispatchError
    {
	int tidx = FrameDispatcher.getServerTidx();

	// Indicate that we want a reply.
	frame.setProperty(GFrame.GAL_ROUND_TRIP_FRAME_KEY, 1);

	// Set the server token id.
	frame.setProperty(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY,
			  tidx);

	// Send the frame.
	try {
	    env.writeFrameToProvider(frame, providerID);
	} catch(IOException ioex) {
	    logger.logErrorMessage("Caught exception while writing frame to wire: " + ioex.toString(), ioex, "FrameDispatcher.dispatchFrame(GFrame, Environment)");
	    return null;
	}
	    
	// Read in the reply

	GalaxyMessage reply = null;
	GFrame replyFrame = null;
	int replyServerTidx = 0;
	GFrame hubOpaqueData = null;
	boolean replyReceived = false;

	while(!replyReceived) {

	    // This block is synchronized on the input stream in order
	    // to synchronize with Server.run.
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
		try {
		    
		    // Check the queue for the reply message
		    reply = getReplyMessage(tidx);
		    
		    // If reply not found on queue, read new message from
		    // input stream.
		    if(reply == null) 
			reply = in.readMessage();
		    
		    replyFrame = (GFrame) reply.getData();
		    logger.logMessage("\nReceived frame:\n" +
						replyFrame.toFormattedString(),
						MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL, 
						"FrameDispatcher.dispatchFrame(GFrame, Environment)");
		} catch(Exception ex) {
		    logger.logErrorMessage("Caught exception while reading in reply: " + ex.toString(), ex, "FrameDispatcher.dispatchFrame(GFrame, Environment)");
		    return (GFrame) null;
		}

		hubOpaqueData = (GFrame) replyFrame.getProperty(GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY);
		
		// If the frame contains a server token index, see if the
		// index matches the one associated with the dispatched frame.

		if(hubOpaqueData.containsProperty(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY)) {
		    replyServerTidx = hubOpaqueData.getInt(GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY).intValue();
		    
		    if(replyServerTidx == tidx) {
			replyReceived = true;
		    } else {
			logger.logMessage("FrameDispatcher.dispatchFrame received a message that is not the reply. Adding it to queue.", MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL, "FrameDispatcher.dispatchFrame(GFrame, Environment)");
			queueMessage(reply);
		    }
		    
		} else {
		    logger.logMessage("FrameDispatcher.dispatchFrame received a non-reply frame. Adding it to queue.", MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL, "FrameDispatcher.dispatchFrame(GFrame, Environment)");
		    queueMessage(reply);
		}
	    } finally {
		mutex.unlock();
	    }
	}
    
	if (reply instanceof ErrorMessage) 
	    throw new DispatchError((GFrame)reply.getData());
	else if (!(reply instanceof ReplyMessage))
	    return (GFrame) null;

	// Move the session id to the top level of the returned frame.
	String sessionId = (String) hubOpaqueData.getProperty(GFrame.GAL_SESSION_ID_FRAME_KEY);
	if(sessionId != null)
	    replyFrame.setProperty(GFrame.GAL_SESSION_ID_FRAME_KEY, sessionId);
	
	// Remove the Hub opaque data from the returned frame.
	replyFrame.removeProperty(GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY);
	return replyFrame;


    }
}
