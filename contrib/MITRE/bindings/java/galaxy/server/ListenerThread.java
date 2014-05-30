/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

import java.io.InterruptedIOException;
import java.io.IOException;

import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import galaxy.io.GalaxyOutputStream;
import galaxy.io.GalaxyInputStream;

import galaxy.lang.GalaxyMessage;
import galaxy.lang.ErrorMessage;
import galaxy.lang.GFrame;

/**
 * This thread accepts clients connections and creates server threads
 * to handle the client requests ("server per client connection" model). It
 * also manages connections to outgoing brokers.
 */
public class ListenerThread extends ServerCreationThread
{ 
    /** This listener's server socket. */
    private ServerSocket serverSocket;

    /** 
     * Flag that indicates if this listener has obtained a port for its
     * socket connection.
     */
    private volatile boolean gotListenerPort = false;

    /** This is the timeout used when blocking on a call to accept(). */
    private final int ACCEPT_TIMEOUT_MS = 1000;
    
    //=========================================================================
    // Constructors
    //=========================================================================

    /**
     * Constructor
     *
     * @param mainServer the <code>MainServer</code> that created this listener
     */
    public ListenerThread(MainServer mainServer)
    {
	super("ListenerThread", mainServer, ServerCreationThread.LISTENER);
    }


    //=========================================================================
    // Testers
    //=========================================================================

    /**
     * Tests if this listener has acquired a port.
     *
     * @return true if this listener has a port, false otherwise
     */
    protected boolean listenerHasPort()
    {
	return gotListenerPort; 
    }
    

    //=========================================================================
    // Thread management methods
    //=========================================================================
    
    /**
     * This is the thread's main routine. It attempts to create a listener
     * on a port. It then loops forever, until interrupted by 
     * <code>ServerCreationThread.stopThread</code>, accepting client
     * connections and creating servers to serve the clients.
     */
    public void run()
    {

	isRunning = true;
	
	try {
	    // Establish a listener. Exit if there is a problem.
	    if(!getListenerConnection()) {
		logMessage("Listener stopped", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL, "ListenerThread.run()");
		mainServer.stop();
		return;
	    }
	    
	    Socket clientSocket;
	    logMessage("Listener waiting for connection request...", MainServer.CONNECTION_VERBOSITY_LEVEL, "ListenerThread.run()");
	    while(isRunning) {

		clientSocket = null;
		
		// Accept a new connection		
		try{
		    clientSocket = serverSocket.accept();
		} catch(InterruptedIOException iioe) {
		    // Call to accept() timed out.
		    clientSocket = null;
		}

		if(clientSocket != null) {

		    logMessage("Listener got connection request.", MainServer.CONNECTION_VERBOSITY_LEVEL, "ListenerThread.run()");
		    
		    out = new GalaxyOutputStream(clientSocket.getOutputStream());
		    in = new GalaxyInputStream(clientSocket.getInputStream());
		    
		    doHandshake(clientSocket);

		    // Print the message for the next call to accept().
		    logMessage("Listener waiting for connection request...", MainServer.CONNECTION_VERBOSITY_LEVEL, "ListenerThread.run()");
		}
	    }
	} catch(Exception ex) {
	    logErrorMessage("Exception caught in listener main loop: " + ex.toString(), ex, "ListenerThread.run()");
	}
	prepareToStop();
	logMessage("Listener stopped.", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL, "ListenerThread.run()");
    }

    /**
     * Performs cleanup and closes this listener's socket.
     */
    private void prepareToStop()
    {
	try{
	    serverSocket.close();
	} catch(Exception ex) {
	    logErrorMessage("Caught exception while trying to close listener socket: " + ex.toString(), ex, "ListenerThread.prepareToStop()");
	}
    }
    
    /**
     * Attempts to create a listener on the port defined in this thread's
     * instance of <code>MainServer</code>.
     *
     * @return true if a listener was established, false if the required port
     *         could not be obtained
     */
    private boolean getListenerConnection()
    {
	int tempPort = mainServer.getPort();
	while(true) {
	    try {
		serverSocket = new ServerSocket(tempPort);
		serverSocket.setSoTimeout(ACCEPT_TIMEOUT_MS);
		mainServer.setPort(tempPort);
		gotListenerPort = true; 
		logMessage("Listener got port " + tempPort + ".", 
			   MainServer.CONNECTION_VERBOSITY_LEVEL,
			   "ListenerThread.getListenerConnection()");
		break;
	    } catch(IOException ioex) {
		logErrorMessage("Caught exception while creating socket using port " + tempPort + ": " + ioex.toString(), ioex, "ListenerThread.getListenerConnection()");
		if(mainServer.isRequiredPort()) {
		    logErrorMessage("Listener could not get the required port",
				    "ListenerThread.getListenerConnection()");
		    return false;
		}
	    }

	    // Try the next port.
	    ++tempPort;
	}
	return true;
    }

    /**
     * Performs the server side handshake with the Hub.
     *
     * @param clientSocket the socket of the client that is attempting to
     *                     connect a server or broker managed by this listener
     */
    private void doHandshake(Socket clientSocket)
    {
	try {
	    // Read in the handshake message.
	    GalaxyMessage msg = in.readMessage();
	    GFrame frame = (GFrame) msg.getData();
	    Environment env = new Environment(frame, in, out);
	    GFrame replyFrame = null;
	    
	    logMessage("\nListener received handshake frame:\n" +
		       frame.toFormattedString() + "\n", 
		       MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
		       "ListenerThread.doHandshake(Socket)");
	    
	    // Get the connection type.
	    int connType = -1;
	    try {
		connType = frame.getInt(GFrame.GAL_CONNECTION_TYPE_FRAME_KEY).intValue();
	    } catch(Exception ex) {
		logErrorMessage("Caught exception in listener while getting connection type: " + ex.toString(), ex, "ListenerThread.doHandshake(Socket)");
		return;
	    }
	    
	    // If the connection request is from a broker client, try to find
	    // a suitable broker server.
	    if(connType == MainServer.GAL_BROKER_LISTENER) {
		
		// First check if this listener is managing any broker servers.
		// If not, send back an error reply
		if(!mainServer.supportsBrokerConnections()) {
		    logMessage("\nListener sending error (\"no match for broker\").", MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL, "ListenerThread.doHandshake(Socket)");
		    env.error("no match for broker",
			      ErrorMessage.GAL_CONN_REJECTION_ERROR);
		} else {
		    String callId = frame.getString(GFrame.GAL_BROKER_CALL_ID_FRAME_KEY);
		    if(callId == null) {
			logErrorMessage("Broker connection requested but no call id supplied.", "ListenerThread.doHandshake(Socket)");
			return;
		    }
		    
		    // Try to find a broker with the specified call id
		    DataOutBroker broker = mainServer.findBroker(callId);
		    
		    // If a broker is not found, send an error reply.
		    if(broker == null) {
			logMessage("\nListener sending error (\"no match for broker\") for callId = " + callId + ".", MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL, "ListenerThread.doHandshake(Socket)");
			env.error("no match for broker",
				  ErrorMessage.GAL_CONN_REJECTION_ERROR);
		    } else {
			
			// Try to add the client to the broker server.
			if(broker.addClientSocket(clientSocket)) {
			    replyFrame = frame;
			    replyFrame.setProperty(":protocol_version", 1);
			    logMessage("\nListener sending pacifier frame.",
				       MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL, "ListenerThread.doHandshake(Socket)");
			    env.reply(replyFrame);
			    broker.setClientHandshakeComplete(clientSocket);
			} else {
			    logMessage("\nListener sending error (\"broker expired\").", 
				       MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
				       "ListenerThread.doHandshake(Socket)");
			    env.error("broker expired",
				      ErrorMessage.GAL_CONN_REJECTION_ERROR);
			}
		    } 
		}
		
	    // Client is requesting a standard connection to a server.
	    } else if (mainServer.isHubListener()) {
		
		Server server = createServer(clientSocket, null);
			
		if (server != null) {
                    server.setServerConnectionType(MainServer.GAL_CONNECTION_LISTENER);
		    mainServer.initServer(server);
		    startServer(server);
		} else {
		    if(tooManyServers) {
			logMessage("\nListener sending error (\"max connections exceeded\").", 
				   MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL,
				   "ListenerThread.doHandshake(Socket)");
			env.error("max connections exceeded",
				  ErrorMessage.GAL_CONN_REJECTION_ERROR);
			tooManyServers = false;
		    } else {
			logErrorMessage("Unknown error in listener while creating a new server.", "ListenerThread.doHandshake(Socket)");
		    }
		}
	    } else {
		// This is a Hub client that started a listener soley to 
		// support an out broker, so no Hub connections are allowed.
		logMessage("\nListener sending error (\"unsupported connection type\").", MainServer.TRAFFIC_SUMMARY_VERBOSITY_LEVEL, "ListenerThread.doHandshake(Socket)");
		env.error("unsupported connection type",
			  ErrorMessage.GAL_CONN_REJECTION_ERROR);
	    }
	} catch(SocketException sex) {
	    // Server has most likely been shut down.
	    try { 
		in.close();
		out.close();
		clientSocket.close();
	    } catch(IOException ioex) {
	    }
	} catch(Exception ex) {
	    logErrorMessage("Caught exception while doing handshake: " + ex.toString(), ex, "ListenerThread.doHandshake(Socket)");
	    try { 
		in.close();
		out.close();
		clientSocket.close();
	    } catch(IOException ioex) {
		logErrorMessage("Caught exception while closing listener socket: " + ioex.toString(), ioex, "ListenerThread.doHandshake(Socket)");
	    }
	} 
    }
}
