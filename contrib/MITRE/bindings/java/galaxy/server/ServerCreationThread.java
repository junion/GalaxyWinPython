/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

import java.lang.reflect.Constructor;
import java.net.Socket;

import galaxy.io.GalaxyInputStream;
import galaxy.io.GalaxyOutputStream;

/**
 * This is a thread that creates and starts new instances of 
 * <code>Server</code>. This class must be extended.
 */
public abstract class ServerCreationThread extends Thread
{
    /** Listener thread type. */
    protected final static int LISTENER = 0;

    /** Hub client thread type. */
    protected final static int CLIENT = 1;

    /** This thread's type. */
    protected int threadType = LISTENER;

    /** The <code>MainServer</code> that created this thread. */
    protected MainServer mainServer;

    /** This threads input stream. */
    protected GalaxyInputStream in; 

    /** This threads output stream. */
    protected GalaxyOutputStream out;

    /** Reference to this thread. */
    protected Thread thread; 

    /** This flag is set if there was an attempt to start too many servers. */
    protected boolean tooManyServers = false;

    /** Flag that indicates if this thread is running. */
    protected volatile boolean isRunning = false;


    //=========================================================================
    // Constructors
    //=========================================================================

    /**
     * Creats a new thread that creates servers.
     *
     * @param mainServer the <code>MainServer</code> that created this thread
     * @param threadType the type of thread created (e.g., a listener, a
     *                   Hub client). The valid values are defined in
     *                   this class.
     */
    public ServerCreationThread(String name, MainServer mainServer, int threadType)
    {
	super(name);
	this.mainServer = mainServer;
	this.threadType = threadType;
	this.thread = this;
    } 

    //=========================================================================
    // Logging and error-reporting methods
    //=========================================================================

    protected void logMessage(String message, int level)
    {
	mainServer.logMessage(message, level);
    }

    protected void logMessage(String message, int level, String location)
    {
	mainServer.logMessage(message, level, location);
    }

    protected void logWarningMessage(String message)
    {
	mainServer.logWarningMessage(message);
    }

    protected void logWarningMessage(String message, String location)
    {
	mainServer.logWarningMessage(message, location);
    }

    protected void logErrorMessage(String message, Exception ex)
    {
	mainServer.logErrorMessage(message, ex);
    }

    protected void logErrorMessage(String message)
    {
	mainServer.logErrorMessage(message);
    }

    protected void logErrorMessage(String message, Exception ex, String location)
    {
	mainServer.logErrorMessage(message, ex, location);
    }

    protected void logErrorMessage(String message, String location)
    {
	mainServer.logErrorMessage(message, location);
    }

    //=========================================================================
    // "Server creation" thread methods
    //=========================================================================

    /**
     * The main routine for this thread.
     */
    public abstract void run(); 

    /**
     * Marks this broker's thread as being interrupted. 
     */
    public void stopThread()
    {  
	thread.interrupt();
	isRunning = false;
    }

    public boolean isRunning()
    { return isRunning; }


    //=========================================================================
    // Server thread methods
    //=========================================================================

    /**
     * Creates a new server to handle a new connection. The server is
     * created dynamically based on the value of
     * <code>MainServer.serverClassName</code>.
     *
     * @param clientSocket the client connection that is to be associated with
     *                     the new server
     * @param sessionId initial session id of the server
     * @return reference to the new server
     */
    protected Server createServer(Socket clientSocket, String sessionId)
    {
	if(mainServer.canSupportNewConnection()) {
	    
	    Server server;
	    Constructor constructor;
	    Class serverClass;
	    Class sig[];
	    
	    String serverClassName = mainServer.getServerClassName();
	    Class mainServerClass = mainServer.getClass();
	    
	    // Identify the classes that make up the argument list of the
	    // servers constructor.
	    sig = new Class[] {mainServerClass, Socket.class};
	
	    // Get a reference to the server's class based on its class name.
	    try {
		serverClass = Class.forName(serverClassName);
	    } catch(Exception ex) {
		logErrorMessage("Caught exception while creating server class \"" + serverClassName + "\":" + ex.toString(), ex, "ServerCreationThread.createServer(Socket, String)");
		return null;
	    }
	    
	    // Get a reference to the constructor for the server.
	    try {
		constructor = serverClass.getConstructor(sig);
	    } catch(Exception ex) {
		logErrorMessage("Caught exception while getting constructor for server class \"" + serverClassName + "\":" + ex.toString(), ex, "ServerCreationThread.createServer(Socket, String)");
		return null;
	    }
	    
	    // Create a new instance of the server.
	    Object args[];
	    try {
		args = new Object[]{mainServer, clientSocket};
		
		server = (Server) constructor.newInstance(args);
		if(threadType == CLIENT) {
		    server.setSessionId(mainServer.getSessionId());
		    server.setActsAsHubClient(true);
		}
	    } catch(Exception ex) {
		logErrorMessage("Caught exception while creating instance of server class \"" + serverClassName + "\": " + ex.toString(), ex, "ServerCreationThread.createServer(Socket, String)");
		return null;
	    }
	    
	    return server;
	} else {
	    logWarningMessage("Max limit on number of servers reached.", "ServerCreationThread.createServer(Socket, String)");
	    tooManyServers = true;
	    return null;
	}
    }
    
    /**
     * Starts the specified server. <code>Server.init</code> is invoked on
     * the server to perform custom initialization. Just before the server is 
     * started, listeners registered with <code>MainServer</code> are notified
     * by calling <code>MainServer.fireNewServer</code>.
     *
     * @param server the server to start
     * @return reference to the server thread object
     *
     */
    protected Thread startServer(Server server) throws Exception
    {
	// Create and initialize the server thread.
	Thread serverThread = new Thread(server, "ServerThread");
	server.setThread(serverThread);
	server.setValidate(mainServer.getValidate());
	server.init();

	// Register the server thread.
	mainServer.registerServer(serverThread, server);
	mainServer.fireNewServer(server);

	// Start the server.
	serverThread.start();

	return serverThread;
    }
}
