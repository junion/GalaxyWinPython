/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

import java.io.IOException;

import java.net.Socket;
import java.net.UnknownHostException;

import java.util.List;
import java.util.ArrayList;

import galaxy.util.HubContactInfo;

/**
 * This thread establishes connections to a specified list of Hubs.
 */
public class ClientThread extends ServerCreationThread
{ 
    /** Collection of socket connections established by this thread. */
    private List sockets;

    /** 
     * List of new Hubs that are to be contacted. This list is used to add
     * Hub connections once this thread is running and has contacted its
     * initial list of Hubs.
     */
    private ArrayList newHubContactInfoCollection;

    /** List of hosts of Hubs to which connections have been established. */
    private List hubHosts;

    /** List of ports of Hubs to which connections have been established. */
    private List hubPorts;

    //=========================================================================
    // Constructors
    //=========================================================================

    /**
     * Constructor
     *
     * @param mainServer the <code>MainServer</code> that created this thread
     */
    public ClientThread(MainServer mainServer)
    {
	super("ClientThread", mainServer, ServerCreationThread.CLIENT);

	ArrayList hubContactInfoCollection = 
	    (ArrayList) mainServer.getHubContactInfoCollection();

	if(hubContactInfoCollection != null) {

	    // Make a COPY of the Hub contact info collection.
	    newHubContactInfoCollection = 
		(ArrayList) hubContactInfoCollection.clone();
	    sockets = new ArrayList(newHubContactInfoCollection.size());
	} else {
	    newHubContactInfoCollection = new ArrayList();
	    sockets = new ArrayList();
	}

	hubHosts = new ArrayList();
	hubPorts = new ArrayList();
    }


    //=========================================================================
    // Thread management methods
    //=========================================================================
    
    /**
     * Adds the specified Hub info to the list of Hubs to contact.
     *
     * @param host name of the Hub's host machine
     * @param port listener port of the Hub
     * @param sessionId session id to use for the connection
     * @param hubContactPolicy Hub contact policy flag
     */
    void contactHub(String host, int port, String sessionId,
		    int hubContactPolicy)
    {
	synchronized(newHubContactInfoCollection) {
	    newHubContactInfoCollection.add(new HubContactInfo(host, port, sessionId, hubContactPolicy));
	}
    }

    /**
     * This is the thread's main routine. It periodically looks for new
     * Hubs to contact.
     */
    public void run()
    {
	isRunning = true;
	
	// Start a server thread for each Hub connection.
	try {
	    
	    HubContactInfo newInfo;
	    Socket socket;
	    int numHubs = 0;
	    Server server;
	    List retryList;
	    int hubContactPolicy;
	    boolean getPolicyFromMainServer = false;

	    while(isRunning) {
	
		retryList = new ArrayList();

		// Check for new Hubs to contact
		synchronized(newHubContactInfoCollection) {
		    numHubs = newHubContactInfoCollection.size();
		    if(numHubs > 0) {
			for(int idx=0; idx<numHubs; ++idx) {
			    newInfo = (HubContactInfo) newHubContactInfoCollection.get(idx);
			    hubContactPolicy = newInfo.getHubContactPolicy();
			    if(hubContactPolicy == -1) {
				getPolicyFromMainServer = true;
				hubContactPolicy = mainServer.getHubContactPolicy();
			    }
			    
			    // Contact Hub
			    socket = connectToHub(newInfo.getHost(),
						  newInfo.getPort());
			    
			    if(socket != null) {

				// Create server and set its connection 
				// type and contact policy
				server = createServer(socket,
						      newInfo.getSessionId());
				server.setServerConnectionType(MainServer.GAL_HUB_CLIENT);

				// Only modify the server's Hub contact policy
				// if it has not already been modified (e.g.,
				// in the specialized server constructor).
				if(!server.wasHubContactPolicyModified()) {
				    if(getPolicyFromMainServer)
					server.setHubContactPolicy(-1);
				    else
					server.setHubContactPolicy(hubContactPolicy);
				} else {
				    hubContactPolicy = server.getHubContactPolicy();
				}

				server.setHubHost(newInfo.getHost());
				server.setHubPort(newInfo.getPort());
				
				// Start server
				if(server != null) {
				    mainServer.initServer(server);
				    startServer(server);
				}
			    } else if((hubContactPolicy & 
				       MainServer.GAL_HUB_CLIENT_CONNECT_FAILURE_MASK) == 0) {
				
				// If the connection attempt failed, and the
				// policy is to retry (i.e., not to shutdown or do a no-op), 
				// add to the retry list.
				retryList.add(newInfo);
				
			    } else if((hubContactPolicy & 
				       MainServer.GAL_HUB_CLIENT_CONNECT_FAILURE_SHUTDOWN) != 0) {
				
				// If the connection attempt failed, and the
				// policy is to shutdown the server, do so.
				isRunning = false;
				mainServer.stop();
			    }
			}
			newHubContactInfoCollection.clear();
			newHubContactInfoCollection.addAll(retryList);
		    }
		}

		if(isRunning) {
		    // Sleep for one second.
		    try{ 
			Thread.sleep(1000);
		    } catch(InterruptedException ie) {
			break;
		    }
		}
	    }
	} catch(Exception ex) {
	    logErrorMessage("Caught exception: " + ex.toString(), ex,
			    "ClientThread.run()");
	}

	prepareToStop();
	logMessage("Client thread stopped", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL, "ClientThread.run()");
    }

    /**
     * Performs cleanup and closes this thread's client connections.
     */
    private void prepareToStop()
    {
	int size = sockets.size();
	Socket socket;
	for(int idx=0; idx<size; ++idx) {
	    socket = (Socket) sockets.get(idx);
	    try {
		socket.close();
	    } catch(Exception ex) {
		logErrorMessage("Caught exception while trying to close client connections: " + ex.toString(), ex, "ClientThread.prepareToStop()");
	    }
	}
    }

    /**
     * Trys to establish a connection with the specified Hub.
     *
     * @param hubHost name of the Hub's host machine
     * @param hubPort listener port of the Hub
     */
    private Socket connectToHub(String hubHost, int hubPort)
    {
	try {
	    Socket socket = new Socket(hubHost, hubPort);
	    sockets.add(socket);
	    hubHosts.add(hubHost);
	    hubPorts.add(new Integer(hubPort));
	    logMessage("Connected to Hub at host " + hubHost + " and port " + hubPort + ".", MainServer.CONNECTION_VERBOSITY_LEVEL, "ClientThread.connectToHub(String, int)");
	    return socket;
	} catch(UnknownHostException uhex) {
	    logErrorMessage("Encountered \"unknown host\" error while attempting to connect to Hub at host \"" + hubHost + "\".", "ClientThread.connectToHub(String, int)");
	    return null;
	} catch(IOException ioex) {
	    logErrorMessage("Caught exception while attempting to connect to Hub at " + hubHost + ":" + hubPort + ": " + ioex.toString(), ioex, "ClientThread.connectToHub(String, int)");   
	    return null;
	}
    }
}
