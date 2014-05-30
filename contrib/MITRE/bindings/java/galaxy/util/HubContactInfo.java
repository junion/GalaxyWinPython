/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.util;

/**
 * Contains Hub contact information. This information is used for servers
 * that establish client connections to Hubs.
 */
public class HubContactInfo 
{
    /** The name of the machine hosting the Hub. */
    private String host;

    /** Listener port of the Hub. */
    private int port = -1;

    /** Default session id to be used for server-side connection. */
    private String sessionId;

    /** The Hub contact policy. */
    private int hubContactPolicy = -1;
    
    public HubContactInfo(String host, int port, String sessionId,
			  int hubContactPolicy)
    {
	this.host = host;
	this.port = port;
	this.sessionId = sessionId;
	this.hubContactPolicy = hubContactPolicy;
    }
    
    public void setHost(String host)
    { this.host = host; }
    
    public String getHost()
    { return host; }
    
    public void setPort(int port)
    { this.port = port; }
    
    public int getPort()
    { return port; }

    public void setSessionId(String sessionId)
    { this.sessionId = sessionId; }
    
    public String getSessionId()
    { return sessionId; }

    public void setHubContactPolicy(int hubContactPolicy)
    { this.hubContactPolicy = hubContactPolicy; }
    
    public int getHubContactPolicy()
    { return hubContactPolicy; }
}

