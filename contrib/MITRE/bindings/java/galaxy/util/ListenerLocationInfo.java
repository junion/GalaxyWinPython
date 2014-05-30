/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.util;

/**
 * Contain information on a listener. This includes the host name and port of
 * the listener and whether or not the listener is managed by a server or a
 * Hub.
 */
public class ListenerLocationInfo
{
    private boolean serverIsListener;
    private String hostName;
    private int port;
    
    public ListenerLocationInfo(boolean serverIsListener, String hostName, int port)
    {
	this.serverIsListener = serverIsListener;
	this.hostName = hostName;
	this.port = port;
    }
    
    public boolean isServerListener()
    { return serverIsListener; }
    
    public String getHostName()
    { return hostName; }
    
    public int getPort()
    { return port; }
}
