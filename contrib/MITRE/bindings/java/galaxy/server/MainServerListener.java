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

package galaxy.server;

public interface MainServerListener {
    public void mainServerStarted(MainServer mainServer);
    public void mainServerStopped(MainServer mainServer);
    public void mainServerNewServer(MainServer mainServer, Server server);
    public void mainServerMessage(MainServer mainServer, String msg);
    public void mainServerWarningMessage(MainServer mainServer, String msg);
    public void mainServerErrorMessage(MainServer mainServer, String msg);
    public void mainServerErrorMessage(MainServer mainServer, String msg, Exception ex);
    public void mainServerFatalErrorMessage(MainServer mainServer, String msg);
    public void mainServerFatalErrorMessage(MainServer mainServer, String msg, Exception ex);
}
