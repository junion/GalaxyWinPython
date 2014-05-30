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

public interface ServerListener {
    public void serverStarted(Server Server);
    public void serverStopped(Server Server);
    public void serverMessage(Server Server, String msg);
    public void serverWarningMessage(Server Server, String msg);
    public void serverErrorMessage(Server Server, String msg);
    public void serverErrorMessage(Server Server, String msg, Exception ex);
    public void serverFatalErrorMessage(Server Server, String msg);
    public void serverFatalErrorMessage(Server Server, String msg, Exception ex);
}
