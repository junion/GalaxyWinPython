/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: Multiply.java,v 1.11 2002/03/21 16:15:29 wohlever Exp $
 */
import galaxy.lang.*;
import galaxy.server.*;
import galaxy.server.ui.ServerUI;
import java.net.*;
import java.io.*;

import galaxy.util.Logger;

public class Multiply extends galaxy.server.Server {

    int factor = 1;
 
    public Multiply(MainServer ms, Socket s) 
	throws IOException {
	super(ms,s);
	initSignatures();
    } 
    
    private void initSignatures() {
    	SigEntry[] foo = {new SigEntry(":int",GalaxyObject.GAL_INT, Signature.GAL_KEY_ALWAYS)};
	addSignature(new Signature("multiply", foo , Signature.GAL_OTHER_KEYS_NEVER, foo, Signature.GAL_OTHER_KEYS_NEVER, Signature.GAL_REPLY_PROVIDED));
    }
    
    public GFrame serverOpMultiply(GFrame fr) {
	int i = fr.getInt(":int").intValue();
	// Do some error checking...
	if ( (Integer.MAX_VALUE/i) < factor) {
	    
	    try {
		getCurrentEnvironment().error("multiply would overflow MAXINT");
	    } catch (Exception ex) {
		logErrorMessage("Caught exception while sending error message: " + ex.toString(), ex);
	    }
	    return (GFrame)null;
	}
	fr.setProperty(":int", i * factor);
	return fr;
    }

    public void serverOpReinitialize(GFrame fr) {
	int f = fr.getInt(":factor").intValue();
	if (f != 0)
	    this.factor = f;
    }

    public static void main(String[] args) {
	
	/*
	ServerUI ui = new ServerUI(args) {
		protected void init() {
		    serverClassName = "Multiply";
		    port = 2900;
		    serverName = "multiply";
		}};
	*/
	
	MainServer mainServer = new MainServer("multiply", args, 2900);
	mainServer.setServerClassName("Multiply");
	try {
	    mainServer.start();
	} catch(Exception ex) {
	    Logger.getLogger().logErrorMessage("Caught exception while staring main server: " + ex.toString(), ex);
	}
    }
}
