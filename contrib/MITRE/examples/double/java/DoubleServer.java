/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: DoubleServer.java,v 1.15 2002/03/21 16:15:29 wohlever Exp $
 */
import galaxy.lang.*;
import galaxy.server.*;
import galaxy.server.ui.ServerUI;
import java.net.*;
import java.io.*;

import galaxy.util.Logger;

public class DoubleServer extends galaxy.server.Server {

    public DoubleServer(MainServer mainServer, Socket socket) 
	throws IOException {
	super(mainServer, socket);
	initSignatures();
    }

    private void initSignatures() {
	SigEntry[] foo = {new SigEntry(":int",GalaxyObject.GAL_INT, Signature.GAL_KEY_ALWAYS), new SigEntry(":program",GalaxyObject.GAL_STRING, Signature.GAL_KEY_SOMETIMES)};
	addSignature(new Signature("twice", foo , Signature.GAL_OTHER_KEYS_NEVER, (SigEntry[])null, Signature.GAL_OTHER_KEYS_NEVER, Signature.GAL_REPLY_NONE));	
	addSignature(new Signature("complex_twice",foo, Signature.GAL_OTHER_KEYS_NEVER, (SigEntry[])null, Signature.GAL_OTHER_KEYS_NEVER, Signature.GAL_REPLY_NONE));
    }

    public void serverOpReinitialize(GFrame frame) {
	String prog = frame.getString(":program");
	
	if (prog == null) prog = "main";

	GFrame fr = new Clause(prog);
	fr.setProperty(":int",1);
	fr.setProperty(":program", prog);
       
	try{
	    getCurrentEnvironment().writeFrame(fr);
	} catch(Exception ex) {
	    logFatalMessage("Caught exception while writing to Hub", ex);
	}
    }

    public GFrame serverOpComplexTwice(GFrame frame) {
	Clause newFr = new Clause("multiply");
	GFrame res=null;

	newFr.setProperty(":int",frame.getInt(":int"));
	try {
	    res = getCurrentEnvironment().dispatchFrame(newFr);
	} catch (DispatchError dex) {
	    logFatalMessage("Received error from Environment.dispatchFrame: " + dex.getData());
	    return (GFrame) null;
	} catch (Exception ex) {
	    logFatalMessage("Caught exception while writing to Hub", ex);
	    return (GFrame) null;
	}

	if (res == null) {
	    logWarningMessage("Didn't hear back from mutliply");
	    return (GFrame) null;
	}
	
	String progName = frame.getString(":program");
	if (progName == null) progName = "main";
	res.setProperty(":program",progName);
	
	return serverOpTwice(res);
    }

    public GFrame serverOpTwice(GFrame frame) {
	int i = ((Integer)frame.getProperty(":int")).intValue();
	String prog = null;
	try {
	    Environment env = getCurrentEnvironment();
	    if ( i == 0) {
		env.error("i is 0");
	    } else if ( (Integer.MAX_VALUE/i) < 2) {
		env.error("double would overflow MAXINT");
	    } else {
		i = i * 2;
		prog = frame.getString(":program");
		if (prog == null) prog = "main";
		GFrame fr = new Clause(prog);
		fr.setProperty(":program",prog);
		fr.setProperty(":int", i);
		env.writeFrame(fr);
	    }
	} catch (Exception ex) {
	    logErrorMessage("Caught exception in serverOpTwice: " + ex.toString(), ex);
	}

	return (GFrame) null;
    }
    
    public GFrame serverOpSeed(GFrame frame) {
	frame.setProperty(":int",3);
	return frame;
    }

    public GFrame serverOpEchoFrame(GFrame frame)
    {
	return frame;
    }

    public GFrame serverOpContinuationComplexTwice(GFrame frame)
    {
	GFrame newFrame = new Clause("multiply");
	int i = ((Integer)frame.getProperty(":int")).intValue();
	newFrame.setProperty(":int", i);  
	ContinueComplexTwice continueComplexTwice = new ContinueComplexTwice();
	getCurrentEnvironment().dispatchFrameWithContinuation(newFrame,
							      continueComplexTwice,
							      frame);
	return (GFrame) null;
    }

    class ContinueComplexTwice implements Continuation 
    {
	public GFrame run(GFrame frame, int msgType, Object continuationState,
			  Environment env)
	{
	    GFrame origFrame = (GFrame) continuationState;
	    GFrame returnFrame;
	    GFrame twiceFrame;
	    String progName;
	    
	    if (frame == null) {
		logWarningMessage("Didn't hear back from multiply");
		return (GFrame) null;
	    }
	    
	    switch (msgType) {
	    case GalaxyMessage.GAL_REPLY_MSG_TYPE:
		progName = origFrame.getString(":program");
		if (progName == null) 
		    progName = "main";

		twiceFrame = new Clause("twice");
		twiceFrame.setProperty(":program", progName);
		int i = ((Integer)frame.getProperty(":int")).intValue();
		twiceFrame.setProperty(":int", i);
		returnFrame = serverOpTwice(twiceFrame);
		return returnFrame;
	    case GalaxyMessage.GAL_ERROR_MSG_TYPE:
		return (GFrame) null;
	    default:
		return (GFrame) null;
	    }
	}
    }

    public static void main(String[] args) {
	
	/*
	ServerUI ui = new ServerUI(args) {
		protected void init() {
		    serverClassName = "DoubleServer";
		    port = 2800;
		    serverName = "double";
		}};
	*/
	
	MainServer mainServer = new MainServer("double", args, 2800);
	mainServer.setServerClassName("DoubleServer");
	try {
	    mainServer.start();
	} catch(Exception ex) {
	    Logger.getLogger().logErrorMessage("Caught exception while starting main server: " + ex.toString(), ex);
	}
    }
}
