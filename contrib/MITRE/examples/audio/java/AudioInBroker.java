/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

import java.io.IOException;
import java.io.File;
import java.io.FileOutputStream;

import galaxy.server.Server;
import galaxy.server.MainServer;
import galaxy.server.DataInBroker;
import galaxy.lang.*;

public class AudioInBroker extends DataInBroker 
{
    private GBinary bdata;

    public AudioInBroker(Server server, String host, int port, String callId)
	throws IOException 
    {
	super(server, host, port, callId);
	bdata = new GBinary();
      
	// You must explicitly call start() once your broker is initialized.
	// For example, you can call it at the end of the constructor, as is
	// done here.
	start();
    }

    public AudioInBroker(Server server, BrokerProxy proxy)
	throws IOException 
    {
	super(server, proxy);
	bdata = new GBinary();
      
	// You must explicitly call start() once your broker is initialized.
	// For example, you can call it at the end of the constructor, as is
	// done here.
	start();
    }

    protected void disconnectReceived()
    {
	try {
	    
	    byte audioData[] = bdata.getBytes();
	    
	    GFrame notifyFrame = new Clause("notify");
	    notifyFrame.setProperty(":notification", "Audio received.");
	    env.writeFrame(notifyFrame);
	    
	    File file = new File("/dev/audio");
	    FileOutputStream fstream = new FileOutputStream(file);
	    fstream.write(audioData);
	    
	    // The call to sleep is here because calling close on the
	    // /dev/audio right after writing the data to /dev/audio often
	    // cloes /dev/audio before all the data is flushed from it
	    // (i.e., the audio gets clipped randomly).
	    server.logMessage("AudioInBroker sleeping...", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL);
	    Thread.sleep(2000);
	    fstream.close();
	    server.logMessage("AudioInBroker awake", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL);	    
	} catch (IOException ioex) {
	    server.logErrorMessage("Caught exception while processing audio data: " + ioex.toString(), ioex);
	} catch (Exception ex) {
	    server.logErrorMessage("Caught exception while processing audio data: " + ex.toString(), ex);
	}
    } 

    protected void abortReceived()
    {
	try { 
	    GFrame notifyFrame = new Clause("notify");
	    notifyFrame.setProperty(":notification", "Audio aborted.");
	    env.writeFrame(notifyFrame);
	} catch(Exception e) {
	}
    }
    
    public  void receivedBinary(GBinary bin) 
    {
	bdata.append(bin.getBytes());
    }
}
