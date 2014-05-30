/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

// $Id: BinaryData.java,v 1.6 2002/02/18 22:47:36 wohlever Exp $
import galaxy.lang.*;
import galaxy.server.*;
import java.io.*;
import java.net.*;

public class BinaryData extends galaxy.server.Server {

    private String audiodata;

    public BinaryData(MainBinary mainServer, Socket socket) 
	throws IOException {
	super(mainServer, socket);
    }

    public void serverOpReinitialize(GFrame frame) {
	String audioFile = frame.getString(":audiofile");
	try {
	    byte[] bs = GBinary.readBinaryFile(audioFile);
	    frame = new Clause("main");
	    frame.setProperty(":audio_data", new GBinary(bs));
	    getCurrentEnvironment().writeFrame(frame);
	} catch (Exception e) {
	    logErrorMessage("OOps exce thrown!", e);
	}
    }

    public GFrame serverOpReceiveAudio(GFrame frame) {
	GBinary bdata = frame.getBinary(":audio_data");
	
	File f = null;
	FileOutputStream fstream = null;
	try {
	    logMessage("About to open file");
	    f = new File("/dev/audio");
	    fstream = new FileOutputStream(f);
	} catch (IOException ioex) {
	    logErrorMessage("Caught exception while opening /dev/audio: " + ioex.toString(), ioex);
	    return frame;
	}
	
	try {
	    logMessage("Writing to file");
	    fstream.write(bdata.getBytes());

            // The call to sleep is here because calling close on the
	    // /dev/audio right after writing the data to /dev/audio often
	    // cloes /dev/audio before all the data is flushed from it
	    // (i.e., the audio gets clipped randomly).
            logMessage("Server sleeping for a few seconds...", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL);
            Thread.sleep(2000);
            fstream.close();
            logMessage("Server awake", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL);

	} catch (Exception ex) {
	    logErrorMessage("Caught exception while writing to /dev/audio: " + ex.toString(), ex);
	}
	return frame;
    }
}
