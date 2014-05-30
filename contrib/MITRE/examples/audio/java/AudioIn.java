/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

import java.io.IOException;
import java.io.File;
import java.io.FileOutputStream;

import java.net.Socket;

import galaxy.lang.GFrame;
import galaxy.lang.BrokerProxy;
import galaxy.lang.GBinary;
import galaxy.lang.Clause;
import galaxy.lang.GalaxyObject;

import galaxy.server.MainServer;
import galaxy.server.Server;
import galaxy.server.DataInBrokerProxy;

public class AudioIn extends Server 
{
    private final int BROKER_ORIGINAL = 0;
    private final int BROKER_PROXY_OBJ = 1;
    private final int BROKER_PROXY_STREAM = 2;
    private final int BROKER_PROXY_ORIGINAL = 3;
    private int brokerMethod = BROKER_ORIGINAL;

    public AudioIn(MainAudioIn mainServer, Socket socket) throws IOException 
    {
	super(mainServer, socket);
    }
    
    public void serverOpReinitialize(GFrame frame) 
    {	
	// Determine how the audio data should be read in.
	String brokerMethodName = frame.getString(":broker_method");
	if(brokerMethodName == null ||
	   brokerMethodName.equals("original_env") ||
	   brokerMethodName.equals("original_comm")) {
	    brokerMethod = BROKER_ORIGINAL;
	} else if( brokerMethodName.equals("proxy_obj")) {
	    brokerMethod = BROKER_PROXY_OBJ;
	} else if( brokerMethodName.equals("proxy_stream")) {
	    brokerMethod = BROKER_PROXY_STREAM;
	} else if( brokerMethodName.equals("proxy_original")) {
	    brokerMethod = BROKER_PROXY_ORIGINAL;
	} else {
	    brokerMethod = BROKER_ORIGINAL;
	}
    }

    public GFrame serverOpReceiveAudio(GFrame frame) 
    {
	AudioInBroker aib = null;
	Object obj = null;
	BrokerProxy proxy = null;
	DataInBrokerProxy brokerProxy = null;

	// Based on the value of brokerMethod, read in the audio data. This
	// demonstrates the various ways inbound brokers can read in data
	// (including the traditional and proxy-based approaches).
	switch(brokerMethod) {
	case BROKER_ORIGINAL:
	    String host = frame.getString(":binary_host");
	    Integer port = frame.getInt(":binary_port");
	    String callId = frame.getString(GFrame.GAL_BROKER_CALL_ID_FRAME_KEY);
	    try {
		aib = new AudioInBroker(this, host, port.intValue(), callId);
	    } catch (Exception ex) {
		try {
		    logAndSendErrorMessage("Error starting DataInBroker (tried to connect to " + host + ":" + port +"): " + ex.toString());
		} catch(IOException ioex) {
		    logErrorMessage("Caught exception while sending error message: " + ioex.toString(), ioex);
		}
		return null;
	    }
	    return frame;

	case BROKER_PROXY_OBJ:
	    proxy = frame.getProxy(":binary_proxy");
	    if(proxy == null) {
		return frame;
	    }
	    
	    try {	
		brokerProxy = new DataInBrokerProxy(this, proxy, false, true);
		obj = brokerProxy.getObject(true);	
		processData(obj);
	    } catch (Exception ex) {
		try {
		    logAndSendErrorMessage("Error while retrieving object: " + ex.toString(), ex);
		} catch(IOException ioex) {
		    logErrorMessage("Caught exception while sending error message: " + ioex.toString(), ioex);
		}
		return null;
	    }
	    return frame;

	case BROKER_PROXY_STREAM:	
	    proxy = frame.getProxy(":binary_proxy");
	    if(proxy == null) {
		return frame;
	    }
	    
	    try {
		brokerProxy = new InBrokerProxy(this, proxy, false, true);	
	    } catch (Exception ex) {
		try {
		    logAndSendErrorMessage("Error while retrieving object: " + ex.toString(), ex);
		} catch(IOException ioex) {
		    logErrorMessage("Caught exception while sending error message: " + ioex.toString(), ioex);
		}
		return null;
	    }
	    return frame;

	case BROKER_PROXY_ORIGINAL:
	    // The only difference between BROKER_PROXY_ORIGINAL and
	    // BROKER_ORIGINAL is the arguments passed to the AudioInBroker
	    // constructor. In this case, the BrokerProxy "proxy" object 
	    // in the incoming frame contains the info needed to contact the 
	    // target outbound broker.
	    proxy = frame.getProxy(":binary_proxy");
	    try {
		aib = new AudioInBroker(this, proxy);
	    } catch (Exception ex) {
		try {
		    logAndSendErrorMessage("Error starting DataInBroker (tried to connect to " + proxy.getHost() + ":" + proxy.getPort() +"): " + ex.toString());
		} catch(IOException ioex) {
		    logErrorMessage("Caught exception while sending error message: " + ioex.toString(), ioex);
		}
		return null;
	    }
	    return frame;
	}
	return null;
    }
  
    private void processData(Object obj)
    {	
	if(obj == null) {
	    logWarningMessage("Did not get any data to process.", "AudioIn.processData(Object)");
	    return;
	}

	if(obj instanceof GBinary) {
	    try {
		
		byte audioData[] = ((GBinary) obj).getBytes();
		
		GFrame notifyFrame = new Clause("notify");
		notifyFrame.setProperty(":notification", "Audio received.");
		getCurrentEnvironment().writeFrame(notifyFrame);
		
		File file = new File("/dev/audio");
		FileOutputStream fstream = new FileOutputStream(file);
		fstream.write(audioData);
		
		// The call to sleep is here because calling close on the
		// /dev/audio right after writing the data to /dev/audio often
		// close /dev/audio before all the data is flushed from it
		// (i.e., the audio gets clipped randomly).
		logMessage("AudioIn sleeping...", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL);
		Thread.sleep(2000);
		fstream.close();
		logMessage("AudioIn awake", MainServer.THREAD_ACTIVITY_VERBOSITY_LEVEL);	    
	    } catch (IOException ioex) {
		logErrorMessage("Caught exception while processing audio data: " + ioex.toString(), ioex);
	    } catch (Exception ex) {
		logErrorMessage("Caught exception while processing audio data: " + ex.toString(), ex);
	    }
	} else {
	    logErrorMessage("AudioIn did not receive a GBinary object. Got " + obj);
	}
    }

    class InBrokerProxy extends DataInBrokerProxy
    {
	
	public InBrokerProxy(Server server, BrokerProxy proxy, boolean immediate, boolean startBroker) throws IOException
	{
	    super(server, proxy, immediate, startBroker);
	}

	public void abortReceived()
	{
	    try { 
		GFrame notifyFrame = new Clause("notify");
		notifyFrame.setProperty(":notification", "Audio aborted.");
		server.getCurrentEnvironment().writeFrame(notifyFrame);
	    } catch(Exception ex) {
	    }
	}

	public void receivedObject(Object obj)
	{ 
	    try {
		((AudioIn) server).processData(obj);
	    } catch (Exception ex) {
		server.logErrorMessage("Caught exception while processing audio data: " + ex.toString(), ex);
	    }
	}
    }
}
