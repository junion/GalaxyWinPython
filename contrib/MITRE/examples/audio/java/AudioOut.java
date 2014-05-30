/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

import java.io.IOException;
import java.net.Socket;

import galaxy.lang.*;
import galaxy.server.Server;
import galaxy.server.MainServer;
import galaxy.server.DataOutBroker;
import galaxy.server.DataOutBrokerProxy;

public class AudioOut extends Server
{
    private final int BLOCK_SIZE = 1024;
    private final int BROKER_ORIGINAL = 0;
    private final int BROKER_PROXY_OBJ = 1;
    private final int BROKER_PROXY_STREAM = 2;
    private final int BROKER_PROXY_ORIGINAL = 3;
    private int brokerMethod = BROKER_ORIGINAL;
    private boolean useStream = false;

    public AudioOut(MainAudioOut mainServer, Socket socket) 
	throws IOException
    {
	super(mainServer, socket);
    }
    
    public void serverOpReinitialize(GFrame frame) 
    {
	// Determine if the audio is to be streamed (i.e., sent as several
	// objects rather than as one object).
	Boolean useStreamBoolean = frame.getIntAsBoolean(":use_stream");
	if(useStreamBoolean != null)
	    useStream = useStreamBoolean.booleanValue();

	// Determine how the audio data should be brokered.
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

	// Read in the audio data based on the information in the incoming
	// frame.
	GBinary audio = getData(frame);

	if(audio != null) {
	    byte bytes[] = audio.getBytes();
	    byte bytesToWrite[] = null;
	    int bytesWritten = 0;
	    int numBytesToWrite = 0;
	    GFrame result = new Clause("main");
	    int pollMs = 50;
	    int timeoutSecs = 50;
	    
	    switch(brokerMethod) {
	    case BROKER_ORIGINAL: // Use the "traditional" brokering approach.
		DataOutBroker broker;
		try {
		    broker = new DataOutBroker(this, pollMs, timeoutSecs);
		} catch (Exception ex) {
		    try {
			logAndSendErrorMessage("Error opening broker connection: " + ex.toString(), ex);
		    } catch(IOException ioex) {
			logErrorMessage("Caught exception while sending error message: " + ioex.toString(), ioex);
		    }
		    return;
		}
		
		try {
		    if(useStream) {
			while(bytesWritten < bytes.length) {
			    if((bytes.length - bytesWritten) >= BLOCK_SIZE)
				numBytesToWrite = BLOCK_SIZE;
			    else
				numBytesToWrite = bytes.length - bytesWritten;
			    bytesToWrite = new byte[numBytesToWrite];
			    System.arraycopy(bytes, bytesWritten, 
					     bytesToWrite, 0, 
					     numBytesToWrite);
			    
			    bytesWritten += numBytesToWrite;
			    broker.write(new GBinary(bytesToWrite));
			}
		    } else {
			broker.write(audio);
		    }
		    broker.close();
		} catch (Exception ex) {
		    try {
			logAndSendErrorMessage("Error writing to Broker: " + ex.toString(), ex);
		    } catch(IOException ioex) {
			logErrorMessage("Caught exception while sending error message: " + ioex.toString(), ioex);
		    }
		    return;
		}
		broker.populateFrame(result, ":binary_host", ":binary_port");
		break;

	    case BROKER_PROXY_OBJ: // Use the "proxy" brokering approach.
	    case BROKER_PROXY_STREAM:
	    case BROKER_PROXY_ORIGINAL:
		try {
		    DataOutBrokerProxy brokerProxy = new DataOutBrokerProxy(GalaxyObject.GAL_BINARY, this, pollMs, timeoutSecs);
		    if(useStream) {
			while(bytesWritten < bytes.length) {
			    if((bytes.length - bytesWritten) >= BLOCK_SIZE)
				numBytesToWrite = BLOCK_SIZE;
			    else
				numBytesToWrite = bytes.length - bytesWritten;
			    bytesToWrite = new byte[numBytesToWrite];
			    System.arraycopy(bytes, bytesWritten, 
					     bytesToWrite, 0, 
					     numBytesToWrite);
			    
			    bytesWritten += numBytesToWrite;
			    brokerProxy.addArrayToArray(new GBinary(bytesToWrite));
			}
		    } else {
			brokerProxy.addArrayToArray(audio);
		    }
		    brokerProxy.close();
		    result.setProperty(":binary_proxy", brokerProxy.getProxy());
		} catch (Exception ex) {
		    try {
			logAndSendErrorMessage("Error writing to Broker: " + ex.toString(), ex);
		    } catch(IOException ioex) {
			logErrorMessage("Caught exception while sending error message: " + ioex.toString(), ioex);
		    }
		    return;
		}
		break;
	    }
	    sendFrame(result);
	}
    }

    private GBinary getData(GFrame frame)
    {
	String file;  
	file = frame.getString(":audiofile");
	if (file.equals("")) {
	    logErrorMessage("No audio filename provided");
	    return null;
	}
	logMessage("Audio file is " + file);
	
	byte[] data;
	try {
	    data = GBinary.readBinaryFile(file);
	    return new GBinary(data);
	} catch (IOException ioex1) {
	    try {
		logAndSendErrorMessage("Problems while reading file: " + file 
				       + "\n" + ioex1.toString(), ioex1);
	    } catch(IOException ioex2) {
		logErrorMessage("Caught exception while sending error message: " + ioex2.toString(), ioex2);
	    }
	    return null;
	}
    }

    private void sendFrame(GFrame frame)
    { 
	logMessage("Sending frame:\n" +
		   frame.toFormattedString(),
		   MainServer.TRAFFIC_DETAILS_VERBOSITY_LEVEL);
	try {
	    getCurrentEnvironment().writeFrame(frame);
	} catch(Exception ex) {
	    try {
		logAndSendErrorMessage("Error writing to broker: " + ex.toString(), ex);
	    } catch(IOException ioex) {
		logErrorMessage("Caught exception while sending error message: " + ioex.toString(), ioex);
	    }
	}
    }
      
    public void serverOpNotify(GFrame frame)
    {
	logMessage("Audio send: " + (String) frame.getProperty(":notification"));
    }
}
