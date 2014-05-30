/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/*
 * $Id: GalaxyOutputStream.java,v 1.16 2002/05/31 21:30:20 wohlever Exp $
 */
package galaxy.io;

import java.io.DataOutputStream;
import java.io.OutputStream;
import java.io.IOException;

import galaxy.lang.GFrame;
import galaxy.lang.DisconnectMessage;
import galaxy.lang.GalaxyMessage;
import galaxy.server.MainServer;
import galaxy.util.Logger;

/**
 * This class provides methods for writing data to a Galaxy Communicator
 * connection.
 */
public class GalaxyOutputStream
{
    /** The output stream. */
    private DataOutputStream outputStream;
    
    private Logger logger;

    /**
     * Constructor
     *
     * @param out the output stream to use
     */
    public GalaxyOutputStream(OutputStream out) throws IOException
    {
        outputStream = new DataOutputStream(out);
	logger = Logger.getLogger();
    }
    
    public void close() throws IOException
    {
	outputStream.close();
    }
    
    /*
     * @deprecated As of Galaxy Communicator 3.0. Use writeObject.
     *
    public void write(GFrame frame, int msgType) throws IOException 
    {
	writeObject(frame, msgType);
    }
    */

    /*
     * @deprecated As of Galaxy Communicator 3.0. Use writeObject.
     *
    public void write(String str, int msgType) throws IOException
    {
	writeObject(str, msgType);
    }
    */

    /*
     * @deprecated As of Galaxy Communicator 3.0. Use writeObject.
     *
    public void writeFrame(GFrame frame, int msgType) throws IOException 
    {
	writeObject(frame, msgType);
    }
    */
    
    /**
     * Writes a message to the output stream.
     *
     * @param msg the message
     * @throws IOException If there is an error writing to the output stream
     */
    public void writeMessage(GalaxyMessage msg) throws IOException
    {
	writeObject(msg.getData(), msg.getType());
    }
    
    /**
     * Writes a disconnect message to the output stream.
     *
     * @param msg the disconnect message 
     * @throws IOException If there is an error writing to the output stream
     */
    public void writeEOT(DisconnectMessage msg) throws IOException
    {
	XdrOutBuffer buffer = new XdrOutBuffer();
	
	if(MainServer.getVerbosityLevel() >= MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL) {
	    XdrOutBuffer xdrBuffer = new XdrOutBuffer();
	    xdrBuffer.writeInteger(msg.getType());
	    xdrBuffer.writeInteger(0);
	    byte[] bytes = xdrBuffer.getOutBuffer();

	    logger.logMessage("\nWriting message of type " + msg.getType() + " (" + GalaxyMessage.getTypeName(msg.getType()) + ") with " + bytes.length + " bytes.", 
					MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL,
					"GalaxyOutputStream.writeEOT(DisconnectMessage)");
	    if(MainServer.getVerbosityLevel() >= MainServer.TRANSPORT_DETAILS_VERBOSITY_LEVEL) {
		logger.logMessage(Util.bytesToString(bytes), 
					    MainServer.TRANSPORT_DETAILS_VERBOSITY_LEVEL);
	    }
	}
	
	synchronized(outputStream) {
	    outputStream.writeInt(msg.getType());
	    outputStream.writeInt(0);
	}
    }

    /**
     * Writes an object to the output stream.
     *
     * @param obj the object
     * @param msgType the message type (object is encapsulated in a message)
     * @throws IOException If there is an error writing to the output stream
     */
    public void writeObject(Object obj, int msgType) throws IOException
    {
	try {
	    XdrOutBuffer buffer = new XdrOutBuffer();

	    // Encode the object.
	    if(!GFrame.encodeObject(buffer, obj)) {
		throw new NotGalaxyStreamable(obj.getClass().getName() + 
					      " is not Galaxy-streamable");
	    }
	    byte bytes[] = buffer.getOutBuffer();
	    
	    if(MainServer.getVerbosityLevel() >= MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL) {
		XdrOutBuffer headerXdrBuffer = new XdrOutBuffer();
		headerXdrBuffer.writeInteger(msgType);
		headerXdrBuffer.writeInteger(bytes.length);
		byte[] headerBytes = headerXdrBuffer.getOutBuffer();

		logger.logMessage("\nWriting message of type " + msgType + " (" + GalaxyMessage.getTypeName(msgType) + ") with " + (bytes.length + headerBytes.length) + " bytes.", 
					    MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL,
					    "GalaxyOutputStream.writeObject(Object, int)");
		
		if(MainServer.getVerbosityLevel() >= MainServer.TRANSPORT_DETAILS_VERBOSITY_LEVEL) {
		    logger.logMessage(Util.bytesToString(headerBytes) + Util.bytesToString(bytes),
						MainServer.TRANSPORT_DETAILS_VERBOSITY_LEVEL);
		}
	    }
	    
	    // Write the data to the connection.
	    synchronized(outputStream) {
		// Write the header info.
		outputStream.writeInt(msgType);
		outputStream.writeInt(bytes.length);

		// Write the data.
		outputStream.write(bytes, 0 ,bytes.length);
	    }
	} catch(IOException ioex) {
	    throw ioex;
	} catch(Exception ex) {
	    logger.logErrorMessage("Caught exception while writing object to wire: " + ex.toString(), ex, "GalaxyOutputStrem.writeObject(Object, int)");
	}
    }
}
