/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: GalaxyInputStream.java,v 1.22 2002/03/26 16:18:31 wohlever Exp $
 */
package galaxy.io;

import java.io.DataInputStream;
import java.io.InputStream;
import java.io.IOException;

import galaxy.lang.GFrame;
import galaxy.lang.Int16;
import galaxy.lang.Int32;
import galaxy.lang.Int64;
import galaxy.lang.Float32;
import galaxy.lang.Float64;
import galaxy.lang.GBinary;
import galaxy.lang.GVector;
import galaxy.lang.Symbol;
import galaxy.lang.GalaxyMessage;
import galaxy.util.FifoMutex;
import galaxy.util.Logger;
import galaxy.server.MainServer;

/**
 * This class provides methods for reading data from a Galaxy Communicator
 * connection.
 */
public class GalaxyInputStream 
{    
    /** The input stream. */
    private DataInputStream inputStream;
    
    /** Mutex used to synchronize access to this input stream. */
    private FifoMutex mutex;

    private Logger logger;

    /**
     * Constructor
     *
     * @param in the input stream to use
     */
    public GalaxyInputStream(InputStream in) throws IOException
    {
	inputStream = new DataInputStream(in);
	mutex = new FifoMutex();
	logger = Logger.getLogger();
    }

    public void close() throws IOException
    {
	inputStream.close();
    }

    /** 
     * Returns this input stream's mutex.
     *
     * @return the mutex
     */
    public FifoMutex getMutex()
    {
	return mutex;
    }

    /**
     * Reads the next message from the input stream.
     *
     * @return the message data
     */
    private MessageData read() throws Exception 
    {
	byte[] bytes = null;
	int length = 0;
	int msgType = 0;

	synchronized(inputStream) {
	    XdrInBuffer inBuffer = new XdrInBuffer(inputStream);
	    msgType = inBuffer.readInteger();
	    length = inBuffer.readInteger();
	    bytes = inBuffer.readBytes(length);
	}
	
	logger.logMessage("\nRead message of type " + msgType + " (" + GalaxyMessage.getTypeName(msgType) + ") with " + bytes.length + " bytes.",
			  MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL,
			  "GalaxyInputStream.read()");
	
	if(MainServer.getVerbosityLevel() >= MainServer.TRANSPORT_DETAILS_VERBOSITY_LEVEL) {	
	    logger.logMessage(Util.bytesToString(bytes),
			      MainServer.TRANSPORT_DETAILS_VERBOSITY_LEVEL);
	}
	
	if (msgType == GalaxyMessage.GAL_DISCONNECT_MSG_TYPE)
	    throw new EOTException();
	
	return new MessageData(bytes, msgType);
    }

    /**
     * Reads an <code>Int16</code> from the input stream.
     * 
     * @return the <code>Int16</code>
     * @throws NotSupportedException If an <code>Int16</code> could not be read
     */
    public Int16 readInt16() throws Exception
    {
	Object object = readObject();
	if(object instanceof Int16)
	    return (Int16) object;
	else
	    throw new NotSupportedException("Object read is not an Int16");
    }

    /**
     * Reads an <code>Int16</code> from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the <code>Int16</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If an <code>Int16</code> could not be read
     */
    public Int16 tryReadInt16() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readInt16();
	    } else {
		return null;
	    }
	}	
    } 

    /**
     * Reads an <code>Int32</code> from the input stream.
     * 
     * @return the <code>Int32</code>
     * @throws NotSupportedException If an <code>Int32</code> could not be read
     */
    public Int32 readInt32() throws Exception 
    {
	Object object = readObject();
	if(object instanceof Int32)
	    return (Int32) object;
	else
	    throw new NotSupportedException("Object read is not an Int32");
    } 

    /**
     * Reads an <code>Int32</code> from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     *
     * @return the <code>Int32</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If an <code>Int32</code> could not be read
     */
    public Int32 tryReadInt32() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readInt32();
	    } else {
		return null;
	    }
	}	
    }

    /**
     * Reads an <code>Int64</code> from the input stream.
     * 
     * @return the <code>Int64</code>
     * @throws NotSupportedException If an <code>Int64</code> could not be read
     */
    public Int64 readInt64() throws Exception 
    {
	Object object = readObject();
	if(object instanceof Int64)
	    return (Int64) object;
	else
	    throw new NotSupportedException("Object read is not an Int64");
    } 

    /**
     * Reads an <code>Int64</code> from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the <code>Int64</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If an <code>Int64</code> could not be read
     */
    public Int64 tryReadInt64() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readInt64();
	    } else {
		return null;
	    }
	}	
    }

    /**
     * Reads an <code>Integer</code> from the input stream.
     * 
     * @return the <code>Integer</code>
     * @throws NotSupportedException If an <code>Integer</code> could not be 
     *                               read
     */
    public Integer readInteger() throws Exception 
    {
	Object object = readObject();
	if(object instanceof Integer)
	    return (Integer) object;
	else
	    throw new NotSupportedException("Object read is not an Integer");
    } 

    /**
     * Reads an <code>Integer</code> from the input stream if data is 
     * available. Note that this method can not detect if the connection has 
     * gone down.
     * 
     * @return the <code>Integer</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If an <code>Integer</code> could not be 
     *                               read
     */
    public Integer tryReadInteger() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readInteger();
	    } else {
		return null;
	    }
	}	
    }
    
    /**
     * Reads a <code>Float32</code> from the input stream.
     * 
     * @return the <code>Float32</code>
     * @throws NotSupportedException If a <code>Float32</code> could not be 
     *                               read
     */
    public Float32 readFloat32() throws Exception 
    {
	Object object = readObject();
	if(object instanceof Float32)
	    return (Float32) object;
	else
	    throw new NotSupportedException("Object read is not a Float32");
    }

    /**
     * Reads a <code>Float32</code> from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the <code>Float32</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If a <code>Float32</code> could not be 
     *                               read
     */
    public Float32 tryReadFloat32() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readFloat32();
	    } else {
		return null;
	    }
	}
    }

    /**
     * Reads a <code>Float64</code> from the input stream.
     * 
     * @return the <code>Float64</code>
     * @throws NotSupportedException If a <code>Float64</code> could not be 
     *                               read
     */
    public Float64 readFloat64() throws Exception 
    {
	Object object = readObject();
	if(object instanceof Float64)
	    return (Float64) object;
	else
	    throw new NotSupportedException("Object read is not a Float64");
    }

    /**
     * Reads a <code>Float64</code> from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the <code>Float64</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If a <code>Float64</code> could not be 
     *                               read
     */
    public Float64 tryReadFloat64() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readFloat64();
	    } else {
		return null;
	    }
	}
    }

    /**
     * Reads a <code>Float</code> from the input stream.
     * 
     * @return the <code>Float</code>
     * @throws NotSupportedException If a <code>Float</code> could not be 
     *                               read
     */
    public Float readFloat() throws Exception 
    {
	Object object = readObject();
	if(object instanceof Float)
	    return (Float) object;
	else
	    throw new NotSupportedException("Object read is not a Float");
    }

    /**
     * Reads a <code>Float</code> from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the <code>Float</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If a <code>Float</code> could not be 
     *                               read
     */
    public Float tryReadFloat() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readFloat();
	    } else {
		return null;
	    }
	}
    }

    /**
     * Reads a <code>GBinary</code> from the input stream.
     * 
     * @return the <code>GBinary</code>
     * @throws NotSupportedException If a <code>GBinary</code> could not be 
     *                               read
     */
    public GBinary readBinary() throws Exception 
    {
	Object object = readObject();
	if(object instanceof GBinary)
	    return (GBinary) object;
	else
	    throw new NotSupportedException("Object read is not a GBinary");
    }

    /**
     * Reads a <code>GBinary</code> from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the <code>GBinary</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If a <code>GBinary</code> could not be 
     *                               read
     */
    public GBinary tryReadBinary() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readBinary();
	    } else {
		return null;
	    }
	}
    }

    /**
     * Reads a <code>GVector</code> from the input stream.
     * 
     * @return the <code>GVector</code>
     * @throws NotSupportedException If a <code>GVector</code> could not be 
     *                               read
     */
    public GVector readList() throws Exception 
    {
	Object object = readObject();
	if(object instanceof GVector)
	    return (GVector) object;
	else
	    throw new NotSupportedException("Object read is not a GVector");
    }

    /**
     * Reads a <code>GVector</code> from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the <code>GVector</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If a <code>GVector</code> could not be 
     *                               read
     */
    public GVector tryReadList() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readList();
	    } else {
		return null;
	    }
	}
    }

    /**
     * Reads a <code>Symbol</code> from the input stream.
     * 
     * @return the <code>Symbol</code>
     * @throws NotSupportedException If a <code>Symbol</code> could not be 
     *                               read
     */
    public Symbol readSymbol() throws Exception 
    {
	Object object = readObject();
	if(object instanceof Symbol)
	    return (Symbol) object;
	else
	    throw new NotSupportedException("Object read is not a Symbol");
    }

    /**
     * Reads a <code>Symbol</code> from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the <code>Symbol</code> or null if no data was available or 
     *         there was an error
     * @throws NotSupportedException If a <code>Symbol</code> could not be 
     *                               read
     */
    public Symbol tryReadSymbol() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readSymbol();
	    } else {
		return null;
	    }
	}
    }
    
    /**
     * Reads a string from the input stream.
     * 
     * @return the string
     * @throws NotSupportedException If a string could not be read
     */
    public String readString() throws Exception
    {
	Object object = readObject();
	if(object instanceof String)
	    return (String) object;
	else
	    throw new NotSupportedException("Object read is not a String");
    } 

    /**
     * Reads a string from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the string or null if no data was available or there was an 
     *         error
     * @throws NotSupportedException If a string could not be read
     */
    public String tryReadString() throws Exception
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readString();
	    } else {
		return null;
	    }
	}
    }
    
    /**
     * Reads a frame from the input stream.
     * 
     * @return the frame
     * @throws NotSupportedException If a frame could not be read
     */
    public GFrame readFrame() throws Exception 
    {
	Object object = readObject();
	if(object instanceof GFrame)
	    return (GFrame) object;
	else
	    throw new NotSupportedException("Object read is not a GFrame");
    }

    /**
     * Reads a frame from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the frame or null if no data was available or there was an 
     *         error
     * @throws NotSupportedException If a frame could not be read
     */
    public GFrame tryReadFrame() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readFrame();
	    } else {
		return null;
	    }
	}
    }
    
    /**
     * Reads a message from the input stream.
     * 
     * @return the message or null if there was an error
     */
    public GalaxyMessage readMessage() throws Exception 
    {
	MessageData msgData = read();
	
	XdrInBuffer buffer = new XdrInBuffer(msgData.bytes);
	Object object = GFrame.decodeObject(buffer);
	if(object == null)
	    return null;
	
	return GalaxyMessage.createMessage(object, msgData.msgType);
    }

    /**
     * Reads a message from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the message or null if no data was available or there was an 
     *         error
     */
    public GalaxyMessage tryReadMessage() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readMessage();
	    } else {
		return null;
	    }
	}
    }

    /**
     * Reads an object from the input stream.
     * 
     * @return the object or null if there was an error
     */
    public Object readObject() throws Exception 
    {
	MessageData msgData = read();

	XdrInBuffer buffer = new XdrInBuffer(msgData.bytes);
	return GFrame.decodeObject(buffer);
    }

    /**
     * Reads an object from the input stream if data is available.
     * Note that this method can not detect if the connection has gone down.
     * 
     * @return the object or null if no data was available or there was an 
     *         error
     */
    public Object tryReadObject() throws Exception 
    {
	synchronized(inputStream) {
	    if(isDataAvailable()) {
		return readObject();
	    } else {
		return null;
	    }
	}
    }

    /**
     * Indicates if there is data available to be read on this input stream.
     * Note that this method can not detect if the connection has gone down.
     *
     * @return true if data is available, false otherwise
     */
    private boolean isDataAvailable() throws IOException
    {
	synchronized(inputStream) {
	    return (inputStream.available() > 0);
	}
    }

    /** Encapsulates input data of a message. */
    private class MessageData 
    {
	/** The data bytes. */
	public byte[] bytes;

	/** The message type. */
	public int msgType;

	public MessageData(byte[] bytes, int msgType)
	{
	    this.bytes = bytes;
	    this.msgType = msgType;
	}
    }
}
