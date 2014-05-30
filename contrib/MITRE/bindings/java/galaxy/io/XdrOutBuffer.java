/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.io;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import galaxy.util.Logger;
import galaxy.server.MainServer;

/**
 * This class writes data to a XDR-encoded byte array.
 */
public class XdrOutBuffer
{
    /** Integer value of "false". */
    public final static int FALSE = 0;

    /** Integer value of "true". */
    public final static int TRUE = 1;

    /** Array of 0-value bytes used to pad outputs. */
    private final byte paddingBytes[] = {0,0,0};

    /** Byte stream that contains the data. */
    private ByteArrayOutputStream byteOutStream;

    /** Data output stream used to write to the byte stream. */
    private DataOutputStream outStream;

    final private String STRING_ENCODING = "ISO-8859-1";

    private Logger logger;

    //-------------------------------------------------------------------------
    // Constructors
    //-------------------------------------------------------------------------

    /**
     * Constructor.
     */
    public XdrOutBuffer()
    {
	byteOutStream = new ByteArrayOutputStream();
	outStream = new DataOutputStream(byteOutStream);
	logger = Logger.getLogger();
    }

    //-------------------------------------------------------------------------
    // Gettor methods
    //-------------------------------------------------------------------------
    
    /**
     * Returns the contents of this buffer as a byte array.
     *
     * @return the byte array
     */
    public byte[] getOutBuffer()
    {
	return byteOutStream.toByteArray();
    } 

    //-------------------------------------------------------------------------
    // Write methods
    //-------------------------------------------------------------------------

    /**
     * Writes a 32-bit signed integer value to the output buffer.
     *
     * @param intValue the integer value
     * @exception IOException if an I/O error occurs
     */
    public void writeInteger(int intValue) throws IOException
    {
	try {
	    outStream.writeInt(intValue);
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Writes an enumerated type value to the output buffer.
     *
     * @param enumeratedValue the enumerated type value
     * @exception IOException if an I/O error occurs
     */
    public void writeEnumeration(int enumeratedValue) throws IOException
    {
	try {
	    writeInteger(enumeratedValue);
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Writes a Java boolean value to the output buffer.
     *
     * @param booleanValue the boolean value
     * @exception IOException if an I/O error occurs
     */
    public void writeBoolean(boolean booleanValue) throws IOException
    {
	try {
	    writeBoolean(booleanValue ? 1 : 0);	
	} catch (IOException ioe) {
	    throw ioe;
	}
    } 

    /**
     * Writes an integer boolean value to the output buffer.
     *
     * @param booleanValue the boolean value
     * @return false if the boolean value is not 0 or 1, true otherwise
     * @exception IOException if an I/O error occurs
     */
    public boolean writeBoolean(int booleanValue) throws IOException
    {
	try {
	    if(booleanValue == FALSE || booleanValue == TRUE) {
		writeInteger(booleanValue);
		return true;
	    }
	    else
		return false;
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Writes a 64-bit signed integer value to the output buffer.
     *
     * @param longValue the integer value
     * @exception IOException if an I/O error occurs
     */
    public void writeLong(long longValue) throws IOException
    {
	try {
	    outStream.writeLong(longValue);
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Writes a 32-bit floating point value to the output buffer.
     *
     * @param floatValue the floating point value
     * @exception IOException if an I/O error occurs
     */
    public void writeFloat(float floatValue) throws IOException
    {
	try {
	    outStream.writeFloat(floatValue);
	} catch (IOException ioe) {
	    throw ioe;
	}
    } 

    /**
     * Writes a 64-bit floating point value to the output buffer.
     *
     * @param doubleValue the floating point value
     * @exception IOException if an I/O error occurs
     */
    public void writeDouble(double doubleValue) throws IOException
    {
	try {
	    outStream.writeDouble(doubleValue);
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Writes an opaque (i.e., uninterpreted) array of bytes to the output
     * buffer. Pad bytes (i.e., value of 0) are appended to the output to
     * ensure that the total number of bytes written to the output buffer is 
     * a multiple of four.
     *
     * @param bytes the byte array
     * @exception IOException if an I/O error occurs
     */
    public void writeFixedLengthOpaqueData(byte[] bytes) throws IOException
    {
	try {
	    // Bytes must be written out in multiples of 4
	    int numPaddingBytes = 0;
	    int remainder = bytes.length % 4;
	    if(remainder != 0)
		numPaddingBytes = 4 - remainder;
	   
	    outStream.write(bytes, 0, bytes.length);
	    if(numPaddingBytes != 0) {
		outStream.write(paddingBytes, 0, numPaddingBytes);
	    }
	} catch (IOException ioe) {
	    throw ioe;
	}
    } 

    /**
     * Writes an opaque (i.e., uninterpreted) array of bytes to the output
     * buffer. Four bytes, with the 32-bit unsigned integer value of the
     * length of the byte array, are first written to the output buffer. Pad 
     * bytes (i.e., value of 0) are appended to the output to
     * ensure that the total number of bytes written to the output buffer is 
     * a multiple of four.
     *
     * @param bytes the byte array
     * @exception IOException if an I/O error occurs
     */
    public void writeVariableLengthOpaqueData(byte[] bytes) throws IOException
    {
	try {
	    writeInteger(bytes.length);
	    writeFixedLengthOpaqueData(bytes);
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Writes a string to the output buffer. Four bytes, with 
     * the 32-bit unsigned integer value of the length of the byte array, are 
     * first written to the output buffer. Pad bytes (i.e., value of 0) are 
     * appended to the output to ensure that the total number of bytes written
     * to the output buffer is a multiple of four.
     *
     * @param stringValue the string
     * @exception UnsupportedEncodingException if the string encoding format
     *            (ISO-8859-1) is not available on the local platform
     * @exception IOException if an I/O error occurs
     */
    public void writeString(String stringValue) 
	throws UnsupportedEncodingException, IOException
    {
	try{
	    byte bytes[] = stringValue.getBytes(STRING_ENCODING);
	    writeVariableLengthOpaqueData(bytes);
	} catch (UnsupportedEncodingException uee) {
	    throw uee;
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Writes an array of 16-bit integers to the buffer.
     * The length of the array is written first as a 32-bit integer.
     *
     * @param shortArray the array
     */
    public void writeShortArray(short[] shortArray) throws IOException
    {
	logger.logMessage("Writing short array of length " + shortArray.length + " to outbound XDR buffer.", MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL, "XdrOutBuffer.writeShortArray(short[])");
	writeInteger(shortArray.length);
	for(int idx=0; idx<shortArray.length; ++idx) {
	    writeInteger((int) shortArray[idx]);
	}
    }
    
    /**
     * Writes an array of 32-bit integers to the buffer.
     * The length of the array is written first as a 32-bit integer.
     *
     * @param intArray the array
     */
    public void writeIntegerArray(int[] intArray) throws IOException
    {
	logger.logMessage("Writing integer array of length " + intArray.length + " to outbound XDR buffer.", MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL, "XdrOutBuffer.writeIntegerArray(int[])");
	writeInteger(intArray.length);
	for(int idx=0; idx<intArray.length; ++idx) {
	    writeInteger(intArray[idx]);
	}
    }

    /**
     * Writes an array of 64-bit integers to the buffer.
     * The length of the array is written first as a 32-bit integer.
     *
     * @param longArray the array
     */
    public void writeLongArray(long[] longArray) throws IOException
    {
	logger.logMessage("Writing long array of length " + longArray.length + " to outbound XDR buffer.", MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL, "XdrOutBuffer.writeLongArray(long[])");
	writeInteger(longArray.length);
	for(int idx=0; idx<longArray.length; ++idx) {
	    writeLong(longArray[idx]);
	}
    }

    /**
     * Writes an array of 32-bit floating point numbers to the buffer.
     * The length of the array is written first as a 32-bit integer.
     *
     * @param floatArray the array
     */
    public void writeFloatArray(float[] floatArray) throws IOException
    {
	logger.logMessage("Writing float array of length " + floatArray.length + " to outbound XDR buffer.", MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL, "XdrOutBuffer.writeFloatArray(float[])");
	writeInteger(floatArray.length);
	for(int idx=0; idx<floatArray.length; ++idx) {
	    writeFloat(floatArray[idx]);
	}
    }

    /**
     * Writes an array of 64-bit floating point numbers to the buffer.
     * The length of the array is written first as a 32-bit integer.
     *
     * @param doubleArray the array
     */
    public void writeDoubleArray(double[] doubleArray) throws IOException
    {
	logger.logMessage("Writing double array of length " + doubleArray.length + " to outbound XDR buffer.", MainServer.TRANSPORT_SUMMARY_VERBOSITY_LEVEL, "XdrOutBuffer.writeDoubleArray(double[])");
	writeInteger(doubleArray.length);
	for(int idx=0; idx<doubleArray.length; ++idx) {
	    writeDouble(doubleArray[idx]);
	}
    }
}
