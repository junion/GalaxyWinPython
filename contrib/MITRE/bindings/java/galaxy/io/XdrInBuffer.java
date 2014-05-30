/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.io;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

/**
 * This class reads data from a XDR-encoded byte array or input stream.
 */
public class XdrInBuffer
{
    /** Integer value of "false". */
    public final static int FALSE = 0;

    /** Integer value of "true". */
    public final static int TRUE = 1;

    /** Data input stream used to read the input data. */
    private DataInputStream inStream;

    final private String STRING_ENCODING = "ISO-8859-1";

    //-------------------------------------------------------------------------
    // Constructors
    //-------------------------------------------------------------------------

    /**
     * Creates a XDR input buffer based on a specified byte array.
     *
     * @param bytes the buffer's data
     */
    public XdrInBuffer(byte bytes[])
    {
	inStream = new DataInputStream(new ByteArrayInputStream(bytes));
    }

    /**
     * Creates a XDR input buffer based on a specified input stream.
     *
     * @param inStream the input stream
     */
    public XdrInBuffer(DataInputStream inStream)
    {
	this.inStream = inStream;
    }

    //-------------------------------------------------------------------------
    // Read methods
    //-------------------------------------------------------------------------

    /**
     * Reads a 32-bit signed integer value from the input buffer.
     *
     * @return the integer value
     * @exception IOException if an I/O error occurs
     */
    public int readInteger() throws IOException
    {
	int intValue = -1;
	try {
	    intValue = inStream.readInt();
	} catch (IOException ioe) {
	    throw ioe;
	}
	return intValue;
    }

    /**
     * Reads an enumerated type value from the input buffer.
     *
     * @return the enumerated type value
     * @exception IOException if an I/O error occurs
     */
    public int readEnumeration() throws IOException
    {
	try {
	    return readInteger();
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Reads a boolean value from the input buffer.
     *
     * @return the boolean value (0 = false, 1 = true)
     * @exception IOException if an I/O error occurs
     */
    public int readBoolean() throws IOException
    {
	try {
	    return readInteger();
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Reads a 64-bit signed integer value from the input buffer.
     *
     * @return the integer value
     * @exception IOException if an I/O error occurs
     */
    public long readLong() throws IOException
    {
	try {
	    return inStream.readLong();
	} catch (IOException ioe) {
	    throw ioe;
	}
    }
    
    /**
     * Reads a 32-bit floating point value from the input buffer.
     *
     * @return the floating point value
     * @exception IOException if an I/O error occurs
     */
    public float readFloat() throws IOException
    {
	try {
	    return inStream.readFloat();
	} catch (IOException ioe) {
	    throw ioe;
	}
    } 
    
    /**
     * Reads a 64-bit floating point value from the input buffer.
     *
     * @return the floating point value
     * @exception IOException if an I/O error occurs
     */
    public double readDouble() throws IOException
    {
	try {
	    return inStream.readDouble();
	} catch (IOException ioe) {
	    throw ioe;
	}
    } 
    
    /**
     * Reads an array of bytes from the input buffer. The number of bytes
     * read is the same as the length of the <code>bytes</code> argument. 
     *
     * @param bytes the array that the bytes will be read into
     * @exception IOException if an I/O error occurs
     */
    public void readFixedLengthOpaqueData(byte bytes[]) throws IOException
    {
	try {
	    inStream.read(bytes, 0, bytes.length);
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Reads an array of bytes from the input buffer. The number of bytes
     * read is given by the first four bytes read off the input buffer
     * (see XdrOutBuffer.writeVariableLengthOpaqueData). Note that return 
     * value does not include any padding bytes (if they were present).
     *
     * @return the byte array
     * @exception IOException if an I/O error occurs
     */
    public byte[] readVariableLengthOpaqueData() throws IOException
    {
	try {
	    // Read the length of the byte array.
	    int length = readInteger();

	    // Read in the byte array.
	    byte bytes[] = new byte[length];
	    readFixedLengthOpaqueData(bytes);

	    // Discard the padding bytes.
	    int numPaddingBytes = 0;
	    int remainder = length % 4;
	    if(remainder != 0) {
		numPaddingBytes = 4 - remainder;
		byte paddingBytes[] = new byte[numPaddingBytes];
		readFixedLengthOpaqueData(paddingBytes);
	    }

	    // Return the bytes.
	    return bytes;
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Reads a string value from the input buffer.
     *
     * @return the string
     * @exception UnsupportedEncodingException if the string encoding format
     *            (ISO-8859-1) is not available on the local platform
     * @exception IOException if an I/O error occurs
     */
    public String readString() throws UnsupportedEncodingException, IOException
    {
	try {
	    // Read in the length of the string.
	    int length = readInteger();

	    // Read in the string bytes.
	    byte bytes[] = new byte[length];
	    readFixedLengthOpaqueData(bytes);
	    
	    // Discard the padding bytes.
	    int numPaddingBytes = 0;
	    int remainder = length % 4;
	    if(remainder != 0) {
		numPaddingBytes = 4 - remainder;
		byte paddingBytes[] = new byte[numPaddingBytes];
		readFixedLengthOpaqueData(paddingBytes);
	    }

	    // Return the string.
	    return new String(bytes, STRING_ENCODING);
	} catch (UnsupportedEncodingException uee) {
	    throw uee;
	} catch (IOException ioe) {
	    throw ioe;
	}
    }

    /**
     * Reads an array of 16-bit integers from the buffer. Note that
     * the XDR encoding only directly supports 32- and 64-bit integers,
     * so 32-bit integers are actually read from the buffer and cast into
     * 16-bit integers. This is fine as long as 32-bits read do indeed only
     * contain 16-bit values.
     *
     * @param length the size of the array to read
     * @return the array
     */
    public short[] readShortArray(int length) throws IOException
    {
	short[] shortArray = new short[length];
	for(int idx=0; idx<length; ++idx) {
	    shortArray[idx] = (short) readInteger();
	}
	return shortArray;
    } 

    /**
     * Reads an array of 32-bit integers from the buffer.
     *
     * @param length the size of the array to read
     * @return the array
     */
    public int[] readIntegerArray(int length) throws IOException
    {
	int[] intArray = new int[length];
	for(int idx=0; idx<length; ++idx) {
	    intArray[idx] = readInteger();
	}
	return intArray;
    }
    
    /**
     * Reads an array of 64-bit integers from the buffer.
     *
     * @param length the size of the array to read
     * @return the array
     */
    public long[] readLongArray(int length) throws IOException
    {
	long[] longArray = new long[length];
	for(int idx=0; idx<length; ++idx) {
	    longArray[idx] = readLong();
	}
	return longArray;
    }

    /**
     * Reads an array of 32-bit floating point numbers from the buffer.
     *
     * @param length the size of the array to read
     * @return the array
     */
    public float[] readFloatArray(int length) throws IOException
    {
	float[] floatArray = new float[length];
	for(int idx=0; idx<length; ++idx) {
	    floatArray[idx] = readFloat();
	}
	return floatArray;
    }

    /**
     * Reads an array of 64-bit floating point numbers from the buffer.
     *
     * @param length the size of the array to read
     * @return the array
     */
    public double[] readDoubleArray(int length) throws IOException
    {
	double[] doubleArray = new double[length];
	for(int idx=0; idx<length; ++idx) {
	    doubleArray[idx] = readDouble();
	}
	return doubleArray;
    }

    /**
     * Reads an array of bytes from the buffer.
     *
     * @param length the number of bytes to read
     * @return array of bytes read
     */
    public byte[] readBytes(int length) throws IOException
    {
	byte[] bytes = new byte[length];
	try {
	    inStream.readFully(bytes);
	} catch (IOException ioe) {
	    throw ioe;
	}
	return bytes;
    }
}
