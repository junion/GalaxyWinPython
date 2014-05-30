/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */

package galaxy.lang;

import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;
import java.io.DataInputStream;
import java.io.ByteArrayInputStream;

import galaxy.io.XdrInBuffer;
import galaxy.util.UUHandler;
import galaxy.util.Logger;

/**
 * This class encapsulates an array of bytes.
 */
public class GBinary extends GalaxyObject implements ArrayObject
{
    /** The array of bytes encapsulated by this object. */
    private byte[] byteArray;

    /**
     * Creates an empty array of bytes.
     */
    public GBinary() 
    {
	byteArray = new byte[0];
	type = GAL_BINARY;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * array of bytes. If the array is null, an empty array is created.
     *
     * @param byteArray the array
     */
    public GBinary(byte[] byteArray) 
    {
	if(byteArray == null)
	    this.byteArray = new byte[0];
	else
	    this.byteArray = byteArray;

	type = GAL_BINARY;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * array of bytes. If the array is null, an empty array is created.
     *
     * @param byteArray the array
     * @param makeCopy if true, a copy of the array is stored. Otherwise,
     *                 the reference to the array is stored. 
     * @throws RuntimeException If there is an error while copying the array.
     */
    public GBinary(byte[] byteArray, boolean makeCopy) throws RuntimeException
    {
	if(byteArray == null)
	    this.byteArray = new byte[0];
	else if(makeCopy) {
	    this.byteArray = new byte[byteArray.length];
	    System.arraycopy(byteArray, 0, this.byteArray, 0, byteArray.length);
	} else
	    this.byteArray = byteArray;

	type = GAL_BINARY;
    }

    /**
     * Returns the length of this object's array of bytes.
     *
     * @return length of array
     */
    public int getSize() 
    {
	return byteArray.length;
    }

    /**
     * Appends an array of bytes to this object's array.
     *
     * @param byteArray the array to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(byte[] byteArray) throws RuntimeException
    {
	if(byteArray != null) {
	    int length = this.byteArray.length + byteArray.length;
	    byte[] temp = new byte[length];
	    System.arraycopy(this.byteArray, 0, temp, 0, this.byteArray.length);
	    System.arraycopy(byteArray, 0, temp, this.byteArray.length, byteArray.length);
	    this.byteArray = temp;
	}
    }

    /**
     * Appends the array of the specified <code>GBinary</code> object
     * to this object's array.
     *
     * @param gbinary the <code>GBinary</code> to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(GBinary gbinary) throws RuntimeException
    {
	append(gbinary.getBytes());
    }

    /**
     * Returns reference to this object's array of bytes.
     *
     * @return reference to the array
     */
    public byte[] getBytes() 
    {
	return byteArray;
    }

    public String toString()
    {
	return new String(byteArray);
    }
	
    public String toEncodedString()
    {
	String encoded = UUHandler.encode(byteArray);
	return "% " + byteArray.length + " " + encoded.length() + " " + encoded;
    }

    public String toFormattedString()
    {
	return "% " + byteArray.length + " " + getTypeName() + " object";
    }
    
    /** 
     * Returns the byte array as an array of floats.
     *
     * @return the array of floats or null if the is an error
     */
    public float[] toFloatArray()
    {
	XdrInBuffer buffer = new XdrInBuffer(byteArray);
	
	try {
	    int size = getSize() / 4; // 4 bytes for each 32-bit float
	    float floatArray[] = new float[size];
	    for(int idx=0; idx<size; ++idx) {
		floatArray[idx] = buffer.readFloat();
	    }
	    return floatArray;
	} catch(IOException ioex) {
	    Logger.getLogger().logErrorMessage("Caught exception while converting byte array to float array: " + ioex.toString(), ioex, "GBinary.toFloatArray()");
	}
	return null;	
    }

    /** 
     * Returns the byte array as an array of floats.
     *
     * @return the array of floats or null if the is an error
     */
    public double[] toDoubleArray() 
    {	
	XdrInBuffer buffer = new XdrInBuffer(byteArray);
	
	try {
	    int size = getSize() / 8; // 8 bytes for each 64-bit float
	    double doubleArray[] = new double[size];
	    for(int idx=0; idx<size; ++idx) {
		doubleArray[idx] = buffer.readDouble();
	    }
	    return doubleArray;
	} catch(IOException ioex) {
	    Logger.getLogger().logErrorMessage("Caught exception while converting byte array to double array" + ioex.toString(), ioex, "GBinary.toDoubleArray()");
	}
	return null;	
    }
    
    /** 
     * Returns the byte array as an array of shorts.
     *
     * @return the array of shorts or null if the is an error
     */
    public short[] toShortArray() 
    {
	// This method's implementation does not use XdrBuffer since the 
	// buffer does not support reading shorts.	    
	DataInputStream inStream = new DataInputStream(new ByteArrayInputStream(byteArray));
	try {
	    int size = getSize() / 2; // 2 bytes for each 16-bit integer
	    short[] shortArray = new short[size];
	    for(int idx=0; idx<size; idx++) {
		shortArray[idx] = inStream.readShort();
	    }
	    return shortArray;
	} catch (IOException ioex) {
	    Logger.getLogger().logErrorMessage("Caught exception while converting byte array to short array: " + ioex.toString(), ioex, "GBinary.toShortArray()");
	}
	return null;
    }

    /** 
     * Returns the byte array as an array of integers.
     *
     * @return the array of integers or null if the is an error
     */
    public int[] toIntArray() 
    {	
	XdrInBuffer buffer = new XdrInBuffer(byteArray);

	try {
	    int size = getSize() / 4; // 4 bytes for each 32-bit integer
	    int intArray[] = new int[size];
	    for(int idx=0; idx<size; ++idx) {
		intArray[idx] = buffer.readInteger();
	    }
	    return intArray;
	} catch(IOException ioex) {
	    Logger.getLogger().logErrorMessage("Caught exception while converting byte array to int array: " + ioex.toString(), ioex, "GBinary.toIntArray()");
	}
	return null;
    }

    /** 
     * Returns the byte array as an array of longs.
     *
     * @return the array of longs or null if the is an error
     */
    public long[] toLongArray() 
    {
	XdrInBuffer buffer = new XdrInBuffer(byteArray);

	try {
	    int size = getSize() / 8; // 8 bytes for each 64-bit integer
	    long longArray[] = new long[size];
	    for(int idx=0; idx<size; ++idx) {
		longArray[idx] = buffer.readLong();
	    }
	    return longArray;
	} catch(IOException ioex) {
	    Logger.getLogger().logErrorMessage("Caught exception while converting byte array to long array: " + ioex.toString(), ioex, "GBinary.toLongArray()");
	}
	return null;
    }
    
    /**
     * Converts a binary file into an array of bytes.
     *
     * @param file the name of the binary file to convert
     * @return the contents of the file in a byte array or null if there was
     *         an error
     */
    public static byte[] readBinaryFile(String file) throws IOException 
    {
	if (file == null || file.equals("")) {
	    return null;
	}
	
	File f = new File(file);
	FileInputStream in = new FileInputStream(f);
	
	long flength = f.length();
	
	if (flength > Integer.MAX_VALUE) {
	    throw new IOException("File exceeds maximum file size (Max file size is " + Integer.MAX_VALUE + ")");
	    
	} else if (flength < 0) {
	    return null;
	}
	
	byte[] bytes = new byte[(int)flength];
	
	//  Read in file... 
	in.read(bytes);
	in.close();

	return bytes;
    }
}
