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

/**
 * This class encapsulates an array of 64-bit integers.
 */
public class Int64 extends GalaxyObject implements ArrayObject
{ 
    /** The array of longs encapsulated by this object. */
    private long[] longArray;

    /**
     * Creates an empty array of longs.
     */
    public Int64() 
    {
	longArray = new long[0];
	type = GAL_INT_64;
    }
    
    /**
     * This constructor creates an object that encapsulates the reference to 
     * an array of longs. If the array is null, an empty array is created.
     *
     * @param lonyArray the array
     */
    public Int64(long[] longArray)
    {
	if(longArray == null)
	    this.longArray = new long[0];
	else
	    this.longArray = longArray;

	type = GAL_INT_64;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * an array of longs. If the array is null, an empty array is created.
     *
     * @param lonyArray the array
     * @param makeCopy if true, a copy of the array is stored. Otherwise,
     *                 the reference to the array is stored. 
     * @throws RuntimeException If there is an error while copying the array.
     */
    public Int64(long[] longArray, boolean makeCopy) throws RuntimeException
    {
	if(longArray == null)
	    this.longArray = new long[0];
	else if(makeCopy) {
	    this.longArray = new long[longArray.length];
	    System.arraycopy(longArray, 0, this.longArray, 0, longArray.length);
	} else
	    this.longArray = longArray;

	type = GAL_INT_64;
    }

    /**
     * Returns the length of this object's array of longs.
     *
     * @return length of array
     */
    public int getSize()
    { 
	return longArray.length;
    } 

    /**
     * Appends an array of longs to this object's array.
     *
     * @param longArray the array to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(long[] longArray) throws RuntimeException
    {
	if(longArray != null) {
	    int length = this.longArray.length + longArray.length;
	    long[] temp = new long[length];
	    System.arraycopy(this.longArray, 0, temp, 0, this.longArray.length);
	    System.arraycopy(longArray, 0, temp, this.longArray.length, longArray.length);
	    this.longArray = temp;
	}
    }

    /**
     * Appends the array of the specified <code>Int64</code> object
     * to this object's array.
     *
     * @param int64 the <code>Int64</code> to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(Int64 int64) throws RuntimeException
    {
	append(int64.getLongArray());
    }

    /**
     * Returns reference to this object's array of longs.
     *
     * @return reference to the array
     */
    public long[] getLongArray()
    {
	return longArray;
    }

    /**
     * Returns this object's array as a byte array (in big endian byte order).
     *
     * @return the byte array
     */
    public byte[] getBytes()
    {
	// The byte array needs to be eight times as long as the long 
	// array, which contains 64-bit (i.e., 8 bytes) values.
	byte bytes[] = new byte[8*longArray.length];
	
	long l = 0;
	for(int idx=0; idx<longArray.length; ++idx) {
	    
	    // Process the high order 32-bits first.
	    l = longArray[idx] >>> 32;
	    bytes[idx*8] = (byte) ((l & 0xFF000000) >>> 24);
	    bytes[idx*8 + 1] = (byte) ((l & 0x00FF0000) >>> 16);
	    bytes[idx*8 + 2] = (byte) ((l & 0x0000FF00) >>> 8);
	    bytes[idx*8 + 3] = (byte) (l & 0x000000FF);
	    
	    // Process the low order 32-bits.
	    l = longArray[idx];
	    bytes[idx*8 + 4] = (byte) ((l & 0xFF000000) >>> 24);
	    bytes[idx*8 + 5] = (byte) ((l & 0x00FF0000) >>> 16);
	    bytes[idx*8 + 6] = (byte) ((l & 0x0000FF00) >>> 8);
	    bytes[idx*8 + 7] = (byte) (l & 0x000000FF);
	}
	return bytes;
    }
}
