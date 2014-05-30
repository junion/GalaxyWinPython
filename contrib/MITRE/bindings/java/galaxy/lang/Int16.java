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
 * This class encapsulates an array of 16-bit integers.
 */
public class Int16 extends GalaxyObject implements ArrayObject
{
    /** The array of shorts encapsulated by this object. */
    private short[] shortArray;

    /**
     * Creates an empty array of shorts.
     */
    public Int16() 
    {
	shortArray = new short[0];
	type = GAL_INT_16;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * an array of shorts. If the array is null, an empty array is created.
     *
     * @param shortArray the array
     */
    public Int16(short[] shortArray)
    {
	if(shortArray == null)
	    this.shortArray = new short[0];
	else
	    this.shortArray = shortArray;
	
	type = GAL_INT_16;
    } 

    /**
     * This constructor creates an object that encapsulates the reference to
     * an array of shorts. If the array is null, an empty array is created. 
     *
     * @param shortArray the array
     * @param makeCopy if true, a copy of the array is stored. Otherwise,
     *                 the reference to the array is stored.
     * @throws RuntimeException If there is an error while copying the array.
     */
    public Int16(short[] shortArray, boolean makeCopy) throws RuntimeException
    {
	if(shortArray == null)
	    this.shortArray = new short[0];
	else if(makeCopy) {
	    this.shortArray = new short[shortArray.length];
	    System.arraycopy(shortArray, 0, this.shortArray, 0, shortArray.length);
	} else
	    this.shortArray = shortArray;

	type = GAL_INT_16;
    }

    /**
     * Returns the length of this object's array of shorts.
     *
     * @return length of array
     */
    public int getSize()
    { 
	return shortArray.length;
    }

    /**
     * Appends an array of shorts to this object's array.
     *
     * @param shortArray the array to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(short[] shortArray) throws RuntimeException
    {
	if(shortArray != null) {
	    int length = this.shortArray.length + shortArray.length;
	    short[] temp = new short[length];
	    System.arraycopy(this.shortArray, 0, temp, 0, this.shortArray.length);
	    System.arraycopy(shortArray, 0, temp, this.shortArray.length, shortArray.length);
	    this.shortArray = temp;
	}
    }

    /**
     * Appends the array of the specified <code>Int16</code> object
     * to this object's array.
     *
     * @param int16 the <code>Int16</code> to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(Int16 int16) throws RuntimeException
    {
	append(int16.getShortArray());
    }

    /**
     * Returns reference to this object's array of shorts.
     *
     * @return reference to the array
     */
    public short[] getShortArray()
    {
	return shortArray;
    }

    /**
     * Returns this object's array as a byte array (in big endian byte order).
     *
     * @return the byte array
     */
    public byte[] getBytes()
    {
	// The byte array needs to be twice as long as the short array,
	// which contains 16-bit (i.e., 2 bytes) values.
	byte bytes[] = new byte[2*shortArray.length];
	
	short s = 0;
	for(int idx=0; idx<shortArray.length; ++idx) {
	    s = shortArray[idx];
	    bytes[idx*2] = (byte) ((s & 0x0000FF00) >>> 8);
	    bytes[idx*2 + 1] = (byte) (s & 0x000000FF);
	}
	return bytes;
    }
}

