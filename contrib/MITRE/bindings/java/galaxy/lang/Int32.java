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
 * This class encapsulates an array of 32-bit integers.
 */
public class Int32 extends GalaxyObject implements ArrayObject
{
    /** The array of ints encapsulated by this object. */
    private int[] intArray;

    /**
     * Creates an empty array of ints.
     */
    public Int32() 
    {
	intArray = new int[0];
	type = GAL_INT_32;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * an array of ints. If the array is null, an empty array is created.
     *
     * @param intArray the array
     */
    public Int32(int[] intArray) 
    {
	if(intArray == null)
	    this.intArray = new int[0];
	else
	    this.intArray = intArray;

	type = GAL_INT_32;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * an array of ints. If the array is null, an empty array is created.
     *
     * @param intArray the array
     * @param makeCopy if true, a copy of the array is stored. Otherwise,
     *                 the reference to the array is stored.
     * @throws RuntimeException If there is an error while copying the array.
     */
    public Int32(int[] intArray, boolean makeCopy) throws RuntimeException
    {
	if(intArray == null)
	    this.intArray = new int[0];
	else if(makeCopy) {
	    this.intArray = new int[intArray.length];
	    System.arraycopy(intArray, 0, this.intArray, 0, intArray.length);
	} else
	    this.intArray = intArray;

	type = GAL_INT_32;
    }

    /**
     * Returns the length of this object's array of ints.
     *
     * @return length of array
     */
    public int getSize()
    { 
	return intArray.length;
    }

    /**
     * Appends an array of ints to this object's array.
     *
     * @param intArray the array to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(int[] intArray) throws RuntimeException
    {
	if(intArray != null) {
	    int length = this.intArray.length + intArray.length;
	    int[] temp = new int[length];
	    System.arraycopy(this.intArray, 0, temp, 0, this.intArray.length);
	    System.arraycopy(intArray, 0, temp, this.intArray.length, intArray.length);
	    this.intArray = temp;
	}
    }

    /**
     * Appends the array of the specified <code>Int32</code> object
     * to this object's array.
     *
     * @param int32 the <code>Int32</code> to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(Int32 int32) throws RuntimeException
    {
	append(int32.getIntArray());
    }

    /**
     * Returns reference to this object's array of ints.
     *
     * @return reference to the array
     */
    public int[] getIntArray()
    {
	return intArray;
    }

    /**
     * Returns this object's array as a byte array (in big endian byte order).
     *
     * @return the byte array
     */
    public byte[] getBytes()
    {
	// The byte array needs to be four times as long as the int array,
	// which contains 32-bit (i.e., 4 bytes) values.
	byte bytes[] = new byte[4*intArray.length];
	
	int i = 0;
	for(int idx=0; idx<intArray.length; ++idx) {
	    i = intArray[idx];
	    bytes[idx*4] = (byte) ((i & 0xFF000000) >>> 24);
	    bytes[idx*4 + 1] = (byte) ((i & 0x00FF0000) >>> 16);
	    bytes[idx*4 + 2] = (byte) ((i & 0x0000FF00) >>> 8);
	    bytes[idx*4 + 3] = (byte) (i & 0x000000FF);
	}
	return bytes;
    }
}
