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
 * This class encapsulates an array of 64-bit floating point numbers.
 */
public class Float64 extends GalaxyObject implements ArrayObject
{ 
    /** The array of doubles encapsulated by this object. */
    private double[] doubleArray;

    /**
     * Creates an empty array of doubles.
     */
    public Float64() 
    {
	doubleArray = new double[0];
	type = GAL_FLOAT_64;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * an array of doubles. If the array is null, an empty array is created.
     *
     * @param doubleArray the array
     */
    public Float64(double[] doubleArray)
    {
	if(doubleArray == null)
	    this.doubleArray = new double[0];
	else
	    this.doubleArray = doubleArray;

	type = GAL_FLOAT_64;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * an array of doubles. If the array is null, an empty array is created.
     *
     * @param doubleArray the array
     * @param makeCopy if true, a copy of the array is stored. Otherwise,
     *                 the reference to the array is stored.
     * @throws RuntimeException If there is an error while copying the array.
     */
    public Float64(double[] doubleArray, boolean makeCopy) throws RuntimeException
    {
	if(doubleArray == null)
	    this.doubleArray = new double[0];
	else if(makeCopy) {
	    this.doubleArray = new double[doubleArray.length];
	    System.arraycopy(doubleArray, 0, this.doubleArray, 0, doubleArray.length);
	} else
	    this.doubleArray = doubleArray;

	type = GAL_FLOAT_64;
    }

    /**
     * Returns the length of this object's array of doubles.
     *
     * @return length of array
     */
    public int getSize()
    { 
	return doubleArray.length;
    }

    /**
     * Appends an array of doubles to this object's array.
     *
     * @param doubleArray the array to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(double[] doubleArray) throws RuntimeException
    {
	if(doubleArray != null) {
	    int length = this.doubleArray.length + doubleArray.length;
	    double[] temp = new double[length];
	    System.arraycopy(this.doubleArray, 0, temp, 0, this.doubleArray.length);
	    System.arraycopy(doubleArray, 0, temp, this.doubleArray.length, doubleArray.length);
	    this.doubleArray = temp;
	}
    }

    /**
     * Appends the array of the specified <code>Float64</code> object
     * to this object's array.
     *
     * @param float64 the <code>Float64</code> to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(Float64 float64) throws RuntimeException
    {
	append(float64.getDoubleArray());
    }

    /**
     * Returns reference to this object's array of doubles.
     *
     * @return reference to the array
     */
    public double[] getDoubleArray()
    {
	return doubleArray;
    }

    /**
     * Returns this object's array as a byte array (in big endian byte order).
     *
     * @return the byte array
     */
    public byte[] getBytes()
    {
	// The byte array needs to be eight times as long as the double 
	// array, which contains 64-bit (i.e., 8 bytes) values.
	byte bytes[] = new byte[8*doubleArray.length];
	
	long l = 0;
	for(int idx=0; idx<doubleArray.length; ++idx) {
	    
	    // Convert the double into long (also 64-bits) since the 
	    // bitwise "&" operation cannot be applied to floating point
	    // numbers.
	    
	    // Process the high order 32-bits first.
	    l = Double.doubleToLongBits(doubleArray[idx]) >>> 32;
	    bytes[idx*8] = (byte) ((l & 0xFF000000) >>> 24);
	    bytes[idx*8 + 1] = (byte) ((l & 0x00FF0000) >>> 16);
	    bytes[idx*8 + 2] = (byte) ((l & 0x0000FF00) >>> 8);
	    bytes[idx*8 + 3] = (byte) (l & 0x000000FF);
	    
	    // Process the low order 32-bits.
	    l = Double.doubleToLongBits(doubleArray[idx]);
	    bytes[idx*8 + 4] = (byte) ((l & 0xFF000000) >>> 24);
	    bytes[idx*8 + 5] = (byte) ((l & 0x00FF0000) >>> 16);
	    bytes[idx*8 + 6] = (byte) ((l & 0x0000FF00) >>> 8);
	    bytes[idx*8 + 7] = (byte) (l & 0x000000FF);
	}
	return bytes;
    }
}
