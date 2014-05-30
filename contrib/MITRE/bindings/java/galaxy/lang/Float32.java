/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: Float32.java,v 1.14 2002/03/14 19:04:06 wohlever Exp $
 */

/** 
 * These Java bindings were originally produced by Intel Corp.,
 * which has granted permission to the Communicator program to
 * use and modify them. The preceding MITRE copyright refers to
 * whatever changes the MITRE Corporation has made to the code. 
 */

package galaxy.lang;

/**
 * This class encapsulates an array of 32-bit floating point numbers.
 */
public class Float32 extends GalaxyObject implements ArrayObject
{
    /** The array of floats encapsulated by this object. */
    private float[] floatArray;

    /**
     * Creates an empty array of floats.
     */
    public Float32() 
    {
	floatArray = new float[0];
	type = GAL_FLOAT_32;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * an array of floats. If the array is null, an empty array is created.
     *
     * @param floatArray the array
     */
    public Float32(float[] floatArray)
    {
	if(floatArray == null)
	    this.floatArray = new float[0];
	else
	    this.floatArray = floatArray;

	type = GAL_FLOAT_32;
    }

    /**
     * This constructor creates an object that encapsulates the reference to 
     * an array of floats. If the array is null, an empty array is created.
     *
     * @param floatArray the array
     * @param makeCopy if true, a copy of the array is stored. Otherwise,
     *                 the reference to the array is stored.
     * @throws RuntimeException If there is an error while copying the array.
     */
    public Float32(float[] floatArray, boolean makeCopy) throws RuntimeException
    {
	if(floatArray == null)
	    this.floatArray = new float[0];
	else if(makeCopy) {
	    this.floatArray = new float[floatArray.length];
	    System.arraycopy(floatArray, 0, this.floatArray, 0, floatArray.length);
	} else
	    this.floatArray = floatArray;

	type = GAL_FLOAT_32;
    }

    /**
     * Returns the length of this object's array of floats. 
     *
     * @return length of array
     */
    public int getSize()
    { 
	return floatArray.length;
    }

    /**
     * Appends an array of floats to this object's array.
     *
     * @param floatArray the array to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(float[] floatArray) throws RuntimeException
    {
	if(floatArray != null) {
	    int length = this.floatArray.length + floatArray.length;
	    float[] temp = new float[length];
	    System.arraycopy(this.floatArray, 0, temp, 0, this.floatArray.length);
	    System.arraycopy(floatArray, 0, temp, this.floatArray.length, floatArray.length);
	    this.floatArray = temp;
	}
    }

    /**
     * Appends the array of the specified <code>Float32</code> object
     * to this object's array.
     *
     * @param float32 the <code>Float32</code> to append
     * @throws RuntimeException If there is an error while appending to the 
     *                          array.
     */
    public void append(Float32 float32) throws RuntimeException
    {
	append(float32.getFloatArray());
    }

    /**
     * Returns reference to this object's array of floats.
     *
     * @return reference to the array
     */
    public float[] getFloatArray()
    {
	return floatArray;
    }

    /**
     * Returns this object's array as a byte array (in big endian byte order).
     *
     * @return the byte array
     */
    public byte[] getBytes()
    {
	// The byte array needs to be four times as long as the float 
	// array, which contains 32-bit (i.e., 4 bytes) values.
	byte bytes[] = new byte[4*floatArray.length];
	
	int i = 0;
	for(int idx=0; idx<floatArray.length; ++idx) {
	    
	    // Convert the float into int (also 32-bits) since the bitwise
	    // "&" operation cannot be applied to floating point numbers.
	    i = Float.floatToIntBits(floatArray[idx]);
	    bytes[idx*4] = (byte) ((i & 0xFF000000) >>> 24);
	    bytes[idx*4 + 1] = (byte) ((i & 0x00FF0000) >>> 16);
	    bytes[idx*4 + 2] = (byte) ((i & 0x0000FF00) >>> 8);
	    bytes[idx*4 + 3] = (byte) (i & 0x000000FF);
	}
	return bytes;
    }
}
