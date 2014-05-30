/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.lang;

/**
 * This is the interface for array objects.
 */
public interface ArrayObject
{
    /**
     * Returns the size of the array.
     *
     * @return the array size
     */
    public int getSize();

    /**
     * Returns the contents of the array as an array of bytes. It is left
     * up to the classes that implement this interface to define the policy
     * by which the native data type of their arrays is converted to byte
     * sequences.
     *
     * @return the array of bytes
     */
    public byte[] getBytes();
}
