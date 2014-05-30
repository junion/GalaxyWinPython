/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

import galaxy.lang.GFrame;

/**
 * This is the interface of the object that resumes the processing of a
 * suspended dispatch function.
 */
public interface Continuation
{
    /**
     * This is the method that is run when the processing of a dispatch 
     * function is resumed.
     *
     * @param frame the <code>GFrame</code> to process
     * @param msgType the message type of the message that contained the frame
     * @param continuationState the state data to be used in the continuation
     * @param env the call environment in which the continuation is to be
     *            processed
     * @return the result of the continued dispatch function
     */
    public GFrame run(GFrame frame, int msgType, Object continuationState,
		      Environment env);
}
