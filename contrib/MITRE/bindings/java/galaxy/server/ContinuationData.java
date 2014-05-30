/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

/**
 * This class encapsulates continuation data.
 */
public class ContinuationData
{
    /** The call environment to be used by the continuation. */
    private Environment env;

    /** The continuation object. */
    private Continuation continuation;

    /** The state data to be used by the continuation. */
    private Object continuationState;

    /** The server token index associated with the continuation. */
    private int serverTidx = -1;

    /**
     * Constructor.
     *
     * @param env the call environment
     * @param continuation the continuation object
     * @param continuationState the continuation state
     * @param serverTidx the server token index
     */
    public ContinuationData(Environment env, Continuation continuation,
			    Object continuationState, int serverTidx)
    {
	this.env = env;
	this.continuation = continuation;
	this.continuationState = continuationState;
	this.serverTidx = serverTidx;
    }

    public Environment getEnvironment()
    { return env; }

    public Continuation getContinuation()
    { return continuation; }
    
    public Object getContinuationState()
    { return continuationState; }

    public int getServerTidx()
    { return serverTidx; }
}
