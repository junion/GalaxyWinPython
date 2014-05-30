/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.util;

import java.util.LinkedList;

/**
 * A first in, first out (FIFO) mutex. This is a reentrant mutex, meaning
 * the same thread can acquire the lock multiple times. However, you must
 * make sure to release the lock the same number of times or else other
 * threads will be unable to acquire the lock.
 */
public class FifoMutex
{
    private LinkedList queue;
    private boolean locked = false; 
    private Thread owner = null;
    private int lockCount = 0;

    public FifoMutex() 
    {
	queue = new LinkedList();
    }

    /**
     * Locks the mutex. This is a blocking operation.
     */
    public void lock()
    {
	WaitNode node = null;

	Thread thread = Thread.currentThread();

	synchronized(this) {
	    if(!locked || thread.equals(owner)) {
		locked = true;
		owner = thread;
		++lockCount;
		return;
	    } else {
		node = new WaitNode(thread);
		queue.addLast(node);
	    }
	}

	try {
	    node.doWait();
	} catch(InterruptedException iex) {
	}

	owner = thread;
    }

    /**
     * Unlocks the mutex. This method must be called once for each time the 
     * thread requested the lock (via the <code>lock</code> method). A
     * thread will not release the lock until this is done.
     */
    public synchronized void unlock()
    {
	WaitNode node = null;

	Thread thread = Thread.currentThread();

	if(!thread.equals(owner))
	    return;

	if(lockCount > 0) {
	    --lockCount;

	    // Current owner locked mutex more than once, so it still owns it.
	    if(lockCount > 0)
		return;

	    if(!queue.isEmpty())
		node = (WaitNode) queue.removeFirst();

	    if(node == null) {
		// No one waiting for the lock so mark it as such.
		locked = false;
		owner = null;
		lockCount = 0;
		return;
	    } else if(node.doNotify()) {
		// Mutex "locked" flag remains "true" since the node that
		// is awakened will now own the mutex.
		owner = node.getThread();
		++lockCount;
		return;
	    }
	} else 
	    return;
    }

    private class WaitNode
    {
	private boolean released = false;
	private Thread thread = null;

	WaitNode(Thread thread)
	{
	    this.thread = thread;
	}

	Thread getThread()
	{
	    return thread;
	}

	synchronized void doWait() throws InterruptedException
	{
	    try {
		while(!released)
		    wait();
	    } catch(InterruptedException iex) {
		if(!released) {
		    released = true;
		    throw iex;
		} else {
		    Thread.currentThread().interrupt();
		}
	    }
	}

	synchronized boolean doNotify()
	{
	    if(released)
		return false;

	    released = true;
	    notify();
	    return true;
	}
    }
}
