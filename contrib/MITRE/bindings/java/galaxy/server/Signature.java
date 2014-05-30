/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Id: Signature.java,v 1.8 2002/04/17 15:46:24 wohlever Exp $
 */

package galaxy.server;

import java.util.Enumeration;

import galaxy.lang.GFrame;
import galaxy.lang.GVector;
import galaxy.lang.Symbol;
import galaxy.lang.GalaxyObject;

public class Signature {

    /**
     * Existence. Used to define the existence of a certain key within a signature
     * <ul>
     * <li>GAL_KEY_ALWAYS - key must always be present in message
     * <li>GAL_KEY_SOMETIMES - key is optional
     * </ul>
     */
    public final static int GAL_KEY_ALWAYS = 0;
    public final static int GAL_KEY_SOMETIMES = 1;
    /**
     * Strictness
     * <ul>
     * <li>GAL_OTHER_KEYS_MAYBE - other keys may be present in message
     * <li>GAL_OTHER_KEYS_NEVER - no undeclared keys are allowed in message
     * </ul>
     */
    public final static int GAL_OTHER_KEYS_MAYBE = 2;
    public final static int GAL_OTHER_KEYS_NEVER = 3;
    /**
     * Return type
     * Used by the hub to check program file correctness (at run time) as well as by server libraries to check for consistent behavior. Possible types are:
     * <ul>
     * <li>GAL_REPLY_PROVIDED - this function always returns something
     * <li>GAL_REPLY_NONE - this function does not return
     * <li>GAL_REPLY_UNKNOWN - the return status of this function is unknown
     * </ul>
     */
    public final static int GAL_REPLY_PROVIDED = 4;
    public final static int GAL_REPLY_NONE = 5;
    public final static int GAL_REPLY_UNKNOWN = 6;

    String name;
    SigEntry[] inkeys;
    int instrict;
    SigEntry[] outkeys;
    int outstrict;
    int reply;
  
    /**
     * Creates a new signature for operation <b>name</b>
     * @see SigEntry
     */
    public Signature(String name, SigEntry[] inkeys, int instrict, SigEntry[] outkeys, int outstrict, int reply) {
	this.name = name;
	this.inkeys = inkeys;
	this.instrict = instrict;
	this.outkeys = outkeys;
	this.outstrict = outstrict;
	this.reply = reply;
    }

    /**
     * @param name Name of the operation for which this is a signature
     * <P>
     * Creates a signature with the following defaults:
     * <ul><li> inkeys = null
     * <li> instrict = GAL_OTHER_KEYS_MAYBE
     * <li> outkeys = null
     * <li> outstrict = GAL_OTHER_KEYS_MAYBE
     * <li> reply = GAL_REPLY_UNKNOWN
     * </ul>
     */
    public Signature(String name) {
	this.name = name;
	inkeys = null;
	instrict = GAL_OTHER_KEYS_MAYBE;
	outkeys = null;
	outstrict = GAL_OTHER_KEYS_MAYBE;
	reply = GAL_REPLY_UNKNOWN;
    }
    
    /**
     * @return the name of the operation this Signature refers to
     */
    public String getName() {
	return name;
    }

    private String[] exclusions = {GFrame.GAL_HUB_OPAQUE_DATA_FRAME_KEY,
				   GFrame.GAL_SERVER_TOKEN_INDEX_FRAME_KEY,
				   GFrame.GAL_SESSION_ID_FRAME_KEY};

    /**
     * Validates this frame according to this signature.
     * @param frames Incoming message to be validated
     * @param server Server object, used to pass loging information back
     * <p>
     * Keys excluded from validation:
     * <ul><li>:tidx
     * <li>:hub_opaque_data
     * <li>:server_id
     * <li>:session_id
     * <li>:reply_requested
     * </ul>
     */
    public boolean validateInput(GFrame frame, Server server) 
    {
	return validate(frame, inkeys, instrict, "input", exclusions, server);
    }
    
    /**
     * Validates this frame according to this signature
     * <p>
     * Keys excluded from validation:
     * <ul><li>:tidx
     * <li>:hub_opaque_data
     * <li>:server_id
     * <li>:session_id
     * <li>:reply_requested
     * </ul>
     * @see #validateInput
     */
    public boolean validateOutput(GFrame frame, Server server) 
    {
	final String event = new String("reply");

	switch(reply) {
	case GAL_REPLY_PROVIDED:
	    if(frame == null) {
		server.logWarningMessage("For method " + name + ": expected frame reply but found NULL.", "Signature.validateOutput(GFrame, Server)");
		return false;
	    }
	    break;
	case GAL_REPLY_NONE:
	    if (frame != null) {
		server.logWarningMessage("For method " + name + ": expected NULL reply but found frame.", "Signature.validateOutput(GFrame, Server)");
		return false;
	    }
	    break;
	case GAL_REPLY_UNKNOWN:
	    break;
	default:
	    server.logWarningMessage("For method " + name + ": unknown reply type specified.", "Signature.validateOutput(GFrame, Server)");
	    return false;
	}
	return this.validate(frame, outkeys, outstrict, event, exclusions, server);
    }
  
    private boolean existsInArray(String key, String[] array) {

	if (array != null) {
	    for(int i=0;i < array.length ;i++) 
		if(key.equals(array[i]))
		    return true;
	}
	return false;
    }

    private boolean validate(GFrame frame, SigEntry[] keys, int strictness, String event, String[] exclusions, Server server) 
    {
	boolean result = true;


	// If there are no keys, don't bother
	if (keys != null) {
	    String[] inkeys = new String[keys.length];
	    for(int i=0;i < keys.length ; i++) {
		Object obj = frame.getProperty(keys[i].name);
		inkeys[i] = keys[i].name;
		// If there is an object and it has the wrong type
		if (obj != null) {
		    int type = GalaxyObject.getTypeForObject(obj);
		    if ( type != keys[i].type) {
			server.logWarningMessage("For method " + name + ": " + event + " key " + keys[i].name + " should be a " + GalaxyObject.nameType(keys[i].type) + ", but is a " + GalaxyObject.nameType(type) + ".", "Signature.validate(GFrame, SigEntry[], int, String, String[], Server)");
			result = false;
		    }
		} else if (keys[i].existence == GAL_KEY_ALWAYS) {
		    // If this key should have existed
		    server.logWarningMessage("For method " + name + ": obligatory " + event + " key " + keys[i].name + " not found.", "Signature.validate(GFrame, SigEntry[], int, String, String[], Server)");
		    result = false;
		}
	    }

	    if (strictness == GAL_OTHER_KEYS_NEVER) {
		for(Enumeration e = frame.propertyKeys(); e.hasMoreElements();) {
		    String key = ((Symbol)e.nextElement()).getString();
		    if(!existsInArray(key,exclusions) && !existsInArray(key,inkeys)) {
			server.logWarningMessage("For method " + name + ": " + event + " key " + key + " not permitted.", "Signature.validate(GFrame, SigEntry[], int, String, String[], Server)");
			result=false;
		    }
		}
	    }
	}
	return result;
    }

    private static GVector keyArrayToVector(SigEntry[] keys) {
	GVector v;
	if (keys != null) {
	    v= new GVector(keys.length);
	    for(int i=0; i < keys.length; i++)
		v.insertElementAt(keys[i].toGVector(),i);
	} else {
	    v = new GVector();
	}
	return v;
    }

    /**
     * Converts this Signature into a GVector. Also know as List
     */
    public GVector toGVector() {
	GVector v = new GVector(6);
	v.addElement(name);
	v.addElement(keyArrayToVector(inkeys));
	v.addElement(new Integer(instrict));
	v.addElement(new Integer(reply));
	v.addElement(keyArrayToVector(outkeys));
	v.addElement(new Integer(outstrict));
	return v;
    }
}
