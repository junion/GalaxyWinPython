/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/**
 * $Header: /afs/rcf/project/ai-contrib/src/cvs-master/GalaxyCommunicator2/contrib/MITRE/bindings/java/galaxy/lang/GFrame.java,v 1.21 2002/04/17 15:48:07 wohlever Exp $
 */

/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */

package galaxy.lang;

import java.util.Hashtable;
import java.util.Enumeration;

import java.io.StringReader;

import java.lang.reflect.Constructor;

import galaxy.io.XdrOutBuffer;
import galaxy.io.XdrInBuffer;
import galaxy.server.MainServer;
import galaxy.util.Logger;

/**
 * The meaning of a sentence in the Galaxy system is represented by a frame.
 * These frames are used to communicate among the various processes comprising
 * a Galaxy system.  They represent discourse history, drive dialog, and
 * represent computer responses.   As a data structure, a frame is not unlike
 * a hash table; it contains key/value pairs.   In each domain, conventions
 * must be established to represent meaning appropriately.   It would be ideal
 * if there were one set of conventions used by all domains.  As each new
 * domain is explored, conventions are improved. 
 *
 * There are several competing factors influencing frame design.  These factors
 * represent the various uses a frame will be put to.  
 * <UL>
 * <LI>You can think of a frame as a command to the domain backend.  Thus in
 * the weather domain, a frame must define weather queries for the backend to
 * execute.   However, as you will see a frame is not simply a command.
 * <LI>From another point of view a frame is a structure designed to be easily
 * converted to natural language.  The frame design must account for conversion
 * to many languages: English, French, Spanish, and Mandarin for example.  Some
 * factors to consider are person, number, case and inflection.  The range of
 * things a Galaxy system can say are fairly constrained and predictable.  The
 * backend will ignore much of this information.
 * <LI>It must be possible to build a frame as a sentence is parsed.  Given the
 * wide latitude speakers have in phrasing their utterances, the frame design 
 * must be very flexible.  At the same time, two sentences having the same 
 * meaning, should be represented by the same frame.
 * <LI>And finally the more mundane constraints imposed by the Galaxy system
 * itself.  The Hub examines a frame, directing it to the
 * appropriate server.   
 * </UL>
 *<P>
 * A frame has a name, a property list, and a set of predicates.
 * <P>
 * The property list is a set of key/value pairs.
 * <P>
 * The base class for the various semantic frames Clause, QSet, and Predicate.
 * <P>
 * toString() will produce a pretty printed version of the frame.
 * <P>
 * An example frame.
 * <PRE>
 *  {c when 
 *      :aux "link" 
 *      :topic {q title 
 *                :name "deep space nine" 
 *                :pred {p on_set }
 *      :domain "Movies" }
 * </PRE>
 * <P>
 * Values may be: 
 * <ul>
 * <table>
 * <tr><td>GFrame       <td>A Galaxy frame
 * <tr><td>Symbol       <td>
 * <tr><td>Integer      <td>
 * <tr><td>Float        <td>To be implemented.
 * <tr><td>String       <td>
 * <tr><td>GVector      <td>This is like <i>list</i> in the C version.
 * </table>
 * </ul>
 *
 * The C version of the frame reader requires white space around <i>every</i> 
 * token. This includes braces, and integers.  But no white space must be 
 * between the opening brace of a frame and it's type.
 *
 * @see Clause
 * @see QSet
 * @see Predicate
 * @see Symbol
 * @see GVector
 */
public class GFrame extends GalaxyObject
{
    private String typeString = "";
    private Symbol name;

    /**
     * For properties. A property's value is accessed by a name.  The name is
     * not part of the property.
     *<P>
     * (name, property)
     */
    private Hashtable properties = new Hashtable();
    
    /**
     * For predicates.  Predicates are frames with there own names. The key 
     * used in the hash table is the name of the frame itself, the value is the
     * frame.
     * <p>
     * (frame.name, frame)
     */
    private Hashtable predicates = new Hashtable();

    /* These two keys form the contents of an error message. */
    public static final String GAL_ERROR_NUMBER_FRAME_KEY = ":errno";
    public static final String GAL_ERROR_DESCRIPTION_FRAME_KEY = ":err_description";
	
    /* This is the key which indicates which session it is. */
    public static final String GAL_SESSION_ID_FRAME_KEY = ":session_id";
	
    /* This is the unique index of the token. */
    public static final String GAL_TOKEN_INDEX_FRAME_KEY = ":tidx";

    /* This is the unique index of the message sent to the Hub. */
    public static final String GAL_SERVER_TOKEN_INDEX_FRAME_KEY = ":server_tidx";
	
    /* This is the key for Hub info which is "opaque" to the server.
       The material in this frame is guaranteed to be returned untouched. */
    public static final String GAL_HUB_OPAQUE_DATA_FRAME_KEY = ":hub_opaque_data";
    
    public static final String GAL_OPAQUE_SCRIPT_MODULE_HUB_FRAME_KEY = ":hub_program_info";

    /* This is the key which reports message signatures back to the Hub. */
    public static final String GAL_SIGNATURES_FRAME_KEY = ":signatures";

    /* This is the key which contains the unique key to match for
       broker connections. */
    public static final String GAL_BROKER_CALL_ID_FRAME_KEY = ":call_id";

    /* This is the key which announces that a server wants an
       answer to its message, or that the Hub wants an answer
       to its message. */
    public static final String GAL_ROUND_TRIP_FRAME_KEY = ":reply_requested";

    /* This key stores the connection type in the handshake. */
    public static final String GAL_CONNECTION_TYPE_FRAME_KEY = ":conn_type";

    /* This key stores the server ID for the Hub. */
    public static final String GAL_SERVER_ID_HUB_FRAME_KEY = ":server_id";

    /* This key stores the current token timestamp. */
    public static final String GAL_TOKEN_TIMESTAMP_KEY = ":timestamp";

    /* This key stores a provider id. */
    public static final String GAL_PROVIDER_ID_FRAME_KEY = ":provider_id";

    // Frame types
    public static final int GAL_NULLFRAME = 0;
    public static final int GAL_TOPIC = 1;
    public static final int GAL_CLAUSE = 2;
    public static final int GAL_PRED = 3;

    // Encodings for server/session locks
    public static final int GAL_SERVER_READS_ONLY_FROM_SESSION = 1;
    public static final int GAL_SESSION_WRITES_ONLY_TO_SERVER = 2;
    public static final int GAL_SERVER_WRITES_ONLY_TO_SESSION = 4;
    public static final int GAL_PERMANENT_LOCK = 8;


    /**
     * The frame type.
     */
    private int frameType = GAL_CLAUSE;

    public GFrame(Symbol name, int frameType)
    {
	this.name = name;
	this.type = GAL_FRAME;
	setFrameType(frameType);
    }

    public void setFrameType(int frameType)
    {
	this.frameType = frameType;
	switch(frameType) {
	case GAL_TOPIC:
	    typeString = "q";
	    break;
	case GAL_CLAUSE:
	    typeString = "c";
	    break;
	case GAL_PRED:
	    typeString = "p";
	    break;
	default:
	    typeString = "";
	}
    }

    public int getFrameType()
    {
	return frameType;
    }

    public Symbol getName()
    {
	return name;
    }
    
    public void setName(Symbol name)
    {
        this.name = name;
    }

    public void setName(String name)
    {
        setName(new Symbol(name));
    }
    
    public void setPropertyVerbatim(Symbol key, Object value)
    {
        properties.put(key, value);
    }

    public void setPropertyVerbatim(String key, Object value)
    {
        properties.put(Symbol.getSymbol(key), value);
    }

    public void setProperty(Symbol key, Object value)
    {
        setPropertyVerbatim(key, value);
    }

    public void setProperty(String key, Object value)
    {
        setProperty(Symbol.getSymbol(key), value);
    }

    public void setProperty(String key, int v)
    {
        setProperty(Symbol.getSymbol(key), new Integer(v));
    }

    public void setProperty(Symbol key, int v)
    {
        setProperty(key, new Integer(v));
    }

    public void setProperty(String key, float f)
    {
        setProperty(Symbol.getSymbol(key), new Float(f));
    }

    public void setProperty(Symbol key, float f)
    {
        setProperty(key, new Float(f));
    }

    public Enumeration propertyKeys()
    {
	return properties.keys();
    }

    public boolean containsProperty(Symbol key)
    {
	return properties.containsKey(key);
    }

    public boolean containsProperty(String key)
    {
	return containsProperty(Symbol.getSymbol(key));
    }

    /* Used only here and in FrameReader */
    public Object getPropertyVerbatim(Symbol key)
    {
        return properties.get(key);
    }
    
    public Object getProperty(Symbol key)
    {
        return getPropertyVerbatim(key);
    }
    
    public Object getProperty(String key)
    {
        return getProperty(Symbol.getSymbol(key));
    }
 
    public GFrame getFrame(String key) 
    {
        return getFrame(Symbol.getSymbol(key));
    }

    public GFrame getFrame(Symbol key) 
    {
	return (GFrame) getProperty(key);
    }

    public String getString(String key)
    {
	return getString(Symbol.getSymbol(key));
    }

    public String getString(Symbol key)
    {
	return (String) getProperty(key);
    }

    public Integer getInt(String key)
    {
	return getInt(Symbol.getSymbol(key));
    }
    public Integer getInt(Symbol key)
    {
	return (Integer) getProperty(key);
    }

    public Boolean getIntAsBoolean(String key)
    {
	return getIntAsBoolean(Symbol.getSymbol(key));
    }
    public Boolean getIntAsBoolean(Symbol key)
    {
	Integer value = (Integer) getProperty(key);
	if(value == null)
	    return null;

	if(value.intValue() == 0)
	    return Boolean.FALSE;
	else
	    return Boolean.TRUE;
    }

    public Float getFloat(String key)
    {
	return getFloat(Symbol.getSymbol(key));
    }

    public Float getFloat(Symbol key)
    {
	return (Float) getProperty(key);
    }

    public GVector getList (String key)
    {
	return getList(Symbol.getSymbol(key));
    }

    public GVector getList (Symbol key)
    {
	return (GVector) getProperty(key);
    }

    public GBinary getBinary(String key)
    {
	return getBinary(Symbol.getSymbol(key));
    }

    public GBinary getBinary(Symbol key)
    {
	return (GBinary) getProperty(key);
    }

    public Int16 getInt16(String key)
    {
	return getInt16(Symbol.getSymbol(key));
    }

    public Int16 getInt16(Symbol key)
    {
	return (Int16) getProperty(key);
    }

    public Int32 getInt32(String key)
    {
	return getInt32(Symbol.getSymbol(key));
    }

    public Int32 getInt32(Symbol key)
    {
	return (Int32) getProperty(key);
    }

    public Int64 getInt64(String key)
    {
	return getInt64(Symbol.getSymbol(key));
    }

    public Int64 getInt64(Symbol key)
    {
	return (Int64) getProperty(key);
    }

    public Float32 getFloat32(String key)
    {
	return getFloat32(Symbol.getSymbol(key));
    }

    public Float32 getFloat32(Symbol key)
    {
	return (Float32) getProperty(key);
    }

    public Float64 getFloat64(String key)
    {
	return getFloat64(Symbol.getSymbol(key));
    }

    public Float64 getFloat64(Symbol key)
    {
	return (Float64) getProperty(key);
    }

    public BrokerProxy getProxy(String key)
    {
	return getProxy(Symbol.getSymbol(key));
    }

    public BrokerProxy getProxy(Symbol key)
    {
	return (BrokerProxy) getProperty(key);
    }

    /* Used only here and in FrameReader */
    public void removePropertyVerbatim(Symbol key)
    {
        properties.remove(key);
    }

    public void removeProperty(Symbol key)
    {
        removePropertyVerbatim(key);
    }
    
    public void removeProperty(String key)
    {
        properties.remove(Symbol.getSymbol(key));
    }

    public Enumeration predicateKeys() 
    {
	return predicates.keys();
    }

    public void setPredicate(Predicate p)
    {
        predicates.put(p.getName(), p);
    }

    public PredicateEnumeration predicates()
    {
        return new PredicateEnumeration(predicates.elements());
    }
    
    // This should probably be removed or generalized!! -SPC 5/8/00
    public TimePredicateEnumeration timePredicates()
    {
        return new TimePredicateEnumeration(this);
    }
    
    public Predicate getPredicate(Symbol name)
    {
        return (Predicate)predicates.get(name);
    }
    
    public void removePredicate(Symbol name)
    {
        predicates.remove(name);
    }

    public GFrame copy()
    {
        GFrame f2=null;
        
        try {
            Class argTypes[] = {galaxy.lang.Symbol.class};
            Constructor c = getClass().getConstructor(argTypes);
            Object args[] = {getName()};
            f2 = (GFrame)c.newInstance(args);
        } catch(Exception ex) {
	    Logger.getLogger().logErrorMessage("Caught exception while copying frame: " + ex.toString(), ex, "GFrame.copy()");
	    return null;
        }
        
        f2.setName(getName());
        
        Enumeration e = properties.keys();
        while(e.hasMoreElements()) {
            Symbol key = (Symbol)e.nextElement();
            Object v = properties.get(key);
            if (v instanceof GFrame)
                v = ((GFrame)v).copy();
            f2.setPropertyVerbatim(key, v);
            
        }
        
        PredicateEnumeration pe = predicates();
        while(pe.hasMorePredicates()) {
            f2.setPredicate((Predicate)pe.nextPredicate().copy());
        }
        
        return f2;
    }
    
    public QSet getTopic()
    {
        QSet topic;
        
        if (this instanceof QSet)
            return (QSet)this;
        
        else {
            topic = (QSet) getProperty(Symbol.TOPIC);
            if (topic == null) {
                topic = new QSet(Symbol.NULL);
                setProperty(Symbol.TOPIC, topic);
            }
            return topic;
        }
    }

    public void addNameToFrame(Symbol name)
    { 
        if (this instanceof QSet) {
            setProperty(Symbol.NAME, name);
        } else
	    if (this instanceof Predicate) {
		setProperty(Symbol.TOPIC, name);
	    }
    }

    public void addPredicateToFrame(Predicate predicate)
    { 
        if (this instanceof QSet)
            addPredicateToTopic(predicate);
        else 
            setPredicate(predicate);
    }

    /**
     * This checks if such a predicate already exists, and if so, it joins it 
     * with an 'and'. 
     */
    public void addPredicateToTopic(Predicate predicate)
    { 
        GFrame oldTopic;
        Object oldTopicObj;
        GFrame newTopic;
        Object newTopicObj;
        GFrame compound;
        
        Predicate entry = findPredicate(predicate.getName());
        if (entry != null) {
            oldTopicObj = entry.getProperty(Symbol.TOPIC);

            if (oldTopicObj != null) {
                newTopicObj = predicate.getProperty(Symbol.TOPIC);
             
            }

            if (oldTopicObj != null 
		|| entry.getProperty(Symbol.NAME) != null) {
                compound = (GFrame)entry.getProperty(Symbol.AND);
                if (compound != null)
		    compound.setProperty(Symbol.AND, predicate);
                else
                    entry.setProperty(Symbol.AND, predicate);
            }
	    
	    // It already exist as a predicate, I'll add any predicates I may 
	    // have to theirs.
            predicate.copyPredicates(entry);
            return;
        }

        setPredicate(predicate);
    }

    /** 
     * Finds and returns a pred in GFrame that has the same name as predicate.
     */
    public Predicate findPredicate(String name)
    {
        return findPredicate(Symbol.getSymbol(name));
    }
    
    public Predicate findPredicate(Symbol name)
    { 
        PredicateEnumeration e = predicates();
        while(e.hasMorePredicates()) {
            Predicate p = e.nextPredicate();
            if (p.getName() == name)
                return p;
        }
        return null;
    }

    public void copyPredicates(GFrame fromFrame)
    { 
        PredicateEnumeration e = fromFrame.predicates();

        while(e.hasMorePredicates()) {
            Predicate p = e.nextPredicate();
            if (p.getName() == Symbol.DEAD)
		continue;
            
	    // Only add the predicate if it isn't there already */
            if (getPredicate(p.getName()) == null) {
                setPredicate(p);
            }
        }
    }

    public void movePredicates(GFrame fromFrame)
    { 
        copyPredicates(fromFrame);
        
        for(;;) {
            PredicateEnumeration e = fromFrame.predicates();
            if (!e.hasMorePredicates()) break;
            Predicate p = e.nextPredicate();
            fromFrame.removePredicate(p.getName());
        }
    }

    public boolean isTime()
    { 
        if (!(this instanceof QSet))
            return false;
            
        return getName() == Symbol.TIME 
            || getName() == Symbol.TIME_OF_DAY;
    }

    public boolean isDate()
    {
        if (!(this instanceof QSet))
            return false;
            
        return getName() == Symbol.DATE;
    }


    public String toString() {
	return toStringBuffer(new StringBuffer()).toString();
    }

    public StringBuffer toStringBuffer(StringBuffer buff) {
	String s = new String();
	if (name !=null)
	    s = name.getString();
	buff.append("{" + typeString + " " + s);
	for(Enumeration e=properties.keys(); e.hasMoreElements();) {
            Symbol key = (Symbol)e.nextElement();
            Object value = properties.get(key);
	    s = " "+key.toString()+" ";
	    appendValue(buff.append(s), 0, value);
        }
        PredicateEnumeration pe = predicates();
        while(pe.hasMorePredicates()) {
            Predicate p = pe.nextPredicate();
	    buff.append(":pred ");
	    p.toStringBuffer(buff);
        }
        buff.append(" }");
	
	return buff;

    }

    public String toFormattedString()
    {
        return toFormattedStringBuffer(new StringBuffer(), 0).toString();
    }

    public StringBuffer toFormattedStringBuffer(StringBuffer buff, int indent)
    {
        String s;
        int indent2 = indent;

        putIndent(buff, indent);
        
        s = "{";
        ++indent2;
        
        s += typeString;
        indent2  += typeString.length();
            
        if (name != null) {
            s += " " + name.getString();
            ++indent2;
        }
        buff.append(s);
        for(Enumeration e=properties.keys(); e.hasMoreElements();) {
            buff.append('\n');
            Symbol key = (Symbol)e.nextElement();
            Object value = properties.get(key);
            putIndent(buff, indent2);
	    s = key.getString()+" ";
            buff.append(s);
            appendFormattedValue(buff, indent2+s.length(), value);
        }
        PredicateEnumeration pe = predicates();
        while(pe.hasMorePredicates()) {
            Predicate p = pe.nextPredicate();
            buff.append('\n');
            putIndent(buff, indent2);
            buff.append(":pred ");
            p.toFormattedStringBuffer(buff, indent2+6);
        }
        buff.append(" }");
        return buff;
    }

    static void putString(StringBuffer buff, String st)
    {
        putString(buff, st, 0);
    }


    public static GFrame parseFrame(String frameString) throws Exception
    {
        FrameReader fr = new FrameReader(new StringReader(frameString));
        
        return fr.getFrame();
    }

    /**
     * Encodes this frame into a XDR buffer.
     *
     * @param buffer the buffer into which this frame is encoded
     * @return true if the encoding was successful, false otherwise
     */
    public boolean encodeFrame(XdrOutBuffer buffer) throws Exception
    {	
	// Encode the object type, frame type, and frame name.
	buffer.writeInteger(GalaxyObject.GAL_FRAME);
	buffer.writeInteger(frameType);	
	buffer.writeString(name.toString());
	
	// Encode the key/value pairs.
	Enumeration keys = propertyKeys();
	buffer.writeInteger(properties.size());
	Symbol key;
	Object value;
	while(keys.hasMoreElements()) {
	    key = (Symbol) keys.nextElement();
	    buffer.writeString(key.toString());
	    value = properties.get(key);
	    if(!GFrame.encodeObject(buffer, value))
		return false;
	}
	
	// Encode the predicates.
	int numPredicates = predicates.size();
	buffer.writeInteger(numPredicates);

	PredicateEnumeration predicateEnum = predicates();
	Predicate predicate;
        while(predicateEnum.hasMorePredicates()) {
	    predicate = predicateEnum.nextPredicate();
	    if(!GFrame.encodeObject(buffer, predicate))
		return false;
        }

	return true;
    }
    
    /**
     * Encodes an object into a XDR buffer.
     *
     * @param buffer the buffer into which the is encoded
     * @param object the object to encode
     * @return true if the encoding was successful, false otherwise
     */
    public static boolean encodeObject(XdrOutBuffer buffer, Object object)
	throws Exception
    {  
	if(object instanceof GalaxyObject) {
	    int objType = ((GalaxyObject) object).getType();

	    switch(objType) {
	    case GalaxyObject.GAL_FRAME:
		if(!((GFrame) object).encodeFrame(buffer))
		    return false;
		break;
	    case GalaxyObject.GAL_LIST:
		buffer.writeInteger(objType);
		GVector list = (GVector) object;
		buffer.writeInteger(list.size());
		Enumeration listElements = list.elements();
		while(listElements.hasMoreElements()) {
		    if(!GFrame.encodeObject(buffer, listElements.nextElement()))
			return false;
		}
		break;
	    case GalaxyObject.GAL_BINARY:
		buffer.writeInteger(objType);
		GBinary binObject = (GBinary) object;
		buffer.writeInteger(binObject.getSize());
		buffer.writeFixedLengthOpaqueData(binObject.getBytes());
		break;
	    case GalaxyObject.GAL_INT_16:
		buffer.writeInteger(objType);
		buffer.writeShortArray(((Int16) object).getShortArray());
		break;
	    case GalaxyObject.GAL_INT_32:
		buffer.writeInteger(objType);
		buffer.writeIntegerArray(((Int32) object).getIntArray());
		break;
	    case GalaxyObject.GAL_INT_64:
		buffer.writeInteger(objType);
		buffer.writeLongArray(((Int64) object).getLongArray());
		break;
	    case GalaxyObject.GAL_FLOAT_32:
		buffer.writeInteger(objType);
		buffer.writeFloatArray(((Float32) object).getFloatArray());
		break;
	    case GalaxyObject.GAL_FLOAT_64:
		buffer.writeInteger(objType);
		buffer.writeDoubleArray(((Float64) object).getDoubleArray());
		break;
	    case GalaxyObject.GAL_PROXY:
		buffer.writeInteger(objType);
		buffer.writeString(((BrokerProxy) object).getCallId());
		buffer.writeString(((BrokerProxy) object).getHost());
		buffer.writeInteger(((BrokerProxy) object).getPort());
		buffer.writeInteger(((BrokerProxy) object).getObjectType());
		break;
	    default:
		Logger.getLogger().logWarningMessage("Can't encode GalaxyObject of type " + GalaxyObject.nameType(objType) + ".", "GFrame.encodeObject(XdrOutBuffer, Object)");
		return false;
	    }
	} else if(object instanceof String) {
	    buffer.writeInteger(GalaxyObject.GAL_STRING);
	    buffer.writeString((String) object);
	} else if(object instanceof Integer) {
	    buffer.writeInteger(GalaxyObject.GAL_INT);
	    buffer.writeInteger(((Integer) object).intValue());
	} else if(object instanceof Float) {
	    buffer.writeInteger(GalaxyObject.GAL_FLOAT);
	    buffer.writeFloat(((Float) object).floatValue());
	} else if(object instanceof Symbol) {
	    buffer.writeInteger(GalaxyObject.GAL_SYMBOL);
	    buffer.writeString(((Symbol) object).toString());
	} else {
	    Logger.getLogger().logWarningMessage("Can't encode object.", "GFrame.encodeObject(XdrOutBuffer, Object)");
	    return false;
	}   
	return true;
    }

    /**
     * Decodes a frame from a XDR buffer.
     *
     * @param buffer the buffer in which the frame is encoded
     * @return the frame
     */
    public static GFrame decodeFrame(XdrInBuffer buffer)
    {
	GFrame frame = null;

	try { 	    

	    // Decode the frame type and frame name.
	    int frameType = buffer.readInteger();

	    String frameNameStr = buffer.readString();
	    Symbol frameName = Symbol.getSymbol(frameNameStr);

	    switch(frameType) {
	    case GFrame.GAL_CLAUSE:
		frame = new Clause(frameName);
		break;
	    case GFrame.GAL_PRED:
		frame = new Predicate(frameName);
		break;
	    case GFrame.GAL_TOPIC:
		frame = new QSet(frameName);
		break;
	    default:
		frame = new Clause(frameName);
	    }
	    frame.setFrameType(frameType);

	    int numKeyValues = buffer.readInteger();
	    String keyNameStr;
	    Symbol keyName;
	    int valueType;
	    Object object = null;

	    // Read in the key/value pairs.
	    for(int idx=0; idx<numKeyValues; ++idx) {
		keyNameStr = buffer.readString();
		keyName = Symbol.getSymbol(keyNameStr);
		object = decodeObject(buffer);

		if(object == null) {
		    Logger.getLogger().logWarningMessage("Encountered a null object while decoding frame.", "GFrame.decodeFrame(XdrInBuffer)");
		    return null;
		}
		    
		frame.setProperty(keyName, object);
	    }

	    // Read in the predicates.
	    int numPredicates = buffer.readInteger();
	    for(int idx=0; idx<numPredicates; ++idx) {
		object = GFrame.decodeObject(buffer);
		if(object == null) {
		    Logger.getLogger().logWarningMessage("Encountered a null predicate while decoding frame.", "GFrame.decodeFrame(XdrInBuffer)");
		    return null;
		}

		frame.setPredicate((Predicate) object);
	    }

	   
	} catch(Exception ex) {
	    Logger.getLogger().logErrorMessage("Caught exception while decoding frame: " + ex.toString(), ex, "GFrame.decodeFrame(XdrInBuffer)");
	    return null;
	}

	return frame;
    }
    
    /**
     * Decodes the next object from a XDR buffer.
     *
     * @param buffer the buffer in which the object is encoded
     * @return the next decoded object
     */
    public static Object decodeObject(XdrInBuffer buffer) 
	throws Exception
    {
	int objectType = buffer.readInteger();
	Object object = null;
	int arrayLength = 0;

	// Total number of bytes to read when reading in arrays of numbers.
	int numBytes = 0;

	switch(objectType) {
	case GalaxyObject.GAL_FRAME:
	case GalaxyObject.GAL_TOPIC_FRAME:
	case GalaxyObject.GAL_CLAUSE_FRAME:
	case GalaxyObject.GAL_PRED_FRAME:
	    object = GFrame.decodeFrame(buffer);
	    break;
	case GalaxyObject.GAL_STRING:
	case GalaxyObject.GAL_TOKEN:
	case GalaxyObject.GAL_KEYWORD:
	case GalaxyObject.GAL_TAG:
	    object = buffer.readString();
	    break;
	case GalaxyObject.GAL_SYMBOL:
	    object = Symbol.getSymbol(buffer.readString());
	    break;
	case GalaxyObject.GAL_INT:
	    object = new Integer(buffer.readInteger());
	    break;
	case GalaxyObject.GAL_FLOAT:
	    object = new Float(buffer.readFloat());
	    break;
	case GalaxyObject.GAL_LIST:
	    int listSize = buffer.readInteger();
	    object = new GVector(listSize);
	    Object listElement;
	    for(int idx=0; idx<listSize; ++idx) {
		listElement = decodeObject(buffer);
		if(listElement == null) {
		    Logger.getLogger().logErrorMessage("Encountered a null list element while decoding object.", "GFrame.decodeObject(XdrInBuffer)");
		    return null;
		}

		((GVector)object).addElement(listElement);
	    }
	    break;
	case GalaxyObject.GAL_BINARY:
	    object = new GBinary(buffer.readVariableLengthOpaqueData())	 ;   
	    break;
	case GalaxyObject.GAL_INT_16:
	    arrayLength = buffer.readInteger();

	    // 16-bit integers are encoded as 32-bit (i.e., 4 bytes) integers
	    numBytes = arrayLength * 4; 
	    object = new Int16(buffer.readShortArray(arrayLength));
	    break;
	case GalaxyObject.GAL_INT_32:
	    arrayLength = buffer.readInteger();
	    numBytes = arrayLength * 4; 
	    object = new Int32(buffer.readIntegerArray(arrayLength));
	    break;
	case GalaxyObject.GAL_INT_64:
	    arrayLength = buffer.readInteger();
	    numBytes = arrayLength * 8; 
	    object = new Int64(buffer.readLongArray(arrayLength));
	    break;
	case GalaxyObject.GAL_FLOAT_32:
	    arrayLength = buffer.readInteger();
	    numBytes = arrayLength * 4; 
	    object = new Float32(buffer.readFloatArray(arrayLength));
	    break;
	case GalaxyObject.GAL_FLOAT_64:
	    arrayLength = buffer.readInteger();
	    numBytes = arrayLength * 8; 
	    object = new Float64(buffer.readDoubleArray(arrayLength));
	    break;
	case GalaxyObject.GAL_PTR:
	    Logger.getLogger().logErrorMessage("Can't decode objects of type GAL_PTR.", "GFrame.decodeObject(XdrInBuffer)");
	    return null;
	case GalaxyObject.GAL_PROXY:
	    String callId = buffer.readString(); // call id
	    String host = buffer.readString(); // host name
	    int port = buffer.readInteger(); // port
	    int proxyObjectType = buffer.readInteger(); // object type
	    object = new BrokerProxy(callId, host, port, proxyObjectType);
	    break;
	default:
	    Logger.getLogger().logErrorMessage("Can't decode objects of type " + GalaxyObject.nameType(objectType) +".", "GFrame.decodeObject(XdrInBuffer)");
	    return null;
	}

	return object;
    }
}
