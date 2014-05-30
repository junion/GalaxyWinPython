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

import java.util.Vector;
import java.util.Enumeration;

/**
 * A Galaxy Vector, or GVector for short.  The name starts with G to 
 * distinguish it from java.util.Vector.  It is distinguished from a Vector in
 * that there are utilities to filter a GVector.  It also knows how to format 
 * itself for pretty printing.
 * <P>
 * This data type corresponds to <i>tlist</i> in the C version of Galaxy. 
 * <P>
 * A tlist is often used in a <i>result set</i>  A <i>result set</i> is a frame
 * which contains ":nfound", and ":values".
 * 
 * @see java.util.Vector
 * @see galaxy.lang.GalaxyObject
 */
public class GVector extends GalaxyObject {

    private Vector list;

    public GVector() {
	this.type = GAL_LIST;
	list = new Vector();
    }

    public GVector(int i) {
	this.type = GAL_LIST;
	list = new Vector(i);
    }

    public GVector(int i, int j) {
	this.type = GAL_LIST;
	list = new Vector(i,j);
    }
	
    public String toEncodedString() {
	StringBuffer buff =new StringBuffer();
	
	buff.append("(");
	for(int i=0; i < list.size();i++) {
	    Object element = list.elementAt(i);
	    appendValue(buff.append(" "), 0, element);
	}
	buff.append(" )");
	return buff.toString();
    }

    public String toFormattedString(int ident) {
	StringBuffer buff =  new StringBuffer();
	format(buff,ident);
	return buff.toString();
    }

    public String toFormattedString() {
	return toFormattedString(0);
    }

    void format(StringBuffer buff, int indent)
    {
        buff.append("(");

	for(int i=0; i < list.size();i++) {
            Object element = list.elementAt(i);
            int oldBuffSize = buff.length();
            appendFormattedValue(buff, indent+1, element);
            if (i+1 < list.size())
                buff.append("\n");
        }
        buff.append(")");
    }

    /**
     * Vector methods implementation
     */
    
    public void addElement(Object obj) {
	list.addElement(obj);
    }

    public boolean removeElement(Object obj) {
	return list.removeElement(obj);
    }

    public void removeAllElements() {
	list.removeAllElements();
    }

    public Object clone() {
	return list.clone();
    }

    public String toString() {
	return list.toString();
    }

    public void copyInto(Object anArray[]) {
	list.copyInto(anArray);
    }

    public void trimToSize() {
	list.trimToSize();
    }
    
    public void ensureCapacity(int minCapacity) {
	list.ensureCapacity(minCapacity);
    }

    public void setSize(int newSize) {
	list.setSize(newSize);
    }
    
    public int capacity() {
	return list.capacity();
    }

    public int size() {
	return list.size();
    }

    public boolean isEmpty() {
	return list.isEmpty();
    }
    
    public Enumeration elements() {
	return list.elements();
    }

    public boolean contains(Object elem) {
	return list.contains(elem);
    }

    public int indexOf(Object elem) {
	return list.indexOf(elem);
    }

    public int indexOf(Object elem,int index) {
	return list.indexOf(elem,index);
    }

    public int lastIndexOf(Object elem) {
	return list.lastIndexOf(elem);
    }

    public int lastIndexOf(Object elem, int index) {
	return list.lastIndexOf(elem,index);
    }

    public Object elementAt(int index){
	return list.elementAt(index);
    }

    public Object firstElement() {
	return list.firstElement();
    }

    public Object lastElement() {
	return list.lastElement();
    }

    public void setElementAt(Object obj,int index) {
	list.setElementAt(obj,index);
    }
    public void removeElementAt(int index) {
	list.removeElementAt(index);
    }

    public void insertElementAt(Object obj, int index) {
	list.insertElementAt(obj,index);
    }
}
