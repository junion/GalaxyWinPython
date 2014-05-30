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

/**
 * $Id: UUHandler.java,v 1.2 2001/06/05 20:35:52 wohlever Exp $
 */

package galaxy.util;

public class UUHandler {

    public UUHandler() {}

    public static String encode(byte[] data) {
	StringBuffer strbuf = new StringBuffer();
	int length = data.length;
	byte b0,b1,b2;

	for(int i=0; i < length; i+= 3) {
	    b0 = data[i];
	    b1=0;b2=0;
	    if(i+1 < length)
		b1 = data[i+1];
	    if (i+2 < length)
		b2 = data[i+2];
	    /* The trick, according to Jonathon, is to avoid the
	       low four bits, so you take the high six of the first,
	       then the low two of the first and the high four of the
	       second, then the low four of the second and the high to
	       of the third, then the low six of the third */
	    strbuf.append((char)(((b0 >> 2) & 0x3F) + 0x20));
	    strbuf.append((char)((((b0 << 4) | ((b1 >> 4) & 0xF)) & 0x3F) + 0x20));
	    strbuf.append((char)((((b1 << 2) | ((b2 >> 6) & 0x3)) & 0x3F) + 0x20));
	    strbuf.append((char)((b2 & 0x3F) + 0x20));
	}
	
	return strbuf.toString();
    }

    public static byte[] decode(String data, int length) {
	byte[] res = new byte[length];

	byte[] source = new byte[data.length()];
	
	for(int i=0; i < data.length();i++)
	    source[i]= (byte)data.charAt(i);
	
	int srlen = source.length;
	int j=0;
	for(int i=0; i < srlen; i+= 4) {
	    byte sa = (byte)(source[i] - 0x20);
	    byte sb = (byte)(source[i+1] - 0x20);
	    byte sc = (byte)(source[i+2] - 0x20);
	    byte sd = (byte)(source[i+3] - 0x20);
	    
	    res[j++] = (byte)(((sa << 2) | ((sb >> 4) & 0x03)) & 0xFF);
	    if (j < length)
		res[j++] = (byte)(((sb << 4) | ((sc >> 2) & 0x0F)) & 0xFF);
	    else break;
	    if (j < length)
		res[j++] = (byte)(((sc << 6) | (sd & 0x3F)) & 0xFF);
	    else break;
	}
	return res;
    }

    public static void main(String args[]) {
	String t1 = "Hello_There";
	byte[] m1 = t1.getBytes();
	System.out.println("Encoding : "+ t1 + " BYTE REP: " +m1);
	UUHandler uu = new UUHandler();
	String bs = uu.encode(m1);
	System.out.println("Binary size = " + m1.length + "\tEncoded size = " + bs.length());
	System.out.println("Encoded message: " + bs);
	System.out.println("Decoding message...");
	String ha = new String(uu.decode(bs, m1.length));
	System.out.println("Decoded message: " + ha);
	System.out.println("Again! Encoding...");
	byte[] m2 = ha.getBytes();
	bs = uu.encode(m2);
	System.out.println("Binary size = " + m2.length + "\tEncoded size = " + bs.length());
	System.out.println("Encoded message: " + bs);
	System.out.println("Decoding message...");
	System.out.println("Decoded message: " + new String(uu.decode(bs, m2.length)) );
	
    }
}
