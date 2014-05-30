/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/*
 * $Header: /afs/rcf/project/ai-contrib/src/cvs-master/GalaxyCommunicator2/contrib/MITRE/bindings/java/galaxy/lang/FrameReader.java,v 1.8 2002/03/14 19:06:36 wohlever Exp $
 */
/* These Java bindings were originally produced by Intel Corp.,
   which has granted permission to the Communicator program to
   use and modify them. The preceding MITRE copyright refers to
   whatever changes the MITRE Corporation has made to the code. */


package galaxy.lang;

import java.io.Reader;
import java.io.StringReader;
import java.io.IOException;

import galaxy.util.UUHandler;

/**
 * This class provides the ability to read a frame from a stream of characters.
 * Construct the object with a character stream Reader, then call getFrame().
 *
 * @see GFrame
 * @see #getFrame()
 */
public class FrameReader 
{
    private final static int NOTOKEN    = 0;
    private final static int LPAREN     = 1;
    private final static int RPAREN     = 2;
    private final static int LBRACE     = 3;
    private final static int RBRACE     = 4;
    private final static int EOF        = 5;
    private final static int INTEGER    = 6;
    private final static int FLOAT      = 7;
    private final static int SYMBOL     = 8;
    private final static int STRING     = 9;
    private final static int BINARY     = 10;
    private final static String NULLOBJECT = "NULLObject";
    
    /** UUEncode/Decoder */
    private UUHandler uu;
    
    private Reader reader;
    
    /** 
     * This is logically the first character of the input stream reader.
     * <br>Special values are:<br>
     * '\u0000'   Indicates this character is empty.<br>
     * '\uffff'   Indicates an end of file has been encountered.<br>
     */
    private char peekChar;
    
    private boolean peekCharValid = false;
    private boolean readingBinary = false;
    
    private boolean tokenValid = false;
    private int tokenType = NOTOKEN;
    private Float floatToken;
    private Integer integerToken;
    private Symbol symbolToken;
    private String stringToken;
    private GBinary binaryToken;
    
    private StringBuffer errorBuff = new StringBuffer();
  
    // Index in to errorBuff to indicate where a syntax error occured.
    private int mark;   
    
    private final static Symbol clauseSymbol = Symbol.getSymbol("c");
    private final static Symbol predicateSymbol = Symbol.getSymbol("p");
    private final static Symbol qsetSymbol = Symbol.getSymbol("q");
    
    /**
     * Construct a frame reader with an input stream.  Use getFrame() to read 
     * the frame.
     *
     * @param reader the reader
     * @see #getFrame
     */
    public FrameReader(Reader reader)
    {
	this.reader = reader;
    }
    
    /**
     * Reads the next value in the input stream and returns it.  Values can 
     * be: Integer, GFrame, String, Symbol, Float and Binary.
     */
    private Object getValue() throws Exception
    {
	int tokenType;
	Object value = null;
	
	tokenType = getToken();
	
	switch(tokenType) {
	case LPAREN:
	    ungetToken();
	    value = getList();
	    break;
	case LBRACE:
	    ungetToken();
	    value = getFrame();
	    break;
	case STRING:
	    value = stringToken;
	    break;
	case INTEGER:
	    value = integerToken;
	    break;
	case FLOAT:
	    value = floatToken;
	    break;
	case SYMBOL:
	    value = symbolToken;
	    break;
	case BINARY:
	    value = binaryToken;
	    break;
	default:
	    throwException("Expected a value");
	}
	return value;
    }
    
    /**
     * Read the list using the character stream Reader and return it.  
     *
     * @exception Exception Thrown if error
     *
     */
    private GVector getList() throws Exception
    {
	GVector gvec = new GVector();
	int tokenType;
	
	// (
	if (getToken() != LPAREN)
	    throwException("Expecting '('");
	
	
	// Loop through all the list elements until we find a ')'.
	for(;;) {
	    tokenType = getToken();
	    if (tokenType == RPAREN)
		return gvec;
	    
	    ungetToken();
	    Object obj = getValue();
	    gvec.addElement(obj);
	}
    }
    
    
    /**
     * Read the frame using the character stream Reader and return it.  
     *
     * @return the frame
     * @exception Exception Thrown if error
     */
    public GFrame getFrame() throws Exception
    {
	GFrame frame;
	Symbol frameType;
	int tokenType;
	
	// {
	if (getToken() != LBRACE)
	    throwException("Expecting '{'");
	
	// Check for optional type char.  White space means no type char.
	char c = getChar();
	if (c == ' ' || c == '\t' || c == '\n') {
	    frameType = null;
	} else {                
	    // c | p | q
	    ungetChar();
	    if (getToken() != SYMBOL)
		throwException("Expecting 'c', 'p', or 'q' after '{'.");
	    
	    if (symbolToken != clauseSymbol && symbolToken != predicateSymbol
		&& symbolToken != qsetSymbol)
		throwException("Expecting 'c', 'p', or 'q' after '{'.");
	    
	    frameType = symbolToken;
	}
	
	Symbol frameName=null;
	
	// optional frame-name
	switch(getToken()) {
	case RBRACE:
	    ungetToken();
	    frameName = null;
	    break;
	case SYMBOL:
	    frameName = symbolToken;
	    break;
	default:
	    throwException("Expecting a frame name");
	}
	
	if (frameType == clauseSymbol)
	    frame = new Clause(frameName);
	else if (frameType == predicateSymbol)
	    frame = new Predicate(frameName);
	else if (frameType == qsetSymbol)
	    frame = new QSet(frameName);
	else  
	    frame = new Clause(frameName);
	
	// Loop through all the key/value pairs until we find a '}'.
	for(;;) {
	    tokenType = getToken();
	    if(tokenType == SYMBOL) {
		
		Symbol name = symbolToken;   
		Object value = getValue();
		if (name.getString().equals(":pred"))
		    frame.setPredicate((Predicate)value);
		else
		    frame.setProperty(name, value);
	    } else if (tokenType == RBRACE)
		break;
	    else
		throwException("Expecting Symbol or  '}'.");
	}   
	
	return frame;
    }
    
    /**
     * Get a token from the input stream, return the token's type.
     * If the particular token type has a value, it is stored in the 
     * corresponding variable.
     * <br>
     * <table border=1>
     * <tr><th>Token type<th>Variable
     * <tr><td>INTEGER<td>integerToken
     * <tr><td>FLOAT<td>floatToken
     * <tr><td>BINARY<td>binaryToken
     * <tr><td>STRING<td>stringToken
     * <tr><td>SYMBOL<td>symbolToken
     * </table>
     *
     */
    private int getToken() throws Exception
    {
	char c;
	
	if (tokenValid) {
	    tokenValid = false;
	    return tokenType;
	}
	
	// Mark this location in the errorBuff as a place to report 
	// syntax errors.
	mark = errorBuff.length();
	
	skipWhiteSpace();
	
	switch(c = getChar()) {
	case '\uffff':
	    break;
	case '(':
	    tokenType = LPAREN;
	    break;
	case ')':
	    tokenType = RPAREN;
	    break;
	case '{':
	    tokenType = LBRACE;
	    break;
	case '}':
	    tokenType = RBRACE;
	    break;
	case '%':
	    tokenType = BINARY;
	    tokenType = getBinary();
	    break;
	case '"':
	    ungetChar();
	    tokenType = getString();
	    break;
	case '-':
	case '0': case '1': case '2': case '3': case '4': 
	case '5': case '6': case '7': case '8': case '9':
	    ungetChar();
	    tokenType = getNumber();
	    break;
	default:
	    if ('a' <= c && c <= 'z'
		||  'A' <= c && c <= 'Z'
		||  '_' == c 
		||  ':' == c) {
		ungetChar();
		tokenType = getSymbol();
	    }
	    break;
	}
	
	return tokenType;
    }
    
    private void ungetToken()
    {
	if (tokenType != NOTOKEN)
	    tokenValid = true;
    }
    
    private int getBinary() throws Exception
    {
	int token;
	int binarySize=0;
	int encodedSize=0;
	
	token = getToken();
	
	if (readingBinary) 
	    throwException("Illformed binary entry, double %");
	else 
	    readingBinary = true;
	
	if (token == INTEGER)
	    binarySize = integerToken.intValue();
	else 
	    throwException("Illformed binary entry: missing binary size");
	getToken();
	
	if (token == INTEGER)
	    encodedSize = integerToken.intValue();
	else 
	    throwException("Illformed binary entry: missing encoded size");
	
	// Read whitespace
	if (!(getChar() == ' '))
	    throwException("No whitespace in between");
	
	StringBuffer str = new StringBuffer(encodedSize);
	for (int i=0; i < encodedSize;i++) {
	    str.append(getChar());
	}
	readingBinary = false;
	binaryToken = new GBinary(uu.decode(str.toString(),binarySize));
	return BINARY; 
    }
    
    private int getNumber() throws Exception
    {
	int i = 0;
	float d = 0;
	int e = 0;
	boolean pose = true;
	int digitCount = 0;
	boolean isFloat = false;
	char c = getChar();
	boolean isNegative = c == '-';
	
	if (isNegative) {
	    skipWhiteSpace();
	    c = getChar();
	    ungetChar();
	    if (!isDigit(c))
		throwException("Expecting a digit after a minus sign ");
	} else
	    ungetChar();
	
	while(isDigit(c = getChar()))
	    i = i*10 + (c-'0');
	
	if (c == '.') {
	    isFloat = true;
	    float place = 10;
	    while(isDigit(c = getChar())) {
		d = d + (c-'0')/place;
		place *= 10;
	    }
	} 
	
	if (c == 'e' || c == 'E') {
	    c = getChar();
	    if (c == '-' || c == '+')
		pose = c == '+';
	    else 
		ungetChar();
	    while(isDigit(c = getChar())) {
		e = e*10 + (c-'0');
	    }
	    ungetChar();
	} else 
	    ungetChar();
	
	if (isFloat) {
	    float f = i+d;
	    if (isNegative)
		f = -f;
	    float ef = 1;
	    if (pose) {
		while(e>0) {
		    ef *= 10;
		    e--;
		}
	    } else {
		while(e>0) {
		    ef /= 10;
		    e--;
		}
	    }
	    floatToken = new Float(f*ef);
	    return FLOAT;
	} else {
	    if (isNegative)
		i = -i;
	    integerToken = new Integer(i);
	    return INTEGER;
	}
	
    }
    
    private boolean isDigit(char c)
    {
	return '0' <= c && c <= '9';
    }
    
    private int getSymbol() throws IOException
    {
	StringBuffer sb = new StringBuffer();
	char c;
	
	while(isSymbolChar(c = getChar())) {
	    sb.append(c);
	}
	
	ungetChar();
	
	symbolToken = Symbol.getSymbol(sb.toString());
	
	return SYMBOL;
    }
    
    private boolean isSymbolChar(char c)
    {
	return 'a' <= c && c <= 'z'
	    || 'A' <= c && c <= 'Z'
	    || '0' <= c && c <= '9'
	    || c == '_'
	    || c == '-'
	    || c == '.'
	    || c == ':';
    }
    
    private int getString() throws Exception
    {
	StringBuffer sb = new StringBuffer();
	
	char c = getChar();  // Skip the double quote (").
	
	for(;;) {
	    c = getChar();
	    if (c == '"') break;
	    if (c == '\n') 
		throwException("new-line in string");
	    if (c == '\uffff')
		throwException("end of input in string");
	    if (c == '\\') {
		c = getChar();      // Needs to work for double quote (").
	    }
	    sb.append(c);
	}
	
	stringToken = sb.toString();
	
	return STRING;
    }
    
    private void skipWhiteSpace() throws IOException
    {
	char c;
	
	while((c = getChar()) == ' ' || c == '\t' || c == '\n' || c == '\r')
	    continue;
	
	ungetChar();
    }
    
    /**
     * Get a character from the input stream and place it in
     * peekChar.  If an end-of-file is encountered, peekChar is set
     * to '\uffff'.
     */
    private char getChar() throws IOException
    {
	if (peekCharValid) {
	    peekCharValid = false;
	    return peekChar;
	}
	
	int i = reader.read();
	if (i == -1) {
	    peekChar = '\uffff';
	} else {
	    peekChar = (char)i;
	    
	    //Keep track of the input for reporting syntax errors.
	    errorBuff.append(peekChar);
	}
	
	return peekChar;
    }
    
    private void ungetChar()
    {
	peekCharValid = true;
    }
    
    private void throwException(String msg) throws Exception
    {
	String errorString = errorBuff.toString();
	msg += '\n';
	msg += errorString.substring(0, mark);
	msg += " ==> ";
	msg += errorString.substring(mark);
	throw new Exception(msg);
    }
    
    /** Test routine. */
    public static void main(String args[]) 
    {
	String f1 = "{c truth :key1 \"value1\" :key2 {p predthing :keya sym1} :key3 -1234}";
	String f2 = 
	    "{c main " +
	    "   :request_frame {c when :domain \"Movies\" :aux \"link\" :topic {q title :name \"deep space nine\" :pred {p on_set }} }  " +
	    "   :key_value \"CLAUSE: when TITLE: deep space nine ON: set\"" +
	    "   :op_name \"turn_management\" " +
	    "   :utterance_id 4" +
	    "   :foo NULLObject" +	    
	    "   :session_id  time_stamp" +
	    "   :a_list (1 2 3 -4.567 4.567000e+10 4.567000e-01 4.567000e+01 -4.567000e10 4.567000e-00 {c truth :and \"justice\"} {p consequence :result positive})" +
	    "}         ";
	String f3 = "{ :nfound 6 :values (\"abc\" \"def\" 1 2 3 4)}";
	//        String f4 = "{q :nfound 6 :values (\"abc\" \"def\" 1 2 3 4)}";
	String ha = "Hello_There";
	GBinary bd = new GBinary(ha.getBytes());
	String f4 = "{c main op_name \"receive_binary\" :binary_data " +bd.toEncodedString() + " ridx 0 tidx 2 }";
	String f5 = "{ myName :nfound 6 :values (\"abc\" \"def\" 1 2 3 4)}";
	String f6 = "{ name tidx 6 :key \"value\" ::key_with_two_colons {c hi}}";
	
	test(f1);
	test(f2);
	test(f3);
	test(f4);
	test(f5);
	test(f6);
    }    
    
    private static void test(String fs)
    {
	StringReader sr = new StringReader(fs);
	
	FrameReader sfr = new FrameReader(sr);
	try  {
	    GFrame f = sfr.getFrame();
	    System.out.println(f.toFormattedString());            
	} catch(Exception ex) {
	    System.err.println("FrameReader.test caught exception: " + ex.toString());
	}        
    }
}
