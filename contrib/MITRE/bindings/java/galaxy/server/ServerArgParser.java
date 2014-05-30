/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.server;

import java.util.List;
import java.util.ArrayList;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;

import galaxy.util.HubContactInfo;
import galaxy.util.ListenerLocationInfo;
import galaxy.util.ArgParser;
import galaxy.util.Logger;

public class ServerArgParser extends ArgParser
{
    private List hubContactInfoCollection = null;
    private Logger logger = null;

    private String helpMessage =
	"-port <port number>                    Specifies the port on which this server\n" +
	"                                       will listen for connections.\n" +
	"-assert                                Prevent the server from searching for a\n" +
	"                                       free port if the specified port is not\n" +
	"                                       available. The specified port is one of\n" +
	"                                       a) port specified in a server location\n" +
	"                                       file (see -server_locations_file below),\n" +
	"                                       b) the port specified by the -port\n" +
	"                                       argument, c) a port passed into a\n" +
	"                                       MainServer  constructor, or d) the\n" +
	"                                       default port (1201). Note that the port\n" +
	"                                       specification in the server location file\n" +
	"                                       takes precedence over the -port argument,\n" +
	"                                       which takes precedence over\n" +
	"                                       programmatically-specified ports, which\n" +
	"                                       take precedence over the default port.\n" +
	"-maxconns <max number of connections>  Maximum number of client connections\n" +
	"                                       supported by this server's listener\n" +
	"                                       (if not > 0, no limit is set). If this\n" +
	"                                       argument is used, but no value is\n" +
	"                                       specified, a default of 1 is used.\n" +
	"-validate                              Validate Server method signatures.\n" +
	"-contact_hub <host:port ...>           Hosts and ports of Hubs to contact on\n" +
	"                                       startup. Note that multiple host/port\n" +
	"                                       pairs are separated with whitespace and\n" +
	"                                       must be enclosed in double quotes:\n" +
	"                                       e.g., -contact_hub \"foo:123 bar:456\".\n" +
	"-session_id <id>                       If -contact_hub is used, lock this server\n" +
	"                                       to the specified Hub session.\n" +
        "-server_locations_file <file>          A file of lines of the form:\n" +
	"                                         <server name> <host:port> [hub|server].\n" +
	"                                       This file defines the locations of the\n" +
	"                                       listeners to use (be they in servers or\n" +
	"                                       Hubs).\n" +
	"-slf_name <tag>                        This is used to force the server to\n" +
	"                                       use the server location information (in\n" +
	"                                       the file specified with the\n" +
	"                                       -server_locations_file argument) that\n" +
	"                                       corresponds to the server name specified\n" +
	"                                       by <tag>.\n" +
	"-verbosity <verbosity level>           The logging verbosity level. Valid\n" +
	"                                       values are the integers 0 (no logging)\n" +
	"                                       through 6 (very verbose logging on).\n" +
	"                                       The default level is 3.\n" +
	"-ui                                    Starts the server's user interface.\n" +
	"                                       If this argument is not used, the user\n" +
	"                                       interface is not displayed, and all\n" +
	"                                       output is sent to standard out and\n" +
	"                                       standard error (typically the display\n" +
	"                                       screen). This argument is only\n" +
	"                                       processed by ServerUI-based servers.\n" +
	"-no_output                             Starts the server and doesn't print any\n" +
	"                                       output anywhere. This argument is only\n" +
	"                                       processed by ServerUI-based servers.\n" +
	"-start                                 Automatically starts the server\n" +
	"                                       (i.e., you don't need to click the \n" +
	"                                       Start button in the user interface).\n" +
	"                                       This argument is only processed by\n" +
	"                                       ServerUI-based servers.\n" +
	"-log <log file name>                   Sets the name of the log file. This\n" +
	"                                       argument is only processed by\n" +
	"                                       ServerUI-based servers.\n" +
	"-append_log                            If the log file already exists, append\n" +
	"                                       to it. This argument is only processed\n" +
	"                                       by ServerUI-based servers.\n" +
	"-main_server_class <server class name> The name of the MainServer class to\n" +
	"                                       start. This argument is only processed\n" +
	"                                       by ServerUI-based servers.\n" +
	"-help                                  Displays descriptions of the above\n" +
	"                                       command line arguments.\n";

    /** 
     * Creates a server arg parser.
     *
     * @param args the command line arguments to parse
     */
    public ServerArgParser(String args[])
    {
	super(args);
	initialize();
    }

    /** 
     * Creates a server arg parser.
     *
     * @param args the command line arguments to parse
     * @param helpText this text is prepended to the message that is displayed
     *                 when the -help command line argument is used
     */
    public ServerArgParser(String args[], String helpText)
    {
	super(args);
	if(helpText != null) {
	    int length = helpText.length();
	    if(helpText.charAt(length-1) != '\n')
		helpMessage = helpText + "\n" + helpMessage;
	    else
		helpMessage = helpText + helpMessage;
	}
	initialize();
    }

    private void initialize()
    {
	logger = Logger.getLogger();

	if(isArg("-help")) {
	    System.out.println("Usage: \n" + helpMessage);
	    System.exit(0);
	}
	
	if(isArg("-contact_hub"))
	    processHostPortCollection();
    }

    private void processHostPortCollection()
    {
	List contactHubArgValue = getArg("-contact_hub");
        if(contactHubArgValue != null) {
	    
	    // First, we need to possibly break the string into separate
	    // host/port strings.


	    // Should be 1.
	    int numValues = contactHubArgValue.size();
	    if(numValues != 1) {
		logger.logWarningMessage("Expected one argument value for '-contact_hub' but got more or less. Stopping processing of '-contact_hub'.", "ServerArgParser.processHostPortCollection()");
		return;
	    }

	    String value = (String) contactHubArgValue.get(0);
	    int valueLength = value.length();
	    char nextChar;
	    boolean readingHostPort = false;
	    List hostPortList = new ArrayList();
	    int startIndex = 0;

	    for(int idx=0; idx<valueLength ; ++idx) {
		nextChar = value.charAt(idx);
		if(!Character.isWhitespace(nextChar)) {

		    // We've read non-whitespace. If we are not currently
		    // reading in a new host/port, start new index.
		    if(!readingHostPort) {
			startIndex = idx;
			readingHostPort = true;
		    }
		} else {

		    // Whitespace separates the host/port pairs, so if we
		    // were reading in a host/port, save it now.
		    if(readingHostPort) {
			readingHostPort = false;
			hostPortList.add(value.substring(startIndex, idx));
		    }
		}
	    }

	    // Finally, add the last host/port string if it exists.
	    if(readingHostPort && startIndex < (valueLength-1))
		hostPortList.add(value.substring(startIndex, valueLength));
		


	    // Second, we need to convert the host/port strings into
	    // a string/integer pair. The delimiter is a colon (:).

	    int numPairs = hostPortList.size();
	    List hostPort;
	    int delimiterIdx = -1;
	    String hostPortStr;
	    String hostStr = "";
	    String portStr = "";
	    int port = -1;
	    HubContactInfo contactInfo;
	    String sessionId = getStringArg("-session_id", null);
	    hubContactInfoCollection = new ArrayList(numPairs);

	    for(int idx=0; idx<numPairs; ++idx) {
		hostPort = new ArrayList(2);
		hostPortStr = (String) hostPortList.get(idx);
		delimiterIdx = hostPortStr.indexOf(':');
		
		// If there is no colon, skip this malformed host/port.
		if(delimiterIdx == -1) {
		    logger.logWarningMessage("Found malformed Hub host/port string: " + hostPortStr + " in '-contact_hub' argument. Ignoring this host/port pair.", "ServerArgParser.processHostPortCollection()");
		    continue;
		}
		
		hostStr = hostPortStr.substring(0, delimiterIdx);
		portStr = hostPortStr.substring(delimiterIdx+1);

		if(hostStr.length() == 0) {
		    logger.logWarningMessage("Found an invalid host string: " + hostStr + " in '-contact_hub' argument. Ignoring this host/port pair.", "ServerArgParser.processHostPortCollection()");
		    continue;
		}

		if(portStr.length() == 0) {
		    logger.logWarningMessage("Found an invalid port string: " + portStr + " in '-contact_hub' argument. Ignoring this host/port pair.", "ServerArgParser.processHostPortCollection()");
		    continue;
		}

		try {
		    port = Integer.parseInt(portStr);
		} catch(NumberFormatException nfe) {
		    logger.logWarningMessage("Could not convert port string \"" + portStr + "\" in '-contact_hub' argument into an integer. Ignoring this host/port pair.", "ServerArgParser.processHostPortCollection()");
		    continue;
		} 

		contactInfo = new HubContactInfo(hostStr, port, sessionId, -1);
		hubContactInfoCollection.add(contactInfo); 
	    }
        }
    }

    /**
     * Processes the server locations file (if present) and returns the
     * listener location information for the specified server. This can either
     * be the settings for the target server's listener or the settings for
     * the listener in the Hub to contact.
     *
     * @param targetServerName look for information for this server
     * @return the listener information or <code>null</code> if no info was
     *         found for the server (or there was an error)
     */
    public ListenerLocationInfo getListenerLocationInfo(String targetServerName)
    {
	/*
	  Each line in the server locations file must be of this form:
	  
	  <server name> <host>:<port> [hub | server]
	  
	  The first element is the server name, the second is the listener
	  location, and the third is whether the Hub or server is
	  the listener. Both Hub and server can use this file to figure
	  out how to set itself up.
	  
	  Comment lines begin with  the # character. Empty lines are ignored.
	*/
	
	if(!isArg("-server_locations_file"))
	    return null;
	
	String fileName = getStringArg("-server_locations_file", null);
	
	if(fileName != null) {
	    
	    // Used to hold server tag, if any.
	    String tag = getStringArg("-slf_name", null);
	    if(tag == null && targetServerName == null) {
		logger.logWarningMessage("Returning null since no -slf_name argument value was specified and no targetServerName argument was provided.", "ServerArgParser.getListenerLocationInfo(String)");
		return null;
	    }

	    // Try to open the file.
	    BufferedReader reader;
	    try {
		reader = new BufferedReader(new FileReader(fileName));
	    } catch(FileNotFoundException fnfe) {
		logger.logWarningMessage("Could not open server locations file '" + fileName + "'. Stopping processing of '-server_locations_file' argument.", "ServerArgParser.getListenerLocationInfo(String)");
		return null;
	    }
	    
	    String nextLine = null;
	    char charArray[] = null;
	    
	    StringBuffer serverName;
	    StringBuffer hostName;
	    StringBuffer portStr;
	    StringBuffer listener;
	    char nextChar = '\0';
	    int lineLength;
	    int startIndex;
	    int currentIndex;
	    boolean gotHost;
	    boolean gotPort;
	    boolean gotColon;
	    boolean hubIsListener;
	    boolean serverIsListener;
	    
	    String hubHostName = null;
	    List hubPorts = new ArrayList();
	    int port = -1;
	    
	    HubContactInfo contactInfo;
	    String sessionId = getStringArg("-session_id", null);
	    boolean serverNamesMatch = false;
	    
	    try {
		
		// Read the next line from the file.
		while((nextLine = reader.readLine()) != null) {
		    lineLength = nextLine.length();
		    charArray = nextLine.toCharArray();
		    
		    if(lineLength == 0)
			continue;
		    
		    // First, find the first non-whitespace character.
		    for(startIndex=0; startIndex<charArray.length; ++startIndex) {
			nextChar = charArray[startIndex];
			if(!Character.isWhitespace(nextChar))
			    break;
		    }
		    
		    // Check if we read an empty line.
		    if(startIndex == charArray.length)
			continue;
		    
		    // Check if we encountered a comment.
		    if(nextChar == '#')
			continue;
		    
		    // Read in the server name.
		    serverName = new StringBuffer(); 
		    for(currentIndex=startIndex; currentIndex<charArray.length; ++currentIndex) {
			nextChar = charArray[currentIndex];
			if(Character.isWhitespace(nextChar))
			    break;
			else
			    serverName.append(nextChar);
		    }
		    
		    // Check for error in server name.
		    if(serverName.length() <= 0) {
			logger.logWarningMessage("Ignoring ill-formed server location info `" + nextLine + "' due to invalid server name.", "ServerArgParser.getListenerLocationInfo(String)");
			continue;
		    } else {
			if(tag != null)
			    serverNamesMatch = serverName.toString().equals(tag);
			else
			    serverNamesMatch = serverName.toString().equals(targetServerName);
		    }
		    
		    // Find the next non-whitespace character.
		    for(startIndex=currentIndex; startIndex<charArray.length; ++startIndex) {
			nextChar = charArray[startIndex];
			if(!Character.isWhitespace(nextChar))
			    break;
		    }
		    
		    // Check if we ran out of characters.
		    if(startIndex == charArray.length) {
			logger.logWarningMessage("Ignoring ill-formed server location info `" + nextLine + "' due to to missing information.", "ServerArgParser.getListenerLocationInfo(String)");
			continue;
		    }
		    
		    // Read in the listener <host>:<port> info.
		    hostName = new StringBuffer();
		    portStr = new StringBuffer();
		    gotHost = false;
		    gotPort = false;
		    gotColon = false;
		    for(currentIndex=startIndex; currentIndex<charArray.length; ++currentIndex) {
			nextChar = charArray[currentIndex];
			if(Character.isWhitespace(nextChar))
			    break;
			
			if(nextChar == ':') {
			    gotColon = true;
			    continue;
			}
			
			if(!gotColon) {
			    gotHost = true;
			    hostName.append(nextChar);
			} else {
			    gotPort = true;
			    portStr.append(nextChar);
			}
		    }
		    
		    // Check for valid host name.
		    if(!gotHost || hostName.length() == 0) {
			logger.logWarningMessage("Ignoring ill-formed server location info `" + nextLine + "' due to missing host information.", "ServerArgParser.getListenerLocationInfo(String)");
			continue;
		    }
		    
		    // Check for valid port number.
		    if(!gotPort || portStr.length() == 0) {
			logger.logWarningMessage("Ignoring ill-formed server location info `" + nextLine + "' due to missing port information.", "ServerArgParser.getListenerLocationInfo(String)");
			continue;
		    } else {
			try{
			    port = Integer.parseInt(portStr.toString());
			} catch(NumberFormatException nfe) {
			    logger.logWarningMessage("Ignoring ill-formed server location info `" + nextLine + "' due invalid port number.", "ServerArgParser.getListenerLocationInfo(String)");
			    continue;
			}
		    }
		    
		    // Find the next non-whitespace character.
		    for(startIndex=currentIndex; startIndex<charArray.length; ++startIndex) {
			nextChar = charArray[startIndex];
			if(!Character.isWhitespace(nextChar))
			    break;
		    }
		    
		    // Check if we ran out of characters.
		    if(startIndex == charArray.length) {
			logger.logWarningMessage("Ignoring ill-formed server location info `" + nextLine + "' due to missing listener type.", "ServerArgParser.getListenerLocationInfo(String)");
			continue;
		    }
		    
		    // Read in the listener id.
		    listener = new StringBuffer();
		    for(currentIndex=startIndex; currentIndex<charArray.length; ++currentIndex) {
			nextChar = charArray[currentIndex];
			if(Character.isWhitespace(nextChar))
			    break;
			else
			    listener.append(nextChar);
		    }
		    
		    hubIsListener = serverIsListener = false;
		    
		    if(listener.toString().compareToIgnoreCase("hub") == 0) {
			hubIsListener = true;
			
			// If we have found info for the target server,
			// update that server's Hub contact info.
			
			if(serverNamesMatch) {
			    // Overwrite any existing Hub contact information
			    // and store the new information.
			    hubContactInfoCollection = new ArrayList();
			    
			    contactInfo = new HubContactInfo(hostName.toString(),
							     port, 
							     sessionId,
							     -1);
			    hubContactInfoCollection.add(contactInfo); 
			}
			
		    } else if(listener.toString().compareToIgnoreCase("server") == 0) {
			serverIsListener = true;
		    } else {
			logger.logWarningMessage("Ignoring ill-formed server location info `" + nextLine + "' due to invalid listener type.", "ServerArgParser.getListenerLocationInfo(String)");
			continue;
		    }
		    
		    // If we found a match for our server, return the info.
		    if(serverNamesMatch)
			return new ListenerLocationInfo(serverIsListener, hostName.toString(), port);
		}
		
		// If we get here, there was no match for our server.
		return null;
		
	    } catch(IOException ioex) {
		logger.logErrorMessage("Caught exception while processing listener location info: " + ioex.toString(), ioex, "ServerArgParser.getListenerLocationInfo(String)");
		return null;
	    } finally {
		// Close the file.
		try {
		    reader.close();
		} catch(Exception ex) {
		    logger.logErrorMessage("Caught exception while processing listener location info: " + ex.toString(), ex, "ServerArgParser.getListenerLocationInfo(String)");
		}
	    }
	} else {
	    logger.logWarningMessage("Did not get a valid server locations file name. Stopping processing of '-server_locations_file' argument.", "ServerArgParser.getListenerLocationInfo(String)");
	    return null;
	}
    }
    
    public List getHubContactInfoCollection()
    {
	return hubContactInfoCollection;
    }
    
    // Test code.
    public static void main(String args[]) 
    {
	ServerArgParser parser = new ServerArgParser(args);
	String serverName = "foo10";
	ListenerLocationInfo info = parser.getListenerLocationInfo(serverName);
	System.out.println("=====================================");
	if(info == null) {
	    System.out.println("No info for server " + serverName);
	} else {
	    System.out.println("isServerListener = " + info.isServerListener());
	    System.out.println("hostName = " + info.getHostName());
	    System.out.println("port = " + info.getPort());
	}
    }
}
