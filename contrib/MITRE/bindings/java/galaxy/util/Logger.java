/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.util;

import galaxy.server.MainServer;

/**
 * This is class is used to display diagnositc log messages.
 */
public class Logger implements DiagnosticLogger
{
    private static Logger logger = new Logger();

    public static Logger getLogger()
    {
	return Logger.logger;
    }

    /**
     * Displays a message to System.out.
     *
     * @param msg the message to display
     * @param level the minimum verbosity level at which to display the message
     *              (verbosity levels are defined in 
     *              <code>galaxy.server.MainServer</code>)
     */
    public void logMessage(String msg, int level)
    {
	logMessage(msg, level, null);
    }

    /**
     * Displays a message to System.out.
     *
     * @param msg the message to display
     * @param level the minimum verbosity level at which to display the message
     *              (verbosity levels are defined in 
     *              <code>galaxy.server.MainServer</code>)
     * @param location "location" of the associated call to this method
     *                  (e.g., the class and method name from which this call
     *                  was made) which is displayed at verbosity levels
     *                  4 and above
     */
    public void logMessage(String msg, int level, String location)
    {
	if(MainServer.getVerbosityLevel() >= level) {
	    String newMsg = msg;
	    if(location != null && MainServer.getVerbosityLevel() >= 4)
		if(msg.startsWith("\n"))
		    newMsg = new String("\n" + location + ": " + msg.substring(1));
		else
		    newMsg = new String("\n" + location + ": " + msg);
	    System.out.println(newMsg);
	}
    }

    /**
     * Displays a message to System.out (at any verbosity level).
     *
     * @param msg the message to display
     */
    public void logMessage(String msg)
    {	    
	System.out.println(msg);
    }

    /**
     * Displays an error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.ERROR_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     */
    public void logErrorMessage(String msg)
    {
	logErrorMessage(msg, null, null);
    }

    /**
     * Displays an error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.ERROR_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     * @param ex the stack trace of this exception is also displayed
     */
    public void logErrorMessage(String msg, Exception ex)
    {
	logErrorMessage(msg, ex, null);
    }

    /**
     * Displays an error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.ERROR_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     * @param location "location" of the associated call to this method
     *                  (e.g., the class and method name from which this call
     *                  was made) which is displayed at verbosity levels
     *                  4 and above
     */
    public void logErrorMessage(String msg, String location)
    {
	logErrorMessage(msg, null, location);
    }

    /**
     * Displays an error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.ERROR_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     * @param ex the stack trace of this exception is also displayed
     * @param location "location" of the associated call to this method
     *                  (e.g., the class and method name from which this call
     *                  was made) which is displayed at verbosity levels
     *                  4 and above
     */
    public void logErrorMessage(String msg, Exception ex, 
				       String location)
    {
	if(MainServer.getVerbosityLevel() >= MainServer.ERROR_VERBOSITY_LEVEL) {
	    String newMsg = msg;
	    if(location != null && MainServer.getVerbosityLevel() >= 4)
		if(msg.startsWith("\n"))
		    newMsg = new String("\n" + location + ": " + msg.substring(1));
		else
		    newMsg = new String("\n" + location + ": " + msg);
	    System.err.println("ERROR: " + newMsg);
	    if(ex != null && MainServer.getVerbosityLevel() >= MainServer.ERROR_DETAILS_VERBOSITY_LEVEL)
		ex.printStackTrace();
	}
    }

    /**
     * Displays a fatal error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.FATAL_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     */
    public void logFatalMessage(String msg)
    {
	logFatalMessage(msg, null, null);
    }

    /**
     * Displays a fatal error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.FATAL_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     * @param ex the stack trace of this exception is also displayed
     */
    public void logFatalMessage(String msg, Exception ex)
    {
	logFatalMessage(msg, ex, null);
    }

    /**
     * Displays a fatal error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.FATAL_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     * @param location "location" of the associated call to this method
     *                  (e.g., the class and method name from which this call
     *                  was made) which is displayed at verbosity levels
     *                  4 and above
     */
    public void logFatalMessage(String msg, String location)
    {
	logFatalMessage(msg, null, location);
    }

    /**
     * Displays a fatal error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.FATAL_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     * @param ex the stack trace of this exception is also displayed
     * @param location "location" of the associated call to this method
     *                  (e.g., the class and method name from which this call
     *                  was made) which is displayed at verbosity levels
     *                  4 and above
     */
    public void logFatalMessage(String msg, Exception ex, 
				       String location)
    {
	if(MainServer.getVerbosityLevel() >= MainServer.FATAL_VERBOSITY_LEVEL) {
	    String newMsg = msg;
	    if(location != null && MainServer.getVerbosityLevel() >= 4)
		if(msg.startsWith("\n"))
		    newMsg = new String("\n" + location + ": " + msg.substring(1));
		else
		    newMsg = new String("\n" + location + ": " + msg);
	    System.err.println("FATAL ERROR: " + newMsg);
	    if(ex != null && MainServer.getVerbosityLevel() >= MainServer.FATAL_DETAILS_VERBOSITY_LEVEL)
		ex.printStackTrace();
	}
    }

    /**
     * Displays a warning message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.WARNING_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     */
    public void logWarningMessage(String msg)
    {
	logWarningMessage(msg, null);
    } 

    /**
     * Displays a warning message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.WARNING_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     * @param location "location" of the associated call to this method
     *                  (e.g., the class and method name from which this call
     *                  was made) which is displayed at verbosity levels
     *                  4 and above
     */
    public void logWarningMessage(String msg, String location)
    {
	if(MainServer.getVerbosityLevel() >= MainServer.WARNING_VERBOSITY_LEVEL) {
	    String newMsg = msg;
	    if(location != null && MainServer.getVerbosityLevel() >= 4)
		if(msg.startsWith("\n"))
		    newMsg = new String("\n" + location + ": " + msg.substring(1));
		else
		    newMsg = new String("\n" + location + ": " + msg);
	    System.out.println("WARNING: " + newMsg);
	} 
    }
}
