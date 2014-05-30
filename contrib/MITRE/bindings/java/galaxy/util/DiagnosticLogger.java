/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

package galaxy.util;

/**
 * This is the diagnositc logging interface.
 */
public interface DiagnosticLogger
{    
    /**
     * Displays a message to System.out.
     *
     * @param msg the message to display
     * @param level the minimum verbosity level at which to display the message
     *              (verbosity levels are defined in 
     *              <code>galaxy.server.MainServer</code>)
     */
    public void logMessage(String msg, int level);

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
    public void logMessage(String msg, int level, String location);

    /**
     * Displays a message to System.out (at any verbosity level).
     *
     * @param msg the message to display
     */
    public void logMessage(String msg);

    /**
     * Displays an error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.ERROR_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     */
    public void logErrorMessage(String msg);

    /**
     * Displays an error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.ERROR_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     * @param ex the stack trace of this exception is also displayed
     */
    public void logErrorMessage(String msg, Exception ex);

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
    public void logErrorMessage(String msg, String location);

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
				       String location);

    /**
     * Displays a fatal error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.FATAL_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     */
    public void logFatalMessage(String msg);

    /**
     * Displays a fatal error message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.FATAL_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     * @param ex the stack trace of this exception is also displayed
     */
    public void logFatalMessage(String msg, Exception ex);

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
    public void logFatalMessage(String msg, String location);

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
				       String location);

    /**
     * Displays a warning message to System.err when the current verbosity
     * level is equal to or greater than 
     * <code>galaxy.server.MainServer.WARNING_VERBOSITY_LEVEL</code>.
     *
     * @param msg the message to display
     */
    public void logWarningMessage(String msg);

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
    public void logWarningMessage(String msg, String location);
}
