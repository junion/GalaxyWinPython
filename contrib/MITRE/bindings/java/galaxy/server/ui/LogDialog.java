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

package galaxy.server.ui;

/*
		A basic implementation of the JDialog class.
*/

import java.io.PrintStream;
import java.io.FileOutputStream;

import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Color;
import java.awt.Font;
import java.awt.Insets;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JOptionPane;

public class LogDialog extends JDialog
{
    LogDialogListener listener;
    
	public LogDialog()
	{
		GridBagLayout gridBagLayout;
		gridBagLayout = new GridBagLayout();
		getContentPane().setLayout(gridBagLayout);
		setVisible(false);
		setSize(413,129);
		titleLabel = new JLabel();
		titleLabel.setText("Logfile control");
		titleLabel.setBounds(166,10,81,15);
		titleLabel.setFont(new Font("Dialog", Font.BOLD, 12));
		titleLabel.setForeground(new Color(-10066279));
		titleLabel.setBackground(new Color(-3355444));
		GridBagConstraints gbc;
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weighty = 1.0;
		gbc.anchor = GridBagConstraints.NORTH;
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(10,0,20,0);
		((GridBagLayout)getContentPane().getLayout()).setConstraints(titleLabel, gbc);
		getContentPane().add(titleLabel);
		logFileLabel = new JLabel();
		logFileLabel.setText("Log file:");
		logFileLabel.setBounds(10,64,44,15);
		logFileLabel.setFont(new Font("Dialog", Font.BOLD, 12));
		logFileLabel.setForeground(new Color(-10066279));
		logFileLabel.setBackground(new Color(-3355444));
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(0,10,0,0);
		((GridBagLayout)getContentPane().getLayout()).setConstraints(logFileLabel, gbc);
		getContentPane().add(logFileLabel);
		logFileField = new JTextField();
		logFileField.setMargin(new Insets(0,0,0,0));
		logFileField.setBounds(65,59,221,25);
		logFileField.setFont(new Font("SansSerif", Font.PLAIN, 12));
		logFileField.setForeground(new Color(0));
		logFileField.setBackground(new Color(16777215));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.weightx = 1.0;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(0,10,0,0);
		((GridBagLayout)getContentPane().getLayout()).setConstraints(logFileField, gbc);
		getContentPane().add(logFileField);
		startStopButton = new JButton();
		startStopButton.setText("Start logging");
		startStopButton.setActionCommand("Start logging");
		startStopButton.setBounds(296,59,107,25);
		startStopButton.setFont(new Font("Dialog", Font.BOLD, 12));
		startStopButton.setForeground(new Color(0));
		startStopButton.setBackground(new Color(-3355444));
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(0,10,0,10);
		((GridBagLayout)getContentPane().getLayout()).setConstraints(startStopButton, gbc);
		getContentPane().add(startStopButton);
		appendLabel = new JLabel();
		appendLabel.setText("append:");
		appendLabel.setBounds(10,99,45,15);
		appendLabel.setFont(new Font("Dialog", Font.BOLD, 12));
		appendLabel.setForeground(new Color(-10066279));
		appendLabel.setBackground(new Color(-3355444));
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(0,10,0,0);
		((GridBagLayout)getContentPane().getLayout()).setConstraints(appendLabel, gbc);
		getContentPane().add(appendLabel);
		appendCheckBox = new JCheckBox();
		appendCheckBox.setBounds(65,95,23,23);
		appendCheckBox.setFont(new Font("Dialog", Font.BOLD, 12));
		appendCheckBox.setForeground(new Color(0));
		appendCheckBox.setBackground(new Color(-3355444));
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(0,10,0,0);
		((GridBagLayout)getContentPane().getLayout()).setConstraints(appendCheckBox, gbc);
		getContentPane().add(appendCheckBox);
		closeButton = new JButton();
		closeButton.setText("Close");
		closeButton.setActionCommand("Close");
		closeButton.setBounds(316,94,67,25);
		closeButton.setFont(new Font("Dialog", Font.BOLD, 12));
		closeButton.setForeground(new Color(0));
		closeButton.setBackground(new Color(-3355444));
		gbc = new GridBagConstraints();
		gbc.gridx = 2;
		gbc.gridy = 2;
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(10,10,10,10);
		((GridBagLayout)getContentPane().getLayout()).setConstraints(closeButton, gbc);
		getContentPane().add(closeButton);

		SymWindow aSymWindow = new SymWindow();
		this.addWindowListener(aSymWindow);
		SymAction lSymAction = new SymAction();
		startStopButton.addActionListener(lSymAction);
		closeButton.addActionListener(lSymAction);
	}

    void addLogDialogListener(LogDialogListener l)
    {
        listener = l;
    }

    void setLogFile(String logFile)
    {
        logFileField.setText(logFile);
    }
    
    void setAppend(boolean append)
    {
        appendCheckBox.setSelected(append);
    }

    void setLogging(boolean logging)
    {
        appendCheckBox.setEnabled(!logging);
        logFileField.setEnabled(!logging);
        if (logging) {
            startStopButton.setText("Stop logging");
            startStopButton.setActionCommand("Stop logging");
        } else {
            startStopButton.setText("Start logging");
            startStopButton.setActionCommand("Start logging");
        }
    }

	public void setVisible(boolean b)
	{
		super.setVisible(b);
	}

	static public void main(String args[])
	{
		(new LogDialog()).setVisible(true);
	}

	public void addNotify()
	{
		// Record the size of the window prior to calling parents addNotify.
		Dimension d = getSize();

		super.addNotify();

		if (fComponentsAdjusted)
			return;
		// Adjust components according to the insets
		setSize(getInsets().left + getInsets().right + d.width, getInsets().top + getInsets().bottom + d.height);
		Component components[] = getContentPane().getComponents();
		for (int i = 0; i < components.length; i++)
		{
			Point p = components[i].getLocation();
			p.translate(getInsets().left, getInsets().top);
			components[i].setLocation(p);
		}
		fComponentsAdjusted = true;
	}

	// Used for addNotify check.
	boolean fComponentsAdjusted = false;

	JLabel titleLabel;
	JLabel logFileLabel;
	JTextField logFileField;
	JButton startStopButton;
	JLabel appendLabel;
	JCheckBox appendCheckBox;
	JButton closeButton;

	private class SymWindow extends WindowAdapter
	{
		public void windowClosing(WindowEvent event)
		{
			Object object = event.getSource();
			if (object == LogDialog.this)
				JDialog1_WindowClosing(event);
		}
	}

	void JDialog1_WindowClosing(WindowEvent event)
	{
        closing();
	}

	private class SymAction implements ActionListener
	{
		public void actionPerformed(ActionEvent event)
		{
			Object object = event.getSource();
			if (object == startStopButton)
				startStopButton_actionPerformed(event);
			else if (object == closeButton)
				closeButton_actionPerformed(event);
		}
	}

	void startStopButton_actionPerformed(ActionEvent event)
	{
	    PrintStream logPrintStream;
	    boolean append = appendCheckBox.isSelected();
	    String logFile = logFileField.getText();
	    
	    if (startStopButton.getActionCommand().equals("Start logging")) {
	        try {
                FileOutputStream fos = new FileOutputStream(logFile, append);
                logPrintStream = new PrintStream(fos);
            } catch(Exception ex) {
                JOptionPane.showMessageDialog(this, "Can not open " + logFile + "\n" + ex,
					      "Log file error", JOptionPane.ERROR_MESSAGE);
                return;
            }
	        
	        setLogging(true);
	    } else {
	        logPrintStream = null;
	        setLogging(false);
	    }
        startStopButton.invalidate();
        startStopButton.validate();

		if (listener != null) {
		    listener.logDialogOptionsSet(logFile, append, logPrintStream);
		}
	}

	void closeButton_actionPerformed(ActionEvent event)
	{
	    closing();
	}
	
	void closing()
	{
		setVisible(false); // hide the Frame
		dispose();	     // free the system resources
		if (listener != null) {
		    listener.logDialogClosed();
		}
	}
}
