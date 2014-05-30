/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

import galaxy.server.*;

public class MainAudioOut extends galaxy.server.MainServer 
{

    public MainAudioOut(String name, String[] args, int port) 
    {
	super(name, args, port);
	serverClassName = "AudioOut";
    }

    public static void main(String[] args) 
    {
	MainAudioOut m = new MainAudioOut("testaudio_send", args, 12345);
	m.start();
    }
}
