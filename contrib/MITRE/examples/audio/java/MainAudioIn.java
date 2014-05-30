/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

import galaxy.server.*;

public class MainAudioIn extends galaxy.server.MainServer
{

    public MainAudioIn(String name, String[] args, int port) 
    {
	super(name, args, port);
	serverClassName = "AudioIn";
    }

    public static void main(String[] args) 
    {
	MainAudioIn m = new MainAudioIn("testaudio_receive", args, 12346);
	m.start();
    }
}
