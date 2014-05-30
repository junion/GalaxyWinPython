/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

// $Id: MainBinary.java,v 1.1.1.1 1999/08/31 15:19:48 sam Exp $
import galaxy.server.*;

public class MainBinary extends galaxy.server.MainServer {

    public MainBinary() {
	serverClassName = "BinaryData";
    }

    public static void main(String[] args) {
	MainBinary m = new MainBinary();
	m.setPort(17800);
	m.start();
    }
	
}
