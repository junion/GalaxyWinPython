/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "generic-server-internal.h"
#ifdef WIN32
/* For exit. */
#include <stdlib.h>
#endif

#include <stdio.h>

int main(int argc, char **argv)
{ 
  GalIO_ServerStruct *server;

  server = GalSS_CmdlineSetupServer(argc, argv);
  if (!server) {
    GalUtil_Fatal("Failed to set up server!\n");
  }
  GalSS_StartAndRunServer(server);
  exit(0);
}
