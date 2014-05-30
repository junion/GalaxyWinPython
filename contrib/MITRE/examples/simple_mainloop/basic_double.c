/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"

#define SERVER_FUNCTIONS_INCLUDE "double_server.h"
#include "galaxy/server_functions.h"

/* In this example, we show how the MIT Communicator main loop
   basically works. This is to contrast it with the fd and timer
   examples in the remainder of this example. */

/* The main() here is essentially the same main() as in
   the Communicator library. */

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
