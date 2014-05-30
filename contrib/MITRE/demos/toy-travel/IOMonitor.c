/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "IOMonitor_server.h"
#include "galaxy/server_functions.h"

#include <stdio.h>

Gal_Frame ReportIO(Gal_Frame frame, void *server_data)
{
  char *who = Gal_GetString(frame, ":who");
  char *utterance = Gal_GetString(frame, ":utterance");
  char *session = GalSS_EnvGetSessionID((GalSS_Environment *) server_data);

  if (!who) who = "somebody";
  
  if (!utterance) {
    printf("In session %s: %s said something.\n", session, who);
  } else {
    printf("In session %s: %s said \"%s\"\n", session, who, utterance);
  }
  fflush(stdout);
  return (Gal_Frame) NULL;
}
