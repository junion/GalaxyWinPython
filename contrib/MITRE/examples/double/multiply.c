/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "multiply_server.h"
#include "galaxy/server_functions.h"

#include <limits.h>
#include <signal.h>

static int Factor = 1;

Gal_Frame multiply(Gal_Frame frame, void *server_data)
{
  int i = Gal_GetInt(frame, ":int");

  if (i == 0) {
    /* We'll loop forever. */
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "i is 0");
    return (Gal_Frame) NULL;
  } else if ((INT_MAX / i) < Factor) {
    /* If we're about to overflow... */
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "multiply would overflow MAXINT");
    return (Gal_Frame) NULL;
  } else {
    Gal_SetProp(frame, ":int",
		Gal_IntObject(Factor * i));
    return frame;
  }
}

Gal_Frame reinitialize(Gal_Frame frame, void *server_data)
{
  if (Gal_GetObject(frame, ":factor")) {
    Factor = Gal_GetInt(frame, ":factor");
  }
  return frame;
}

