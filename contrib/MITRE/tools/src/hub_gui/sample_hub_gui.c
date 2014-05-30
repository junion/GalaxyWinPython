/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "sample_hub_gui_server.h"
#include "galaxy/server_functions.h"

/* 
   This is a (relatively empty) implementation of a Hub GUI server. It is
   meant to illustrate the required dispatch functions and the interface with
   the Hub's debugging mechanism (see hub_debug_prompt).
*/
   
Gal_Frame reinitialize(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame provider_status(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame add_menu_items(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame new_service_type(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame message_event(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame session_status(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame session_lock_status(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame session_alarm_status(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame token_status(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame listener_status(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame message_queue_status(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}

Gal_Frame hub_debug_prompt(Gal_Frame frame, void *server_data)
{ 
  Gal_Frame reply_frame = Gal_MakeFrame("hub_debug_cmd", GAL_CLAUSE);
  int buf_size = 256;
  char cmd[buf_size];
  char *help_msg = "C: disable debug and continue\nc: continue\nd: session DB\ne: exit Hub\ng: globals\nh: help\nl: locks\nm: message\nr: server\ns: session\nt: token\n";

  /* Get the debug command. */
  printf("%s", help_msg);
  printf("--> ");
  fflush(stdout);
  fgets(cmd, buf_size, stdin);

  Gal_SetProp(reply_frame, ":debug_cmd", Gal_StringObject(cmd));
  return reply_frame;
}

Gal_Frame hub_debug_info(Gal_Frame frame, void *server_data)
{
  return (Gal_Frame) NULL;
}
