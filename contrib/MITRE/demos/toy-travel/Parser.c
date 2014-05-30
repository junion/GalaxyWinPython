/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "Parser_server.h"
#include "galaxy/server_functions.h"

#include "component_engine.h"

Gal_Frame Parse(Gal_Frame frame, void *server_data)
{
  char *input_string;  
  ParseTree *p;
  Gal_Frame reply;
  Gal_Frame parse_frame;

  /* Deconstruction and type checking */
  input_string = Gal_GetString(frame, ":input_string");
  if (!input_string) {
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "no input string");
    return (Gal_Frame) NULL;
  }

  /* Core processing */
  p = ParseSentence(input_string);

  /* Reply construction */
  if (!p) {
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "no parse");
    return (Gal_Frame) NULL;
  }
  parse_frame = ParseTreeToFrame(p);
  if (!parse_frame) {
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "can't convert parse to frame");
    return (Gal_Frame) NULL;
  }
  reply = Gal_MakeFrame("reply", GAL_CLAUSE);  
  Gal_SetProp(reply, ":frame", Gal_FrameObject(parse_frame));
  return reply;
}

/* Command line argument information */

static char *oas[] = {
  "-parser_data file",  "parser data", (char *) NULL,
  NULL
};

/* Server initialization function */

void *_GalSS_init_server(GalIO_ServerStruct *server, int argc, char **argv)
{
  char *data_file = (char *) NULL;
  
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0)	
    exit(1);
  
  if (GalUtil_OAExtract(argc, argv, oas, "-parser_data",
			GAL_OA_STRING, &data_file) == 0) {
    GalUtil_Warn("No -parser_data argument. Exiting.");
    exit(1);
  }
  InitializeParser(data_file);
  free(data_file);
  return (void *) NULL;
}
