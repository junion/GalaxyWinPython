/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "Generator_server.h"
#include "galaxy/server_functions.h"

#include "component_engine.h"

Gal_Frame Generate(Gal_Frame frame, void *server_data)
{
  Gal_Frame db_result;
  Gal_Frame output_frame;
  char *output_string = (char *) NULL;
  Gal_Frame reply;
  
  /* Deconstruction and type checking */
  output_frame = Gal_GetFrame(frame, ":output_frame");
  db_result = Gal_GetFrame(frame, ":db_result");

  if ((!output_frame) && (!db_result)) {
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "no DB result or output frame");
    return (Gal_Frame) NULL;
  }

  /* Core processing */
  if (output_frame) {
    ParseTree *p = FrameToParseTree(output_frame);

    if (!p) {
      GalSS_EnvError((GalSS_Environment *) server_data,
		     "can't convert output frame");
      return (Gal_Frame) NULL;
    }
    output_string = GenerateSentenceFromTree(p);
  } else if (db_result) {    
    DBResult *db = FrameToDBResult(db_result);

    if (!db) {
      GalSS_EnvError((GalSS_Environment *) server_data,
		     "can't convert DB result");
      return (Gal_Frame) NULL;
    }    
    output_string = GenerateSentenceFromDBResult(db);
  }

  /* Reply construction */
  if (!output_string) {
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "no output string");
    return (Gal_Frame) NULL;
  }
  reply = Gal_MakeFrame("reply", GAL_CLAUSE);  
  Gal_SetProp(reply, ":output_string", Gal_StringObject(output_string));
  return reply;
}

/* Command line argument information */

static char *oas[] = {
  "-generator_data file",  "generator data", (char *) NULL,
  NULL
};

/* Server initialization function */

void *_GalSS_init_server(GalIO_ServerStruct *server, int argc, char **argv)
{
  char *data_file = (char *) NULL;
  
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0)	
    exit(1);
  
  if (GalUtil_OAExtract(argc, argv, oas, "-generator_data",
			GAL_OA_STRING, &data_file) == 0) {
    GalUtil_Warn("No -generator_data argument. Exiting.");
    exit(1);
  }
  InitializeGenerator(data_file);
  free(data_file);
  return (void *) NULL;
}
