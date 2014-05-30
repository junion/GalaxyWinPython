/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "Backend_server.h"
#include "galaxy/server_functions.h"

#include "component_engine.h"

Gal_Frame Retrieve(Gal_Frame frame, void *server_data)
{
  char *sql_query;
  DBResult *db;
  Gal_Frame db_result_frame;

  /* Deconstruction and type checking */
  sql_query = Gal_GetString(frame, ":sql_query");
  if (!sql_query) {
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "no SQL query");
    return (Gal_Frame) NULL;
  }

  /* Core processing */
  db = RetrieveDBResult(sql_query);

  /* Reply construction */
  if (!db) {
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "no DB result");
    return (Gal_Frame) NULL;
  }
  db_result_frame = DBResultToFrame(db);
  if (!db_result_frame) {
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "can't convert DB result to frame");
    return (Gal_Frame) NULL;
  }
  return db_result_frame;
}

/* Command line argument information */

static char *oas[] = {
  "-backend_data file",  "backend data", (char *) NULL,
  NULL
};

/* Server initialization function */

void *_GalSS_init_server(GalIO_ServerStruct *server, int argc, char **argv)
{
  char *data_file = (char *) NULL;
  
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0)	
    exit(1);
  
  if (GalUtil_OAExtract(argc, argv, oas, "-backend_data",
			GAL_OA_STRING, &data_file) == 0) {
    GalUtil_Warn("No -backend_data argument. Exiting.");
    exit(1);
  }
  InitializeBackend(data_file);
  free(data_file);
  return (void *) NULL;
}
