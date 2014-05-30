/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#ifndef WIN32
#include <signal.h>
#endif
#include <math.h>
#include "galaxy/galaxy_all.h"
#include "hub_internal.h"

HUB *Hub = NULL;

static char *oas[] = {
  "-debug", "run in debug mode",
  "-color", "color output",
  "-pgm_file file", "specify hub program file", NULL,
  "-init string", "hub variables to be added to welcome frame: -init \":domain \\\"Pegasus\\\"\"", NULL,
  "-validate", "validate server operation lists and message sends and returns against message signatures",
  "-locations locs", "space-delimited sequence of server@host(:location) overrides", NULL,
  "-server_locations_file file", "a file of lines server host:port [hub|server]", NULL,
  "-verbosity level", "set the verbosity of the Hub", NULL,
  "-gui host:port", "optional gui (host is <listener> to set up Hub listener)", NULL,
  "-suppress_pacifier", "don't print the pacifier dots",
  NULL
};

static void exit_for_purify(void)
{
  exit(0);
}

/* SAM 6/25/02: Let's add some support for using Insure
   to do Hub memory management analysis. */

#ifdef __INSURE__
#include <signal.h>
extern void _Gal_DestroySymTable();
extern int _Gal_FreeAllFrames();
extern void _GalIO_FreeReaderQueue();
extern int _Gal_FreeAllByteBuffers();
extern int _Gal_FreeAllObjects();
extern int _Gal_FreeAllPointerBuffers();
extern int _Gal_FreeAllVlists();
extern void _GalIO_FreeReaderQueue();

static void __Gal_Exit(int sig)
{
  printf("Requesting Hub exit.\n"); fflush(stdout);
  force_hub_exit();
}
#endif

extern struct SESSION *Sessions;

int main(int argc, char **argv, char **envp)
{
  char *ctl_file_name = NULL, *init_vars = NULL;
  int   debug = 0;
  int   color = 0;
  int   validate = 0;
  int   verbosity = 0;
  HUB  *hub = NULL;
  char *location_overrides = NULL;
  char *locations_file = NULL;
  int suppress_pacifier = 0;
  char *gui_location = (char *) NULL;

  Gal_InitializeStatics();

#ifdef __INSURE__  
  Gal_AddSignalHandler(SIGINT, __Gal_Exit);
#endif

  /* The HUB uses random numbers in its server selection.
     Let's not have the same pattern every time, please. */
  srand((unsigned int) time(NULL));

#ifndef WIN32
  signal(SIGUSR1, (void (*)())exit_for_purify);
#endif

  /* process command line arguments */
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0) {
    GalUtil_Fatal("Command line argument processing failed");
  }

  GalUtil_OAExtract(argc, argv, oas, "-debug", GAL_OA_INT, &debug);
  GalUtil_OAExtract(argc, argv, oas, "-color", GAL_OA_INT, &color);
  GalUtil_OAExtract(argc, argv, oas, "-pgm_file", GAL_OA_STRING, &ctl_file_name);
  GalUtil_OAExtract(argc, argv, oas, "-init", GAL_OA_STRING, &init_vars);
  GalUtil_OAExtract(argc, argv, oas, "-validate", GAL_OA_INT, &validate);
  /* We can specify both -locations and -server_locations_file. The
     file overrides everything else. */
  GalUtil_OAExtract(argc, argv, oas, "-locations", GAL_OA_STRING, &location_overrides);
  GalUtil_OAExtract(argc, argv, oas, "-server_locations_file", GAL_OA_STRING, &locations_file);
  GalUtil_OAExtract(argc, argv, oas, "-suppress_pacifier",
		    GAL_OA_INT, &suppress_pacifier);
  if (GalUtil_OAExtract(argc, argv, oas, "-verbosity",
			GAL_OA_INT, &verbosity)) {
    galutil_verbose = verbosity;
  }
  GalUtil_OAExtract(argc, argv, oas, "-gui",
		    GAL_OA_STRING, &gui_location);

  if (color)
    GalUtil_VerboseUseColor();
  else
    GalUtil_VerboseUseBW();

  if (ctl_file_name) {
    hub = new_hub(ctl_file_name, validate, debug, suppress_pacifier,
		  gui_location);
    if (!hub)
      GalUtil_Fatal("Problem creating new hub");
  } else {
    GalUtil_Fatal("No program file specified");
  }

  /* initialize the hub object */
  if (!init_hub(hub, init_vars,
		location_overrides,
		locations_file))
    GalUtil_Fatal("Problem initializing the hub with program file %s", ctl_file_name);    

  process_sessions(hub);

  quit_hub(hub);

  /* Because of reporting requirements, the sessions
     should be freed before the Hub. */
  
  while (Sessions) {
    _GalHUB_SessionUnlockAndEnd(hub, Sessions, 1);
  }
  
  if (hub)
    free_hub(hub);

#ifdef __INSURE__
  /* I'll need to free the program, and shut down 
     and free all the servers. */
  HC_FreePrograms();
  /* Now that all the connections are done, we can free the reader queue. */
  _GalIO_FreeReaderQueue();
  _Gal_FreeAllObjects();
  if (_Gal_FreeAllFrames()) {
    _Gal_DestroySymTable();
  }
  _Gal_FreeAllByteBuffers();
  _Gal_FreeAllPointerBuffers();
  _Gal_FreeAllVlists();
#endif

  exit(0);
}

/* 
  for Emacs...
  Local Variables:
  mode: C
  comment-column: 50
  fill-column: 110
  c-indent-level: 2
  c-continued-statement-offset: 2
  c-brace-offset: -2
  c-argdecl-indent: 2
  c-label-offset: -2
  End:
*/
