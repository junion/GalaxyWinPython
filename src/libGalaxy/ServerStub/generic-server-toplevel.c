/*
  Portions of this file (c) Copyright 1998 - 2000 M.I.T.
  Portions of this file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* SAM 9/20/99: the toplevel main loop has been moved to its own file,
   now that there are other exported functions in this file. */

#include <stdio.h>
#ifndef WIN32
#include <signal.h>
#endif
#include "galaxy/sysdep.h"

#if defined(__solaris__) && defined(GAL_THREADS)
/* For thr_setconcurrency() */
#include <thread.h>
#endif

#include "generic-server-internal.h"
#include "galaxy/program.h"

/* SAM 2/7/00: -thread won't work on non-Posix yet. */

static char *oas[] = {
  "-port port", "run as a server, listening for client connections on this port", NULL,
  "-assert", "exit unhappily if we fail to get the desired port",
  "-color", "enable color printing in a cxterm",
#ifdef GAL_THREADS
  "-thread", "use threads instead of timed tasks (experimental)",
#endif /* GAL_THREADS */
  "-nottloop", "do not use the timed_tasks_loop mechanism",
  "-ttloop", "obsolete -- maintained for compatibility only",
  "-maxconns max", "maximum number of connections for ttloop", "1",
  "-validate", "validate each message send and return against the message signature",
  "-verbosity level", "set the verbosity of this server", NULL,
  "-contact_hub \"host:port...\"", "run as client, contacting Hubs at the specified host and port pairs (overrides default port setting, but not -port)", NULL,
  "-server_locations_file file", "a file of lines server host:port [hub|server]", NULL,
  "-slf_name name", "if -server_locations_file is used, optional file index", NULL,
  "-session_id id", "if -contact_hub is used, lock this client to the specified Hub session", NULL,
  NULL
};

/* And these functions are for toplevel interaction when people
   write their own main loop. */

GalSS_ServerArgs *GalSS_DefaultServerArgs()
{
  GalSS_ServerArgs *arg_pkg;
  
  arg_pkg = (GalSS_ServerArgs *) calloc(1, sizeof(GalSS_ServerArgs));
  arg_pkg->server_listen_status = GAL_LISTENING_SERVER;
  arg_pkg->max_conns = 1;
  arg_pkg->verbosity = -1;
  arg_pkg->do_initialize_defaults = 1;
  arg_pkg->loop_type = GAL_LOOP_TT;
  /* All the rest are the moral equivalent of 0. */
  arg_pkg->oas_descr = GalUtil_OADigest(oas);
  return arg_pkg;
}

unsigned short GalSS_SAFixPort(GalSS_ServerArgs *arg_pkg, unsigned short port)
{
  unsigned short old_port = arg_pkg->server_port;
  
  arg_pkg->server_port = port;
  arg_pkg->port_disabled = 1;
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-port");
  return old_port;
}

char *GalSS_SAFixServerLocations(GalSS_ServerArgs *arg_pkg, const char *server_locations_file)
{
  char *loc_file = arg_pkg->server_locations_file;

  if (server_locations_file) {
    arg_pkg->server_locations_file = _gal_strdup(server_locations_file);
    /* The information represented here will be digested
       when the argument package is processed. */
  } else {
    arg_pkg->server_locations_file = (char *) NULL;
  }
  arg_pkg->server_locations_disabled = 1;
  /* If you have server locations, they override -port and -contact_hub. */
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-server_locations_file");
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-port");
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-contact_hub");
  return loc_file;
}

char *GalSS_SAFixSLFName(GalSS_ServerArgs *arg_pkg, const char *slf_name)
{
  char *old_slf_name = arg_pkg->slf_name;

  if (slf_name) {
    arg_pkg->slf_name = _gal_strdup(slf_name);
  } else {
    arg_pkg->slf_name = (char *) NULL;
  }
  arg_pkg->slf_disabled = 1;
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-slf_name");
  return old_slf_name;
}

int GalSS_SAFixMaxConns(GalSS_ServerArgs *arg_pkg, int max_conns)
{
  int old_max_conns = arg_pkg->max_conns;
  
  arg_pkg->max_conns = max_conns;
  arg_pkg->maxconns_disabled = 1;
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-maxconns");
  return old_max_conns;
}

int GalSS_SAFixVerbosity(GalSS_ServerArgs *arg_pkg, int verbosity)
{
  int old_verbosity = arg_pkg->verbosity;
  
  arg_pkg->verbosity = verbosity;
  arg_pkg->verbosity_disabled = 1;
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-verbosity");
  return old_verbosity;
}

int GalSS_SAFixColor(GalSS_ServerArgs *arg_pkg, int color)
{
  int old_color = arg_pkg->use_color;
  
  arg_pkg->use_color = color;
  arg_pkg->color_disabled = 1;
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-color");
  return old_color;
}

int GalSS_SAFixAssert(GalSS_ServerArgs *arg_pkg, int assert)
{
  int old_do_assert = arg_pkg->do_assert;
  
  arg_pkg->do_assert = assert;
  arg_pkg->assert_disabled = 1;
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-assert");
  return old_do_assert;
}

int GalSS_SAFixValidate(GalSS_ServerArgs *arg_pkg, int validate)
{
  int old_validate = arg_pkg->validate;
  
  arg_pkg->validate = validate;
  arg_pkg->validate_disabled = 1;
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-validate");
  return old_validate;
}

int GalSS_SAFixLoopType(GalSS_ServerArgs *arg_pkg, int loop_type)
{
  int old_loop_type = arg_pkg->loop_type;
  
  arg_pkg->loop_type = loop_type;
  arg_pkg->loop_type_disabled = 1;
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-nottloop");
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-ttloop");
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-thread");
  return old_loop_type;
}

int GalSS_SAFixServerListenStatus(GalSS_ServerArgs *arg_pkg,
				  int server_listen_status)
{
  int old_listen_status = arg_pkg->server_listen_status;
  
  arg_pkg->server_listen_status = server_listen_status;
  arg_pkg->server_listen_status_disabled = 1;
  if (!(server_listen_status & GAL_HUB_CLIENT)) {
    arg_pkg->contact_hub_info_disabled = 1;
    GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-contact_hub");
    GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-session_id");
  }
  return old_listen_status;
}

char *GalSS_SAFixContactHubInfo(GalSS_ServerArgs *arg_pkg,
				const char *client_pair_string,
				const char *session_id,
				char **old_session_id_ptr)
{
  char *old_client_pair_string = arg_pkg->client_pair_string;
  char *old_session_id = arg_pkg->session_id;
  
  if (client_pair_string) {
    arg_pkg->client_pair_string = _gal_strdup(client_pair_string);
  } else {
    arg_pkg->client_pair_string = (char *) NULL;
  }
  if (session_id) {
    arg_pkg->session_id = _gal_strdup(session_id);
  } else {
    arg_pkg->session_id = (char *) NULL;
  }
  arg_pkg->contact_hub_info_disabled = 1;
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-contact_hub");
  GalUtil_RemoveOASArg(arg_pkg->oas_descr, "-session_id");
  if (!old_session_id_ptr) {
    if (old_session_id)
      free(old_session_id);
  } else {
    *old_session_id_ptr = old_session_id;
  }
  return old_client_pair_string;
}

/* For backward compatibility. */

GalSS_ServerArgs *GalSS_ExtractServerArgs(int argc, char **argv,
					  int *new_argc_ptr,
					  char ***new_argv_ptr)
{
  return GalSS_ExtractCmdlineServerArgs((GalSS_ServerArgs *) NULL,
					argc, argv, new_argc_ptr,
					new_argv_ptr);
}

/* SAM 7/2/02: It occurs to me that it would be better from
   the point of view of encapsulation to move the code which
   processes the server location file into GalSS_ExtractCmdlineServerArgs,
   since what I'm really ultimately interested in is the contact_info.
   Unfortunately, since the default server args don't include
   the server name, I can't do that.

   Well, if you think hard enough...I've managed to set up
   something rickety which will allow me to extract the
   information in question relatively simply. */

GalIO_ServerStruct *_GalIO_CreateInitializationPrototype();

static void __GalSS_PostprocessContactInfo(GalSS_ServerArgs *arg_pkg)
{
  if (arg_pkg->server_locations_file) {
    
    /* SAM 7/2/02: We should do this standardization here, rather
       than later. I've set up a (rickety) mechanism by which
       we can use the existing API to retrieve the info
       that's usually stored by _GalSS_InitializeDefaults. */
    
    /* If we have a server locations file, we try to find
       the appropriate entry for this server. The name of
       the server must be retrieved by using _GalSS_InitializeDefaults.
       Oops, this isn't yet true in the bindings. Sigh.
       I'll have to give an option for the server name to be passed
       in to the function which encapsulates the arguments. Grrr.
    */
    
    /* SAM 4/19/02: New syntax for server locations argument. If
       there's a colon, treat it as the name of the entry. */
    
    char *server_name = arg_pkg->server_name;
    GalSS_ServerLocationEntry *server_loc = (GalSS_ServerLocationEntry *) NULL;
    char client_pair_buffer[1024];
    
    if ((!server_name) && arg_pkg->do_initialize_defaults) {
      GalIO_ServerStruct *s = _GalIO_CreateInitializationPrototype();
      
      _GalSS_InitializeDefaults(s);
      /* The server name is almost always static memory. */
      server_name = GalIO_GetServerName(s);
      free(s);
    }

    if (arg_pkg->slf_name) {
      server_name = arg_pkg->slf_name;
    }

    if (server_name) {    
      server_loc = GalSS_FindServerLocationEntry(arg_pkg->server_locations_file,
						 server_name);
    
      /* I could try to combine these with the command line, but
	 I think I'll just let it stand by itself. */
    
      if (server_loc) {
	/* If we find an appropriate entry, we should be able
	   to update the server_listen_status, etc. */
	switch (server_loc->hub_or_server) {
	case GAL_SL_HUB_LISTENS:
	  arg_pkg->server_listen_status = GAL_HUB_CLIENT;
	  sprintf(client_pair_buffer, "%s:%d",
		  server_loc->provider_spec.host,
		  server_loc->provider_spec.port);
	  if (arg_pkg->client_pair_string)
	    free(arg_pkg->client_pair_string);
	  arg_pkg->client_pair_string = _gal_strdup(client_pair_buffer);
	  break;
	case GAL_SL_SERVER_LISTENS:
	  arg_pkg->server_listen_status = GAL_CONNECTION_LISTENER;
	  arg_pkg->server_port = server_loc->provider_spec.port;
	  break;
	}
	_GalSS_FreeServerLocationEntry(server_loc);
      } else {
	GalUtil_Warn("Couldn't find entry for server %s in %s",
		     server_name, arg_pkg->server_locations_file);
      }
    } else {
      GalUtil_Warn("Couldn't determine server name to look up entry in %s",
		   arg_pkg->server_locations_file);
    }
  }
}

GalSS_ServerArgs *GalSS_ExtractCmdlineServerArgs(GalSS_ServerArgs *arg_pkg,
						 int argc, char **argv,
						 int *new_argc_ptr,
						 char ***new_argv_ptr)
{
  int new_argc, our_new_argc;
  char **new_argv,**our_new_argv;
  int use_ttloop;
  int use_threads = 0;
  char *hub_location = (char *) NULL;
  GalSS_ServerArgs *initial_arg_pkg = arg_pkg;
  char **local_oas;

  Gal_InitializeStatics();

  if (!arg_pkg) {
    arg_pkg = GalSS_DefaultServerArgs();
  }
  local_oas = arg_pkg->oas_descr->oas;
  
  if (!GalUtil_OASplitArgs(argc, argv, local_oas, NULL, &our_new_argc,
		     &our_new_argv, &new_argc, &new_argv)) {
    if (!initial_arg_pkg)
      GalSS_FreeArgPkg(arg_pkg);
    return (GalSS_ServerArgs *) NULL;
  }

  if (GalUtil_OACheckUsage(our_new_argc, our_new_argv, local_oas, NULL) == 0) {
    _GalSS_print_usage(argc, argv);    
    if (!initial_arg_pkg)
      GalSS_FreeArgPkg(arg_pkg);
    return (GalSS_ServerArgs *) NULL;
  }
    
  /* If we don't get a server port, make sure it uses the default. */
  if (!arg_pkg->port_disabled) {
    if (GalUtil_OAExtract(our_new_argc, our_new_argv, local_oas, "-port",
			  GAL_OA_SHORT, &(arg_pkg->server_port))) {
      /* Make sure it's a listener on top of whatever else. */
      arg_pkg->server_listen_status = arg_pkg->server_listen_status | GAL_LISTENING_SERVER;
    }
  }
  
  /* Now that we've extracted the port, extract the hub location. This
     will override the port if necessary. */
  if (!arg_pkg->contact_hub_info_disabled) {
    if (GalUtil_OAExtract(our_new_argc, our_new_argv, local_oas,
			  "-contact_hub",
			  GAL_OA_STRING, &hub_location)) {
      /* SAM 2/28/02: Now OAExtract is guaranteed to copy the string. */
      arg_pkg->client_pair_string = hub_location;
      if (!arg_pkg->server_listen_status_disabled) {
	if (arg_pkg->server_port != 0) {
	  /* The server port has been set on the command line. */
	  arg_pkg->server_listen_status = arg_pkg->server_listen_status | GAL_HUB_CLIENT | GAL_LISTENING_SERVER;
	} else {
	  /* DISABLE LISTENING. */
	  arg_pkg->server_listen_status = (arg_pkg->server_listen_status & ~GAL_LISTENING_SERVER) | GAL_HUB_CLIENT;
	}
      }
    }
  }

  if (!arg_pkg->server_locations_disabled) {
    /* SAM 2/28/02: OAExtract is now guaranteed to copy the string. */
    GalUtil_OAExtract(our_new_argc, our_new_argv, local_oas,
		      "-server_locations_file", GAL_OA_STRING,
		      &(arg_pkg->server_locations_file));
  }

  if (!arg_pkg->slf_disabled) {
    GalUtil_OAExtract(our_new_argc, our_new_argv, local_oas,
		      "-slf_name", GAL_OA_STRING,
		      &(arg_pkg->slf_name));
  } 

  if (!arg_pkg->maxconns_disabled) {
    GalUtil_OAExtract(our_new_argc, our_new_argv, local_oas, "-maxconns",
		      GAL_OA_INT, &(arg_pkg->max_conns));
  }

  if (!arg_pkg->color_disabled) {
    if (GalUtil_OAExtract(our_new_argc, our_new_argv, local_oas, "-color", GAL_OA_INT, NULL))
      arg_pkg->use_color = 1;
  }

  if (!arg_pkg->verbosity_disabled) {
    GalUtil_OAExtract(our_new_argc, our_new_argv, local_oas, "-verbosity",
	       GAL_OA_INT, &(arg_pkg->verbosity));
  }

  if (!arg_pkg->contact_hub_info_disabled) {
    /* SAM 2/28/02: OAExtract is now guaranteed to copy the string. */
    GalUtil_OAExtract(our_new_argc, our_new_argv, local_oas, "-session_id",
		      GAL_OA_STRING, &(arg_pkg->session_id));   
  }

  if (!arg_pkg->assert_disabled) {
    arg_pkg->do_assert = GalUtil_OAExtract(our_new_argc, our_new_argv,
				    local_oas, "-assert", GAL_OA_INT, NULL);
  }
  if (!arg_pkg->validate_disabled) {
    arg_pkg->validate = GalUtil_OAExtract(our_new_argc, our_new_argv,
				   local_oas, "-validate", GAL_OA_INT, NULL);
  }
  if (!arg_pkg->loop_type_disabled) {
    use_ttloop = !GalUtil_OAExtract(our_new_argc, our_new_argv,
			     local_oas, "-nottloop", GAL_OA_INT, NULL);
#ifdef GAL_THREADS
    use_threads = GalUtil_OAExtract(our_new_argc, our_new_argv,
			     local_oas, "-thread", GAL_OA_INT, NULL);
#endif /* GAL_THREADS */
    if (use_threads) {
      arg_pkg->loop_type = GAL_LOOP_THREADS;
    } else if (use_ttloop) {
      arg_pkg->loop_type = GAL_LOOP_TT;
    } else {
      arg_pkg->loop_type = GAL_LOOP_EXTERNAL;
    }
  }

  GalSS_FreeArgv(our_new_argc, our_new_argv);
  
  *new_argc_ptr = new_argc;
  *new_argv_ptr = new_argv;

  __GalSS_PostprocessContactInfo(arg_pkg);
  
  return arg_pkg;
}

void GalSS_FreeArgPkg(GalSS_ServerArgs *arg_pkg)
{
  if (arg_pkg->client_pair_string)
    free(arg_pkg->client_pair_string);
  if (arg_pkg->session_id)
    free(arg_pkg->session_id);
  if (arg_pkg->server_locations_file)
    free(arg_pkg->server_locations_file);
  if (arg_pkg->slf_name)
    free(arg_pkg->slf_name);
  if (arg_pkg->server_name)
    free(arg_pkg->server_name);
  _GalUtil_FreeOASDescription(arg_pkg->oas_descr);
  free(arg_pkg);
}

/* This could very well change. It's used almost exclusively
   by the bindings, except for one call by a deprecated function.

   SAM 8/9/02: I'm adding a slot to the argument packages which the
   bindings can use which will tell it not to bother calling
   _GalSS_InitializeDefaults. This is because I've added a warning
   to the default call (it should never be called, frankly),
   and I don't want it to be called by the bindings. I tried
   putting an overriding version in the binding support library,
   but that turned out to be an awful idea when it was time to do
   static linking. */

GalSS_ServerArgs *_GalSS_EncapsulateArguments(const char *server_name,
					      unsigned short server_port,
					      int max_conns,
					      int use_color,
					      int do_assert,
					      int loop_type,
					      int validate,
					      int verbosity,
					      int server_listen_status,
					      const char *client_pair_string,
					      const char *session_id,
					      const char *server_locations_file,
					      const char *slf_name,
					      int do_initialize_defaults)
{
  GalSS_ServerArgs *arg_pkg = GalSS_DefaultServerArgs();

  /* Added this for binding support. C doesn't use it yet. */
  if (server_name)
    arg_pkg->server_name = _gal_strdup(server_name);
  else
    arg_pkg->server_name = (char *) NULL;
  arg_pkg->server_port = server_port;
  arg_pkg->max_conns = max_conns;
  arg_pkg->use_color = use_color;
  arg_pkg->do_assert = do_assert;
  arg_pkg->loop_type = loop_type;
  arg_pkg->validate = validate;
  arg_pkg->verbosity = verbosity;  
  arg_pkg->server_listen_status = server_listen_status;
  if (client_pair_string)
    arg_pkg->client_pair_string = _gal_strdup(client_pair_string);
  else
    arg_pkg->client_pair_string = (char *) NULL;
  if (session_id)
    arg_pkg->session_id = _gal_strdup(session_id);
  else
    arg_pkg->session_id = (char *) NULL;
  if (server_locations_file)
    arg_pkg->server_locations_file = _gal_strdup(server_locations_file);
  else
    arg_pkg->server_locations_file = (char *) NULL;
  if (slf_name)
    arg_pkg->slf_name = _gal_strdup(slf_name);
  else
    arg_pkg->slf_name = (char *) NULL;
  arg_pkg->do_initialize_defaults = do_initialize_defaults;
  __GalSS_PostprocessContactInfo(arg_pkg);
  return arg_pkg;
}

/* Deprecated, dammit. I can't expand the command line without
   changing the signature of this function. No one should use it. */

GalIO_ServerStruct *GalSS_InitializeServerToplevel(unsigned short server_port,
						   int max_conns,
						   int use_color,
						   int do_assert,
						   int loop_type,
						   int validate,
						   int verbosity,
						   int server_listen_status,
						   const char *client_pair_string,
						   const char *session_id,
						   int new_argc,
						   char **new_argv)
{
  GalSS_ServerArgs *arg_pkg;
  GalIO_ServerStruct *s;

  arg_pkg = _GalSS_EncapsulateArguments((char *) NULL,
					server_port, max_conns,
					use_color, do_assert,
					loop_type, validate,
					verbosity, server_listen_status,
					client_pair_string, session_id,
					(char *) NULL, (char *) NULL, 1);

  s = GalSS_InitializeServerFromServerArgs(arg_pkg, new_argc, new_argv);
  GalSS_FreeArgPkg(arg_pkg);
  return s;
}

GalIO_ServerStruct *GalSS_CmdlineInitializeServer(int argc, char **argv)
{
  GalIO_ServerStruct *s = GalSS_CmdlineSetupServer(argc, argv);

  if (!s) {
    return (GalIO_ServerStruct *) NULL;
  } 
  return GalIO_ServerStart(s);
}

void GalSS_FreeArgv(int argc, char **argv)
{
  int i;
  
  for (i = 0; i < argc; i++) 
    free(argv[i]);	
  free(argv);
}

GalIO_ServerStruct *GalSS_CmdlineSetupServer(int argc, char **argv)
{
  int new_argc;
  char **new_argv;
  GalSS_ServerArgs *arg_pkg;
  GalIO_ServerStruct *s;

  arg_pkg = GalSS_ExtractCmdlineServerArgs((GalSS_ServerArgs *) NULL,
					   argc, argv, &new_argc, &new_argv);

  if (!arg_pkg) {
    /* Something bad happened. */
    return (GalIO_ServerStruct *) NULL;
  }

  s = GalSS_SetupServer(arg_pkg, new_argc, new_argv);  

  GalSS_FreeArgPkg(arg_pkg);
  /* SAM 5/20/02: I'd like to do this, but there's always the
     possibility that someone saved away the contents of any of this
     argument array, or the array itself.
     GalSS_FreeArgv(new_argc, new_argv); */
  return s;
}

/* SAM 9/21/99: Encapsulated the behavior which sets up a server
   in order to expose it. */

static void exit_for_purify(int sig)
{
  exit(0);
}

/* For backward compatibility. Doesn't enable listener-in-Hub or
   verbosity. Also, I can't expand the command line without
   changing the signature of this function. No one should use it.
*/

GalIO_ServerStruct *GalSS_InitializeServer(unsigned short server_port,
					   int max_conns, int use_color,
					   int do_assert, int use_ttloop,
					   int validate,
					   int new_argc, char **new_argv)
{
  return GalSS_InitializeServerToplevel(server_port, max_conns,
					use_color, do_assert,
					use_ttloop, validate,
					-1, GAL_LISTENING_SERVER,
					(char *) NULL, (char *) NULL,
					new_argc, new_argv);
}

extern void
_GalIO_ServerRequirePort(GalIO_ServerStruct *scomm, int require_port);

GalIO_ServerStruct *GalSS_SetupServer(GalSS_ServerArgs *arg_pkg,
				      int new_argc, char **new_argv)
{
  unsigned short server_port = arg_pkg->server_port;
  int max_conns = arg_pkg->max_conns;
  int use_color = arg_pkg->use_color;
  int do_assert = arg_pkg->do_assert;
  int loop_type = arg_pkg->loop_type;
  int validate = arg_pkg->validate;
  int verbosity = arg_pkg->verbosity;
  int server_listen_status = arg_pkg->server_listen_status;
  char *client_pair_string = arg_pkg->client_pair_string;
  char *session_id = arg_pkg->session_id;
  void *server_data;
  GalIO_ServerStruct *new_server;

  Gal_InitializeStatics();

  /* -1 assumes it's handled elsewhere. */
  
  if (use_color > 0)    
    GalUtil_VerboseUseColor();
  else if (use_color == 0)
    GalUtil_VerboseUseBW();

  /* -1 means it's the default. */

  if (verbosity > -1)
    galutil_verbose = verbosity;

  /* We will still use Gal_AddTask in the thread case. */

  /* If server_listen_status is 1, GalIO_ServerCreate will still do the right thing,
     although some of it will turn out to be superfluous. */     
  
  if (loop_type != GAL_LOOP_EXTERNAL) {    
    new_server = GalIO_ServerCreate(0, do_assert, GalSS_FrameHandler, (void *) NULL, 0, 0);
  } else {
    new_server = GalIO_ServerCreate(0, do_assert, GalSS_FrameHandler, (void *) NULL, -1, 0);
  }
  
  if (new_server == NULL)
    return (GalIO_ServerStruct *) NULL;

  if (arg_pkg->do_initialize_defaults)
    _GalSS_InitializeDefaults(new_server);

  /* Just in case the defaults aren't run. In the bindings, for instance. */

  if (arg_pkg->server_name)
    GalIO_SetServerName(new_server, _gal_strdup(arg_pkg->server_name));

  /* We set max conns and server port after the defaults to
     make sure that the defaults don't override the
     command line. Make sure the right things happen for acting as client.
     I've done all the computation concerning the server location
     files when I digest the command line arguments. */

  GalIO_SetServerListenStatus(new_server,
			      server_listen_status,
			      client_pair_string,
			      session_id);

  if (max_conns > 0) {
    GalIO_SetServerMaxConnections(new_server, max_conns);
  }

  if (server_port != 0) {
    GalIO_SetServerDefaultPort(new_server, server_port);
  }

  if (loop_type == GAL_LOOP_THREADS) {
    /* Enable threading. */
    Gal_EnableTimedTaskThreads();
  }

  if (validate)
    GalIO_EnableDispatchFnValidation(new_server);
  
  /* server-specific initialization: _GalSS_init_server should exit on error */

  if (!Gal_SignalsInitialized())
    Gal_InitializeSignals();
#ifndef WIN32
  Gal_AddSignalHandler(SIGUSR1, exit_for_purify);
#endif

  /* I've deprecated using the return value from _GalSS_init_server
     to set the server data. So if you set it in the init function
     itself, we'd better not reset it to NULL. So only update
     if the value is non-NULL. */
  server_data = _GalSS_init_server(new_server, new_argc, new_argv);

  if (server_data) {
    GalIO_SetServerData(new_server, server_data, NULL);
  }
  return new_server;
}

GalIO_ServerStruct *
GalSS_InitializeServerFromServerArgs(GalSS_ServerArgs *arg_pkg,
				     int new_argc, char **new_argv)
{
  GalIO_ServerStruct *new_server = GalSS_SetupServer(arg_pkg, new_argc,
						     new_argv);
  if (!new_server) {
    return (GalIO_ServerStruct *) NULL;
  } else {
    return GalIO_ServerStart(new_server);
  }
}

/* Returns 1 if the server has run successfully, 0 if
   it failed to start up. */

int GalSS_StartAndRunServer(GalIO_ServerStruct *server)
{  
  if (!GalIO_ServerStart(server)) {
    /* SAM 2/28/02: Turns out that GalIO_ServerStart
       takes care of the destruction of the server. */
    GalUtil_Warn("Server startup failed");
    return 0;
  } else {
    GalSS_RunServer(server);
    return 1;
  }
}

/* SAM 9/28/99: If we're running threads, we have to make
   sure that the process doesn't exit until the server is
   destroyed. In general, what that means is that we
   loop forever. The right thing to do, it seems, is to exit
   the main thread. We can't join on the server thread, because
   the only thing that shuts things down is C-c at the console... */

/* SAM 2/28/02: I'm going to add some support here for what happens
   when Insure++ is running. I want to make sure that all the memory
   is freed, and I don't want to do it in umpteen places. */

#ifdef __INSURE__
#include <signal.h>
extern void _Gal_DestroySymTable();
extern int _Gal_FreeAllFrames();
extern void _GalIO_FreeReaderQueue();
extern int _Gal_FreeAllByteBuffers();
extern int _Gal_FreeAllObjects();
extern int _Gal_FreeAllPointerBuffers();
extern int _Gal_FreeAllVlists();
#endif

void GalSS_RunServer(GalIO_ServerStruct *server)
{
  /* If timed tasks are configured to run, then if threads
     are enabled, the timed task handler will start a thread
     for each task, so we can exit. If threads aren't enabled,
     then we run the timed task loop. */

#if defined(GAL_THREADS) && defined(__solaris__)
  /* Apparently, Solaris 2.5 doesn't timeslice threads, so
     you need to boost the concurrency level. */
  thr_setconcurrency(2);
#endif
  
  if (GalIO_ServerUsesTimedTasks(server)) {
    if (!Gal_TimedTaskThreadsEnabled()) {
      Gal_TimedTasksLoop();
    } else {
      /* It's a bad plan to exit the main thread, since the caller might
	 want to do some cleanup. But we want to duplicate the situation
	 where the system returns from this function only when the
	 timed task loop is terminated. So we define a waiter.

	 I don't need to conditionalize this on GAL_PTHREADS, because
	 Gal_EnableTimedTaskThreads() takes care of that. */
      Gal_TimedTaskLoopThreadWaiter();
    }
  }
#ifdef __INSURE__
  GalIO_SetServerDone(server);
  GalIO_DestroyServerStruct(server);
  _GalIO_FreeReaderQueue();
  _Gal_FreeAllObjects();
  if (_Gal_FreeAllFrames()) {
    _Gal_DestroySymTable();
  }
  _Gal_FreeAllByteBuffers();
  _Gal_FreeAllPointerBuffers();
  _Gal_FreeAllVlists();
#endif
}

/* 
 * Starting an individual connection
 */

/* SAM 7/2/02: Sometimes we want to contact the Hub from a server
   using an individual connection. We can try to do this using an
   environment (which will generate a permanently locked session), or
   using a bare connection. If we provide no server, a minimal
   server will be set up (no listener, etc.). We will need to provide
   the looping (internal vs. external) and other properties. */

/* On the command line, I want to be able to control the session,
   host/port, and loop type, at least. How can I set this up so that
   it's still clean, but it interacts appropriately with command
   line arguments, when present? */

/* There are three steps to getting started with the server:
   setup (digest the arguments), start (configure the callbacks),
   and run. In this case, we need to do setup and start before
   we add the connection, so the connection gets added
   correctly. */

GalIO_ServerStruct *
GalSS_CmdlineSetupServerForHubContact(int argc, char **argv,
				      char **client_string_ptr,
				      char **session_id_ptr,
				      int allow_listener,
				      int client_poll_flags,
				      int loop_type)
{
  int new_argc;
  char **new_argv;
  GalSS_ServerArgs *arg_pkg, *final_arg_pkg;
  GalIO_ServerStruct *s;
  char *old_client_string = (char *) NULL;
  char *old_session_id = (char *) NULL;

  arg_pkg = GalSS_DefaultServerArgs();
    
  if (loop_type > -1) {
    GalSS_SAFixLoopType(arg_pkg, loop_type);
  }

  /* Fix the client poll flags. */
  if (client_poll_flags > -1) {
    if (!allow_listener) {
      /* The only flags should be GAL_HUB_CLIENT_ flags. */
      if (client_poll_flags &
	  ~(GAL_HUB_CLIENT_CONNECT_FAILURE_MASK |
	    GAL_HUB_CLIENT_DISCONNECT_MASK |
	    GAL_HUB_CLIENT)) {
	GalUtil_Warn("Ignoring poll flags not relevant to Hub client");
	client_poll_flags = client_poll_flags & GAL_HUB_CLIENT_CONNECT_FAILURE_MASK & GAL_HUB_CLIENT_DISCONNECT_MASK;
      }
    }
    client_poll_flags = client_poll_flags | GAL_HUB_CLIENT;
  } else {
    client_poll_flags = GAL_HUB_CLIENT |
      GAL_HUB_CLIENT_CONNECT_FAILURE_NOOP |
      GAL_HUB_CLIENT_DISCONNECT_NOOP;
  }

  /* This will not affect adding the listener with -port
     if it appears. */
  GalSS_SAFixServerListenStatus(arg_pkg, client_poll_flags);

  if (!allow_listener) {
    /* Disable -port, since it's only used for listening. */
    GalSS_SAFixPort(arg_pkg, 0);
    /* Ditto -assert. */
    GalSS_SAFixAssert(arg_pkg, 0);
    /* Ditto -maxconns. */
    GalSS_SAFixMaxConns(arg_pkg, 0);
  }

  final_arg_pkg = GalSS_ExtractCmdlineServerArgs(arg_pkg, argc, argv,
						 &new_argc, &new_argv);

  if (!final_arg_pkg) {
    /* Something bad happened. */
    if (arg_pkg)
      GalSS_FreeArgPkg(arg_pkg);    
    return (GalIO_ServerStruct *) NULL;
  }

  /* Now, before we actually create the server, retrieve and remove the
     info about host, port, and session. All the right things
     will have happened with respect to the server location file. */
  old_client_string = GalSS_SAFixContactHubInfo(final_arg_pkg, (char *) NULL,
						(char *) NULL,
						&old_session_id);
  if (client_string_ptr) {
    *client_string_ptr = old_client_string;
  } else if (old_client_string) {
      free(old_client_string);
  }

  if (session_id_ptr) {
    *session_id_ptr = old_session_id;
  } else if (old_session_id) {
    free(old_session_id);
  }

  /* Now, and only now, set up the server. */
  s = GalSS_SetupServer(final_arg_pkg, new_argc, new_argv);

  GalSS_FreeArgPkg(final_arg_pkg);
  /* SAM 5/20/02: I'd like to do this, but there's always the
     possibility that someone saved away the contents of any of this
     argument array, or the array itself.
     GalSS_FreeArgv(new_argc, new_argv); */
  return s;
}

static void __galio_terminate_gcomm(GalIO_CommStruct *scomm,
				    void *caller_data)
{
  Gal_EndTasks(1);
}

static GalIO_CommStruct *
__GalSS_ISetupConnection(const char *host, unsigned short port,
			 char **session_id,
			 char *retrieved_contact_info,
			 char *retrieved_session_id,
			 GalIO_ServerStruct *scomm,
			 int shutdown_after,
			 int use_session_id)
{
  char *host_to_use = (char *) host;
  GalIO_ServerLocation *locs = (GalIO_ServerLocation *) NULL;
  char *session_id_to_use = (char *) NULL;
  GalIO_CommStruct *gcomm = (GalIO_CommStruct *) NULL;

  if (use_session_id)
    session_id_to_use = *session_id;
        
  if (retrieved_contact_info) {
    if (host_to_use || (port > 0)) {
      GalUtil_Warn("Ignoring value for -contact_hub");
      free(retrieved_contact_info);
    } else {
      /* Get the data out of the server locations. There had
	 better be only one of them. */
      locs = GalIO_DigestServerLocations(retrieved_contact_info);
      
      if (GalIO_NumServerLocations(locs) != 1) {
	GalUtil_Warn("Number of server locations must be exactly one");
	if (locs) GalIO_FreeServerLocations(locs);
	return (GalIO_CommStruct *) NULL;
      }
      host_to_use = GalIO_NthHostAndPort(locs, 0, &port);	
    }
  }

  if (retrieved_session_id && use_session_id)
    session_id_to_use = retrieved_session_id;

  if (use_session_id && !session_id_to_use) {
    /* Fix the session ID. */
    struct timeval now;
    char new_session_id[256];
    
    _gal_gettimeofday(&now);
    sprintf(new_session_id, "session-%d.%.3d",
	    (int) now.tv_sec, (int) (now.tv_usec / 1000));
    session_id_to_use = _gal_strdup(new_session_id);
  }

  /* At this point, we're guaranteed of having a server, and we
     have the appropriate host, port and session ID. Now, we
     can create the connection. */
  
  gcomm = GalIO_ContactHub(host_to_use, port, scomm,
			   session_id_to_use, -1);

  if (gcomm) {
    /* Now, run it through the verification handler. */
    while (1) {
      int res = GalIO_VerificationHandler(gcomm);

      if (res == -1) {
	GalUtil_Warn("Failed to confirm connection");
	GalIO_SetCommDone(gcomm);
	GalIO_DestroyCommStruct(gcomm);
	gcomm = (GalIO_CommStruct *) NULL;
      } else if (res == 1) {
	break;
      }
    }
    /* Once we've set up the connection, and we know
       it's good, we add a callback to shut down the
       main loop when the connection's done, maybe. */
    if (shutdown_after) {
      GalIO_AddConnectionCallback(gcomm, 
				  GAL_CONNECTION_DESTRUCTION_EVENT,
				  __galio_terminate_gcomm,
				  (void *) NULL);
    }
  }
  
  if (use_session_id) {
    *session_id = session_id_to_use;
  }

  if (locs) GalIO_FreeServerLocations(locs);
  return gcomm;
}

GalIO_CommStruct *GalSS_SetupConnection(const char *host,
					unsigned short port,
					char *retrieved_contact_info,
					int shutdown_after,
					GalIO_ServerStruct *scomm)
{
  char *session_id = (char *) NULL;
  return __GalSS_ISetupConnection(host, port, &session_id,
				  retrieved_contact_info,
				  (char *) NULL,
				  scomm, shutdown_after, 0);
}

/* The difference with the environment is that it will
   always set up a session ID, which means it will always
   be locked to a session. */

GalSS_Environment *GalSS_SetupEnvironment(const char *host,
					  unsigned short port,
					  const char *session_id,
					  char *retrieved_contact,
					  char *retrieved_session,
					  int shutdown_after,
					  GalIO_ServerStruct *scomm)
{
  char *session_id_to_use = (char *) session_id;
  GalIO_CommStruct *gcomm;

  gcomm = __GalSS_ISetupConnection(host, port, &session_id_to_use,
				   retrieved_contact,
				   retrieved_session,
				   scomm, shutdown_after, 1);
  if (gcomm) {
    GalSS_Environment *env = GalSS_EnvCreate(gcomm);
    GalSS_EnvUpdateSessionID(env, session_id_to_use);
    GalSS_EnvLock(env);
    return env;
  } else {
    return (GalSS_Environment *) NULL;
  }
}							    
