/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* SAM 8/21/00: Most of the bindings seem to be kind of
   unhappy with out args (and multiple in args corresponding to
   a single binding-level object), so I'm going to write some
   intermediate support stuff. Half of these correspond to
   array implementations for sequences, and the other half
   deal with outarg issues (like the fact that pointer
   dereferencing in Allegro is kind of opaque). */

#include "binding_support.h"

char *Gal_FrameToString(Gal_Frame fr, int how_to)
{
  int bufsize = 0;
  return Gal_PrintFrameToString(fr, (char *) NULL, &bufsize, how_to);
}

/* I'll use the Gal_PointerBuffer to handle this. */

Gal_PointerBuffer *GBGal_GetProperties(Gal_Frame fr)
{
  int num_strings = 0;
  char **strings = Gal_GetProperties(fr, &num_strings);

  /* Make a pointer buffer of length num_strings,
     manage the memory, don't make it dynamic, don't
     free the elements. */
  return Gal_MakePointerBuffer((void **) strings,
			       GAL_CHAR_STAR_PTYPE,
			       num_strings, num_strings,
			       1, 0, NULL, 0, 0);
}

/* And now I do the same thing for lists. */

Gal_Object GBGal_EmptyListObject()
{
  return Gal_CreateListObject((Gal_Object *) NULL, 0,
			       _gal_free_object, 1);
}

/* Finally, outargs of various sorts. */

static GBGal_ResultArray *__GBGal_NewResultArray ()
{
  return (GBGal_ResultArray *) calloc(1, sizeof(GBGal_ResultArray));
}

Gal_Frame GBGal_ResultArrayFrame(GBGal_ResultArray *r)
{
  return r->frame;
}

int GBGal_ResultArrayStatus(GBGal_ResultArray *r)
{
  return r->status;
}

GalIO_CommStruct *GBGal_ResultArrayCommStruct(GBGal_ResultArray *r)
{
  return r->comm_struct;
}

GalIO_MsgType GBGal_ResultArrayMsgType(GBGal_ResultArray *r)
{
  return r->msg_type;
}

void GBGal_FreeResultArray(GBGal_ResultArray *r)
{
  free(r);
}

GBGal_ResultArray *GBGalIO_CommReadFrame(GalIO_CommStruct *gcomm, int do_block)
{
  GBGal_ResultArray *r = __GBGal_NewResultArray();

  r->status = GalIO_CommReadFrame(gcomm, &(r->frame), do_block);
  return r;
}
  
GBGal_ResultArray *GBGalIO_CommReadMessage(GalIO_CommStruct *gcomm, int do_block)
{
  GBGal_ResultArray *r = __GBGal_NewResultArray();

  r->status = GalIO_CommReadMessage(gcomm, &(r->frame),
				    &(r->msg_type), do_block);
  return r;
}

GBGal_ResultArray *GBGalIO_DispatchViaHub(GalIO_CommStruct *gcomm, Gal_Frame frame)
{
  GBGal_ResultArray *r = __GBGal_NewResultArray();
  
  r->frame = GalIO_DispatchViaHub(gcomm, frame, &(r->msg_type));
  return r;
}

GBGal_ResultArray *GBGalSS_EnvDispatchFrame(GalSS_Environment *env,
					    Gal_Frame frame)
{
  GBGal_ResultArray *r = __GBGal_NewResultArray();

  r->frame = GalSS_EnvDispatchFrame(env, frame, &(r->msg_type));
  return r;
}

GBGal_ResultArray *GBGalSS_EnvDispatchFrameToProvider(GalSS_Environment *env,
						      Gal_Frame frame,
						      char *provider)
{
  GBGal_ResultArray *r = __GBGal_NewResultArray();

  r->frame = GalSS_EnvDispatchFrameToProvider(env, frame, provider, &(r->msg_type));
  return r;
}

GBGal_ResultArray *GBGalIO_ClientConnect(char *name,
					 char *host, unsigned short port,
					 int silent,
					 Gal_Frame welcome_frame)
{
  GBGal_ResultArray *r = __GBGal_NewResultArray();

  r->comm_struct = GalIO_ClientConnect(name, host, port, silent,
				       welcome_frame, &(r->frame));
  return r;
}

GBGal_ResultArray *GBGalIO_ServerCallbackHandler(GalIO_ServerStruct *gcomm, int read_blocking)
{
  GBGal_ResultArray *r = __GBGal_NewResultArray();

  r->status = GalIO_ServerCallbackHandler(gcomm, read_blocking,
					  &(r->comm_struct));
  return r;
}

/* And now, general support for more callback stuff. */

/* Since I'm going to do native argument parsing, I need to ensure
   that argc and argv don't do anything. I also need to handle
   the verbosity problem.
*/

extern
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
					      int do_initialize_defaults);

GalSS_ServerArgs *_GBGalSS_EncapsulateArguments(char *server_name,
						unsigned short server_port,
						int max_conns,
						int do_assert,
						int validate,
						int verbosity,
						int server_listen_status,
						char *client_pair_string,
						char *session_id,
						char *server_locations_file,
						char *slf_name)
{
  /* It's kind of important to have a server name. */
  if (!server_name)
    server_name = "<unknown>";
  return _GalSS_EncapsulateArguments(server_name,
				     server_port,
				     max_conns,
				     0,
				     do_assert,
				     GAL_LOOP_EXTERNAL,
				     validate,
				     verbosity,
				     server_listen_status,
				     client_pair_string,
				     session_id,
				     server_locations_file,
				     slf_name, 0);
}

GalIO_ServerStruct *
GBGalSS_SetupServer(GalSS_ServerArgs *arg_pkg)
{
  return GalSS_SetupServer(arg_pkg, 0, (char **) NULL);
}
