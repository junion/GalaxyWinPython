/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef __BINDING_SUPPORT_H__
#define __BINDING_SUPPORT_H__

#include <stdlib.h>
#include "galaxy/galaxy_all.h"

char *Gal_FrameToString(Gal_Frame fr, int how_to);

/* It doesn't look to me like it's very easy to deal with
   lists of properties, for example, as returned in two variables.
   So I'll package it up a little here. */

Gal_PointerBuffer *GBGal_GetProperties(Gal_Frame fr);

/* Lists, now. */

Gal_Object GBGal_EmptyListObject();

/* Out args of various sorts. */

typedef struct __gbgal_result_array {
  Gal_Frame frame;
  GalIO_MsgType msg_type;
  int status;
  GalIO_CommStruct *comm_struct;
} GBGal_ResultArray;

Gal_Frame GBGal_ResultArrayFrame(GBGal_ResultArray *r);
int GBGal_ResultArrayStatus(GBGal_ResultArray *r);
GalIO_CommStruct *GBGal_ResultArrayCommStruct(GBGal_ResultArray *r);
GalIO_MsgType GBGal_ResultArrayMsgType(GBGal_ResultArray *r);
void GBGal_FreeResultArray(GBGal_ResultArray *r);

GBGal_ResultArray *GBGalIO_CommReadFrame(GalIO_CommStruct *gcomm, int do_block);
GBGal_ResultArray *GBGalIO_CommReadMessage(GalIO_CommStruct *gcomm, int do_block);
GBGal_ResultArray *GBGalIO_DispatchViaHub(GalIO_CommStruct *gcomm, Gal_Frame frame);
GBGal_ResultArray *GBGalSS_EnvDispatchFrame(GalSS_Environment *env,
					    Gal_Frame frame);
GBGal_ResultArray *GBGalSS_EnvDispatchFrameToProvider(GalSS_Environment *env,
						      Gal_Frame frame,
						      char *provider);
GBGal_ResultArray *GBGalIO_ClientConnect(char *name,
					 char *host, unsigned short port,
					 int silent,
					 Gal_Frame welcome_frame);
GBGal_ResultArray *GBGalIO_ServerCallbackHandler(GalIO_ServerStruct *gcomm, int read_blocking);

/* Callback support. */

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
						char *slf_name);
GalIO_ServerStruct *
GBGalSS_SetupServer(GalSS_ServerArgs *arg_pkg);
#endif

