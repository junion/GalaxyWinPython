/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _H_GENERIC_SERVER_INTERNAL
#define _H_GENERIC_SERVER_INTERNAL

#include "galaxy/generic-server.h"

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

/* **************************************************************** */

/*
 *  _GalSS_InitializeDefaults is defined in server.h for each server.
 */
void _GalSS_InitializeDefaults(GalIO_ServerStruct *server);

/* Temporary until we update the headers. */

void _GalSS_InitializeDialogueDefaults(GalIO_ServerStruct *server);

/*  
 *  Library internal functions
 */

/* generic-server.c */
Gal_Frame _galss_evaluate_hub_frame_with_data(Gal_Frame frame, void *server_data);

int _GalSS_FrameReturnHandler(GalSS_Environment *env, Gal_Frame frame,
			      Gal_Frame result);

/* frame-util.c */

void _GalSS_EnvAdministerNewMessage(GalSS_Environment *env,
				    Gal_Frame msg, const char *provider_id);
Gal_Frame _GalSS_UpdateNormalReply(GalSS_Environment *env,
				   Gal_Frame res, Gal_Frame frame);

/* server_locations.c */

void _GalSS_FreeServerLocationEntry(GalSS_ServerLocationEntry *entry);

GalSS_ProviderSpec *
_GalSS_PopulateProviderSpec(char *server_chunk,
			    char *location_chunk,
			    GalSS_ProviderSpec *existing_spec);
void _GalSS_ClearProviderSpec(GalSS_ProviderSpec *spec);
void _GalSS_InitializeProviderSpec(GalSS_ProviderSpec *spec);

/* continuation.c */

void _GalSS_ApplyDispatchFnCallbacks(GalSS_Environment *env,
				     Gal_Frame f);

#endif
